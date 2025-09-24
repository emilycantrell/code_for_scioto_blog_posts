library(tidyverse)
library(triangle)
library(mc2d)

#### MONTE CARLO SIMULATION FOR SCHOOL LUNCH COST/BENEFIT ANALYSIS ####
# For notes and additional information, see: 
# https://docs.google.com/spreadsheets/d/1dOL8Gw6EUNaj-Atetg23RMIF14dlg7UiZDVQDYSsvEk/edit?gid=722151259#gid=722151259

#### BASIC INPUTS #### 

# Basic inputs & outputs
state_spending <- 300000000 # Estimate based on Senate Bill 109 https://search-prod.lis.state.oh.us/api/v2/general_assembly_136/legislation/sb109/00_IN/pdf/
benefits_to_residents <- state_spending # Most or all of this spending goes directly to Ohio residents

# Marginal excess burden of taxation
# These values come from Table 16-11 of Boardman et al., Cost-Benefit Analysis Concepts and Practice, 4th edition
# I will use a triangular distribution. 
mbet_mode = 0.185
mbet_min = 0.115
mbet_max = 0.430

# Discount rate
# The mode & minimum value & the choice to use a triangular distribution are based on 
# WSIPP Cost-Benefit Technical Documentation https://www.wsipp.wa.gov/TechnicalDocumentation/WsippBenefitCostTechnicalDocumentation.pdf
# The maximum value is based on federal guidance for the discount rate.
discount_rate_mode <- 0.035
discount_rate_min <- 0.020
discount_rate_max <- 0.070 

#### OBESITY REDUCTION ####

# Estimates for how universal meals affect obesity
# Localio et al. https://publications.aap.org/pediatrics/article/153/4/e2023063749/196881/Universal-Free-School-Meals-Policy-and-Childhood?autologincheck=redirected
# The CI is symmetric, so I'm assuming a normal distribution.
mean_obesity_reduction <- 0.0060 # 0.6 percentage point reduction in obesity rate
obesity_reduction_ci_lower <- 0.0014 # upper/lower are switched compared to the paper since I am using them as positive numbers here
obesity_reduction_ci_upper <- 0.0107
se_obesity_reduction <- (obesity_reduction_ci_upper - obesity_reduction_ci_lower) / (2 * 1.96)

# Estimates for how obesity affects healthcare costs
# Finkelstein & Trogdon https://pmc.ncbi.nlm.nih.gov/articles/PMC2253570/ 
# I used the BLS calculator to translate $220 (CI: $30, $450) from June 2006 dollars to April 2025 dollars
# The CI is not symmetric, so I will use a PERT distribution to allow for skewness. 
estimated_healthcare_cost_associated_with_obesity <- 347.83
healthcare_cost_ci_lower <- 47.43
healthcare_cost_ci_upper <- 711.47

# Total number of public school students (used to estimate number of reduced obesity cases)
number_of_public_school_students_in_Ohio <- 1551386 # https://ohiobythenumbers.com/assets2025/media/2025-Ohio-EDU-Numbers.pdf 

#### INCREASE IN STUDENT LIFETIME EARNINGS ####

# Universal meal test score improvements: https://www.sciencedirect.com/science/article/abs/pii/S0272775719307605
improvement_in_math_scores_estimate <- 0.061 # 0.061 standard deviation improvement for grades 3-5
improvement_in_math_scores_se <- 0.014

# Earnings associated with test score improvement
# Earnings and test scores: https://journals.sagepub.com/doi/full/10.1177/2332858420928985
men_earnings_for_1_sd_math_increase_estimate <- 0.070
men_earnings_for_1_sd_math_increase_se <- 0.011
women_earnings_for_1_sd_math_increase_estimate <- 0.048
women_earnings_for_1_sd_math_increase_se <- 0.013

# Enrollment
# https://ohiobythenumbers.com/assets2025/media/2025-Ohio-EDU-Numbers.pdf 
third_grade_enrollment <- 117582
fourth_grade_enrollment <- 121093
fifth_grade_enrollment <- 122020

# Average annual earnings in Ohio
# https://www.bls.gov/charts/state-employment-and-unemployment/average-hourly-earnings-and-weekly-hours-and-earnings-by-state.htm
average_annual_earnings <- 1126.21*52

# Age assumptions
current_age_3rd_grade <- 8
current_age_4th_grade <- 9
current_age_5th_grade <- 10

# Years until students reach age 33 or 55 (the study examines earnings increase in this age range)
years_until_age_33_3rd <- 33 - current_age_3rd_grade
years_until_age_33_4th <- 33 - current_age_4th_grade
years_until_age_33_5th <- 33 - current_age_5th_grade
years_until_age_55_3rd <- 55 - current_age_3rd_grade
years_until_age_55_4th <- 55 - current_age_4th_grade
years_until_age_55_5th <- 55 - current_age_5th_grade

#### FAMILY MEAL PREP TIME ####
# Note: for the estimated amount of time families would save on meal preparation,
# I use the original point estimates rather than creating a distribution for the Monte Carlo, 
# because there are no uncertainty estimates available and no established range of reasonable values.

# Value of time saved from parent lunch prep
# These values come from O'Keefe et al.: https://schoolnutrition.org/wp-content/uploads/2022/06/Comparison-of-Costs-Between-School-and-Packed-Lunches-Spring2020.pdf
# As a simple assumption, I will create a PERT distribution where the minimum is 0 minutes (i.e., parent would not have prepared a lunch),
# and the maximum is twice the estimated prep time from O'Keefe (this might reflect additional time spent on shopping, dish washing, etc.).
# Note: an alternative approach would be to estimate the distribution shape from the box plot in O'Keefe et al.'s Figure 2.
convenience_lunch_prep_time_estimate <- 1.25 # minutes, from O'Keefe et al.
proportion_making_convenience_lunches <- 0.343 # from O'Keefe et al.
homemade_lunch_prep_time_estimate <- 4 # minutes, from O'Keefe et al.
proportion_making_homemade_lunches <- 0.657 # from O'Keefe et al.
lunch_prep_time_estimate <- convenience_lunch_prep_time_estimate*proportion_making_convenience_lunches + 
  homemade_lunch_prep_time_estimate*proportion_making_homemade_lunches # weighted average

# Value of time saved from parent breakfast prep
# I could not find evidence for how much time parents spend preparing breakfast, so I will use the same values as for 
# convenience lunch prep time, assuming that options like cereal or yogurt are similar to a "convenience" lunch.
breakfast_prep_time_estimate <- convenience_lunch_prep_time_estimate

# Average hourly earnings in Ohio in 2025
# https://www.bls.gov/charts/state-employment-and-unemployment/average-hourly-earnings-and-weekly-hours-and-earnings-by-state.htm
average_hourly_earnings <- 33.22 # in dollars

# Calculate monetary value of prep time saved per person
lunch_prep_time_monetary_value_per_person_per_day <- lunch_prep_time_estimate * (average_hourly_earnings / 60)
breakfast_prep_time_monetary_value_per_person_per_day <- breakfast_prep_time_estimate * (average_hourly_earnings / 60)

# Estimate how many kids would take up school lunch & breakfast who previously ate food from home
# Current participation rates: https://frac.org/wp-content/uploads/Reach-Report-2025.pdf 
# Note that the average daily participation rates in the FRAC report include CEP schools, so they may overestimate participation in non-CEP schools.
current_free_school_breakfast_participation <- 336446 # number of kids currently participating in free school breakfast in Ohio
current_reduced_price_breakfast_participation <- 23267 
current_full_price_breakfast_participation <- 104389 
total_avg_daily_breakfast_participation <- sum(current_free_school_breakfast_participation,
                                             current_reduced_price_breakfast_participation,
                                             current_full_price_breakfast_participation)

current_free_school_lunch_participation <- 555292
current_reduced_price_lunch_participation <- 51591
current_full_price_lunch_participation <- 341472
total_avg_daily_lunch_participation <- sum(current_free_school_lunch_participation,
                                          current_reduced_price_lunch_participation,
                                          current_full_price_lunch_participation)

# Changes in participation through CEP: Logan et al. 2014 (report to USDA): https://fns-prod.azureedge.us/sites/default/files/CEPEvaluation.pdf
proportion_increase_in_breakfast_participation_due_to_CEP <- 0.094
proportion_increase_in_lunch_participation_due_to_CEP <- 0.052

# Multiply current participation by estimated change in participation 
additional_average_daily_breakfast_participation_estimate <- total_avg_daily_breakfast_participation * proportion_increase_in_breakfast_participation_due_to_CEP
additional_average_daily_lunch_participation_estimate <- total_avg_daily_lunch_participation * proportion_increase_in_lunch_participation_due_to_CEP

# School days per year
# https://www.pewresearch.org/short-reads/2023/09/07/in-the-u-s-180-days-of-school-is-most-common-but-length-of-school-day-varies-by-state/
school_days_per_year <- 182

# Put it all together
lunch_prep_time_total_monetary_value <- 
  lunch_prep_time_monetary_value_per_person_per_day * 
  additional_average_daily_lunch_participation_estimate * 
  school_days_per_year 

breakfast_prep_time_total_monetary_value <- 
  breakfast_prep_time_monetary_value_per_person_per_day * 
  additional_average_daily_breakfast_participation_estimate * 
  school_days_per_year

#### MONTE CARLO ####

replications <- 1000000

# Create storage vectors
mbet_cost_vec <- numeric(replications)
total_healthcare_savings_vec <- numeric(replications)
pv_earnings_vec <- numeric(replications)

# Simulation

set.seed(43235)
for(i in 1:replications) { 
  #### MBET and discount rate ####
  mbet <- rtriangle(n = 1, a = mbet_min, b = mbet_max, c = mbet_mode)
  discount_rate <- rtriangle(n = 1, a = discount_rate_min, b = discount_rate_max, c = discount_rate_mode)

  # Apply MBET
  mbet_cost <- state_spending * mbet
  
  #### Obesity reduction ####
  
  # To be conservative, I am assuming the effects on obesity last only in the year when the program is implemented. 
  obesity_reduction <- rnorm(n = 1, mean = mean_obesity_reduction, sd = se_obesity_reduction)
  healthcare_cost_per_obesity_case <- rpert(n = 1, min = healthcare_cost_ci_lower, mode = estimated_healthcare_cost_associated_with_obesity, max = healthcare_cost_ci_upper)
  total_healthcare_savings <- number_of_public_school_students_in_Ohio * obesity_reduction * healthcare_cost_per_obesity_case

  #### Student lifetime earnings ####
  
  # Test score & earnings impacts
  improvement_in_math_scores <- rnorm(n = 1, mean = improvement_in_math_scores_estimate, sd = improvement_in_math_scores_se)
  men_earnings_for_1_sd_math_increase <- rnorm(n = 1, mean = men_earnings_for_1_sd_math_increase_estimate, sd = men_earnings_for_1_sd_math_increase_se)
  women_earnings_for_1_sd_math_increase <- rnorm(n = 1, mean = women_earnings_for_1_sd_math_increase_estimate, sd = women_earnings_for_1_sd_math_increase_se)
  earnings_for_1_sd_math_increase <- (men_earnings_for_1_sd_math_increase + women_earnings_for_1_sd_math_increase) / 2 # average between men & women assuming 50% of population each
  earnings_gained_per_year <- improvement_in_math_scores * earnings_for_1_sd_math_increase * average_annual_earnings
  
  # Vector of discount factors to apply to lifetime earnings
  discount_factors_3rd <- 1 / ((1 + discount_rate) ^ (years_until_age_33_3rd:years_until_age_55_3rd))
  discount_factors_4th <- 1 / ((1 + discount_rate) ^ (years_until_age_33_4th:years_until_age_55_4th))
  discount_factors_5th <- 1 / ((1 + discount_rate) ^ (years_until_age_33_5th:years_until_age_55_5th))
  
  # Present value of earnings gains per student
  pv_earnings_3rd <- earnings_gained_per_year * sum(discount_factors_3rd)
  pv_earnings_4th <- earnings_gained_per_year * sum(discount_factors_4th)
  pv_earnings_5th <- earnings_gained_per_year * sum(discount_factors_5th)
  
  # Total present value across all students
  total_pv_earnings_gained <- pv_earnings_3rd * third_grade_enrollment +
                              pv_earnings_4th * fourth_grade_enrollment +
                              pv_earnings_5th * fifth_grade_enrollment
  
  #### Store results ####
  mbet_cost_vec[i] <- mbet_cost
  total_healthcare_savings_vec[i] <- total_healthcare_savings
  pv_earnings_vec[i] <- total_pv_earnings_gained
  }

# Sum the costs and benefits
total_costs_vec <- state_spending + mbet_cost_vec
total_benefits_vec <- benefits_to_residents + total_healthcare_savings_vec + pv_earnings_vec +
  lunch_prep_time_total_monetary_value + breakfast_prep_time_total_monetary_value
net_benefits_vec <- total_benefits_vec - total_costs_vec

# Make histogram of costs
ggplot(data = tibble(total_costs = total_costs_vec), aes(x = total_costs)) +
  geom_histogram(bins = 50, fill = "red", color = "black") +
  labs(title = "Histogram of Costs from Universal School Meals",
       x = "Total Costs (in dollars)",
       y = "Frequency") +
  theme_minimal()

# Make histogram of benefits
ggplot(data = tibble(total_benefits = total_benefits_vec), aes(x = total_benefits)) +
  geom_histogram(bins = 50, fill = "green", color = "black") +
  labs(title = "Histogram of Benefits from Universal School Meals",
       x = "Total Benefits (in dollars)",
       y = "Frequency") +
  theme_minimal()

# Make histogram of net benefits
ggplot(data = tibble(net_benefits = net_benefits_vec), aes(x = net_benefits)) +
  geom_histogram(bins = 50, fill = "blue", color = "black") +
  labs(title = "Histogram of Net Benefits from Universal School Meals",
       x = "Net Benefits (in dollars)",
       y = "Frequency") +
  theme_minimal()

# What is the probability that net benefits are positive?
mean(net_benefits_vec > 0)

# What are the 5th, 50th, and 95th percentiles of net benefits?
quantile(net_benefits_vec, probs = c(0.01, 0.05, 0.5, 0.95, 0.99))

#### NOTES ####

# PERT Distributions
# There is precedent for using PERT distributions for healthcare costs, which are commonly right-skewed, e.g.: 
  # Reynecke, D. P., & Robson, J. P. (2016). Use of the Pert and Uniform Distributions with the Central Limit Theorem (CLT) to Decrease Variance in Complication Cost Estimation in Probabilistic Models. Value in Health, 19(7), A374.
  # Nascimento de Lima, P., Vardavas, R., Katragadda, S. P., Armour, P., Gadwah-Meaden, C., Fateh, B., ... & Hernandez, H. (2024). Cost-Benefit Analysis of Comprehensive Military Eye Examination Policies.
# For another example of PERT distributions in CBAs, see: 
  # Cook, D. C., Fraser, R. W., & McKirdy, S. J. (2021). A benefit–cost analysis of different response scenarios to COVID‐19: A case study. Health Science Reports, 4(2), e286.