# Load packages, using groundhog for version control
library(groundhog)
groundhog.library(c("readxl", "tidyverse"), "2025-04-01")

#### CALCULATE 2015 CEP PARTICIPATION RATE ####

# Load data downloaded from https://education.ohio.gov/Topics/Student-Supports/Food-and-Nutrition/Resources-and-Tools-for-Food-and-Nutrition/Data-for-Free-and-Reduced-Price-Meal-Eligibility
meal_data_2015 <- read_excel("~/Scioto_Analysis/school_lunch_code/October_2015_-Data_for_Free-and-Reduced-Price-Meal.xlsx", 
                       sheet = "Data")

# Remove the note from the bottom of the Excel sheet
meal_data_2015 <- meal_data_2015 %>%
  filter(!is.na(County)) # Removes a note in the Sponsor IRN column & blank rows

# If enrollment is "<10", treat enrollment as 5 since 5 is the midpoint between 1 and 9 
# This applies to very few schools
meal_data_2015 <- meal_data_2015 %>%
  mutate(
    Enrollment = case_when(
      Enrollment == "<10" ~ 5,
      Enrollment == "-" ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(Enrollment))
    )
  )

# Calculate the proportion of children in schools that participate in CEP 
cep_enrollment_2015 <- meal_data_2015 %>%
  filter(`NSLP Provision` == "Community Eligibility Provision") %>%
  summarise(cep_enrollment = sum(as.numeric(Enrollment), na.rm = TRUE)) %>%
  pull(cep_enrollment)
total_enrollment_2015 <- meal_data_2015 %>%
  summarise(total_enrollment = sum(as.numeric(Enrollment), na.rm = TRUE)) %>%
  pull(total_enrollment)
print(cep_enrollment_2015/total_enrollment_2015)

#### CALCULATE 2024 CEP PARTICIPATION RATE ####

# Load data downloaded from https://education.ohio.gov/Topics/Student-Supports/Food-and-Nutrition/Resources-and-Tools-for-Food-and-Nutrition/Data-for-Free-and-Reduced-Price-Meal-Eligibility
meal_data_2024 <- read_excel("~/Scioto_Analysis/school_lunch_code/October-2024-FY2025-Data-for-Free-and-Reduced-Price-Meal-Eligibilty.xlsx", 
                       sheet = "Data")

# Remove the note from the bottom of the Excel sheet
meal_data_2024 <- meal_data_2024 %>%
  filter(!is.na(`Sponsor IRN`)) # Removes a note in the County column & blank rows

# If enrollment is "< 10", treat enrollment as 5 since 5 is the midpoint between 1 and 9 
# This applies to very few schools
# Note an additional space in the string compared to the 2015 data ("<10" vs. "< 10")
meal_data_2024 <- meal_data_2024 %>%
  mutate(
    Enrollment = case_when(
      Enrollment == "< 10" ~ 5,
      Enrollment == "-" ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(Enrollment))
    )
  )

# Calculate the proportion of children in schools that participate in CEP 
cep_enrollment_2024 <- meal_data_2024 %>%
  filter(`NSLP Provision` == "Community Eligibility Provision") %>%
  summarise(cep_enrollment = sum(as.numeric(Enrollment), na.rm = TRUE)) %>%
  pull(cep_enrollment)
total_enrollment_2024 <- meal_data_2024 %>%
  summarise(total_enrollment = sum(as.numeric(Enrollment), na.rm = TRUE)) %>%
  pull(total_enrollment)
print(cep_enrollment_2024)
print(cep_enrollment_2024/total_enrollment_2024)

#### EXAMINE IDENTIFIED STUDENT PERCENTAGES ####

# In CEP participating schools, what percent of students are eligible (i.e. to make the school eligible for CEP)?
# This is the "identified student percentage (ISP)"
cep_participating_schools <- meal_data_2024 %>%
  filter(`NSLP Provision` == "Community Eligibility Provision") %>%
  mutate(percent_cep_eligible = `CEP Eligible Students` / Enrollment)

# What proportion of schools have at least 62.5% of students eligible for CEP? 
# This is the threshold to guarantee full federal reimbursement, although 
# since CEP can be administered at the district level, the threshold at an individual 
# school does not always determine the level of reimbursement 
mean(cep_participating_schools$percent_cep_eligible >= 0.625, na.rm = TRUE) * 100

# Calculate median
median_value <- median(cep_participating_schools$percent_cep_eligible, na.rm = TRUE)
print(median_value)

# Create dummy data frame for the median line on the plot
median_df <- data.frame(x = median_value)

# Generate plot
cep_participating_schools %>%
  ggplot(aes(x = percent_cep_eligible)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))), 
    fill = "#007ACC", color = "white", bins = 50
  ) +
  geom_vline(data = median_df, aes(xintercept = x, color = "Median"), linetype = "dashed", size = 1, show.legend = TRUE) +
  scale_color_manual(name = NULL, values = c("Median" = "#D64545")) +
  guides(color = guide_legend(override.aes = list(linetype = "dashed", size = 1))) +
  labs(
    x = "Percent of Students in the School Who are \"Identified\" for CEP Eligibility",
    y = "Percent of CEP-Participating Ohio Schools"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 12)
  )