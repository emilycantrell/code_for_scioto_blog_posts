# This file counts the number of children in Ohio whose schools participate in CEP or Provision 2, 
# and the number of children eligible for free, reduced, or full-priced meals whose schools do NOT participate in CEP or Provision 2.
# Data from https://education.ohio.gov/Topics/Student-Supports/Food-and-Nutrition/Resources-and-Tools-for-Food-and-Nutrition/Data-for-Free-and-Reduced-Price-Meal-Eligibility

# Load packages, using groundhog for version control
library(groundhog)
groundhog.library(c("readxl", "tidyverse", "ggrepel", "scales"), "2025-04-01")

# Read in the data
meal_data <- read_excel("~/Scioto_Analysis/school_lunch_code/October-2024-FY2025-Data-for-Free-and-Reduced-Price-Meal-Eligibilty.xlsx", 
                       sheet = "Data")

#### DATA CLEANING ####

# Remove the note from the bottom of the Excel sheet
meal_data <- meal_data %>%
  filter(
    !County %in% c(
      "Note:",
      "This data is self reported that the sponsors report in the Claims Reimbursement and Reporting System (CRRS).",
      "The data may be revised and updated at a later date.",
      "Date Posted: 02-12-2025"
    ), 
    !is.na(County)
  )

# If enrollment is "< 10", treat enrollment as 5 since 5 is the midpoint between 1 and 9 
# This applies to only 10 schools 
meal_data <- meal_data %>%
  mutate(Enrollment = if_else(Enrollment == "< 10", 5, as.numeric(Enrollment))) %>%
  # For these schools, calculate the estimated number of students receiving free and 
  # reduced price meals based on the assumption of 5 students enrolled
  mutate(`Free Lunch Applications` = if_else(`Free Lunch Applications` == "< 10", 
                                             round(5*`Percent Free Lunch`), 
                                             as.numeric(`Free Lunch Applications`)),
         `Reduced Price Lunch Applications` = if_else(`Reduced Price Lunch Applications` == "< 10", 
                                                      round(5*`Percent Reduced Price Lunch`), 
                                                      as.numeric(`Reduced Price Lunch Applications`)))

#### CALCULATIONS ####

# Count the number of children enrolled/eligible in each meal program
program_enrollment <- meal_data %>%
  summarise(
    cep_enrollment = sum(if_else(`NSLP Provision` == "Community Eligibility Provision", Enrollment, 0), na.rm = TRUE),
    provision_2_enrollment = sum(if_else(`NSLP Provision` == "Provision 2", Enrollment, 0), na.rm = TRUE),
    free_lunch_applications = sum(if_else(`NSLP Provision` == "Traditional", `Free Lunch Applications`, 0), na.rm = TRUE),
    reduced_price_lunch_applications = sum(if_else(`NSLP Provision` == "Traditional", `Reduced Price Lunch Applications`, 0), na.rm = TRUE),
    enrollment_traditional = sum(if_else(`NSLP Provision` == "Traditional", Enrollment, 0), na.rm = TRUE),
    students_with_no_free_or_reduced_lunch = enrollment_traditional - free_lunch_applications - reduced_price_lunch_applications
  ) %>%
  select(-enrollment_traditional)

#### PIE CHART ####

# Reshape into long form
program_long <- program_enrollment %>%
  pivot_longer(cols = everything(),
               names_to = "category",
               values_to = "students")

# Clean up the labels
program_long <- program_long %>%
  mutate(category = case_when(
    category == "cep_enrollment" ~ "Community Eligibility Provision",
    category == "provision_2_enrollment" ~ "Provision 2",
    category == "free_lunch_applications" ~ "Free Meal Applications",
    category == "reduced_price_lunch_applications" ~ "Reduced-Price Meal Applications",
    category == "students_with_no_free_or_reduced_lunch" ~ "Full-Price Meals",
    TRUE ~ category
  ))

# Calculate percentages
program_long <- program_long %>%
  mutate(
    pct = students / sum(students),
    lbl = paste0(category, ": ", percent(pct, accuracy = 1))
  )

# Print the plot with labels so that I can see which slice corresponds to which category
ggplot(program_long, aes(x = "", y = students, fill = category)) +
  geom_col(width = 1, color = "white") +
  # rotate 90° (pi/2 radians) because this makes it easier to fit the labels on the page
  coord_polar(theta = "y", start = pi/2.15) +  
  geom_text(aes(label = lbl),
            position = position_stack(vjust = 0.5),
            size = 3.5, color = "black") +
  theme_void() +
  theme(legend.position = "none")  # hide legend

# Print the plot without labels; I will add labels manually for easier control over graphic design
ggplot(program_long, aes(x = "", y = students, fill = category)) +
  geom_col(width = 1, color = "white") +
  # rotate to make it easier to fit the labels on the page
  coord_polar(theta = "y", start = pi/2.15) +  
  theme_void() +
  theme(legend.position = "none")  # hide legend

# Save the pie chart 
ggsave("meal_enrollment_pie.png", width = 6, height = 6, dpi = 300)

#### QUALITY ASSURANCE CHECKS ####

# Check that the enrollment numbers add up correctly 
sum(meal_data$Enrollment) == rowSums(program_enrollment)

# If the school lunch data is close to a complete accounting of schools in the state, the
# total count of children should be somewhere between 1.68 million (Ohio public school enrollment) 
# and 1.9 million (total Ohio school age population), to align with statistics from 
# https://nces.ed.gov/programs/digest-dashboard/state/ohio
total_enrollment <- meal_data %>%
  summarise(total_enrollment = sum(Enrollment, na.rm = TRUE)) %>%
  pull(total_enrollment)
total_enrollment > 1680000 & total_enrollment < 1900000

# Note: The row Della School of Coding and Design says the proportion of students who receive
# reduced price lunch is 0.001779, which does not make sense for 60 students as it corresponds
# to 0.1 students. The row lists "Reduced Price Lunch Applications" as 0, so I assume 0 reduced
# price lunches for the school.