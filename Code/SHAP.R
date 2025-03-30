library(tidyverse)
tryCatch({
  library(rstudioapi)
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}, error=function(cond){message(paste("cannot change working directory"))
})
#Load Student Information with Graduation Predictions
data_w_predictions <- read.csv("data_w_predictions.csv")

#Load Model Input Student Aggregated Information for All students
model_input <- read.csv("model_input.csv")

# Filter to at risk students
at_risk_students <- data_w_predictions %>%
  filter(Prob_Did_Not_Graduate >= 0.50) %>%
  select(Student_ID) %>%
  unique()

SHAP_set <- model_input %>%
  filter(Student_ID %in% at_risk_students$Student_ID)

explainer_path<- "C:\\Users\\nicko\\OneDrive - University of Nebraska at Omaha\\Documents\\Classes\\Project Management\\Course Project\\Project Data\\MPS data model\\Model\\Model - Base\\rf_explainer.rds"

model_explainer <- readRDS(explainer_path)

student_feature_contribution <- data.frame()
# Process each student one by one with SHAP values
for (i in 1:nrow(SHAP_set)) {
  # Get SHAP values for this student
  student_shap <- predict_parts(
    explainer = model_explainer,
    new_observation = SHAP_set[i, ],
    type = "shap"
  )
  
  print("Done with Student")
  
  # Convert to data frame
  student_df <- as.data.frame(student_shap)
  
  # Add student ID and other metadata
  student_df$Student_ID <- SHAP_set$Student_ID[i]
  student_df$Model_ID <- "RF_Graduation_v1"
  student_df$Prediction_ID <- i
  
  # Append to the main results
  student_feature_contribution <- rbind(student_feature_contribution, student_df)
}
# Clean up column names if needed
names(student_feature_contribution)[names(student_feature_contribution) == "variable"] <- "Feature_ID"
names(student_feature_contribution)[names(student_feature_contribution) == "contribution"] <- "Contribution_Score"

# Create a dataframe with variable names and descriptions
variable_descriptions <- data.frame(
  variable_name = c(
    "UnexcusedAbs", "SocialSciences", "Avg_GPA", "Math", "RepeatedCourses", 
    "Sci", "Suspensions", "English", "NumberFours", "Tardies", 
    "SummerSchool_Y", "Guidance", "Academy", "Majors", 
    "Last_FreeAndReducedStatus", "ExcusedAbs", "SPED", "ACP", 
    "ELL", "NumberFives", "ProgramABC"
  ),
  description = c(
    "The total number of unexcused absences",
    "The average number of social science credits completed each semester",
    "The student's average GPA at the end of each semester",
    "The average number of math credits completed each semester",
    "The total number of repeated courses in the student's high school career",
    "The average number of science credits completed each semester",
    "The total number of suspensions in the student's high school career",
    "The average number of english credits completed each semester",
    "The total number of Fours in the student's high school career",
    "The total number of tardies in the student's high school career",
    "The total number of summer school sessions attended by the student",
    "Guidance Counselor visits: low = 0-1, medium = 2-7, high = 8-12, very high = 13+",
    "The total number of semesters the student was part of Millard Academy",
    "The total number of major behavioral referrals in high school",
    "The student's most recent Free and Reduced Lunch status",
    "The total number of excused absences in the student's high school career",
    "The student's most recent Special Education status",
    "The student's most recent ACP status",
    "The student's most recent ELL status",
    "The total number of Fives in the student's high school career",
    "The total number of semesters the student participated in ABC Programs"
  )
)

averaged_student_feature_importance <- student_feature_contribution %>%
  # First calculate average contribution by Student_ID and Feature_ID
  group_by(Student_ID, Feature_ID) %>%
  summarize(
    avg_contribution = mean(Contribution_Score, na.rm = TRUE),
    sign = if(mean(Contribution_Score, na.rm = TRUE) >= 0) 1 else -1,
    variable_value = first(variable_value),
    variable_name = first(variable_name),
    .groups = "drop"  # Changed NULL to "drop" for clarity
  ) %>%
  # Then group by Student_ID and sign for ranking
  group_by(Student_ID, sign) %>%
  # Arrange by absolute contribution value
  arrange(desc(abs(avg_contribution)), .by_group = TRUE) %>%
  # Add ranking within each Student_ID and sign group
  mutate(feature_rank = row_number()) %>%
  ungroup() %>%
  # Join with the descriptions dataframe
  left_join(variable_descriptions, by = "variable_name")

averaged_student_feature_importance <- student_feature_contribution %>%
  # First calculate average contribution by Student_ID and Feature_ID
  group_by(Student_ID, Feature_ID) %>%
  summarize(
    avg_contribution = mean(Contribution_Score, na.rm = TRUE),
    sign = if(mean(Contribution_Score, na.rm = TRUE) >= 0) 1 else -1,
    .groups = NULL,
    variable_value = first(variable_value),
    variable_name = first(variable_name)
  ) %>%
  # Then group by Student_ID and sign for ranking
  group_by(Student_ID, sign) %>%
  # Arrange by absolute contribution value
  arrange(desc(abs(avg_contribution)), .by_group = TRUE) %>%
  # Add ranking within each Student_ID and sign group
  mutate(feature_rank = row_number()) %>%
  ungroup()



write_csv(averaged_student_feature_importance, "student_feature_importance.csv")
