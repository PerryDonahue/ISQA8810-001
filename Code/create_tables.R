library(tidyverse)
tryCatch({
  library(rstudioapi)
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}, error=function(cond){message(paste("cannot change working directory"))
})

data <- read.csv("data_w_predictions.csv")
current_school_year <- "2022-23"
current_year <- data %>% filter (SchoolYear == current_school_year)
write_csv(current_year, "current_year.csv")


credit_data <- data %>%
  group_by(Student_ID) %>%
  slice_tail(n = 2) %>%
  mutate(credit_ratio = ( Credits_Completed/ Credits_Attempted)) %>%
  group_by(Student_ID) %>%
  summarise(avg_credit_completion_ratio = mean(credit_ratio, na.rm = TRUE))

quartiles <- quantile(credit_data$avg_credit_completion_ratio, probs = c(0.02, 0.05,0.15, .2), na.rm = TRUE)
print(quartiles)
credit_data <- credit_data %>%
  mutate(avg_credit_completion_ratio = case_when(
    is.na(avg_credit_completion_ratio) ~ "low", 
    avg_credit_completion_ratio <= .6 ~ "low",
    avg_credit_completion_ratio <= .85 ~ "medium",
    avg_credit_completion_ratio >.85 ~ "high",
    TRUE ~ as.character(avg_credit_completion_ratio)))


data <- data %>%
  mutate(SummerSchool_Y = ifelse(SummerSchool == 'Y', 1, 0),
         CountA = ifelse(Program.A == 'Y', 1, 0),
         CountB = ifelse(Program.B == 'Y', 1, 0),
         CountC = ifelse(Program.C == 'Y', 1, 0),
         countAcademy = ifelse(Academy == 'Y', 1, 0),
         English = rowSums(select(., ELA09A, ELA09B, ELA10A, ELA10B, ELA11A, ELA11B, ELAORALCOMM, EELAELECT, FUNDELA), na.rm = TRUE),
         Math = rowSums(select(., MA220 , MATH , FUNDMATH), na.rm = TRUE),
         Sci = rowSums(select(., BIOA, BIOB , SCIENCE , FUNDSCIENCE), na.rm = TRUE),
         SocialSciences=rowSums(select(., WORLDGEO,WORLDHISA, WORLDHISB, USHISA, USHISB, USGOV, FUNDSOCIAL,FINANCIALLIT ,FINEARTS), na.rm = TRUE),
         
         Guidance_Bin = case_when(
           Guidance <= 1 ~ "low",
           Guidance <= 7 ~ "medium",
           Guidance <= 12 ~ "high",
           Guidance > 12 ~ "very_high",
           TRUE ~ as.character(Guidance)),  # This line handles any unexpected cases
         # This line handles any unexpected cases
         Nurse_Bin = case_when(
           Nurse <= 1 ~ "low",
           Nurse <= 7 ~ "medium",
           Nurse <= 12 ~ "high",
           Nurse > 12 ~ "very_high",
           TRUE ~ as.character(Nurse))
         
  )

data_train <- data %>%
  group_by(Student_ID) %>%
  mutate(First_Graduated = first(Graduated)) %>%  # Capture the first 'Graduated' status per Student_ID
  summarise(SummerSchool_Y = sum(SummerSchool_Y),          # Sum up 'Y' in SummerSchool
            ProgramABC = sum(CountA + CountB + CountC),
            Academy = sum(countAcademy),
            Max_Moves = max(Moves, na.rm = TRUE),
            Last_FreeAndReducedStatus = last(FreeAndReducedStatus),
            ELL=last(ELL),
            ACP=last(ACP),
            SPED=last(SPED),
            Guidance=last(Guidance_Bin),
            NumberFours=last(NumberFours),
            NumberFives=last(NumberFives),
            Nurse=last(Nurse_Bin),
            NumberFives=sum(NumberFives),
            NumberFours=sum(NumberFours),
            RepeatedCourses=sum(RepeatedCourses),
            Avg_GPA = mean(gpa, na.rm = TRUE),
            Suspensions=sum(Suspensions),
            Majors=sum(Majors),
            ExcusedAbs=sum(ExcusedAbs),
            UnexcusedAbs=sum(UnexcusedAbs),
            Tardies=sum(Tardies),
            English=mean(English),
            Math=mean(Math),
            Sci=mean(Sci),
            SocialSciences=mean(SocialSciences),
            Graduated = first(First_Graduated))   # Get the first 'Graduated' captured



data_train <- data_train %>%
  left_join(credit_data, by = "Student_ID")

write_csv(data_train, "model_input.csv")
