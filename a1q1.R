#import libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stargazer)
library(reshape2)
library(scales)

#Q1
#Setup
rm(list = ls())
set.seed(33410712) 
VCData = read.csv("/Users/paramithaaa/Documents/Monash/Academics/FIT3152/Assignment 1/WVSExtract.csv")
VC = VCData[sample(1:nrow(VCData),50000, replace=FALSE),]
VC = VC[,c(1:6, sort(sample(7:46,17, replace = FALSE)), 47:53, sort(sample(54:69,10, replace = FALSE)))]

#Overview
nrow(VC) #50000 rows
ncol(VC) #40 columns
str(VC)

#Print Variable Types
variable_types = data.frame(
  Variable = names(VC),
  Data_Type = sapply(VC, class)
)
print(variable_types)

#Cleaning
#Check if not answered (-2) is random
no_answer = sapply(VC, function(x) sum(x == -2, na.rm = TRUE))
no_answer_proportion = no_answer / nrow(VC)
no_answer_proportion

subset = VC[VC$Country == "CZE", ]
no_answer = sapply(subset, function(x) sum(x == -2, na.rm = TRUE))
no_answer_proportion = no_answer / nrow(subset)
no_answer_proportion

#missing values
missing_codes = sapply(VC, function(x) sum(x %in% c(-2, -3, -4, -5)))
missing_codes
VC = VC %>% mutate(MF = ifelse(MF == -5, "Others", MF))

# Delete N/A
VC = VC %>% filter(if_all(everything(), ~ !.x %in% c(-2, -3, -4, -5)))
VC = VC %>% mutate(across(-c(MF, Country), ~ as.integer(as.character(.))))
str(VC)
missing_codes = sapply(VC, function(x) sum(x %in% c(-2, -3, -4, -5)))
missing_codes

#Calculate "don't know" or "-1" values
unique_values = sapply(VC, function(x) sum(x == -1, na.rm = TRUE))
unique_values
cols = c("MF", "Age", "Edu", "Employment")
VC = VC %>% filter(if_all(all_of(cols), ~ . != -1))

# Remove rows where PIA == PIAB (mutually exclusive)
VC = VC %>% filter(PIA != PIAB)

#Gender
VC = VC %>% mutate(MF = ifelse(MF == 1, "Male", ifelse(MF == 2, "Female", ifelse(MF == -5, "Others", MF))))
VC_gender = data.frame(table(VC$MF))
VC_gender

ggplot(VC_gender, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("Others" = "grey", "Male" = "blue", "Female" = "purple")) +
  theme_minimal() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))

#Age
VC$Age = as.numeric(VC$Age)
summary(VC$Age)
ggplot(VC, aes(x = Age, fill = factor(MF))) + geom_histogram(binwidth = 2, position = "dodge", alpha = 0.5) +
  labs(title = "Age Distribution of Survey Respondents by Gender", x = "Age", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("Others" = "grey", "Male" = "blue", "Female" = "purple")) +
  theme_minimal()

#Education
edu_labels = c(
  "0. Early childhood / No education",
  "1. Primary education",
  "2. Lower secondary",
  "3. Upper secondary",
  "4. Post-secondary non-tertiary",
  "5. Short-cycle tertiary",
  "6. Bachelor or equivalent",
  "7. Master or equivalent",
  "8. Doctoral or equivalent"
)

VC_Edu = data.frame(table(VC$Edu))
VC_Edu$Edu_Label = factor(VC_Edu$Var1, levels = 0:8, labels = edu_labels)

ggplot(VC_Edu, aes(x = Edu_Label, y = Freq, fill = Edu_Label)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  labs(title = "Distribution of Education Levels", x = "Education Level", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14), legend.position = "none", plot.title = element_text(size = 14, face = "bold"))

#Employment
employment_labels = c(
  "1. Full time",
  "2. Part time",
  "3. Self employed",
  "4. Retired/pensioned",
  "5. Housewife",
  "6. Student",
  "7. Unemployed",
  "8. Other"
)

VC_Employment = data.frame(table(VC$Employment))
VC_Employment$Employment_Label = factor(VC_Employment$Var1, levels = 1:8, labels = employment_labels)

ggplot(VC_Employment, aes(x = Employment_Label, y = Freq, fill = Employment_Label)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  labs(title = "Distribution of Employment Type", x = "Employment Type", y = "Count") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold")
  )

#Get mode
get_mode <- function(x) {
  uniq_v <- unique(x)
  uniq_v[which.max(tabulate(match(x, uniq_v)))]
}

summarize <- function(df, vars, category_name) {
  cat(paste0("\n--- ", category_name, " ---\n"))
  
  stats <- lapply(vars, function(var) {
    values <- df[[var]]
    data.frame(
      Variable = var,
      Mean = round(mean(values, na.rm = TRUE), 2),
      Median = median(values, na.rm = TRUE),
      IQR = IQR(values, na.rm = TRUE),
      Min = min(values, na.rm = TRUE),
      Max = max(values, na.rm = TRUE),
      Mode = get_mode(values),
      Category = category_name
    )
  })
  
  summary_table = do.call(rbind, stats)
  return(summary_table)
  
  modes = sapply(df[vars], get_mode)
  return(data.frame(Variable = vars, Mode = modes, Category = category_name))
}

# Boxplots
plot_boxplots <- function(df, vars, title) {
  df_long <- df %>%
    pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") %>%
    mutate(Variable = reorder(Variable, Value, FUN = median))
  
  ggplot(df_long, aes(x = Variable, y = Value, fill = Variable)) +
    geom_boxplot() +
    stat_summary(fun = median, geom = "crossbar", width = 0.5, color = "orange", size = 0.4) +
    scale_fill_brewer(palette = "Blues") +
    theme_minimal() +
    labs(title = title, x = "", y = "Values") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
}

# Variables per category
confidence_vars = c("CArmedForces", "CUnions", "CPolice", "CPParties", "CParliament", 
                     "CCivilService", "CUniversities", "CElections", "CBanks", "CEnvOrg")
interpersonal_vars = c("TPeople", "TFamily", "TNeighbourhood", "TKnow", "TMeet")
importance_vars = c("VFamily", "VLeisure", "VWork")
wellbeing_vars = c("HOverall", "HChoice", "HShelter")
economic_vars = c("EEquality", "EGovernment", "EHardWork")
education_security_vars = c("SEducation")
priority_vars = c("PIA", "PIAB")
science_vars = c("STBetter")
news_source_vars = c("PRadio", "PEmail", "PInternet", "PSocial")
democracy_vars = c("PDemImp", "PDemCurrent", "PSatisfied")

# Run summaries + get modes + plot for each category

summary = summarize(VC, confidence_vars, "Confidence")
plot_boxplots(VC, confidence_vars, "Confidence in Institutions")

summary = summarize(VC, interpersonal_vars, "Interpersonal Trust")
plot_boxplots(VC, interpersonal_vars, "Interpersonal Trust")

summary = summarize(VC, importance_vars, "Importance in Life")
plot_boxplots(VC, importance_vars, "Importance in Life")

summary = summarize(VC, wellbeing_vars, "Well-being")
plot_boxplots(VC, wellbeing_vars, "Well-being")

summary = summarize(VC, economic_vars, "Economic Attitudes")
plot_boxplots(VC, economic_vars, "Economic Attitudes")

summary = summarize(VC, education_security_vars, "Education Security")
plot_boxplots(VC, education_security_vars, "Education Security")

summary = summarize(VC, priority_vars, "Priorities")
plot_boxplots(VC, priority_vars, "Priorities")

summary = summarize(VC, science_vars, "Trust in Science")
plot_boxplots(VC, science_vars, "Trust in Science")

summary = summarize(VC, news_source_vars, "News Sources")
plot_boxplots(VC, news_source_vars, "News Sources")

summary = summarize(VC, democracy_vars, "Democracy")
plot_boxplots(VC, democracy_vars, "Democracy")

