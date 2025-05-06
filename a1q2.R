#import libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stargazer)
library(reshape2)
library(scales)

#Q2a
focus_country_data = VC %>% filter(Country == "CZE")
other_countries_data = VC %>% filter(Country != "CZE")

#Functions to compute and plot distributions
compute_distribution <- function(var_names, df_focus, df_other, group_label) {
  bind_rows(
    bind_rows(lapply(var_names, function(var) {
      dist = prop.table(table(df_focus[[var]])) * 100
      df = as.data.frame(dist)
      df$Country_Group = "CZE"
      df$Variable = var
      colnames(df) = c("Level", "Percentage", "Country_Group", group_label)
      df
    })),
    bind_rows(lapply(var_names, function(var) {
      dist = prop.table(table(df_other[[var]])) * 100
      df = as.data.frame(dist)
      df$Country_Group = "Other"
      df$Variable = var
      colnames(df) = c("Level", "Percentage", "Country_Group", group_label)
      df
    }))
  )
}

plot_distribution <- function(data, level_col, group_col, facet_col, title, xlab) {
  data[[level_col]] = as.factor(data[[level_col]])
  
  ggplot(data, aes_string(x = level_col, y = "Percentage", fill = "Country_Group")) +
    geom_col(position = "dodge", alpha = 0.8) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    scale_fill_manual(values = c("CZE" = "darkblue", "Other" = "lightblue")) +
    labs(title = title,
         x = xlab,
         y = "Percentage of Responses",
         fill = "Country Group") +
    theme_minimal() +
    facet_wrap(as.formula(paste("~", facet_col)), scales = "free_y")
}

# Education
edu_labels = c(
  "0" = "ISCED 0: Early childhood / No education",
  "1" = "ISCED 1: Primary",
  "2" = "ISCED 2: Lower secondary",
  "3" = "ISCED 3: Upper secondary",
  "4" = "ISCED 4: Post-secondary non-tertiary",
  "5" = "ISCED 5: Short-cycle tertiary",
  "6" = "ISCED 6: Bachelor or equivalent",
  "7" = "ISCED 7: Master or equivalent",
  "8" = "ISCED 8: Doctoral or equivalent"
)
edu_distribution = compute_distribution("Edu", focus_country_data, other_countries_data, "Variable")
edu_distribution$Country_Group = factor(edu_distribution$Country_Group)
edu_distribution$Level = as.character(edu_distribution$Level)
edu_distribution$Level = factor(edu_distribution$Level, levels = names(edu_labels), labels = edu_labels)

ggplot(edu_distribution, aes(x = Country_Group, y = Percentage, fill = as.factor(Level))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Education Level Distribution by Country Group",
       x = "Country Group", y = "Percentage", fill = "Education Level") +
  theme_minimal()

# Employment
emp_labels = c(
  "1" = "Full time (30 hours a week or more)",
  "2" = "Part time (less than 30 hours a week)",
  "3" = "Self employed",
  "4" = "Retired/pensioned",
  "5" = "Housewife not otherwise employed",
  "6" = "Student",
  "7" = "Unemployed",
  "8" = "Other"
)
employment_distribution = compute_distribution("Employment", focus_country_data, other_countries_data, "Variable")
employment_distribution$Level = as.character(employment_distribution$Level)
employment_distribution$Level = factor(employment_distribution$Level, levels = names(emp_labels), labels = emp_labels)
ggplot(employment_distribution, aes(x = Country_Group, y = Percentage, fill = as.factor(Level))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Employment Type Distribution by Country Group",
       x = "Country Group", y = "Percentage", fill = "Employment Type") +
  theme_minimal()

# Trust
trust_vars = c("TPeople", "TFamily", "TNeighbourhood", "TKnow", "TMeet")
trust_distribution = compute_distribution(trust_vars, focus_country_data, other_countries_data, "Trust_Variable")
plot_distribution(trust_distribution, "Level", "Country_Group", "Trust_Variable", 
                  "Trust Levels: CZE vs. Other Countries", "Trust Level")

# Importance
importance_vars = c("VFamily", "VLeisure", "VWork")
importance_distribution = compute_distribution(importance_vars, focus_country_data, other_countries_data, "Importance_Variable")
plot_distribution(importance_distribution, "Level", "Country_Group", "Importance_Variable", 
                  "Importance in Life: CZE vs. Other Countries", "Importance Level")

# Well-being
wellbeing_vars = c("HOverall", "HChoice", "HShelter")
wellbeing_distribution = compute_distribution(wellbeing_vars, focus_country_data, other_countries_data, "Wellbeing_Variable")
plot_distribution(wellbeing_distribution, "Level", "Country_Group", "Wellbeing_Variable", 
                  "Well-being Levels: CZE vs. Other Countries", "Well-being Level")

# Economic
economic_vars = c("EEquality", "EGovernment", "EHardWork")
economic_distribution = compute_distribution(economic_vars, focus_country_data, other_countries_data, "Economic_Variable")
plot_distribution(economic_distribution, "Level", "Country_Group", "Economic_Variable", 
                  "Economic Views: CZE vs. Other Countries", "Economic Level")

# Secure Education
secure_edu_vars = "SEducation"
secure_edu_distribution = compute_distribution(secure_edu_vars, focus_country_data, other_countries_data, "Secure_Edu_Variable")
plot_distribution(secure_edu_distribution, "Level", "Country_Group", "Secure_Edu_Variable", 
                  "Security of Education: CZE vs. Other Countries", "Security Level")

# Priority
priority_vars = c("PIA", "PIAB")
priority_distribution <- compute_distribution(priority_vars, focus_country_data, other_countries_data, "Priority_Variable")
plot_distribution(priority_distribution, "Level", "Country_Group", "Priority_Variable", 
                  "Priorities: CZE vs. Other Countries", "Priority Level")

# Science & Technology
st_vars = "STBetter"
st_distribution = compute_distribution(st_vars, focus_country_data, other_countries_data, "St_Variable")
plot_distribution(st_distribution, "Level", "Country_Group", "St_Variable", 
                  "Science & Technology Trust: CZE vs. Other Countries", "Trust Level")

# News
news_vars = c("PRadio", "PEmail", "PInternet", "PSocial")
news_distribution = compute_distribution(news_vars, focus_country_data, other_countries_data, "News_Variable")
plot_distribution(news_distribution, "Level", "Country_Group", "News_Variable", 
                  "News Sources: CZE vs. Other Countries", "Frequency Level")

# Democratic Institutions
democratic_vars = c("PDemImp", "PDemCurrent", "PSatisfied")
democratic_distribution <- compute_distribution(democratic_vars, focus_country_data, other_countries_data, "Democratic_Variable")
plot_distribution(democratic_distribution, "Level", "Country_Group", "Democratic_Variable", 
                  "Democracy Satisfaction: CZE vs. Other Countries", "Level")

# Confidence in Institutions
confidence_vars = c("CArmedForces", "CUnions", "CPolice", "CPParties", "CParliament", "CCivilService", "CUniversities", "CElections", "CBanks", "CEnvOrg")
confidence_distribution = compute_distribution(confidence_vars, focus_country_data, other_countries_data, "Confidence_Variable")
plot_distribution(confidence_distribution, "Level", "Country_Group", "Confidence_Variable", 
                  "Confidence in Institutions: CZE vs. Other Countries", "Confidence Level")

#Q2b and c

#One-hot encoding
VC = VC %>%
  mutate(
    MF_male = ifelse(MF == "Male", 1, 0),
    MF_female = ifelse(MF == "Female", 1, 0),
    MF_others = ifelse(MF == "Others", 1, 0),
    
    Emp_fulltime = ifelse(Employment == 1, 1, 0),  # Full time
    Emp_parttime = ifelse(Employment == 2, 1, 0),  # Part time
    Emp_self = ifelse(Employment == 3, 1, 0),  # Self employed
    Emp_retired = ifelse(Employment == 4, 1, 0),  # Retired/pensioned
    Emp_housewife = ifelse(Employment == 5, 1, 0),  # Housewife
    Emp_student = ifelse(Employment == 6, 1, 0),  # Student
    Emp_unemp = ifelse(Employment == 7, 1, 0),  # Unemployed
    Emp_other = ifelse(Employment == 8, 1, 0),   # Other
    
    PIA_1 = ifelse(PIA == 1, 1, 0),   # Economic growth
    PIA_2 = ifelse(PIA == 2, 1, 0),   # Strong defence
    PIA_3 = ifelse(PIA == 3, 1, 0),   # Public say in jobs/communities
    PIA_4 = ifelse(PIA == 4, 1, 0),   # Beautification
    
    PIAB_1 = ifelse(PIAB == 1, 1, 0), # Economic growth
    PIAB_2 = ifelse(PIAB == 2, 1, 0), # Strong defence
    PIAB_3 = ifelse(PIAB == 3, 1, 0), # Public say in jobs/communities
    PIAB_4 = ifelse(PIAB == 4, 1, 0)  # Beautification
  )

variables = c("TPeople", "TFamily", "TNeighbourhood", "TMeet", "TKnow", 
              "VFamily", "VLeisure", "VWork", "HOverall", "HChoice", "HShelter", 
              "EEquality", "EGovernment", "EHardWork", "SEducation", "STBetter", 
              "PRadio", "PEmail", "PInternet", "PSocial", "PDemImp", 
              "PDemCurrent", "PSatisfied", 
              "MF_male", "MF_female", "MF_others", 
              "Age", "Edu", 
              "Emp_fulltime", "Emp_parttime", "Emp_self", "Emp_retired", 
              "Emp_housewife", "Emp_student", "Emp_unemp", 
              "PIA_1", "PIA_2", "PIA_3",
              "PIAB_1", "PIAB_2", "PIAB_3")

response_vars = c("CArmedForces", "CUnions", "CPolice", "CPParties", "CParliament", 
                  "CCivilService", "CUniversities", "CElections", "CBanks", "CEnvOrg")

focus_country_data = VC[VC$Country == "CZE" & rowSums(VC[, c(variables, response_vars)] == -1, na.rm = TRUE) == 0, ]
other_countries_data = VC[VC$Country != "CZE" & rowSums(VC[, c(variables, response_vars)] == -1, na.rm = TRUE) == 0, ]

focus_models = list()
other_models = list()

for (response in response_vars) {
  formula_model <- as.formula(paste(response, "~", paste(variables, collapse=" + ")))
  focus_models[[response]] <- lm(formula_model, data=focus_country_data)
  other_models[[response]] <- lm(formula_model, data=other_countries_data)
}

par(mfrow = c(2,2))
plot(focus_models[[response_vars[1]]])
plot(focus_models[[response_vars[2]]])
plot(focus_models[[response_vars[3]]])
plot(focus_models[[response_vars[4]]])
plot(focus_models[[response_vars[5]]])
plot(focus_models[[response_vars[6]]])
plot(focus_models[[response_vars[7]]])
plot(focus_models[[response_vars[8]]])
plot(focus_models[[response_vars[9]]])
plot(focus_models[[response_vars[10]]])

#Other country model plots
plot(other_models[[response_vars[1]]])
plot(other_models[[response_vars[2]]])
plot(other_models[[response_vars[3]]])
plot(other_models[[response_vars[4]]])
plot(other_models[[response_vars[5]]])
plot(other_models[[response_vars[6]]])
plot(other_models[[response_vars[7]]])
plot(other_models[[response_vars[8]]])
plot(other_models[[response_vars[9]]])
plot(other_models[[response_vars[10]]])

focus_country_models = lapply(response_vars[1:10], function(r) focus_models[[r]])
other_country_models = lapply(response_vars[1:10], function(r) other_models[[r]])

#Print stargazer table
#Czech Republic
sink("focus_models_output.html")
stargazer(focus_country_models,type = "html")
sink()

#Other Country
sink("other_country_output.html")
stargazer(other_country_models,type = "html")
sink()

#Heatmap
extract_p_values_and_plot <- function(models, title) {
  p_matrix = sapply(models, function(model) coef(summary(model))[, "Pr(>|t|)"])
  p_matrix = as.data.frame(p_matrix)
  p_matrix$Variable = rownames(p_matrix)
  p_long = melt(p_matrix, id.vars = "Variable")
  
  ggplot(p_long, aes(x = variable, y = Variable, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "red", mid = "red", high = "white", midpoint = 0) +
    theme_minimal() +
    labs(title = title, x = "Response Variables", y = "Predictor Variables") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Plot heatmap for Czech Republic
extract_p_values_and_plot(focus_country_models, "Heatmap of p-values (Czech Republic)")

# Plot heatmap for other countries
extract_p_values_and_plot(other_country_models, "Heatmap of p-values (Other Countries)")


#Q2b and c but with chosen variables

variables_extract = c("PDemImp", "PSocial", "TFamily", "PRadio", "TMeet", "PEmail", "HChoice", "TKnow", 
              "Emp_student", "PSatisfied", "TNeighbourhood", "MF_male")
variables_other_extract = c("TNeighbourhood", "TFamily", "PDemCurrent", "PSatisfied", "TMeet", "TPeople", 
                            "TKnow", "HOverall", "EHardWork", "PIA_2", "PIAB_2", "Emp_housewife", 
                            "Emp_self", "PIA_3", "PIA_1")

focus_country_data_extract = VC[VC$Country == "CZE" & rowSums(VC[, c(variables_extract, response_vars)] == -1, na.rm = TRUE) == 0, ]
other_countries_data_extract = VC[VC$Country != "CZE" & rowSums(VC[, c(variables_other_extract, response_vars)] == -1, na.rm = TRUE) == 0, ]
focus_models_extract = list()
other_models_extract = list()

for (response in response_vars) {
  formula_model <- as.formula(paste(response, "~", paste(variables_other_extract, collapse=" + ")))
  focus_models_extract[[response]] <- lm(formula_model, data=focus_country_data)
  other_models_extract[[response]] <- lm(formula_model, data=other_countries_data)
}

focus_country_models_extract = lapply(response_vars[1:10], function(r) focus_models_extract[[r]])
other_country_models_extract = lapply(response_vars[1:10], function(r) other_models_extract[[r]])

sink("focus_models_extract_output.html")
stargazer(focus_country_models_extract,type = "html")
sink()

sink("other_models_extract_output.html")
stargazer(other_country_models_extract,type = "html")
sink()

