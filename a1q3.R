#import libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stargazer)
library(reshape2)
library(scales)

#Q3
comb_external_data = read.csv("combined_data.csv")
vars = c("Average.Income", "CPI", "Democracy.score", "Freedom.score", "GDP", "Population", "Tertiary.rate", "Unemployment.rate")

comb_external_data = comb_external_data %>%
  select(Country, all_of(vars)) %>%
  drop_na()

#boxplot to identify outliers
data_cols = as_tibble(comb_external_data%>% select(., all_of(vars)))
data_long = data_cols %>% pivot_longer(vars, names_to = "column")
ggplot(data_long, aes(x = value)) + geom_boxplot() + facet_wrap(~ column, scales = "free")

#delete outliers
clean_extdata = comb_external_data %>% select(Country, all_of(vars))

remove_outliers <- function(x) {
  Q1 = quantile(x, 0.25, na.rm = TRUE)
  Q3 = quantile(x, 0.75, na.rm = TRUE)
  IQR_val = Q3 - Q1
  lower_bound = Q1 - 1.5 * IQR_val
  upper_bound = Q3 + 1.5 * IQR_val
  x[x >= lower_bound & x <= upper_bound]
}

for (var in vars) {
  clean_extdata = clean_extdata %>%
    filter(
      .data[[var]] >= quantile(.data[[var]], 0.25, na.rm = TRUE) - 1.5 * IQR(.data[[var]], na.rm = TRUE) &
        .data[[var]] <= quantile(.data[[var]], 0.75, na.rm = TRUE) + 1.5 * IQR(.data[[var]], na.rm = TRUE)
    )
}

data_cols = as_tibble(clean_extdata%>% select(., all_of(vars)))
data_long = data_cols %>% pivot_longer(vars, names_to = "column")
ggplot(data_long, aes(x = value)) + geom_boxplot() + facet_wrap(~ column, scales = "free")

write.csv(clean_extdata, "clean_extdata.csv", row.names = FALSE)
clean_extdata[,vars] = scale( clean_extdata[,vars])

#Elbow Method
elbowdata = data.frame()

for (k in 1:20) {
  kfit = kmeans(clean_extdata[, vars], centers = k, nstart = 6)
  print(kfit$tot.withinss)
  
  elbowdata = rbind(elbowdata, t(c(k, kfit$tot.withinss)))
}

colnames(elbowdata) = c("k", "tot.within.ss")

plot(elbowdata$k, elbowdata$tot.within.ss, type = "b", 
     xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares", 
     main = "Elbow Method to Determine Optimal k")

#Clustering
kfit = kmeans(clean_extdata[, vars], centers = 6, nstart = 6)

kcentroids = aggregate(clean_extdata[, vars], by = list(cluster = kfit$cluster), mean)
print(kcentroids)

clean_extdata$cluster = kfit$cluster

country_cluster = clean_extdata %>%
  filter(Country == "CZE") %>%
  select(Country, cluster)
print(country_cluster)

select_country = clean_extdata%>%
  filter(cluster == country_cluster$cluster[1])

print(select_country)

# Correlation matrix
correlation_matrix = cor(clean_extdata[, vars], use = "complete.obs")
cor_melted = melt(correlation_matrix)
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "lightblue", high = "darkblue", mid = "lightblue", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap of Selected Variables", x = "Variables", y = "Variables")


#Clustering plot using Principal Component Analysis
pca_result = prcomp(clean_extdata[, vars], center = TRUE, scale. = TRUE)
pca_data = data.frame(pca_result$x[, 1:2])
pca_data$Country = clean_extdata$Country
pca_data$Cluster = as.factor(clean_extdata$cluster)

ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster, label = Country)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(size = 2.5, vjust = -0.5) +
  labs(title = "Country Clustering (With PCA Projection)",
       x = "PC 1", y = "PC 2") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2")

#Q3c
selected_countries = select_country$Country
selected_countries = selected_countries[selected_countries != "CZE"]
cluster_country_data = VC %>%
  filter(Country %in% selected_countries) %>%
  filter(rowSums(across(all_of(c(variables, response_vars)), ~ . == -1), na.rm = TRUE) == 0)

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

select_country_models = list()

for (response in response_vars) {
  formula_model <- as.formula(paste(response, "~", paste(variables, collapse=" + ")))
  select_country_models[[response]] <- lm(formula_model, data=cluster_country_data)
}

par(mfrow = c(2,2))
plot(select_country_models[[response_vars[1]]])
plot(select_country_models[[response_vars[2]]])
plot(select_country_models[[response_vars[3]]])
plot(select_country_models[[response_vars[4]]])
plot(select_country_models[[response_vars[5]]])
plot(select_country_models[[response_vars[6]]])
plot(select_country_models[[response_vars[7]]])
plot(select_country_models[[response_vars[8]]])
plot(select_country_models[[response_vars[9]]])
plot(select_country_models[[response_vars[10]]])

select_cluster_country_models = lapply(response_vars[1:10], function(r) select_country_models[[r]])

#Print stargazer table
sink("select_models_output.html")
stargazer(select_cluster_country_models,type = "html")
sink()

#Heatmap
extract_p_values_and_plot(select_country_models, "Heatmap of p-values (Cluster Countries)")



