# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(corrplot)
library(lmtest)
library(car)
library(ggpubr)      # For combining plots
library(sandwich)    # For robust standard errors
library(margins)
library(pROC)
library(caret)
library(tidyr)

# Import the dataset
data <- read_excel("D:/My_Profile/Msc_UKVI/Westminster/5.Predictive_Analysis_for_Decision_Making/Coursework1_24thFeb/nls80.xlsx")

#Question 1: MULTIPLE LINEAR REGRESSION
#1a) Exploratory Data Analysis

# Renaming columns for clarity
data <- data %>%
  rename(
    MonthlyEarnings         = wage,
    WeeklyHours             = hours,
    IQ_Score                = iq,
    Knowledge_World_Work    = kww,
    Years_of_Education      = educ,
    Work_Experience         = exper,
    Current_Employer_Tenure = tenure,
    Age                     = age,
    Marital_Status          = married,
    Race_Black              = black,
    South_Indicator         = south,
    Urban_Indicator         = urban,
    Siblings                = sibs,
    Birth_Order             = brthord,
    Mothers_Education       = meduc,
    Fathers_Education       = feduc,
    Log_Wage                = lwage
  )

# Define the selected variables
selected_vars <- c("MonthlyEarnings", "Years_of_Education", "Work_Experience")
# Print the variable to check its contents
print(selected_vars)

# Define continuous variables for EDA
continuous_vars <- c("MonthlyEarnings", "WeeklyHours", "IQ_Score", "Knowledge_World_Work", 
                     "Years_of_Education", "Work_Experience", "Current_Employer_Tenure", 
                     "Age", "Siblings", "Birth_Order", "Mothers_Education", "Fathers_Education", "Log_Wage")

# Ensure that variables are numeric
for (var in continuous_vars) {
  if (!is.numeric(data[[var]])) {
    data[[var]] <- as.numeric(as.character(data[[var]]))
  }
}

# Summary statistics
summary_stats <- summary(data[continuous_vars])
print(summary_stats)

# Select a subset of variables for appropriate visualizations
selected_vars <- c("MonthlyEarnings", "Years_of_Education", "Work_Experience")

# faceted plotting

data_long <- data %>%
  select(all_of(selected_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Create faceted histogram for the selected variables
p_hist <- ggplot(data_long, aes(x = Value)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 30) +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(title = "Faceted Histograms")

# Create faceted boxplot for the selected variables
p_box <- ggplot(data_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Faceted Boxplots")

# Combine the two plots into one figure using ggpubr
library(ggpubr)
combined_plot <- ggarrange(p_hist, p_box, ncol = 1, nrow = 2)
print(combined_plot)



# Correlation matrix and its visualization
  
corr_matrix <- cor(data[continuous_vars], use = "complete.obs")
print(round(corr_matrix, 2))
corrplot(corr_matrix, method = "color", type = "upper", addCoef.col = "black",
         tl.cex = 0.8, title = "Correlation Matrix of Continuous Variables", mar = c(0,0,1,0))

#1b) Model Development

# Develop the multiple linear regression model for Log_Wage
model <- lm(Log_Wage ~ Years_of_Education + Work_Experience + Current_Employer_Tenure + IQ_Score + Age, data = data)
summary(model)

#Check for Multicollinearity using Variance Inflation Factor (VIF)
#vif_values <- vif(model)
#print(vif_values)  # Display VIF values

#bptest(model)  # Breusch-Pagan test for heteroskedasticity

#Interpretation:
#p-value < 0.05 → Heteroskedasticity exists (violates OLS assumption).
#p-value > 0.05 → No heteroskedasticity (OLS assumptions hold).


#1d) Diagnostic Tests

#Check for Heteroskedasticity (Breusch-Pagan Test)
# Breusch-Pagan test for heteroskedasticity
bp_test <- bptest(model)
print(bp_test)

#Interpretation:
#p-value < 0.05 → Heteroskedasticity exists (violates OLS assumption).
#p-value > 0.05 → No heteroskedasticity (OLS assumptions hold).
# If heteroskedasticity is present, use robust standard errors:

coeftest(model, vcov = vcovHC(model, type = "HC3"))

#Checking for Multicollinearity using Variance Inflation Factor (VIF)
# Calculate Variance Inflation Factors (VIF)
vif_values <- vif(model)
print(vif_values)

#Interpretation:
#If VIF > 10, the variable has high collinearity and may need to be removed or adjusted.
#If VIF between 5-10, moderate correlation exists (may still be acceptable).
#If VIF < 5, no serious collinearity issue.

#Residual Diagnostics
#Residuals vs. Fitted Plot
p1 <- ggplot(data, aes(x = fitted(model), y = resid(model))) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")

# Normal Q-Q Plot
p2 <- ggplot(data, aes(sample = resid(model))) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot")

# Histogram of Residuals
p3 <- ggplot(data, aes(x = resid(model))) +
  geom_histogram(color = "black", fill = "lightblue", bins = 30) +
  labs(title = "Histogram of Residuals", x = "Residuals")

# Combine Plots
ggarrange(p1, p2, p3, ncol = 3, nrow = 1)

# Outlier & Influential Point Detection

# Cook's Distance
plot(model, which = 4, main = "Cook's Distance Plot")
# Leverage vs. Studentized Residuals
plot(model, which = 5, main = "Leverage vs. Residuals")
#Autocorrelation Test (Durbin-Watson Test) 
dwtest(model)

# Q2: GENERALIZED LINEAR MODELS – LOGIT AND PROBIT
  
# (a) Binary Variable Creation

data$univ_edu <- ifelse(data$Years_of_Education >= 16, 1, 0)
data$univ_edu <- factor(data$univ_edu, levels = c(0, 1), labels = c("No", "Yes"))

# Quick check of distribution
table(data$univ_edu)

# (b) Logit and Probit Models

# Work_Experience, Current_Employer_Tenure, IQ_Score, Age, and Mothers_Education

logit_model <- glm(
  univ_edu ~ Work_Experience + Current_Employer_Tenure + IQ_Score + Age + Mothers_Education, 
  data = data, 
  family = binomial(link = "logit")
)
summary(logit_model)

probit_model <- glm(
  univ_edu ~ Work_Experience + Current_Employer_Tenure + IQ_Score + Age + Mothers_Education, 
  data = data, 
  family = binomial(link = "probit")
)
summary(probit_model)

# (d) Marginal Effects
# Calculate marginal effects for the Logit model

margins_logit <- margins(logit_model)
summary(margins_logit)

# Calculate marginal effects for the Probit model
margins_probit <- margins(probit_model)
summary(margins_probit)


#e) Model Comparison
cat("AIC for Logit Model:", AIC(logit_model), "\n")
cat("AIC for Probit Model:", AIC(probit_model), "\n")
cat("BIC for Logit Model:", BIC(logit_model), "\n")
cat("BIC for Probit Model:", BIC(probit_model), "\n")

# Predicted probabilities for each model
logit_probs <- predict(logit_model, type = "response")
probit_probs <- predict(probit_model, type = "response")

# Creating index for rows actually used (no missing values)
common_idx_logit <- !is.na(logit_probs) & !is.na(data$univ_edu)
common_idx_probit <- !is.na(probit_probs) & !is.na(data$univ_edu)

# Building ROC objects using the matched indices
roc_logit <- roc(data$univ_edu[common_idx_logit], logit_probs[common_idx_logit])
roc_probit <- roc(data$univ_edu[common_idx_probit], probit_probs[common_idx_probit])

cat("Logit AUC:", auc(roc_logit), "\n")
cat("Probit AUC:", auc(roc_probit), "\n")

# Plot both ROC curves
plot(roc_logit, col = "blue", main = "ROC Curves: Logit vs. Probit")
plot(roc_probit, col = "red", add = TRUE)
legend("bottomright", legend = c("Logit", "Probit"), col = c("blue","red"), lty = 1)

#Confusion Matrix for classification
pred_logit <- ifelse(logit_probs > 0.5, "Yes", "No")
pred_probit <- ifelse(probit_probs > 0.5, "Yes", "No")

# For confusion matrices, also subset to rows actually used by the model
logit_cm_idx <- !is.na(logit_probs) & !is.na(data$univ_edu)
probit_cm_idx <- !is.na(probit_probs) & !is.na(data$univ_edu)

cat("Logit Confusion Matrix:\n")
print(confusionMatrix(
  factor(pred_logit[logit_cm_idx], levels = c("No","Yes")),
  data$univ_edu[logit_cm_idx]
))

cat("Probit Confusion Matrix:\n")
print(confusionMatrix(
  factor(pred_probit[probit_cm_idx], levels = c("No","Yes")),
  data$univ_edu[probit_cm_idx]
))



#Q3 SIMULATION STUDY ON ENDOGENEITY

#a) Data Generation

set.seed(123)
n <- 1000

# True parameter values
beta0 <- 0.5
beta1 <- 0.05   # effect of Work_Experience
beta2 <- 0.1    # effect of Years_of_Education

#exogenous variable Work_Experience
Work_Experience <- rnorm(n, mean = 10, sd = 2)

# error term
error <- rnorm(n, mean = 0, sd = 1)

# Generating endogenous Years_of_Education (introducing endogeneity via error)
Years_of_Education <- 12 + 0.5 * error + rnorm(n, mean = 0, sd = 0.5)

# Log_Wage based on the DGP
Log_Wage <- beta0 + beta1 * Work_Experience + beta2 * Years_of_Education + error

# simulation data frame
sim_data <- data.frame(Work_Experience, Years_of_Education, Log_Wage)

#b) OLS Estimation
simulations <- 1000
ols_coef_exper <- numeric(simulations)
ols_coef_educ  <- numeric(simulations)

## Monte Carlo Simulation
for(i in 1:simulations) {
  Work_Experience <- rnorm(n, mean = 10, sd = 2)
  error <- rnorm(n, mean = 0, sd = 1)
  Years_of_Education <- 12 + 0.5 * error + rnorm(n, mean = 0, sd = 0.5)
  Log_Wage <- beta0 + beta1 * Work_Experience + beta2 * Years_of_Education + error
  
  model_sim <- lm(Log_Wage ~ Work_Experience + Years_of_Education)
  ols_coef_exper[i] <- coef(model_sim)["Work_Experience"]
  ols_coef_educ[i]  <- coef(model_sim)["Years_of_Education"]
}


# first 10 estimated coefficients for each variable
cat("First 10 estimated coefficients for 'exper':\n")
print(head(ols_coef_exper, 10))

cat("First 10 estimated coefficients for 'educ':\n")
print(head(ols_coef_educ, 10))

# summary statistics (mean, min, max, quartiles) for the coefficient estimates
cat("Summary of OLS estimates for 'exper':\n")
print(summary(ols_coef_exper))


cat("Summary of OLS estimates for 'educ':\n")
print(summary(ols_coef_educ))


# c) Calculate biases
bias_exper <- mean(ols_coef_exper) - beta1
bias_educ  <- mean(ols_coef_educ)  - beta2
cat("Bias in OLS for Work_Experience:", round(bias_exper, 4), "\n")
cat("Bias in OLS for Years_of_Education:", round(bias_educ, 4), "\n")

# Visualize the distribution of OLS estimates with histograms
hist(ols_coef_exper, main = "Distribution of OLS Estimates for Work_Experience", 
     xlab = "Coefficient for Work_Experience", col = "skyblue", breaks = 30)
hist(ols_coef_educ, main = "Distribution of OLS Estimates for Years_of_Education", 
     xlab = "Coefficient for Years_of_Education", col = "salmon", breaks = 30)

#d) 2SLS Estimation 

library(AER)

iv_coef_exper <- numeric(simulations)
iv_coef_educ  <- numeric(simulations)

for(i in 1:simulations) {
  Work_Experience <- rnorm(n, mean = 10, sd = 2)
  error <- rnorm(n, mean = 0, sd = 1)
  
  # Generate instrument z for Years_of_Education
  z <- rnorm(n, mean = 15, sd = 2)
  
  Years_of_Education <- 12 + 0.5 * error + 0.8 * z + rnorm(n, mean = 0, sd = 0.5)
  Log_Wage <- beta0 + beta1 * Work_Experience + beta2 * Years_of_Education + error
  
  model_iv <- ivreg(Log_Wage ~ Work_Experience + Years_of_Education | Work_Experience + z)
  iv_coef_exper[i] <- coef(model_iv)["Work_Experience"]
  iv_coef_educ[i]  <- coef(model_iv)["Years_of_Education"]
}

# Compare 2SLS estimates to the true values
bias_iv_exper <- mean(iv_coef_exper) - beta1
bias_iv_educ  <- mean(iv_coef_educ)  - beta2

cat("Bias in 2SLS for Work_Experience:", round(bias_iv_exper, 4), "\n")
cat("Bias in 2SLS for Years_of_Education:", round(bias_iv_educ, 4), "\n")


#Compare OLS vs. 2SLS Bias

cat("Comparing OLS vs. 2SLS bias for 'exper':\n")
cat("OLS Bias:", round(bias_exper, 4),
    " | 2SLS Bias:", round(bias_iv_exper, 4), "\n\n")
cat("Comparing OLS vs. 2SLS bias for 'educ':\n")
cat("OLS Bias:", round(bias_educ, 4),
    " | 2SLS Bias:", round(bias_iv_educ, 4), "\n")


# Visualize distributions

hist(iv_coef_exper, main = "Distribution of 2SLS Estimates for Work_Experience", 
     xlab = "Coefficient for Work_Experience", col = "lightgreen", breaks = 30)
hist(iv_coef_educ, main = "Distribution of 2SLS Estimates for Years_of_Education", 
     xlab = "Coefficient for Years_of_Education", col = "lightpink", breaks = 30)

