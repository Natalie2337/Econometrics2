library(AER)
library(dplyr)
library(broom)
library(systemfit)
library(ivmodel)

data <- read.csv("draft.csv", header = TRUE)

#Q1
model <- lm(lwage ~ vet, data = data)
summary(model)

# Check endogeneity
residuals_ols <- resid(model)
cor_test <- cor.test(residuals_ols, data$vet)
print(cor_test)

# Hausman test
iv_model <- ivreg(lwage ~ vet | elig, data = data)

hausman_test <- function(ols, iv) {
  # coefs
  b_ols <- coef(ols)
  b_iv <- coef(iv)
  
  # covarians matrix
  V_ols <- vcov(ols)
  V_iv <- vcov(iv)
  
  # 
  diff_beta <- b_iv - b_ols
  V_diff <- V_iv - V_ols
  
  #
  stat <- t(diff_beta) %*% solve(V_diff) %*% diff_beta
  p_value <- pchisq(stat, df = length(b_ols) - 1, lower.tail = FALSE)
  
  return(list(statistic = stat, p_value = p_value))
}

result <- hausman_test(model, iv_model)
print(result)

#Q4

#first stage
first_stage <- lm(vet ~ elig, data=data)
summary(first_stage)  # Check whether Z influence D 

#second stage
late_model <- ivreg(lwage ~ vet | elig, data = data)
summary(late_model)

late_estimate <- coef(late_model)["vet"]
cat("Estimated LATE:", late_estimate, "\n")


#Q5

#1.Use LATE for interaction term:

data$vet_yob <- data$vet * data$yob
data$elig_yob <- data$elig * data$yob

# First stage regression: Predict D using instrument Z, allowing for birth year interaction
multi_first_stage <- lm(vet ~ elig_yob, data = data)
summary(multi_first_stage)  

# Second stage regression: Estimate heterogeneous LATE using 2SLS
multi_late_model <- ivreg(lwage ~ vet_yob | elig_yob, data = data)
summary(multi_late_model)


## 

# First stage regression: Predict D using instrument Z, allowing for birth year interaction
first_stage0 <- lm(vet~ elig * yob, data = data)
summary(first_stage0)  # Check instrument strength

# Second stage regression: Estimate heterogeneous LATE using 2SLS
late_model0 <- ivreg(lwage ~ vet * yob | elig * yob, data = data)
summary(late_model0)

# Extract LATE estimates by birth year
late_estimates <- coef(late_model0)
print(late_estimates)

library(ggplot2)
late_df <- data.frame(Birth_Year = levels(data$yob), LATE = late_estimates)
ggplot(late_df, aes(x = Birth_Year, y = LATE)) +
  geom_point() + geom_line() +
  labs(title = "LATE by Birth Year", x = "Birth Year", y = "LATE Estimate")

#2.Use group-wise regression

draft_data <- read.csv("draft.csv")

#group by regression
group_results <- draft_data %>%
  group_by(yob) %>%
  do(tidy(lm(lwage ~ vet, data = .))) %>%
  filter(term == "vet")  

print(group_results)

yob_weights <- draft_data %>%
  group_by(yob) %>%
  summarise(weight = n()/nrow(draft_data))

# weighted average
weighted_avg_effect <- sum(group_results$estimate * yob_weights$weight)
print(weighted_avg_effect)






