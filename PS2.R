#Set working directory
#getwd()
#setwd("./Documents/R-for-Econometrics")

# Read .dta data
library(haven)
library(dplyr)
library(broom)
library(ggplot2)
library(segmented)
data <- read_dta("lalonde2.dta")


#Q1
covariates <- c("age", "educ", "black", "married", "nodegree", "hisp", "kids18", "kidmiss", "re74")


temp <- data %>% filter (sample == 2 & treated == 1)
print(temp)

data <- data %>%
  mutate(group = case_when(
    treated == 1 & sample == 1 ~ "Treated_NSW",
    treated == 0 & sample == 1 ~ "Comparison_NSW",
    sample == 2 ~ "Comparison_CPS",
    TRUE ~ "Other"
  ))


table <- data %>%
  group_by(group) %>%
  filter(group != "Other") %>%
  summarise(across(all_of(covariates), list(
    mean = \(x) mean(x, na.rm = TRUE),
    sd = \(x) sd(x, na.rm = TRUE)
  )))

print(table)


#Q2
data_filtered <- data %>%
  filter(group %in% c("Treated_NSW", "Comparison_CPS"))

model_no_cov <- lm(re78 ~ treated, data = data_filtered)  
formula <- as.formula(paste("re78 ~ treated +", paste(covariates, collapse = " + ")))
model_with_cov <- lm(formula, data = data_filtered)

summary(model_no_cov)
summary(model_with_cov)


#Q3
data_cps <- data %>%
  filter(group == "Comparison_CPS") 


ggplot(data_cps, aes(x = educ, y = re78)) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line with confidence interval
  theme_minimal() +
  labs(title = "Education and Real Earnings",
       x = "Years of Education",
       y = "Real Earnings in 1978")


ggplot(data_cps, aes(x = factor(educ), y = re78)) +  # 
  geom_boxplot(outlier.shape = NA, fill = "#69b3a2", alpha = 0.5) + 
  theme_minimal() +
  labs(title = "Education and Real Earnings",
       x = "Years of Education",
       y = "Real Earnings in 1978")

# Q4

# Create piecewise terms
data_cps$edu_above12 <- pmax(0, data_cps$educ - 12)
data_cps$edu_above16 <- pmax(0, data_cps$educ - 16)

model <- lm(re78 ~ educ + edu_above12 + edu_above16, data = data_cps)
summary(model)

# Plot the results
# Get model coefficients
coeffs <- coef(model)
intercept <- coeffs[1]
slope1 <- coeffs[2]  # Slope before 12 years
slope2 <- slope1 + coeffs[3]  # Slope between 12 and 16 years
slope3 <- slope2 + coeffs[4]  # Slope after 16 years
# Generate predictions for each segment separately

educ_seq <- seq(min(data_cps$educ), max(data_cps$educ), by = 0.1)

# Create a data frame for segmented predictions
pred_df <- data.frame(
  educ = educ_seq,
  segment = factor(
    case_when(
      educ_seq <= 12 ~ "Before 12 years",
      educ_seq > 12 & educ_seq <= 16 ~ "Between 12-16 years",
      educ_seq > 16 ~ "After 16 years"
    ),
    levels = c("Before 12 years", "Between 12-16 years", "After 16 years")
  ),
  re78 = case_when(
    educ_seq <= 12 ~ intercept + slope1 * educ_seq,
    educ_seq > 12 & educ_seq <= 16 ~ (intercept + slope1 * 12) + slope2 * (educ_seq - 12),
    educ_seq > 16 ~ (intercept + slope1 * 12 + slope2 * 4) + slope3 * (educ_seq - 16)
  )
)

# Plot the segmented piecewise regression with different colors
ggplot(data_cps, aes(x = educ, y = re78)) +
  geom_point(alpha = 0.5) +  # Scatter plot of data points
  geom_line(data = pred_df, aes(x = educ, y = re78, color = segment), size = 1.2) +  # Color-coded piecewise line
  geom_vline(xintercept = c(12, 16), linetype = "dashed", color = "black", size = 1) +  # Threshold markers
  scale_color_manual(values = c("Before 12 years" = "red", 
                                "Between 12-16 years" = "blue", 
                                "After 16 years" = "green")) +  # Custom segment colors
  labs(title = "Piecewise Linear Regression: re78 vs. educ",
       x = "Years of Education (educ)",
       y = "Earnings in 1978 (re78)",
       color = "Education Range") +
  theme_minimal()


########## Use Package "Segmented": automatic choose breakpoints


base_model <- lm(re78 ~ educ, data = data_cps)
seg_model <- segmented(base_model, seg.Z = ~educ, psi = c(12, 16))
summary(seg_model)

data_cps$predicted <- predict(seg_model)
breakpoints <- seg_model$psi[, "Est."]

ggplot(data_cps, aes(x = educ, y = re78)) +
  geom_point(alpha = 0.5, color = "grey") +  
  geom_line(aes(y = predicted), color = "red", size = 1.2) +  
  geom_vline(xintercept = breakpoints, linetype = "dashed", color = "blue") + 
  labs(title = "Segmented Regression: re78 vs. educ",
       x = "Years of Education (educ)",
       y = "Earnings in 1978 (re78)") +
  theme_minimal()


