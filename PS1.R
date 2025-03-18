# Set working directory
#getwd()
#setwd("./Documents/R-for-Econometrics")

# Read .dta data
library(haven)
data <- read_dta("lalonde2.dta")

#Only use NSW data (sample=1)
data <- subset(data, data$sample == 1)
head(data)
str(data)
dim(data)

# Data cleaning
processed_data <- na.omit(data) # dealing with missing values
processed_data <- processed_data[!duplicated(processed_data), ] # dealing with duplicate values
sapply(processed_data, is.numeric) # check whether the value of each variable is numerics
str(processed_data)
dim(processed_data)
summary(processed_data)

# Observe the treated dataset
treated_d <- subset(processed_data, treated == 1)
head(treated_d)
nontreated_d <- subset(processed_data, treated == 0)
head(nontreated_d)

##q1
avg_treated_RE_1978 <- mean(processed_data$re78[processed_data$treated == 1],na.rm = TRUE) # na.rm = TRUE: ignore missing value
avg_nontreated_RE_1978 <- mean(processed_data$re78[processed_data$treated == 0],na.rm = TRUE)
print(avg_treated_RE_1978)
print(avg_nontreated_RE_1978)
difference <- avg_treated_RE_1978 - avg_nontreated_RE_1978
print(difference)

##q2
model <- lm(re78 ~ 1 + treated, data = processed_data) #linear regression with interception
# the default regression contains intercept, so it is the same as: 
# model <- lm(re78 ~ treated, data = processed_data)
summary(model)
print(coefficients(model)["treated"])

##q3

# predicted value and residuals:
pred_y <- predict(model)
print(pred_y)
resids <- residuals(model)
print(model)

#check P1: y_bar = X_bar * b
y_bar <- mean(processed_data$re78)
X_mean <- c(1,mean(processed_data$treated)) # Contains the constant term
#print(X_mean)
b <- coefficients(model)
#print(b)
y_multiply <- sum(X_mean*b)
print(y_bar)
print(y_multiply)
err <- y_bar - y_multiply
print(err)

#check P2: Sum of actual values equals sum of predicted values

sum_y <- sum(processed_data$re78)
sum_pred_y <- sum(pred_y)
sum_of_err <- sum_y - sum_pred_y
print(sum_of_err)

#check P3: Sum of residual is zero

sum_resids <- sum(resids)
print(sum_resids)

#check P4: Residuals are orthogonal to regressors

X <- model.matrix(model) # X matrix containing the intercept
print(X)
rst <- t(X) %*% resids # compute X'e
print(rst)

#check P5: Residuals are orthogonal to predicted values

rst2 <- t(pred_y) %*% resids # compute y_hat'e
print(rst2)

##q4

model2 <- lm(re78 ~ 1 + treated + age + educ + black + married + nodegree + hisp + kids18 + kidmiss + re74, data = processed_data) #linear regression with interception
summary(model2)


