#Question 1
#a.
observed <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, ncol = 3, byrow = TRUE)
colnames(observed) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
rownames(observed) <- c("Upper class", "Lower class")
row_marginals <- rowSums(observed)
col_marginals <- colSums(observed)
total_frequency <- sum(observed)
for (i in 1:2) {
  for (j in 1:3) {
    expected[i, j] <- (row_marginals[i] * col_marginals[j]) / total_frequency
  }
}
chi_squared <- sum((observed - expected)^2 / expected)
print(chi_squared)


#b.
df <- (nrow(observed) - 1) * (ncol(observed) - 1)
p_value <- 1 - pchisq(chi_squared, df)
cat("P-value:", p_value)
#We hypothesize that H0:The likelihood of police soliciting bribes is class-independent
#                    H1:The likelihood of a police officer soliciting a bribe is related to class
#If α = 0.1, this means that the significance level of the hypothesis test is 0.1. since the resulting p-value has a value of 0.1502306 > 0.1,
#we do not reject H0 that the likelihood of police officers soliciting bribes is unrelated to class

#c
for (i in 1:2) {
  for (j in 1:3) {
    standardized_residuals[i, j] <- (observed[i, j] - expected[i,j])/sqrt(expected[i,j])
  }
}
print(standardized_residuals)
#d
 #(Upper, Not Stopped) is 0.1360828, which means that in the "Upper" category, the observed frequency of "Not Stopped" is slightly higher than the expected frequency, but the difference is not particularly significant.
 #(Upper, Bribe requested) is -0.8153742, which means that the number of observations of "Bribe requested" in the "Upper" category is significantly lower than the expected frequency, indicating that it occurs less often in this combination.
 #(upper, Stopped/given warning) is 0.818923, which means that the observed frequency of "Stopped/given warning" is slightly higher than the expected frequency in the "upper" category, but the difference is not particularly significant.
 #For the "lower" category, the interpretation of the standardized residuals is similar to that of the upper category.


#Question 2
#a
#Null assumption(H0): The reservation policy has no impact on the number of new or rehabilitated drinking water facilities in the village
#Alternative Hypothesis (H1): The reservation policy have an impact on the number of new or rehabilitated drinking water facilities in villages


#b
data <- read.csv(url)
water_total <- data$water
reservation <- data$reserved
model <- lm(water_total ~ reservation, data = data)
summary(model)

#c
#Intercept: The Intercept represents the estimated average number of water facilities when the reservation variable is 0. In this model, the Intercept estimate is 14.738. In this model, the Intercept is estimated to be 14.738, which means that when the reservation variable is 0 (i.e., there is no reservation policy), the average number of water facilities is approximately 14.738.
#Coefficient estimate for RESERVATION: In this model, the coefficient estimate for the RESERVATION variable is 9.252. this indicates the effect of the reservation policy on the number of water facilities. The coefficient estimate tells us that each unit increase in reservation is associated with an increase in the number of water facilities of about 9.252 units, holding other factors constant.
#t-value and p-value: t-value and p-value are used to measure the significance of the coefficient estimates. In this model, the t-value of the coefficient estimate for the RESERVATION variable is 2.344 and the p-value is 0.0197. This means that the effect of the reservation policy on the number of water resource facilities is statistically significant because the p-value is less than the level of significance typically used (usually 0.05).
# Overall,the coefficient estimate has a RESERVATION variable of 9.252, a t-value of 2.344, and a p-value of 0.0197, which implies that the effect of the reservation policy on the number of water facilities is statistically significant. However, the low R-squared value indicates that the model fit is relatively low and the explanatory power of the reservation policy on the number of water facilities is weak.


