
#==============================================================================#
#                     Linear Regression Modelling                              #
#==============================================================================#

load("onlinenews.RData")

require(car)
require(nortest)
require(glmnet)

train <- data  # training set (already loaded)

#==============================================================================#
#                      Model A: Stepwise + VIF Screening                      #
#==============================================================================#

fullm <- lm(shares ~ ., data = train)
summary(fullm)

# Stepwise variable selection using AIC
step(fullm, direction = "backward")

# For numeric variables:    VIF > 5    => multicollinearity problem
# For categorical variables (>2 levels): GVIF > 3.16 => multicollinearity problem

# ITER 1: fit model from stepwise output
mod <- lm(shares ~ n_tokens_title + num_videos + kw_max_min +
            kw_avg_min + kw_max_max + kw_min_avg + kw_max_avg + kw_avg_avg +
            LDA_02 + abs_title_subjectivity + abs_title_sentiment_polarity +
            channel, data = train)
vif(mod)
# Remove kw_max_min.

# ITER 2:
mod <- lm(shares ~ n_tokens_title + num_videos +
            kw_avg_min + kw_max_max + kw_min_avg + kw_max_avg + kw_avg_avg +
            LDA_02 + abs_title_subjectivity + abs_title_sentiment_polarity +
            channel, data = train)
vif(mod)
# Remove kw_avg_avg.

# ITER 3:
mod <- lm(shares ~ n_tokens_title + num_videos +
            kw_avg_min + kw_max_max + kw_min_avg + kw_max_avg +
            LDA_02 + abs_title_subjectivity + abs_title_sentiment_polarity +
            channel, data = train)
vif(mod)
# No multicollinearity problem present.

mA <- mod

#==============================================================================#
#                       Model A: Diagnostic Plots                             #
#==============================================================================#

resA <- rstudent(mA)
fitA <- fitted(mA)

par(mfrow = c(2, 3))

# 1. LINEARITY: Residuals vs Fitted
plot(mA, which = 1, main = "Linearity:")

# 2. NORMALITY: Q-Q Plot
qqnorm(resA, main = "Normality: Q-Q Plot")
qqline(resA, col = "red", lwd = 1.5)

# 3. HOMOSCEDASTICITY: Scale-Location
plot(fitA, sqrt(abs(resA)),
     main = "Homoscedasticity: Scale-Location",
     xlab = "Fitted values", ylab = "Sqrt. Abs. Studentized Residuals")
abline(h = mean(sqrt(abs(resA))), col = "red", lty = 2, lwd = 1.5)
abline(h = c(-2, 2), col = "blue", lty = 2)

# 4. INDEPENDENCE: Time Series Plot
plot(resA, type = "l",
     main = "Independence: Residuals vs Order",
     xlab = "Observation Order",
     ylab = "Studentized Residuals")
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

# 5. INDEPENDENCE: ACF Plot
acf(resA, main = "Independence: ACF of Res")

# 6. COOK'S DISTANCE to identify influential points
plot(mA, which = 5)

# Check the influential observations flagged in Cook's distance plot (1236, 1313, 2557)
obs       <- c(1236, 1313, 2557)
influence <- influence.measures(mod)
influence$infmat[obs, ]
# Observation 1236 is the major concern -- Cook's D = 7.09
# (while the others are 0.44 and 0.73 respectively).
# This single observation is drastically pulling the coefficient estimates.

View(train[c(1236, 1313, 2557), ])
# For these observations the value for kw_min_min is negative (-1),
# which is not logical since it must be a positive number.
# This suggests a data entry error -- remove these observations.

train.clean <- train[-c(1236, 1313, 2557), ]

# Refit model on clean data
mA.clean <- lm(shares ~ n_tokens_title + num_videos +
                 kw_avg_min + kw_max_max + kw_min_avg + kw_max_avg +
                 LDA_02 + abs_title_subjectivity + abs_title_sentiment_polarity +
                 channel, data = train.clean)

res.cl <- rstudent(mA.clean)
fit.cl <- fitted(mA.clean)

#------ Diagnostic Plots (after cleaning)

par(mfrow = c(2, 3))

# 1. LINEARITY: Residuals vs Fitted
plot(mA.clean, which = 1, main = "Linearity:")

# 2. NORMALITY: Q-Q Plot
qqnorm(res.cl, main = "Normality: Q-Q Plot")
qqline(res.cl, col = "red", lwd = 1.5)

# 3. HOMOSCEDASTICITY: Scale-Location
plot(fit.cl, sqrt(abs(res.cl)),
     main = "Homoscedasticity: Scale-Location",
     xlab = "Fitted values", ylab = "Sqrt. Abs. Studentized Residuals")
abline(h = mean(sqrt(abs(res.cl))), col = "red", lty = 2, lwd = 1.5)
abline(h = c(-2, 2), col = "blue", lty = 2)

# 4. INDEPENDENCE: Time Series Plot
plot(res.cl, type = "l",
     main = "Independence: Residuals vs Order",
     xlab = "Observation Order",
     ylab = "Studentized Residuals")
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

# 5. INDEPENDENCE: ACF Plot
acf(res.cl, main = "Independence: ACF of Res")

# 6. COOK'S DISTANCE
plot(mA.clean, which = 5)

View(train[c(1427, 385, 1558), ])

#------ Assumption Tests

# Linearity:
residualPlots(mA,       test = TRUE, type = "rstudent")  # kw_avg_min is problematic (p < 2e-16)
residualPlots(mA.clean, test = TRUE, type = "rstudent")  # kw_max_avg is problematic (p < 2e-16)

# Normality:
lillie.test(resA)    # p < 2.2e-16 => reject H0 => normality is violated
lillie.test(res.cl)  # p < 2.2e-16 => reject H0 => normality is violated

# Homoscedasticity:
qfitA    <- cut(fitA,   breaks = quantile(fitA),   include.lowest = TRUE)
qfitA.cl <- cut(fit.cl, breaks = quantile(fit.cl), include.lowest = TRUE)
leveneTest(resA,   qfitA)    # p < 2.2e-16 => reject H0 => homoscedasticity is violated
leveneTest(res.cl, qfitA.cl) # p = 1.398e-15 => reject H0 => homoscedasticity is violated

# Independence:
durbinWatsonTest(mA,       max.lag = 8)
durbinWatsonTest(mA.clean, max.lag = 8)
# No indication of serial correlation.

#------ Predictive Ability of Model A

pred_mA      <- predict(mA.clean, newdata = test)
actual       <- test$shares
rmse_test_mA <- sqrt(mean((actual - pred_mA)^2))
print(rmse_test_mA)
# Out-of-sample RMSE (mA) = 14770.31

#==============================================================================#
#            LASSO (Elastic-Net) Regression for Variable Screening            #
#==============================================================================#

mfull <- lm(shares ~ ., data = train.clean)

X     <- model.matrix(mfull)[, -1]
lasso <- glmnet(X, train.clean$shares)

# Use 10-fold CV to find a reasonable value for lambda
lasso1 <- cv.glmnet(X, train.clean$shares, alpha = 1)
lasso1$lambda
lasso1$lambda.min
lasso1$lambda.1se

par(mfrow = c(1, 2))
plot(lasso1)
abline(v = log(c(lasso1$lambda.min, lasso1$lambda.1se)), lty = 2)
plot(lasso1$glmnet.fit, xvar = "lambda", label = TRUE)

# Coefficients for lambda.min:
coef(lasso1, s = "lambda.min")
# Kept: n_tokens_title, num_videos, num_keywords, kw_avg_avg,
#       self_reference_avg_sharess, LDA_02, LDA_03, avg_negative_polarity,
#       max_negative_polarity, channelEntertainment, channelLifestyle, channelOther

# Coefficients for lambda.1se:
coef(lasso1, s = "lambda.1se")
# Kept none (intercept only)

#==============================================================================#
#                  Model B: Fit Using LASSO-Screened Variables                #
#==============================================================================#

# Create a reduced channel variable: combine levels LASSO did not keep into
# a single reference/baseline category.
kept_levels <- c("Entertainment", "Lifestyle", "Other")

train.clean$channel_reduced <- as.character(train.clean$channel)
train.clean$channel_reduced[!train.clean$channel_reduced %in% kept_levels] <- "Reference"
train.clean$channel_reduced <- factor(train.clean$channel_reduced)

# Apply same recoding to test data
test$channel_reduced <- as.character(test$channel)
test$channel_reduced[!test$channel_reduced %in% kept_levels] <- "Reference"
test$channel_reduced <- factor(test$channel_reduced)

train.clean$channel_reduced <- relevel(train.clean$channel_reduced, ref = "Reference")
test$channel_reduced        <- relevel(test$channel_reduced,        ref = "Reference")

# Fit model B
mB <- lm(shares ~ n_tokens_title + num_videos + num_keywords + kw_avg_avg +
            self_reference_avg_sharess + LDA_02 + LDA_03 + avg_negative_polarity +
            max_negative_polarity + channel_reduced, data = train.clean)
summary(mB)

# Stepwise check -- model remains the same
step(mB, direction = "backward")

# Multicollinearity check
vif(mB)
# All good.

#------ Predictive Ability of Model B

pred_mB      <- predict(mB, newdata = test)
actual       <- test$shares
rmse_test_mB <- sqrt(mean((actual - pred_mB)^2))
print(rmse_test_mB)
# Out-of-sample RMSE (mB) = 14714.5 < 14770.31 = RMSE (mA)
# Model B's predictions are typically off by 14714.5 shares.
# Model A's predictions are typically off by 14770.31 shares.
# Model B has better predictive ability => proceed with Model B.

#==============================================================================#
#                     Model B: Assumption Checks                              #
#==============================================================================#

resB <- rstudent(mB)
fitB <- fitted(mB)

par(mfrow = c(2, 3))

# Residuals vs Fitted
plot(mB, which = 1, main = "Linearity:")

# Q-Q Plot
qqnorm(resB, main = "Normality: Q-Q Plot")
qqline(resB, col = "red", lwd = 1.5)

# Scale-Location
plot(fitB, sqrt(abs(resB)),
     main = "Homoscedasticity: Scale-Location",
     xlab = "Fitted values", ylab = "Sqrt. Abs. Studentized Residuals")
abline(h = mean(sqrt(abs(resB))), col = "red", lty = 2, lwd = 1.5)
abline(h = c(-2, 2), col = "blue", lty = 2)

# Time Series Plot
plot(resB, type = "l",
     main = "Independence: Residuals vs Order",
     xlab = "Observation Order",
     ylab = "Studentized Residuals")
abline(h = 0, col = "red", lty = 2, lwd = 1.5)

# ACF Plot
acf(resB, main = "Independence: ACF of Res")

# Cook's Distance
plot(mB, which = 5)

#------ Assumption Tests for Model B

residualPlots(mB, test = TRUE, type = "rstudent")
# At least one p-value < a => linearity is violated.
# Only problematic variable: kw_avg_avg.

lillie.test(resB)
# p-value < 2.2e-16 => normality is violated.

qfitB <- cut(fitB, breaks = quantile(fitB), include.lowest = TRUE)
leveneTest(resB, qfitB)
# p-value < 2.2e-16 => homoscedasticity is violated.

#==============================================================================#
#              Attempted Transformations to Fix Violations                    #
#==============================================================================#

# m0: log(shares) -- log transform on response
m0   <- lm(log(shares) ~ n_tokens_title + num_videos + num_keywords + kw_avg_avg +
             self_reference_avg_sharess + LDA_02 + LDA_03 + avg_negative_polarity +
             max_negative_polarity + channel_reduced, data = train.clean)
res0 <- rstudent(m0)
fit0 <- fitted(m0)

par(mfrow = c(1, 3))
plot(m0, which = 1, main = "Linearity:")
qqnorm(res0, main = "Normality: Q-Q Plot")
qqline(res0, col = "red", lwd = 1.5)
plot(fit0, sqrt(abs(res0)), main = "Homoscedasticity: Scale-Location",
     xlab = "Fitted values", ylab = "Sqrt. Abs. Studentized Residuals")
abline(h = mean(sqrt(abs(res0))), col = "red", lty = 2, lwd = 1.5)
abline(h = c(-2, 2), col = "blue", lty = 2)

residualPlots(m0, test = TRUE, type = "rstudent")
# Log transformation caused more non-linearity problems.
lillie.test(res0)
# p < 2.2e-16 => normality not solved.
qfit0 <- cut(fit0, breaks = quantile(fit0), include.lowest = TRUE)
leveneTest(res0, qfit0)
# p < 2.2e-16 => homoscedasticity not solved.

#----
# m1: poly(kw_avg_avg, 2) -- polynomial term for the non-linear predictor
m1   <- lm(shares ~ n_tokens_title + num_videos + num_keywords + poly(kw_avg_avg, 2) +
             self_reference_avg_sharess + LDA_02 + LDA_03 + avg_negative_polarity +
             max_negative_polarity + channel_reduced, data = train.clean)
res1 <- rstudent(m1)
fit1 <- fitted(m1)

par(mfrow = c(1, 3))
plot(m1, which = 1, main = "Linearity:")
qqnorm(res1, main = "Normality: Q-Q Plot")
qqline(res1, col = "red", lwd = 1.5)
plot(fit1, sqrt(abs(res1)), main = "Homoscedasticity: Scale-Location",
     xlab = "Fitted values", ylab = "Sqrt. Abs. Studentized Residuals")
abline(h = mean(sqrt(abs(res1))), col = "red", lty = 2, lwd = 1.5)
abline(h = c(-2, 2), col = "blue", lty = 2)

residualPlots(m1, test = TRUE, type = "rstudent")
# NON-LINEARITY PROBLEM IS SOLVED.
lillie.test(res1)
# p < 2.2e-16 => normality still violated.
qfit1 <- cut(fit1, breaks = quantile(fit1), include.lowest = TRUE)
leveneTest(res1, qfit1)
# p < 2.2e-16 => homoscedasticity still violated.
# poly(kw_avg_avg, 2) solved non-linearity but could not fix normality/homoscedasticity.

#----
# m2: log(shares) + poly(kw_avg_avg, 2)
m2   <- lm(log(shares) ~ n_tokens_title + num_videos + num_keywords + poly(kw_avg_avg, 2) +
             self_reference_avg_sharess + LDA_02 + LDA_03 + avg_negative_polarity +
             max_negative_polarity + channel_reduced, data = train.clean)
res2 <- rstudent(m2)
fit2 <- fitted(m2)

par(mfrow = c(1, 3))
plot(m2, which = 1, main = "Linearity:")
qqnorm(res2, main = "Normality: Q-Q Plot")
qqline(res2, col = "red", lwd = 1.5)
plot(fit2, sqrt(abs(res2)), main = "Homoscedasticity: Scale-Location",
     xlab = "Fitted values", ylab = "Sqrt. Abs. Studentized Residuals")
abline(h = mean(sqrt(abs(res2))), col = "red", lty = 2, lwd = 1.5)
abline(h = c(-2, 2), col = "blue", lty = 2)

residualPlots(m2, test = TRUE, type = "rstudent")
# Caused more non-linearity problems.
lillie.test(res2)
# p < 2.2e-16 => non-normality not solved.
qfit2 <- cut(fit2, breaks = quantile(fit2), include.lowest = TRUE)
leveneTest(res2, qfit2)
# p < 2.2e-16 => heteroscedasticity not solved.

#----
# m3: I(kw_avg_avg^2) + I(self_reference_avg_sharess^2)
m3   <- lm(shares ~ n_tokens_title + num_videos + num_keywords + I(kw_avg_avg^2) +
             I(self_reference_avg_sharess^2) + LDA_02 + LDA_03 + avg_negative_polarity +
             max_negative_polarity + channel_reduced, data = train.clean)
res3 <- rstudent(m3)
fit3 <- fitted(m3)

par(mfrow = c(1, 3))
plot(m3, which = 1, main = "Linearity:")
qqnorm(res3, main = "Normality: Q-Q Plot")
qqline(res3, col = "red", lwd = 1.5)
plot(fit3, sqrt(abs(res3)), main = "Homoscedasticity: Scale-Location",
     xlab = "Fitted values", ylab = "Sqrt. Abs. Studentized Residuals")
abline(h = mean(sqrt(abs(res3))), col = "red", lty = 2, lwd = 1.5)
abline(h = c(-2, 2), col = "blue", lty = 2)

residualPlots(m3, test = TRUE, type = "rstudent")
# I(kw_avg_avg^2) still exhibits non-linearity (p = 0.0003574).
lillie.test(res3)
# p < 2.2e-16 => non-normality not solved.
qfit3 <- cut(fit3, breaks = quantile(fit3), include.lowest = TRUE)
leveneTest(res3, qfit3)
# p = 7.188e-16 => heteroscedasticity not solved.

#----
# m4: sqrt(kw_avg_avg)
m4   <- lm(shares ~ n_tokens_title + num_videos + num_keywords + sqrt(kw_avg_avg) +
             self_reference_avg_sharess + LDA_02 + LDA_03 + avg_negative_polarity +
             max_negative_polarity + channel_reduced, data = train.clean)
res4 <- rstudent(m4)
fit4 <- fitted(m4)

par(mfrow = c(1, 3))
plot(m4, which = 1, main = "Linearity:")
qqnorm(res4, main = "Normality: Q-Q Plot")
qqline(res4, col = "red", lwd = 1.5)
plot(fit4, sqrt(abs(res4)), main = "Homoscedasticity: Scale-Location",
     xlab = "Fitted values", ylab = "Sqrt. Abs. Studentized Residuals")
abline(h = mean(sqrt(abs(res4))), col = "red", lty = 2, lwd = 1.5)
abline(h = c(-2, 2), col = "blue", lty = 2)

residualPlots(m4, test = TRUE, type = "rstudent")
# LINEARITY SOLVED.
lillie.test(res4)
# p < 2.2e-16 => non-normality not solved.
qfit4 <- cut(fit4, breaks = quantile(fit4), include.lowest = TRUE)
leveneTest(res4, qfit4)
# p < 2.2e-16 => heteroscedasticity not solved.

# After many transformations, none could fix all assumption violations simultaneously.
# We interpret Model B for assignment purposes and explicitly state why its
# coefficient estimates cannot be fully trusted.

#---- Centre all numeric predictors so the intercept has a logical interpretation
# (corresponds to a post with average values for all numeric predictors)

x1.centered  <- train.clean$n_tokens_title            - mean(train.clean$n_tokens_title)
x2.centered  <- train.clean$num_videos                - mean(train.clean$num_videos)
x3.centered  <- train.clean$num_keywords              - mean(train.clean$num_keywords)
x4.centered  <- train.clean$kw_avg_avg                - mean(train.clean$kw_avg_avg)
x5.centered  <- train.clean$self_reference_avg_sharess - mean(train.clean$self_reference_avg_sharess)
x6.centered  <- train.clean$LDA_02                    - mean(train.clean$LDA_02)
x7.centered  <- train.clean$LDA_03                    - mean(train.clean$LDA_03)
x8.centered  <- train.clean$avg_negative_polarity     - mean(train.clean$avg_negative_polarity)
x9.centered  <- train.clean$max_negative_polarity     - mean(train.clean$max_negative_polarity)
x10          <- train.clean$channel_reduced

mB.centered <- lm(train.clean$shares ~ x1.centered + x2.centered + x3.centered +
                    x4.centered + x5.centered + x6.centered + x7.centered +
                    x8.centered + x9.centered + x10)
summary(mB.centered)
# Proceed with interpretation.
