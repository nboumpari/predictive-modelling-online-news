
#==============================================================================#
#                Logistic Regression Modelling & Profiles                     #
#==============================================================================#

load("onlinenews.RData")

require(car)
require(nortest)
require(pROC)

# Assumes train.clean, test, and channel_reduced are available from 03_linear_modelling.R
# If running standalone, re-run the data prep steps from that script first.

#==============================================================================#
#                       Fit Logistic Regression Model                         #
#==============================================================================#

# Since shares are counts and are non-negative, a natural choice would be to
# model them using Poisson or Negative Binomial distribution. However, considering
# the large range and variability of shares, we instead model a binary outcome
# (viral vs typical) using logistic regression.

# Construct binary response using the same 90th percentile threshold as before
viral <- ifelse(train.clean$shares >= 6200, 1, 0)
newdat <- train.clean
newdat$shares <- NULL
newdat$channel_reduced <- NULL
newdat$viral <- viral

# Fit full logistic model
logit_full <- glm(viral ~ ., data = newdat, family = binomial(link = "logit"))

# Backward stepwise selection
step(logit_full, direction = "backward")

# Result of stepwise:
step_logit <- glm(formula = viral ~ n_unique_tokens + num_videos + average_token_length +
                    num_keywords + kw_min_max + kw_avg_max + kw_min_avg + kw_max_avg +
                    kw_avg_avg + self_reference_avg_sharess + LDA_02 + global_sentiment_polarity +
                    global_rate_negative_words + rate_negative_words + min_positive_polarity +
                    min_negative_polarity + abs_title_subjectivity + abs_title_sentiment_polarity +
                    channel,
                  family = binomial(link = "logit"), data = newdat)

#------ Multicollinearity Check (iterative VIF removal)

vif(step_logit)
# Remove kw_avg_avg (VIF = 13.4)

vif(glm(formula = viral ~ n_unique_tokens + num_videos + average_token_length +
          num_keywords + kw_min_max + kw_avg_max + kw_min_avg + kw_max_avg +
          self_reference_avg_sharess + LDA_02 + global_sentiment_polarity +
          global_rate_negative_words + rate_negative_words + min_positive_polarity +
          min_negative_polarity + abs_title_subjectivity + abs_title_sentiment_polarity +
          channel,
        family = binomial(link = "logit"), data = newdat))
# Remove rate_negative_words (VIF = 5.65)

vif(glm(formula = viral ~ n_unique_tokens + num_videos + average_token_length +
          num_keywords + kw_min_max + kw_avg_max + kw_min_avg + kw_max_avg +
          self_reference_avg_sharess + LDA_02 + global_sentiment_polarity +
          global_rate_negative_words + min_positive_polarity +
          min_negative_polarity + abs_title_subjectivity + abs_title_sentiment_polarity +
          channel,
        family = binomial(link = "logit"), data = newdat))
# Remove channel (VIF = 5.17)

vif(glm(formula = viral ~ n_unique_tokens + num_videos + average_token_length +
          num_keywords + kw_min_max + kw_avg_max + kw_min_avg + kw_max_avg +
          self_reference_avg_sharess + LDA_02 + global_sentiment_polarity +
          global_rate_negative_words + min_positive_polarity +
          min_negative_polarity + abs_title_subjectivity + abs_title_sentiment_polarity,
        family = binomial(link = "logit"), data = newdat))
# All clear.

# Fit final logistic regression model
logit <- glm(formula = viral ~ n_unique_tokens + num_videos + average_token_length +
               num_keywords + kw_min_max + kw_avg_max + kw_min_avg + kw_max_avg +
               self_reference_avg_sharess + LDA_02 + global_sentiment_polarity +
               global_rate_negative_words + min_positive_polarity +
               min_negative_polarity + abs_title_subjectivity + abs_title_sentiment_polarity,
             family = binomial(link = "logit"), data = newdat)
summary(logit)
# Residual deviance: 1857.8 < 1980.4 (null deviance) -- model explains meaningful variance.

#==============================================================================#
#                         Goodness of Fit                                     #
#==============================================================================#

#------ Null vs. Fitted Model
# H0: null model has better fit
# H1: fitted model has better fit
test.stat <- logit$null.deviance - logit$deviance
df.test <- logit$df.null - logit$df.residual
cv <- qchisq(0.95, df.test)
test.stat ; cv
# test.stat > cv => reject H0 => fitted model has better fit than null.

#------ Deviance Goodness of Fit Test
# H0: model has good fit
# H1: not H0
dev <- logit$deviance
df <- logit$df.residual
cv <- qchisq(0.95, df)
dev ; cv
# deviance << cv => cannot reject H0 => model has good fit.

#==============================================================================#
#                        Predictive Performance                               #
#==============================================================================#

#------ In-Sample ROC Curve and AUC

newdat$phat <- predict(logit, newdata = newdat, type = "response")
roc_obj_in <- roc(newdat$viral, newdat$phat)
plot(roc_obj_in, main = "ROC Curve")
auc(roc_obj_in)
# AUC = 0.6888
# If we randomly pick one viral and one typical post, the model will assign a
# higher predicted probability to the viral post ~69% of the time.
# Meaningfully better than chance (0.50).
# The model shows modest but meaningful discriminatory power.

#------ In-Sample Classification Accuracy

pred_prob <- predict(logit, type = "response")
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)
actual <- logit$y
accuracy <- mean(pred_class == actual)
accuracy
# In-sample accuracy = 89.7% !!
# Almost 90% of observations are correctly classified.
# BUT this does not guarantee good out-of-sample accuracy.

#------ Out-of-Sample Classification Accuracy

datanew <- test
datanew$viral <- ifelse(test$shares >= 6200, 1, 0)

test_logit <- glm(formula = viral ~ n_unique_tokens + num_videos + average_token_length +
                    num_keywords + kw_min_max + kw_avg_max + kw_min_avg + kw_max_avg +
                    self_reference_avg_sharess + LDA_02 + global_sentiment_polarity +
                    global_rate_negative_words + min_positive_polarity +
                    min_negative_polarity + abs_title_subjectivity + abs_title_sentiment_polarity,
                  family = binomial(link = "logit"), data = datanew)

test_prob <- predict(test_logit, type = "response")
test_class <- ifelse(test_prob >= 0.5, 1, 0)
test_actual <- datanew$viral
test_accuracy <- mean(test_class == test_actual)
test_accuracy
# Out-of-sample accuracy = 89.6%
# The model's predictive performance generalises well to unseen data.
# The predictors retain explanatory and predictive power beyond the estimation sample.
# (Very close to in-sample accuracy -- limited overfitting.)

#------ Out-of-Sample ROC Curve

roc_obj_out <- roc(test_actual, test_prob)
auc(roc_obj_out)
# AUC = 0.6467

par(mfrow = c(1, 2))
plot(roc_obj_in,  main = "ROC Curve (in-sample)")
plot(roc_obj_out, main = "ROC Curve (out-of-sample)")

#==============================================================================#
#                               Profiles                                      #
#==============================================================================#

# Compute mean values for predictors in the final logistic model
profiles <- train.clean[, c(
  "n_unique_tokens", "num_videos", "average_token_length", "num_keywords",
  "kw_min_max", "kw_avg_max", "kw_min_avg", "kw_max_avg",
  "self_reference_avg_sharess", "LDA_02", "global_sentiment_polarity",
  "global_rate_negative_words", "min_positive_polarity", "min_negative_polarity",
  "abs_title_subjectivity", "abs_title_sentiment_polarity")]

round(aggregate(cbind(n_unique_tokens, num_videos, average_token_length, num_keywords,
                      kw_min_max, kw_avg_max, kw_min_avg, kw_max_avg,
                      self_reference_avg_sharess, LDA_02, global_sentiment_polarity,
                      global_rate_negative_words, min_positive_polarity,
                      min_negative_polarity, abs_title_subjectivity,
                      abs_title_sentiment_polarity) ~ viral,
                data = profiles, FUN = mean), 2)

#---- Typical Post Profile Means:
# n_unique_tokens = 0.53          | num_videos = 1.10
# average_token_length = 4.60     | num_keywords = 7.20
# kw_min_max = 12970.50           | kw_avg_max = 254336.30
# kw_min_avg = 1097.70            | kw_max_avg = 5355.90
# self_reference_avg_sharess = 5332.1
# LDA_02 = 0.23                   | global_sentiment_polarity = 0.12
# global_rate_negative_words = 0.02 | min_positive_polarity = 0.09
# min_negative_polarity = -0.52   | abs_title_subjectivity = 0.34
# abs_title_sentiment_polarity = 0.15

#---- Viral Post Profile Means:
# n_unique_tokens = 0.53          | num_videos = 1.95
# average_token_length = 4.42     | num_keywords = 7.58
# kw_min_max = 22262.51           | kw_avg_max = 278181.1
# kw_min_avg = 1306.96            | kw_max_avg = 8033.98
# self_reference_avg_sharess = 10860.24
# LDA_02 = 0.14                   | global_sentiment_polarity = 0.13
# global_rate_negative_words = 0.02 | min_positive_polarity = 0.09
# min_negative_polarity = -0.54   | abs_title_subjectivity = 0.35
# abs_title_sentiment_polarity = 0.21

#------ Plug Profile Values into Logistic Model

x_typical <- c(n_unique_tokens             = 0.53,
               num_videos                  = 1.09,
               average_token_length        = 4.59,
               num_keywords                = 7.21,
               kw_min_max                  = 12970.50,
               kw_avg_max                  = 254336.3,
               kw_min_avg                  = 1097.74,
               kw_max_avg                  = 5355.93,
               self_reference_avg_sharess  = 5332.06,
               LDA_02                      = 0.23,
               global_sentiment_polarity   = 0.12,
               global_rate_negative_words  = 0.02,
               min_positive_polarity       = 0.09,
               min_negative_polarity       = -0.52,
               abs_title_subjectivity      = 0.34,
               abs_title_sentiment_polarity = 0.15)

log_odds_typical <- coef(logit)["(Intercept)"] + sum(coef(logit)[-1] * x_typical)
prob_typical <- 1 / (1 + exp(-log_odds_typical))
prob_typical
# 0.0860 -- Articles with average characteristics of typical news have about
# a 1 in 12 chance of going viral.

x_viral <- c(n_unique_tokens             = 0.53,
             num_videos                  = 1.95,
             average_token_length        = 4.42,
             num_keywords                = 7.58,
             kw_min_max                  = 22262.51,
             kw_avg_max                  = 278181.1,
             kw_min_avg                  = 1506.96,
             kw_max_avg                  = 8033.98,
             self_reference_avg_sharess  = 10860.24,
             LDA_02                      = 0.14,
             global_sentiment_polarity   = 0.13,
             global_rate_negative_words  = 0.02,
             min_positive_polarity       = 0.09,
             min_negative_polarity       = -0.54,
             abs_title_subjectivity      = 0.35,
             abs_title_sentiment_polarity = 0.21)

log_odds_viral <- coef(logit)["(Intercept)"] + sum(coef(logit)[-1] * x_viral)
prob_viral <- 1 / (1 + exp(-log_odds_viral))
prob_viral
# 0.1304 -- Articles with characteristics matching the viral group average have
# about a 1 in 8 chance of going viral.

#==============================================================================#
#                  Profiles for Model B (Linear Regression)                  #
#==============================================================================#

# Find average values for predictors in Model B
round(aggregate(. ~ viral, data = train.clean[, c("n_tokens_title", "num_videos",
                                                   "num_keywords", "kw_avg_avg",
                                                   "self_reference_avg_sharess",
                                                   "LDA_02", "LDA_03",
                                                   "avg_negative_polarity",
                                                   "max_negative_polarity")],
                FUN = mean), 2)

# Get modal channel for each viral status (most frequent level)
tt <- table(viral, train.clean$channel_reduced)
mode_channel_typical <- names(which.max(tt[1, ])) ; print(mode_channel_typical)  # reference
mode_channel_viral   <- names(which.max(tt[2, ])) ; print(mode_channel_viral)    # reference

# Plug profile values into Model B
y_typical <- data.frame(
  n_tokens_title             = 10.40,
  num_videos                 = 1.09,
  num_keywords               = 7.21,
  kw_avg_avg                 = 3023.20,
  self_reference_avg_sharess = 5332.06,
  LDA_02                     = 0.23,
  LDA_03                     = 0.20,
  avg_negative_polarity      = -0.26,
  max_negative_polarity      = -0.11,
  channel_reduced            = "Reference"
)

y_viral <- data.frame(
  n_tokens_title             = 10.38,
  num_videos                 = 1.95,
  num_keywords               = 7.58,
  kw_avg_avg                 = 3929.97,
  self_reference_avg_sharess = 10860.24,
  LDA_02                     = 0.14,
  LDA_03                     = 0.33,
  avg_negative_polarity      = -0.27,
  max_negative_polarity      = -0.11,
  channel_reduced            = "Reference"
)

pred_typical <- predict(mB, newdata = y_typical) ; print(pred_typical)  # 3298.84
pred_viral   <- predict(mB, newdata = y_viral)   ; print(pred_viral)    # 4482.425

