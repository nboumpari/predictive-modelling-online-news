
#==============================================================================#
#                       Exploratory Data Analysis                              #
#==============================================================================#

load("onlinenews.RData")

require(psych)
require(sjPlot)
require(corrplot)
require(Hmisc)
require(nortest)
require(car)
require(plotrix)

#==============================================================================#
#                    EDA for Numeric Variables                                 #
#==============================================================================#

numeric_data <- data[sapply(data, is.numeric)]
head(numeric_data)

#------ Normality Check for All Numeric Variables

# n = 3000 --> Kolmogorov-Smirnov with Lilliefors correction

pvals <- numeric(length(numeric_data))
names(pvals) <- names(numeric_data)
vars <- names(numeric_data)

for (v in vars) {
  x <- na.omit(data[[v]])
  pvals[v] <- lillie.test(x)$p.value
}
pvals
# All p-values < a => reject H0 for all variables => normality doesn't hold.

#------ Correlation Matrix (Bivariate)

# Since normality is rejected for all numeric variables, we cannot fully trust
# the p-values from Pearson's method, although we can use it as a visual.
tab_corr(numeric_data, triangle = "lower")

# Consider the Spearman correlations and p-values
spr <- rcorr(as.matrix(numeric_data), type = "spearman")
spr$r  # Spearman correlations
spr$P  # Spearman p-values

# Variables with which shares has a statistically significant correlation:
# shares - n_unique_tokens             (p = 5.296e-03)   --> (-0.0509)
# shares - n_non_stop_unique_tokens    (p = 1.361e-05)   --> (-0.0793)
# shares - num_hrefs                   (p = 2.246e-08)   --> ( 0.1019)
# shares - num_self_hrefs              (p = 3.219e-03)   --> ( 0.0538)
# shares - num_imgs                    (p = 3.085e-07)   --> ( 0.0933)
# shares - num_videos                  (p = 4.689e-04)   --> ( 0.0638)
# shares - average_token_length        (p = 1.553e-04)   --> (-0.0690)
# shares - num_keywords                (p = 8.700e-11)   --> ( 0.1181)
# shares - kw_max_min                  (p = 5.873e-09)   --> ( 0.1060)
# shares - kw_avg_min                  (p = 3.123e-10)   --> ( 0.1146)
# shares - kw_min_avg                  (p = 3.308e-06)   --> ( 0.0848)
# shares - kw_max_avg                  (p = 0)           --> ( 0.2583)
# shares - kw_avg_avg                  (p = 0)           --> ( 0.2713)
# shares - self_reference_min_shares   (p = 0)           --> ( 0.1815)
# shares - self_reference_max_shares   (p = 0)           --> ( 0.1905)

#------ Correlation Plot

# Keep variables from Spearman and illustrate using a correlation plot
variables <- c("shares", "n_unique_tokens", "n_non_stop_unique_tokens", "num_hrefs",
               "num_self_hrefs", "num_imgs", "num_videos", "average_token_length",
               "num_keywords", "kw_max_min", "kw_avg_min", "kw_min_avg",
               "kw_max_avg", "kw_avg_avg", "self_reference_min_shares", "self_reference_max_shares")
subset      <- data[, variables]
cor_matrix  <- cor(subset)

dev.off()
corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.7, tl.col = "black")

# Ellipse method
corrplot(cor(subset), method = "ellipse", type = "upper")

#------ Scatterplots (Bivariate)

par(mfrow = c(3, 5), mar = c(5, 4, 2, 1))
vars <- variables[-1]  # excluding shares
for (v in vars) {
  plot(data[[v]], data$shares,
       col  = "blue", pch = 19, cex = 0.6,
       xlab = v, ylab = "Shares")
}

# While individual variables show statistically significant correlations with
# shares (p < 0.05), the scatterplots and correlation values reveal weak linear
# relationships, suggesting that predicting viral success requires a
# multivariable approach when modelling.

#------ Histograms

num_cols_names <- names(numeric_data)
length(num_cols_names)  # 45 numeric variables -- split in 4 sections

par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
for (v in num_cols_names[1:10]) {
  hist(data[[v]], col = "blue", border = "white", xlab = v,
       main = paste("Hist. of", v), cex.main = 0.9, cex.lab = 0.8)
}

par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
for (v in num_cols_names[11:20]) {
  hist(data[[v]], col = "blue", border = "white", xlab = v,
       main = paste("Hist. of", v), cex.main = 0.9, cex.lab = 0.8)
}

par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
for (v in num_cols_names[21:30]) {
  hist(data[[v]], col = "blue", border = "white", xlab = v,
       main = paste("Hist. of", v), cex.main = 0.9, cex.lab = 0.8)
}

par(mfrow = c(1, 5), mar = c(4, 4, 2, 1))
for (v in num_cols_names[31:35]) {
  hist(data[[v]], col = "blue", border = "white", xlab = v,
       main = paste("Hist. of", v), cex.main = 0.9, cex.lab = 0.8)
}

#------ Boxplots

par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
for (v in num_cols_names[1:10]) {
  boxplot(data[[v]], col = "green", xlab = v,
          main = paste("Boxplot of", v), cex.main = 0.9, cex.lab = 0.8)
}

par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
for (v in num_cols_names[11:20]) {
  boxplot(data[[v]], col = "green", xlab = v,
          main = paste("Boxplot of", v), cex.main = 0.9, cex.lab = 0.8)
}

par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
for (v in num_cols_names[21:30]) {
  boxplot(data[[v]], col = "green", xlab = v,
          main = paste("Boxplot of", v), cex.main = 0.9, cex.lab = 0.8)
}

par(mfrow = c(1, 5), mar = c(4, 4, 2, 1))
for (v in num_cols_names[31:35]) {
  boxplot(data[[v]], col = "green", xlab = v,
          main = paste("Boxplot of", v), cex.main = 0.9, cex.lab = 0.8)
}

#------ Normal QQ Plots

par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
for (v in num_cols_names[1:10]) {
  qqnorm(data[[v]], main = paste("Normal QQP. of", v), cex.main = 0.8)
  qqline(data[[v]], col = "red")
}

par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
for (v in num_cols_names[11:20]) {
  qqnorm(data[[v]], main = paste("Normal QQP. of", v), cex.main = 0.8)
  qqline(data[[v]], col = "red")
}

par(mfrow = c(2, 5), mar = c(4, 4, 2, 1))
for (v in num_cols_names[21:30]) {
  qqnorm(data[[v]], main = paste("Normal QQP. of", v), cex.main = 0.8)
  qqline(data[[v]], col = "red")
}

par(mfrow = c(1, 5), mar = c(4, 4, 2, 1))
for (v in num_cols_names[31:35]) {
  qqnorm(data[[v]], main = paste("Normal QQP. of", v), cex.main = 0.8)
  qqline(data[[v]], col = "red")
}

# Comments on Normal QQ Plots:
# Some sample distributions are highly skewed, resulting in the curve flattening
# at the bottom/left and shooting up at the top/right.
# For other variables such as counts, sums, or shares, we cannot have negative
# numbers. Since the Normal distribution allows negative values, comparing a
# strictly positive variable to a normal distribution results in the curvature
# we notice in some QQ plots.

dev.off()  # reset

#------ Summary Statistics (Univariate)

# Descriptives for all numeric variables (present only the most important ones).
# Rounded for presentation purposes.
descriptives <- round(psych::describe(numeric_data), 2)
descriptives

# From the previous analysis, presenting only variables related to:
# shares, num_hrefs, kw_avg_avg, num_imgs, num_videos, num_keywords

chosen_vars <- c("shares", "num_hrefs", "kw_avg_avg", "num_imgs", "num_videos", "num_keywords")

par(mfrow = c(2, 6), mar = c(4, 4, 2, 2))
for (cv in chosen_vars) {
  hist(data[[cv]], pch = 19, cex = 0.6,
       main = paste("Hist. of", cv), xlab = cv,
       col = "blue", border = "white", cex.main = 0.9, cex.lab = 0.8)
}

for (cv in chosen_vars) {
  qqnorm(data[[cv]], main = paste("Normal QQp.", cv), cex.main = 0.8, cex.lab = 0.8)
  qqline(data[[cv]], col = "red", lwd = 1.5)
}

par(mfrow = c(1, 6))
for (cv in chosen_vars) {
  boxplot(data[[cv]], pch = 19, cex = 0.6,
          main = paste("Boxp. of", cv), xlab = cv,
          col = "palegreen2", cex.main = 0.9)
}

dev.off()  # reset

#==============================================================================#
#                    EDA for Categorical Variables                             #
#==============================================================================#

#------ Pie Charts

par(mar = c(1, 0, 1, 0))
beige_palette <- colorRampPalette(c("lightyellow", "tan", "brown"))

# Distribution of Articles by Day
day_counts <- table(data$day)
day_perc <- round(100 * prop.table(day_counts), 1)
labels1 <- paste(names(day_counts), "\nFreq:", day_counts, "\n(", day_perc, "%)", sep = "")
pie(day_counts, labels = labels1,
    main = "Distribution of Articles by Day",
    col = beige_palette(7), border = "white", cex = 0.8, cex.main = 0.9)

# Distribution of Articles by Data Channel
channel_counts <- table(data$channel)
channel_perc <- round(100 * prop.table(channel_counts), 1)
labels2 <- paste(names(channel_counts), "\nFreq:", channel_counts, "\n(", channel_perc, "%)", sep = "")
pie(channel_counts, labels = labels2,
    main = "Distribution of Articles by Channel",
    col = c("cadetblue1", "lightblue", "royalblue1",
            "lightseagreen", "turquoise", "blue3", "skyblue4"),
    border = "white", cex = 0.8, cex.main = 0.9)

# Distribution of Typical vs Viral Posts
popularity_counts <- table(viral)
popularity_perc <- round(100 * prop.table(popularity_counts), 1)
labels3 <- paste(names(popularity_counts), "\nFreq:", popularity_counts, "\n(", popularity_perc, "%)", sep = "")
pie(popularity_counts, labels = labels3,
    main = "Distribution of Articles by Popularity",
    col = c("lavenderblush4", "lightslateblue"),
    border = "white", cex = 0.8, cex.main = 0.9)

#------ Barplots

dev.off()
par(mfrow = c(1, 2), mar = c(5, 4, 6, 3), oma = c(0, 0, 0, 0))

# Distribution of Typical vs Viral Posts by Day
count1 <- table(viral, data$day)
perc1  <- prop.table(count1)
b1 <- barplot(perc1, horiz = TRUE, las = 2,
              main = "Distribution of Typical vs Viral Posts by Day",
              xlab = "Relative Frequency",
              col = c("grey", "aquamarine"), border = "white")
legend("top", legend = c("Typical", "Viral"),
       fill = c("grey", "aquamarine"), border = "white",
       horiz = TRUE, bty = "n", inset = c(0.05, -0.15), xpd = TRUE)

# Distribution of Typical vs Viral Posts by Channel
count2 <- table(viral, data$channel)
perc2  <- prop.table(count2)
b2 <- barplot(perc2, horiz = TRUE, las = 2,
              main = "Distribution of Typical vs Viral Posts by Channel",
              xlab = "Relative Frequency",
              col = c("grey", "dodgerblue"), border = "white")
legend("top", legend = c("Typical", "Viral"),
       fill = c("grey", "dodgerblue"), border = "white",
       horiz = TRUE, bty = "n", inset = c(0.05, -0.15), xpd = TRUE)

#==============================================================================#
#                         Pairwise Comparisons                                 #
#==============================================================================#

# All tests are performed at a = 0.05 (5%)

dev.off()

#---- (1) Shares ~ Channel Type

# H0: mu1 = mu2 = mu3 = mu4 = mu5 = mu6 = mu7
# H1: mui != muj, for at least one i, j = 1, ..., 7 with i != j

aov1 <- aov(data$shares ~ data$channel)
summary(aov1)
res1 <- residuals(aov1)

# Normality check: n = 3000 --> KS with Lilliefors correction
lillie.test(res1)
qqnorm(res1, main = "Normal QQPlot of Res1")
qqline(res1, col = "purple")
# p-value < 2.2e-16 => reject H0 => normality does not hold.
# We can also see a heavy right tail in the QQ plot which visually confirms this.

# Homoscedasticity Check
leveneTest(data$shares, data$channel)
# p-value = 5.211e-07 => reject H0 => homoscedasticity does not hold.

# Since normality is rejected but we have a large enough sample (>50), we assess
# whether or not the mean is a sufficient metric for the central location.
by(data$shares, data$channel, mean)
by(data$shares, data$channel, median)
# There are big differences between means and medians for every group.

by(data$shares, data$channel, kurtosi)  # huge kurtosis present for some groups
by(data$shares, data$channel, skew)     # big skewness present for some groups

# The mean is not a sufficient metric => proceed with non-parametric Kruskal-Wallis
# H0: M1 = M2 = M3 = M4 = M5 = M6 = M7
# H1: Mi != Mj, for at least one i, j = 1, ..., 7 with i != j

kruskal.test(data$shares ~ data$channel)
# p-value < 2.2e-16 => reject H0 => statistically significant differences in
# median shares exist between channel categories.

# Post-hoc analysis to identify which medians differ
pairwise.wilcox.test(data$shares, data$channel)

# The only pairs WITHOUT statistically significant differences in median shares:
# Business - Entertainment  (p = 0.27506)
# World - Entertainment     (p = 0.52954)
# Lifestyle - Other         (p = 0.52954)
# Social Media - Other      (p = 0.52954)
# All other pairs have statistically significant differences.

# Complementary Boxplot
boxplot(data$shares ~ data$channel,
        main = "Shares ~ Channel Categories",
        xlab = "Channel Categories",
        ylab = "Log(shares)",
        col  = grey.colors(7),
        log  = "y")

#---- (2) Shares ~ Day of the Week

# H0: mu1 = mu2 = mu3 = mu4 = mu5 = mu6 = mu7
# H1: mui != muj, for at least one i, j = 1, ..., 7 with i != j
# where mui: mean shares for the i-th day of the week

aov2 <- aov(data$shares ~ data$day)
summary(aov2)
res2 <- residuals(aov2)

# Normality Check
lillie.test(res2)
qqnorm(res2)
qqline(res2, col = "purple")
# p-value < 2.2e-16 => reject H0 => normality does not hold.

# Homoscedasticity Check
leveneTest(data$shares, data$day)
# p-value = 0.6696 => cannot reject H0 => homoscedasticity holds.

# Large enough samples -- assess whether mean is sufficient for central location
by(data$shares, data$day, mean)
by(data$shares, data$day, median)
# Big differences in means and medians between groups.
by(data$shares, data$day, kurtosi)  # huge kurtosis
by(data$shares, data$day, skew)     # big skewness

# Mean is not a good metric => proceed with non-parametric Kruskal-Wallis
# H0: M1 = M2 = M3 = M4 = M5 = M6 = M7
# H1: Mi != Mj, for at least one i, j = 1, ..., 7 with i != j
# where Mi: median shares for the i-th day of the week

kruskal.test(data$shares ~ data$day)
# p-value = 2.799e-14 => reject H0 => statistically significant differences in
# median shares exist between posts published on different days.

# Post-hoc analysis to identify which groups differ
pairwise.wilcox.test(data$shares, data$day)
# Mon - Fri (p = 0.00035)
# Sun - Fri (p = 4.1e-05)
# Sat - Mon (p = 0.00013)
# Mon - Sun (p = 1.3e-05)
# Thu - Sat (p = 3.4e-06)
# Tue - Sat (p = 2.6e-06)
# Wed - Sat (p = 1.2e-07)
# Thu - Sun (p = 1.1e-07)
# Tue - Sun (p = 9.7e-08)
# Wed - Sun (p = 3.0e-09)

# Complementary Boxplot (log scale to see more clearly given heavy skew)
boxplot(data$shares ~ data$day, log = "y",
        xlab  = "Day of the Week",
        ylab  = "Log(Shares)",
        main  = "Shares by Day of the Week",
        col   = c(2:7))

#---- (3) num_imgs (one-sample test)

# H0: mu = 3
# H1: mu < 3

# Normality Check
lillie.test(data$num_imgs)
qqnorm(data$num_imgs, main = "Normal QQPlot of NumImgs")
qqline(data$num_imgs, col = "purple")
# p-value < 2.2e-16 => reject H0 => normality does not hold.
# A very heavy right tail is visible in the QQ plot.

# Large enough sample (n > 50): is mean a sufficient metric?
mean(data$num_imgs)    # 4.37
median(data$num_imgs)  # 1.00
# Big difference between mean and median.

kurtosi(data$num_imgs)  # very large positive kurtosis (28.14 >> 0)
skew(data$num_imgs)     # very large positive skewness  (4.28 >> 0)

# Mean is not sufficient => proceed with non-parametric Wilcoxon test
# H0: M = 3
# H1: M < 3
wilcox.test(data$num_imgs, mu = 3, alternative = "less")
# p-value < 2.2e-16 => reject H0 => the median number of images per post is
# statistically significantly less than 3.
# Practically, half of all posts in the sample have fewer than 3 images.

#---- (4) num_videos (one-sample test)

# H0: mu = 1
# H1: mu > 1

# Normality Test
lillie.test(data$num_videos)
qqnorm(data$num_videos, main = "Normal QQPlot of NumVideos")
qqline(data$num_videos, col = "purple")
# p-value < 2.2e-16 => reject H0 => normality does not hold.

mean(data$num_videos)
median(data$num_videos)
# Big enough difference given the scale.

kurtosi(data$num_videos)  # huge positive kurtosis (82 >> 0)
skew(data$num_videos)     # huge positive skewness  (7.6 >> 0)

# Mean is not sufficient => proceed with non-parametric Wilcoxon test
# H0: M = 1
# H1: M > 1
wilcox.test(data$num_videos, mu = 1, alternative = "greater")
# p-value = 1 => cannot reject H0.
# Practically, 50% of posts have 1 or fewer videos.
# Most posts contain no videos or at most 1, so posts with multiple videos are uncommon.

#---- (5) num_imgs ~ viral

# H0: M1 = M2 (median images: typical vs viral)
# H1: M1 != M2

by(data$num_imgs, viral, length)  # n1 = 2692 (>50), n2 = 308 (>50)
by(data$num_imgs, viral, lillie.test)
# Typical: p < 2.2e-16 => reject H0
# Viral:   p < 2.2e-16 => reject H0
# Normality does not hold for either group.

by(data$num_imgs, viral, mean)
by(data$num_imgs, viral, median)
# Big differences between means and medians in both groups.

wilcox.test(data$num_imgs ~ viral)
# p-value = 0.1539 => cannot reject H0.
# The median number of images does not differ significantly between typical and
# viral posts. The number of images does not make or break a post's popularity.

#---- (6) num_videos ~ viral

# H0: mu1 = mu2 (mean videos: typical vs viral)
# H1: mu1 != mu2

by(data$num_videos, viral, length)
by(data$num_videos, viral, lillie.test)
# Typical: p < 2.2e-16 => reject H0
# Viral:   p < 2.2e-16 => reject H0
# Normality does NOT hold for either group.

by(data$num_videos, viral, mean)
by(data$num_videos, viral, median)
# Not big differences + large enough sample => proceed with parametric t-test

t.test(data$num_videos ~ viral)
# p-value = 0.01416 => reject H0.

# Error bar plot
means <- tapply(data$num_videos, viral, mean)
sds   <- tapply(data$num_videos, viral, sd)
ns    <- tapply(data$num_videos, viral, length)
ses   <- sds / sqrt(ns)

bp <- barplot(means, ylim = c(0, max(means + ses) * 1.2),
              xlab = "Viral Status", ylab = "Number of Videos",
              main = "Average Number of Videos by Viral Status",
              col = c("lightblue", "lightcoral"), border = "white",
              names.arg = c("Typical", "Viral"))
plotCI(bp, means, uiw = ses, add = TRUE, pch = NA)
# The mean number of videos in a viral post seems much larger than in a typical post.

#---- (7) n_tokens_title ~ viral

# H0: mu1 = mu2 (mean title words: typical vs viral)
# H1: mu1 != mu2

by(data$n_tokens_title, viral, lillie.test)
# Typical: p < 2.2e-16 / Viral: p = 1.894e-12 => reject H0 for both groups.

by(data$n_tokens_title, viral, mean)
by(data$n_tokens_title, viral, median)
# Very small difference (almost none) => proceed with parametric t-test

t.test(data$n_tokens_title ~ viral)
# p-value = 0.9558 => cannot reject H0.
# The number of words in the title does not play a statistically significant
# role in the post's popularity.

#---- (8) n_tokens_content ~ viral

# H0: M1 = M2 (median content words: typical vs viral)
# H1: M1 != M2

by(data$n_tokens_content, viral, lillie.test)
# Typical: p < 2.2e-16 / Viral: p < 2.2e-16 => reject H0 for both groups.

by(data$n_tokens_content, viral, mean)
by(data$n_tokens_content, viral, median)
# Big differences => proceed with Wilcoxon test

wilcox.test(data$n_tokens_content ~ viral)
# p-value = 0.01881 => reject H0.
# The number of words in the content DOES play a statistically significant role
# in the popularity of the post.
boxplot(data$n_tokens_content ~ viral,
        main = "Boxplot of Content Words per Virality Status",
        col  = c("grey", "blue"),
        xlab = "Viral Status", ylab = "Number of Words in Content",
        cex.main = 0.8)

#---- (9) is_weekend ~ viral

# H0: independence
# H1: not independent

tab <- table(data$is_weekend, viral)
chisq.test(tab)$expected
chisq.test(tab)   # p-value = 0.6133 => cannot reject H0
fisher.test(tab)  # p-value = 0.5347 => cannot reject H0
# The popularity of the post is independent of whether it is a weekday or weekend.

#---- (10) viral ~ channel type

# H0: independence
# H1: not H0

tab <- table(viral, data$channel)
chisq.test(tab)$expected  # all e_ij >= 5
chisq.test(tab)
# p-value < 2.2e-16 => reject H0.
# The popularity of a post depends on the channel type it is posted to.

