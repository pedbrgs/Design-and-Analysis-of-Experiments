# Design and Analysis of Experiments (EEE933)
# Team D: Pedro Vinicius, Samara Silva and Savio Vieira

# Clear workspace
rm(list=ls())
# Clear all plots
graphics.off()
# Clear console
cat("\014")

# Load data generation method
source('data_generation.R')
# Statistical package
library(stats)
# Ggplot2 package
library(ggplot2)
# Plotly package
library(plotly)

# ------------ Current version parameters (populational) ------------ #

# Mean
mu_c <- 50
# Standard deviation
sigma_c <- sqrt(100)
# Variance
sigma2_c <- sigma_c^2

# ------------ For the test on the mean cost ------------ #

# Desired significance level (Type I error probability)
alpha <- 0.01
# Confidence level
conf_level <- 1 - alpha
# Minimally relevant effect size
delta_star <- 4
# Desired power (statistical power)
pi <- 0.8
# Type II error probability
beta <- 1 - pi

# Assuming equivalence of variances (initial estimate of the variance)
sigma_n <- sigma_c

# Define the sample size to be used in this experiment
(params <- power.t.test(delta = delta_star,
             sd = sigma_n,
             sig.level = alpha, 
             power = pi,
             type = "one.sample",
             alternative = "one.sided"))

# Number of observations
n <- ceiling(params$n)

# Random seed
set.seed(1007)

# Collect the sample with n observations
data <- data_generation(n = n)

# Saves data to the csv file
write.table(data, file = 'data.csv', row.names = FALSE, col.names = FALSE)

# ------------ New version parameters (sample) ------------ #

# Mean
mu_n <- mean(data)
# Standard deviation
sigma_n <- sd(data)
# Variance
sigma2_n <- var(data)

# ------------ Exploratory Analysis ------------ #

# Histogram
histogram <- ggplot(data = as.data.frame(data), mapping = aes(x = data))
histogram + geom_histogram(lwd = 0.3, bins = 20, color = 'black', fill = 'gray') + scale_x_continuous(name = 'Amostra') + scale_y_continuous(name = 'Frequência')

# Boxplot
boxplot <- ggplot(data = as.data.frame(data), mapping = aes(y = data))
boxplot + geom_boxplot(lwd = 0.3) + scale_x_continuous(name = 'Amostra') + scale_y_continuous(name = '') + theme(axis.text.x = element_blank())

# QQ-Plot
qqplot <- ggplot(data = as.data.frame(data), mapping = aes(sample = data))
qqplot + geom_qq_line() + geom_qq() + scale_y_continuous(name = 'Amostra') + scale_x_continuous(name = 'Quantil')

# ------------ Hypothesis Testing ------------ #

(t_test <- t.test(x = data, 
                mu = mu_c, 
                alternative = "less", 
                conf.level = conf_level))

# Confidence Interval
CI <- t_test$conf.int[1:2]

# ------------ Checking Assumptions ------------ #

# Wilcoxon Signed-Ranks Test, because it does not assume normality
(wilcoxon_test <- wilcox.test(x = data,
                  alternative = "less",
                  mu = mu_c, 
                  paired = FALSE, 
                  exact = NULL, 
                  correct = TRUE,
                  conf.int = FALSE, 
                  conf.level = conf_level))

# ------------ For the test on the variance of the cost ------------ #

# Desired significance level (Type I error probability)
alpha <- 0.05