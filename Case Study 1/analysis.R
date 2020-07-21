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

# ------------ Populational parameters ------------ #

# Mean
mu <- 50
# Standard deviation
sigma <- sqrt(100)
# Variance
sigma2 <- sigma^2

# ------------ For the test on the mean cost ------------ #

# Desired significance level (Type I error probability)
alpha <- 0.01
# Minimally relevant effect size
delta_star <- 4
# Desired power (statistical power)
pi <- 0.8
# Type II error probability
beta <- 1 - pi

# Define the sample size to be used in this experiment
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/power.t.test
power.t.test(power = pi, delta = delta_star, sig.level = alpha, sd = sigma)
n <- power.t.test(power = pi, delta = delta_star, sig.level = alpha, sd = sigma)$n

# Collect the sample with n observations
data <- data_generation(n = ceiling(n))

# ------------ For the test on the variance of the cost ------------ #

# Desired significance level (Type I error probability)
alpha <- 0.05