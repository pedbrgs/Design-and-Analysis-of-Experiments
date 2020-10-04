# Design and Analysis of Experiments (EEE933)
# Team D: Pedro Vinícius, Samara Silva and Savio Vieira

# Clear workspace
rm(list=ls())
# Clear all plots
graphics.off()
# Clear console
cat("\014")

# Install required packages if needed
packages_needed <- c("CAISEr","ggplot2", "multcomp")
for (package_name in packages_needed) {      
  if (!(package_name %in% rownames(installed.packages()))){
    install.packages(package_name)
  }
}

library(CAISEr)

# Significance level
alpha <- 0.05
# Desired power
pi <- 0.80
#  Minimally interesting effect
d <- 0.5
# Number of algorithms/configurations
K <- 10
# Number of comparisons (K*(K-1))/2
nc <- (K*(K-1))/2

# Calculates number of instances for the comparison of multiple algorithms
ninstances <- calc_instances(ncomparisons = nc, 
                      d = d, 
                      power = pi, 
                      sig.level = alpha, 
                      alternative.side = "one.sided", 
                      power.target = "mean")$ninstances

cat('Número de instâncias necessárias:', ninstances)
