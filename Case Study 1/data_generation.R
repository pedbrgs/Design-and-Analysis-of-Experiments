# Design and Analysis of Experiments (EEE933)
# Team D: Pedro Vinicius, Samara Silva and Savio Vieira

# Install required package and set up simulation
# install.packages("ExpDE")

# Set up the data-generating procedure
library(ExpDE)
## Warning: package 'ExpDE' was built under R version 3.6.1

# n: Number of observations

data_generation <- function(n){
  
  mre <- list(name = "recombination_bin", cr = 0.9)
  mmu <- list(name = "mutation_rand", f = 2)
  mpo <- 100
  mse <- list(name = "selection_standard")
  mst <- list(names = "stop_maxeval", maxevals = 10000)
  mpr <- list(name = "sphere", xmin = -seq(1, 20), xmax = 20 + 5 * seq(5, 24))
  
  data <- c()
  # Generate n observations
  for (i in 1:n){
    sample <- ExpDE(mpo, mmu, mre, mse, mst, mpr, showpars = list(show.iters = "none"))$Fbest
    data <- c(data, sample)
  }
  
  return(data)
  
}