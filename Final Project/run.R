  # Design and Analysis of Experiments (EEE933)
# Pedro Vinicius A. B. de Venâncio

# Clear workspace
rm(list=ls())
# Clear all plots
graphics.off()
# Clear console
cat("\014")

# Load algorithm library
files = list.files('./ExpDE/R/')
for (file in files) {
  file <- paste('/ExpDE/R/', file, sep = '')
  dir <- paste(getwd(), file, sep = '')
  source(dir)
}

# Problem Definitions CEC2013 Benchmark (https://bit.ly/3httVfE)

# 28 functions (1 to 28)
allfuns <- 1:28
# Test dimensions (10, 50)
testdims <- c(10, 50)
# Number of runs
n_runs = 33

# Search range
search_range <- c(-100, 100)

# Population size
popsize <- 200
# Selection parameters
selpars <- list(name = "selection_standard")
# Convergence parameters
stopcrit <- list(names = "stop_maxeval", maxevals = 100000)
# Echoing
showpars <- list(show.iters = "none", showevery = 10)

# Loading data from the irace package
load('./irace.Rdata')

## Three best ExpDE versions obtained by the hyperparameter optimization using the irace package

# Mutation parameters
mutpars <- list(list(name = iraceResults$state$eliteConfigurations[1,]$mutpars.name, f = iraceResults$state$eliteConfigurations[1,]$mutpars.f, nvecs = 1),
                list(name = iraceResults$state$eliteConfigurations[2,]$mutpars.name, f = iraceResults$state$eliteConfigurations[2,]$mutpars.f, nvecs = 1),
                list(name = iraceResults$state$eliteConfigurations[3,]$mutpars.name, f = iraceResults$state$eliteConfigurations[3,]$mutpars.f, nvecs = 1))
# Recombination parameters
recpars <- list(list(name = iraceResults$state$eliteConfigurations[1,]$recpars.name, cr = iraceResults$state$eliteConfigurations[1,]$recpars.cr),
                list(name = iraceResults$state$eliteConfigurations[2,]$recpars.name, cr = iraceResults$state$eliteConfigurations[2,]$recpars.cr),
                list(name = iraceResults$state$eliteConfigurations[3,]$recpars.name, cr = iraceResults$state$eliteConfigurations[3,]$recpars.cr))

# Number of ExpDE versions
algos <- length(recpars)

# Fitness (33x(3*28*2)) -> (33x168)
fs <- matrix(nrow = n_runs, ncol = length(allfuns)*length(testdims)*algos)
# Times (33x(3*28*2)) -> (33x168)
ts <- matrix(nrow = n_runs, ncol = length(allfuns)*length(testdims)*algos)

# Initializing iterator
i <- 0

# Iterating over the ExpDE versions
for (alg in 1:algos) {
  
  cat('#-------------------------------------#\n')
  cat('Algorithm:', alg, '\n')
  
  # Iterating over the dimensions of the problem
  for (dim in testdims) {
    
    cat('Dimension:', dim, '\n')
    
    # Iterating over the test functions
    for (fun in allfuns) {
      
      cat('Function:', fun, '\n') 
      
      # Current column
      i <- i + 1
      
      # Running n_runs times
      for (n in 1:n_runs) {
        
        cat('Run:', n, '\n')
        
        # Start time
        start_time <- Sys.time()
        
        # Problem parameters
        probpars <- list(name  = "test_functions", i = fun, 
                         xmin = rep(search_range[1], dim), 
                         xmax = rep(search_range[2], dim))
        # Fixed seed
        seed <- 1234
        # Run ExpDE
        out <- ExpDE(popsize, mutpars[[alg]], recpars[[alg]], selpars, 
                     stopcrit, probpars, seed, showpars)
        
        # Extract observation
        fs[n,i] <- out$Fbest
        
        # End time
        end_time <- Sys.time()
        
        # Running time
        ts[n,i] <- end_time - start_time
        
      }
    }
  }
}

# Saving data
data <- cbind(fs, ts)
data <- data.frame(data)
filename <- paste(getwd(),'/FProj.csv', sep = "")
write.csv(data, file = filename)