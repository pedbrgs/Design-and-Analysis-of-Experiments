# Design and Analysis of Experiments (EEE933)
# Pedro Vinicius A. B. de Venâncio

# Clear workspace
rm(list=ls())
# Clear all plots
graphics.off()
# Clear console
cat("\014")

# Irace package
library('irace')

# Load algorithm library
files = list.files('./ExpDE/R/')
for (file in files) {
  file <- paste('/ExpDE/R/', file, sep = '')
  dir <- paste(getwd(), file, sep = '')
  source(dir)
}

scenario <- defaultScenario()
# Seed for the experiment
scenario$seed <- 123456
# Runner function (def. below)
scenario$targetRunner <- "target.runner"
# Forbidden configs
scenario$forbiddenFile <- "forbidden.txt"
scenario$debugLevel <- 1
# Tuning budget
scenario$maxExperiments <- 50000
# Test all final elite configurations
scenario$testNbElites <- 7

# Number of cores to be used by irace (set with caution!)
# nc <- parallel::detectCores() - 1
# scenario$parallel <- nc

# Read tunable parameter list from file
parameters <- readParameters("./parameters.txt")

### Build test instances

# Training dimensions (2, 30, 100)
dims <- c(2, 30, 100)

# 28 functions (1 to 28)
instances <- 1:28
allfuns <- expand.grid(instances, dims, stringsAsFactors = FALSE)
scenario$instances <- paste0(allfuns[,1], "_", allfuns[,2])

target.runner <- function(experiment, scenario){
  
  force(experiment)
  conf <- experiment$configuration
  inst <- experiment$instance
  
  # Problem
  fdef <- unlist(strsplit(inst, split = "_"))
  
  # Search range
  search_range <- c(-100, 100)
  
  # Population size
  popsize  <- 200
  
  # Mutation parameters
  if(conf$mutpars.name == "mutation_current_to_pbest"){
    mutpars  <- list(name = conf$mutpars.name, f = conf$mutpars.f, p = conf$mutpars.p, nvecs = 1)  
  }
  else{
    mutpars  <- list(name = conf$mutpars.name, f = conf$mutpars.f, nvecs = 1)
  }
  
  # Recombination parameters
  if (conf$recpars.name == "recombination_arith") {
    recpars  <- list(name = conf$recpars.name)
  }
  if(conf$recpars.name == "recombination_bin"){
    recpars  <- list(name = conf$recpars.name, cr = conf$recpars.cr)
  }
  if(conf$recpars.name == "recombination_blxAlphaBeta"){
    recpars  <- list(name = conf$recpars.name, alpha = conf$recpars.alpha, beta = conf$recpars.beta)
  }
  if(conf$recpars.name == "recombination_mmax"){
    recpars  <- list(name = conf$recpars.name, lambda = conf$recpars.lambda)
  }
  if(conf$recpars.name == "recombination_sbx"){
    recpars  <- list(name = conf$recpars.name, eta = conf$recpars.eta)
  }
  
  # Selection parameters
  selpars  <- list(name = "selection_standard")
  
  # Convergence parameters
  stopcrit <- list(names = "stop_maxeval", maxevals = 100000)
  
  # Problem parameters
  probpars <- list(name  = "test_functions", i = as.numeric(fdef[1]), 
                   xmin = rep(search_range[1], as.numeric(fdef[2])), 
                   xmax = rep(search_range[2], as.numeric(fdef[2])))
  
  # Echoing
  showpars <- list(show.iters = "none", showevery = 10)
  
  # Print current experiment settings on the terminal
  cat('Recombination:', conf$recpars.name, 'Mutation:', conf$mutpars.name, 'Function:', fdef[1], 'Dimension:', fdef[2], '\n')
  
  # Fixed seed
  seed <- conf$seed
  
  # Run ExpDE
  out <- ExpDE(popsize, mutpars, recpars, selpars, stopcrit, probpars, seed, showpars)
  
  return(list(cost = out$Fbest))
  
}

## Running the experiment
irace.output <- irace::irace(scenario, parameters)