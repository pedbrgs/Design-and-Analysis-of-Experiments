# Design and Analysis of Experiments (EEE933)
# Pedro Vinicius A. B. de Venâncio

# Clear workspace
rm(list=ls())
# Clear all plots
graphics.off()
# Clear console
cat("\014")

# Import libraries
library(ExpDE)
library(smoof)

fn <- function(X){
  # <- if a single vector is passed as X
  if(!is.matrix(X)) X <- matrix(X, nrow = 1)
  Y <- apply(X, MARGIN = 1,
             FUN = smoof::makeRosenbrockFunction(dimensions = dim))
  return(Y)
}

# Minimum number of dimensions
Dmin <- 2
# Maximum number of dimensions
Dmax <- 150

# Dimensions
dimensions <- c(Dmin:Dmax, Dmin:Dmax)
# Number of runs
n_runs <- 33

# Fitness (33x149)
fs <- matrix(nrow = n_runs, ncol = length(dimensions))
# Times (33x149)
ts <- matrix(nrow = n_runs, ncol = length(dimensions))

# For each problem dimension
for (i in 1:length(dimensions)) {
  
    # Current dimension
    dim <- dimensions[i]
    cat('Dimension:', dim, '\n')
    
    if(i <= length(dimensions)/2){
        ## Config 1 
        cat('Algorithm 1\n')
        recpars <- list(name = "recombination_blxAlphaBeta", alpha = 0.4, beta = 0.4)
        mutpars <- list(name = "mutation_rand", f = 4)
    } else{
        ## Config 2
        cat('Algorithm 2\n')
        recpars <- list(name = "recombination_eigen", othername = "recombination_bin", cr = 0.9)
        mutpars <- list(name = "mutation_best", f = 2.8)
    }
    
    #  n_runs times
    for (n in 1:n_runs){
      
        cat('Run:', n, '\n')
          
        # Start time
        start_time <- Sys.time()
        
        selpars <- list(name = "selection_standard")
        stopcrit <- list(names = "stop_maxeval", maxevals = 5000 * dim, maxiter = 100 * dim)
        probpars <- list(name = "fn", xmin = rep(-5, dim), xmax = rep(10, dim))
        popsize = 5 * dim
        
        # Run algorithm on Rosenbrock function
        out <- ExpDE(mutpars = mutpars,
                     recpars = recpars,
                     popsize = popsize,
                     selpars = selpars,
                     stopcrit = stopcrit,
                     probpars = probpars,
                     showpars = list(show.iters = "none", showevery = 20))
        
        # Extract observation
        fs[n,i] <- out$Fbest
        
        # End time
        end_time <- Sys.time()
        
        # Running time
        ts[n,i] <- end_time - start_time
        
    }

}

# Saving data
data <- cbind(fs, ts)
data <- data.frame(data)
filename <- paste(getwd(),'/CS4.csv', sep = "")
write.csv(data, file = filename)