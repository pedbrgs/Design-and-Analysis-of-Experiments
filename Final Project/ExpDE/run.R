# Evolutionary Computation (EEE882)
# Pedro Vinicius A. B. de Venâncio

# Clear workspace
rm(list=ls())
# Clear all plots
graphics.off()
# Clear console
cat("\014")

# Load algorithm library
files = list.files('R/')
for (file in files) {
  file <- paste('/R/', file, sep = '')
  dir <- paste(getwd(), file, sep = '')
  source(dir)
}

# Problem Definitions (https://bit.ly/3httVfE)
# 1 Sphere Function -1400
# 2 Rotated High Conditioned Elliptic Function -1300
# 3 Rotated Bent Cigar Function -1200
# 4 Rotated Discus Function -1100
# 5 Different Powers Function -1000
# 6 Rotated Rosenbrock’s Function -900
# 7 Rotated Schaffers F7 Function -800
# 8 Rotated Ackley’s Function -700
# 9 Rotated Weierstrass Function -600
# 10 Rotated Griewank’s Function -500
# 11 Rastrigin’s Function -400
# 12 Rotated Rastrigin’s Function -300
# 13 Non-Continuous Rotated Rastrigin’s Function -200
# 14 Schwefel's Function -100
# 15 Rotated Schwefel's Function 100
# 16 Rotated Katsuura Function 200
# 17 Lunacek Bi_Rastrigin Function 300
# 18 Rotated Lunacek Bi_Rastrigin Function 400
# 19 Expanded Griewank’s plus Rosenbrock’s Function 500
# 20 Expanded Scaffer’s F6 Function 600
# 21 Composition Function 1 (n=5,Rotated) 700
# 22 Composition Function 2 (n=3,Unrotated) 800
# 23 Composition Function 3 (n=3,Rotated) 900
# 24 Composition Function 4 (n=3,Rotated) 1000
# 25 Composition Function 5 (n=3,Rotated) 1100
# 26 Composition Function 6 (n=5,Rotated) 1200
# 27 Composition Function 7 (n=5,Rotated) 1300
# 28 Composition Function 8 (n=5,Rotated) 1400
# Search Range: [-100,100]^D

# Problem index
problem <- 1
# Number of dimensions
D <- 10
# Search range
search_range <- c(-100, 100)

# Population size
popsize  <- 200
# Mutation parameters
mutpars  <- list(name = "mutation_best", f = 0.6, nvecs = 1)
# Recombination parameters
recpars  <- list(name = "recombination_sbx", eta = 10)
# Selection parameters
selpars  <- list(name = "selection_standard")
# Convergence parameters
stopcrit <- list(names = "stop_maxeval", maxevals = 100000)
# Problem parameters
probpars <- list(name  = "test_functions", i = problem, 
                 xmin = rep(search_range[1], D), 
                 xmax = rep(search_range[2], D))
seed     <- 1234
showpars <- list(show.iters = "numbers", showevery = 10)
# Run ExpDE
out      <- ExpDE(popsize, mutpars, recpars, selpars, 
                  stopcrit, probpars, seed, showpars)
