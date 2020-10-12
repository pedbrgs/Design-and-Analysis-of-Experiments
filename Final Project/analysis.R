# Design and Analysis of Experiments (EEE933)
# Team D: Pedro Vinícius, Samara Silva and Savio Vieira

# Clear workspace
rm(list=ls())
# Clear all plots
graphics.off()
# Clear console
cat("\014")

# CAISEr package
library(CAISEr)
# Statistical package 
library(stats)
# Ggplot2 package
library(ggplot2)
# Reshape2 package
library(reshape2)

# Number of runs
nruns <- 51
# Number of functions
nfunc <- 28
# Number of dimensions
ndims <- 2
# Number of algorithms
nalgs <- 5
# Number of variables (fitness and runtime)
nvar <- 2

# Significance level
alpha <- 0.05
# Minimally interesting effect
d <- 0.5
# Number of algorithms/configurations
K <- 5
# Number of comparisons (K*(K-1))/2
nc <- (K*(K-1))/2

# Calculates power
power <- calc_instances(ncomparisons = nc, 
                      ninstances = (nfunc*ndims),
                      d = d, 
                      sig.level = alpha, 
                      alternative.side = "one.sided", 
                      power.target = "mean")$power

# Number of columns
ncols <- nfunc*ndims*nalgs*nvar + 1

# Load data
data <- read.csv(file = 'data/FProj51.csv', sep = ',', head = FALSE, skip = 1)[,2:ncols]

# Splitting data into two dataframes (fitness and runtime)
dfs <- tapply(as.list(data), gl(nvar, ncol(data)/nvar), as.data.frame)
fs <- dfs$`1`
ts <- dfs$`2`

# Splitting fitness data by algorithm
fs <- tapply(as.list(fs), gl(ncol(fs)/(nfunc*ndims), (nfunc*ndims)), as.data.frame)
# Splitting runtime data by algorithm
ts <- tapply(as.list(ts), gl(ncol(ts)/(nfunc*ndims), (nfunc*ndims)), as.data.frame)

# Renaming column names
# colnames <- c(paste(1:28, "10", sep = "_"), paste(1:28, "50", sep = "_"))
colnames <- c(1:(nfunc*ndims))
for (i in 1:length(fs)){
  colnames(fs[[i]]) <- colnames
  colnames(ts[[i]]) <- colnames
}

# Removing instances that the algorithm did not converge
diverged <- c(2,3,30,31)
cleaned_fs <- lapply(fs, function(fs) fs[,-diverged])

# Runtime average
tmean <- sapply(ts, colMeans)
# Fitness average
fmean <- sapply(cleaned_fs, colMeans)

# Preprocessing data to plot lineplot
fmean_ <- melt(fmean)

# Lineplot
p <- ggplot(fmean_, aes(x = as.factor(Var1), y = value, group = Var2, color = Var2)) + geom_line()
p + labs(x = "Instância", y = "Fitness médio") + geom_point(size = 2) + 
  guides(color=guide_legend(title="Algoritmo"))

# Peak
peak <- c("4", "15", "23", "32", "43", "51", "56")
merged <- lapply(cleaned_fs, function(fs) fs[,peak])
# Preprocessing data to plot boxplot
merged <- lapply(merged, melt, id.var = NULL)
merged <- do.call("rbind", merged)
label <- c(sapply(1:nalgs, rep, times = (nruns*length(peak))))
merged <- cbind(merged, label)

# Boxplot
p <- ggplot(data = merged, aes(x = as.factor(variable), y=value, fill = as.factor(label)))
p + geom_boxplot() + labs(fill = "Algoritmo") + 
    labs(x = "Instância", y = "Fitness")
