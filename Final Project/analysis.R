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
# Agricolae package
library(agricolae)

## ------------- Design of Experiments ------------- ##

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

# Calculates power (all instances)
all <- calc_instances(ncomparisons = nc, 
                      ninstances = (nfunc*ndims),
                      d = d, 
                      sig.level = alpha, 
                      alternative.side = "one.sided", 
                      power.target = "mean")$power

# Number of columns
ncols <- nfunc*ndims*nalgs*nvar + 1

## ------------- Data preprocessing ------------- ##

# Load data
data <- read.csv(file = 'FProj.csv', sep = ',', head = FALSE, skip = 1)[,2:ncols]

# Splitting data into two dataframes
dfs <- tapply(as.list(data), gl(nvar, ncol(data)/nvar), as.data.frame)
# Fitness data
fs <- dfs$`1`
# Runtime data
ts <- dfs$`2`

# Splitting fitness data by algorithm
fs <- tapply(as.list(fs), gl(ncol(fs)/(nfunc*ndims), (nfunc*ndims)), as.data.frame)
# Splitting runtime data by algorithm
ts <- tapply(as.list(ts), gl(ncol(ts)/(nfunc*ndims), (nfunc*ndims)), as.data.frame)

# Renaming column names
colnames <- c(1:(nfunc*ndims))
for (i in 1:length(fs)){
  colnames(fs[[i]]) <- colnames
  colnames(ts[[i]]) <- colnames
}

# Global optimal solutions
gopt <- c(seq(-1400, -100, 100), seq(100, 1400, 100))
# Normalization of fitness
normalized_fs <- lapply(fs, function(x) t(apply(x, 1, "/", gopt)))
# Normalization of runtime
normalized_ts <- lapply(ts, function(x) apply(x, 2, function(x) (x - min(x))/(max(x)-min(x))))

# Removing instances that the algorithm did not converge
diverged <- c(2,3,30,31,32,43,51)
cl_fs <- lapply(fs, function(fs) fs[,-diverged])

# Runtime average
tmean <- sapply(ts, colMeans)
# Fitness average
fmean <- sapply(fs, colMeans)
# Cleaned fitness average
cl_fmean <- sapply(cl_fs, colMeans)

## ------------- Exploratory Data Analysis ------------- ##

# To plot fitness
ref <- c(gopt,gopt)[-diverged]
fplot <- cbind(ref, cl_fmean)
fplot <- melt(fplot)
colnames(fplot) <- c("Instancia", "Algoritmo", "f")

# Fitness lineplot
# pdf(file = "fmean.pdf", width = 10, height = 4)
p <- ggplot(fplot, aes(x = as.factor(Instancia), y = f, group = Algoritmo, color = Algoritmo)) + geom_line()
p + labs(x = "Instância", y = "Fitness médio") + geom_point(size = 2) + 
    guides(color=guide_legend(title="Algoritmo"))
# dev.off()

# To plot runtime
tmean <- melt(tmean)
colnames(tmean) <- c("Instancia", "Algoritmo", "t")

# Runtime plot
# pdf(file = "tmean.pdf", width = 10, height = 4)
p <- ggplot(tmean, aes(x = as.factor(Instancia), y = t, group = Algoritmo, color = Algoritmo)) + geom_line()
p + labs(x = "Instância", y = "Tempo decorrido médio (s)") + geom_point(size = 2) + 
  guides(color=guide_legend(title="Algoritmo"))
# dev.off()

# Peak
peak <- c("15", "23", "42", "50")
merged <- lapply(cl_fs, function(fs) fs[,peak])
# Preprocessing data to plot boxplot
merged <- lapply(merged, melt, id.var = NULL)
merged <- do.call("rbind", merged)
label <- c(sapply(1:nalgs, rep, times = (nruns*length(peak))))
merged <- cbind(merged, label)

# Boxplot
# pdf(file = "finstances.pdf", width = 11, height = 3)
p <- ggplot(data = merged, aes(x = as.factor(variable), y=value, fill = as.factor(label)))
p + geom_boxplot(alpha = 0.4) + labs(fill = "Algoritmo") + 
    scale_fill_manual(values = c("#B79F00", "#00BA38", "#00BFC4", "#619CFF", "#F564E3")) +
    labs(x = "Instância", y = "Fitness")
# dev.off()

## ------------- Statistical Analysis ------------- ##

# Calculates power (all instances that the algorithms converged)
some <- calc_instances(ncomparisons = nc, 
                       ninstances = (nfunc*ndims-length(diverged)),
                       d = d, 
                       sig.level = alpha, 
                       alternative.side = "one.sided", 
                       power.target = "mean")$power

ncomp <- rep(1:nc, 2)
power <- data.frame(cbind(all, some))
colnames(power) <- c("56", "49")
power <- melt(power, id.vars = NULL)
power <- cbind(power, ncomp)

# Relationship between the number of comparisons and the power of the test
# pdf(file = "power.pdf", width = 6, height = 4)
p <- ggplot(data = power, aes(x = ncomp, y = value, color = variable))
p + xlab('Número de comparações') + 
  ylab('Potência do teste') + 
  geom_line(linetype = 'dashed') +
  geom_point(size = 2) + scale_x_continuous( breaks=ncomp) +
  guides(color=guide_legend(title="# Instâncias"))
# dev.off()

# Preprocessing data
fmean <- melt(fmean)
colnames(fmean) <- c('Instancia', 'Algoritmo', 'f')
for (i in 1:2) fmean[, i] <- as.factor(fmean[, i])
# Fitting an Analysis Of Variance (AOV) model
model <- aov(formula = f~Algoritmo+Instancia, data = fmean)
# Summarizing model
summary(model)

#Coeficiente de determina��o -r2
cat('Coeficiente de determina��o:', summary.lm(model)$r.squared)

# Relative blocking efficiency (E)
df <- as.data.frame(summary(model)[[1]])
MSblocks <- df["Instancia","Mean Sq"]
MSe <- df["Residuals","Mean Sq"]
a <- length(unique(fmean$Algoritmo))
b <- length(unique(fmean$Instancia))
E <- ((b - 1) * MSblocks + b * (a - 1) * MSe) / ((a * b - 1) * MSe)
cat('Relative blocking efficiency (E):', E)


# Shapiro-Wilk test for normality
shapiro.test(model$residuals)

# Adjusting grid for plots
par(mar = c(5, 5, 3, 1), mgp = c(3, .35, 0),
    cex.axis = .9, bg = "white", fg = "black",
    col.axis = "black", col.lab = "black",
    mfrow = c(1, 2))
# QQ-Plot
# pdf(file = "qqplot.pdf", width = 5, height = 5)
plot(model, which = 2, panel.first=grid(lty = "solid"))
# dev.off()
# Constant Leverage plot: Residual vs Factor Levels
# pdf(file = "leverage.pdf", width = 5, height = 5)
plot(model, which = 5, panel.first=grid(lty = "solid"))
# dev.off()

