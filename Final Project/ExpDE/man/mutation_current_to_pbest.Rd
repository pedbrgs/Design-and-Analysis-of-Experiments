% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutation_current_to_pbest.R
\name{mutation_current_to_pbest}
\alias{mutation_current_to_pbest}
\title{/current-to-pbest mutation for DE}
\usage{
mutation_current_to_pbest(X, mutpars)
}
\arguments{
\item{X}{population matrix}

\item{mutpars}{mutation parameters (see \code{Mutation parameters} for details)}
}
\value{
Matrix \code{M} containing the mutated population
}
\description{
Implements the "/current-to-pbest" mutation for the ExpDE framework
}
\details{
This routine also implements one special case: 
\itemize{
 \item current-to-best mutation (\code{current_to_best}), by setting 
   \code{mutpars$p = 1}); 
 \item Flat recombination (\code{flat}), by setting 
   \code{recpars$alpha = recpars$beta = 0})
}
}
\section{Mutation Parameters}{

The \code{mutpars} parameter contains all parameters required to define the 
mutation. \code{mutation_current_to_pbest()} understands the following fields in 
\code{mutpars}:
\itemize{
   \item \code{f} : scaling factor for difference vector(s).\cr
   Accepts numeric vectors of size 1 or \code{nvecs}.
   \item \code{p} : either the number of "best" vectors to use (if given as a 
   positive integer) or the proportion of the population to use as "best"
   vectors (if 0 < p < 1).
}
}

\section{Warning}{

This routine will search for the performance vector 
of population \code{X} (\code{J}) in the parent environment (using 
\code{parent.frame()}. This variable must be defined for 
\code{mutation_current_to_pbest()} to work.
}

\section{References}{

J. Zhang, A.C. Sanderson, 
"JADE: Adaptive differential evolution with optional external archive". 
IEEE Transactions on Evolutionary Computation 13:945-958, 2009
}

\author{
Felipe Campelo (\email{fcampelo@ufmg.br})
}
