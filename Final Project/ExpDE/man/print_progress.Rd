% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_progress.R
\name{print_progress}
\alias{print_progress}
\title{Print progress of DE}
\usage{
print_progress()
}
\description{
Echoes the progress of DE to the terminal
}
\section{Parameters}{

This routine accesses all variables defined in the calling environment using
\code{parent.frame()}, so it does not require any explicit input parameters.
However, the calling environment must contain:
\itemize{
   \item \code{showpars}: list containing parameters that control the printed
                          output of \code{moead()}. Parameter \code{showpars}
                          can have the following fields:
   \itemize{
     \item \code{$show.iters = c("dots", "numbers", "none")}: type of output.
                               Defaults to \code{"numbers"}.
     \item \code{$showevery}: positive integer that determines how frequently
                              the routine echoes something to the terminal.
                              Defaults to \code{1}.
   }
   \item \code{iters()} : counter function that registers the iteration
                          number
}
}

