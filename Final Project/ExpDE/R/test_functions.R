# Design and Analysis of Experiments (EEE933)
# Pedro Vinicius A. B. de Venâncio

# Import library (https://bit.ly/3httVfE)
library(cec2013)

test_functions <- function(i, x){
    f <- cec2013(i = i, x = x)
    return(f);
}