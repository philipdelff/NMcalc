##' Convert a Variance of a normal distribution to a CV of a
##' log-normal distribution
##'
##' @param omega A variance of a normal distribution
##' @export

CVlnorm <- function(omega){
    sqrt(exp(omega)-1)
}
