##' Logit function
##' @param x a number to transform
##' @seealso invlogit
##' @export

logit <- function(x) log(x/(1-x))

##' Inverse logit function
##' @param x a number to transform
##' @seealso logit
##' @export
invlogit <- function(x) 1/(1+exp(-x))
