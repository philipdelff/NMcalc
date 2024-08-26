##' CV of log-normal dist baed on omega parameters
##' CV based variance like provided in Nonmem's \code{OMEGA} metrix.
##' @param omega A variance as provided in diagonal om the Nonmem OMEGA matrix
##' @return CV of the distribution (numeric)
##' @details This is a very simple function. All it does is
##' \code{sqrt(exp(omega)-1)}.
##' @export
##' 
CVlnorm <- function(omega){
    sqrt(exp(omega)-1)
}
