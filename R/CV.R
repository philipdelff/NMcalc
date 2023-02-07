##' Calculate coefficient of variation of data
##'
##' @param x The data
##' @param log If TRUE, the geometric coefficient of variation is
##'     calculated. This is sqrt(exp(var(log(x))-1).
##' @details This function is intended to be used on data. For a
##'     log-normal THETA1*EXP(ETA(1)) 'Nonmem' parameter, do
##'     CV=sqrt(exp(OMEGA\[1,1\])-1).
##' @return A numeric
##' @importFrom stats var sd
##' @examples
##' set.seed(139)
##' x1 <- rnorm(1000,mean=5)
##' CV(x1)
##' CV(x1,log=TRUE)
##' x2 <- exp(x1)
##' CV(x2)
##' CV(x2,log=TRUE)
##' @export


CV <- function(x,log=FALSE) {
    if(log){
        cv <- sqrt(exp(var(log(x)))-1)
    } else {
        cv <- sd(x)/mean(x)
    }
    cv
}
