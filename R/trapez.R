##' trapezoidal AUC on linear scale
##' @description This is a numerical integration of y with respect to
##'     x by the trapezoidal method on linear scale.
##' @param x The vector to integrate y with respect to (typically TIME
##'     to get AUC).
##' @param y The variable to integrate.
##' @param cum Return the cumulative trapezoidal AUC? If false
##'     (default) a single number is returned. If true, a vector is
##'     returned. Notice, the vector is one element shorter than x and
##'     y. 
##' @param na.rm Remove indexes in x and y wherever x or y are NA.
##' @family Calc
##' @export
trapez <- function(x, y, cum=FALSE, na.rm = FALSE){

    if((any(is.na(y)) || any(is.na(x))) && !na.rm) 
    {
        ## warning("y or x contains NA with na.rm=F - returning NA\n")
        return(NA_real_)	
    }


    ## Remove any missing values and issue warning if removing values
    if(na.rm) {
        ## Locate missing values in both vectors
        miss <- is.na(x) | is.na(y)
        if(any(miss)) {
            x <- x[!miss]
            y <- y[!miss]
            if(length(x) < 2) {
                warning("No valid observations remaining after NA removal.")
                return(NA)
            }
        }
    }
    
    
    dx <- diff(x)
    yfirst <- y[-length(y)]
    ylast <- y[-1]

    if(cum) {
        res <- c(cumsum(dx*(yfirst+ylast)) / 2)
    } else {
        res <- sum(dx*(yfirst+ylast)) / 2
    }
    res
}
