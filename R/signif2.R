##' round to fixed number of significant digits
##'
##' @param x a numeric vector.
##' @param digits number of significant digits to round to. Must be an
##'     integer larger than 0.
##' @param add pad with zeros where digits>nchar(x\[i\]). Currently not used.
##' @param ... additional arguments passed to formatC.
##' @family DataWrangling
##' @examples
##' x <- c(1.24e-4,1.1334e6,1.1,22.00000,10.00,1)
##' signif(x,3)
##' as.character(signif(x,3))
##' signif2(x,3)
##' signif2(x,3)
##' signif2(c(.2,11.84),2)
##' ## digits has no effect when x==0
##' signif2(0,1)
##' signif2(0,3)
##' \dontrun{
##' signif2(3205,-1)
##' }
##' @export

signif2 <- function(x,digits=1,add,...){

    stopifnot(is.numeric(x))
    if(!(is.numeric(digits)&&length(digits)==1&&as.integer(digits)==digits&&digits>0)){
        stop("digits must be an integer>0")
    }

    res <- formatC(signif(x,digits=digits), digits=digits,format="fg", flag="#",...)
    sub("\\.$","",res)
}

