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

####### dropped this one because it is more complicated and it is off when x==0
## signif2 <- function(x,digits=1,add=T,debug=FALSE) {
##     if(debug) browser()

## ### check arguments
##     ## check that x is numeric 
##     stopifnot(is.numeric(x))
##     if(!(is.numeric(digits)&&length(digits)==1&&as.integer(digits)==digits&&digits>0)){
##         stop("digits must be an integer>0")
##     }
##     if(!add){
##         warning("add=FALSE. This is experimental. Please check output carefully.")
##     }
## ### check arguments done

##     rounded1 <- signif(x,digits)
##     rounded2 <- sub("\\.0*$","",as.character(rounded1))

##     rounded3 <- rounded2

    
##     if(add){
## ### pad with zeros where digits>nchar(x[i]) 
##         padfun <- function(y){
##             digits1 <- nchar(sub("\\.", "", y))
##             ## digits1 <- nchar(sub("^0*\\(.+\\)","\\1",sub("\\.","",y)))
##             ## digits1 <- nchar(sub("^0*","",sub("\\.","",y)))
##             if(digits1>=digits) return(y)
##             ## browser()

##             if(!grepl("\\.",y)&&digits1<digits) y <- paste0(y,".")
##             ## nchar(sub("^0*(.*)","\\1",sub("\\.","",y)))
##             y <- paste0(y,
##                         paste(rep(0,max(0,digits-digits1)),collapse="")
##                         )
##             y
##         }
##         rounded3 <- unlist(lapply(rounded3,padfun))
##     }
    
##     rounded3
## }

