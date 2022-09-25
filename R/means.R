##' calculate arithmetic or geometric mean and confidence intervals
##'
##' @param x vector to calculate the geometric mean of
##' @param type type of mean or median. Default is arithmetic,
##'     geometric and median are available as well. Only first letters
##'     needed, so say "geo" or even "g" is enough.
##' @param na.rm Remove NA's before doing calculations?
##' @param z.rm removes zeros before calculation? Default is
##'     FALSE. Can only be TRUE if type="geometric".
##' @param ci if TRUE, a data.frame including point estimate and
##'     confidence interval returned. If FALSE, a numeric representing
##'     the mean value returned.
##' @param dist.ci The distribution to use for the confidence
##'     interval. Default and only supported is "t". If
##'     type=geometric, this is applied after transformation to
##'     gaussian.
##' @param p.ci probability covered by confidence interval. Default is
##'     0.95
##' @param colnames If ci, this defines the column names of the
##'     resulting data frame. Default is c("est","ll","ul").
##' @param format The format of the result. Possible values are df and
##'     num.
##' @import stats
##' @return If ci=FALSE, a numeric. If ci=TRUE, a data.frame.
##' @export
##' @examples
##' x <- 1:100
##' means(x, type="arithmetic", ci=TRUE)
##' means(x, type="geometric", ci=TRUE)
##' means(x, type="median", ci=TRUE)


means <- function(x,type="arithmetic",na.rm=FALSE,z.rm=FALSE,ci=FALSE,
                  dist.ci="t",p.ci=.95,colnames=c("est","ll","ul"),
                  format = "df") {

    
    type <- gsub("(^ +| +$)","",type)
    type <- tolower(type)

    if(type == substr("arithmetic",1,nchar(type))){
        type <- "arithmetic"
    } else if(type == substr("geometric",1,nchar(type))){
        type <- "geometric"
    } else if(type == substr("median",1,nchar(type))){
        type <- "median"
    } else {
        stop("type has to be the first letters of either arithmetic or geometric.")
    }

    stopifnot(format%in%c("df","num"))

    if(na.rm){
        x <- x[!is.na(x)]
    }
    
    if( type!="geometric" ) {
        if(z.rm) stop("z.rm can only be TRUE when type==geometric")
    }

    est <- switch(type,
                  geometric = {
                      if(z.rm) x <- x[x!=0]
                      exp(mean(log(x)))
                  },
                  arithmetic = {
                      mean(x)
                  },
                  median = {
                      median(x)
                  }
                  )

    
    if(!ci){
        return(est)
    }
    
    if(!dist.ci=="t") stop("Only t-dist supported.")

    nobs <- length(x)
    if(nobs<2) {
        type = "tooFewObs"
    }

    out <- switch(type,
                  tooFewObs = {
                      warning("less than two observations. Skipping CI.")
                      out <- c(est,NA,NA)
                      out
                  },
                  geometric = {
                      w.ci <- qt(p=1-(1-p.ci)/2,df=nobs-1)*sd(log(x))/sqrt(nobs)
                      out <- c(est,exp(log(est)-w.ci),exp(log(est)+w.ci))
                      out
                  },
                  arithmetic = {
                      w.ci <- qt(p=1-(1-p.ci)/2,df=nobs-1)*sd(x)/sqrt(nobs)
                      out <- c(est,est-w.ci,est+w.ci)
                      out
                  },
                  median = {
                      q <- 0.5
                      x <- sort(x)
                      w.ci <- qt(p=1-(1-p.ci)/2,df=nobs-1)*sqrt(nobs*q*(1-q))
                      j <- nobs*q - w.ci
                      k <- nobs*q + w.ci
                      ## if(length(x[ceiling(j)])<1) browser()

                      ilow <- ceiling(j)
                      if(ilow < 1) ilow <- 1
                      ihigh <- ceiling(k)
                      if(ihigh > nobs) ihigh <- nobs
                      
                      out <- c(est,x[ilow],x[ihigh])
                      out
                  }
                  )
    

    if(format == "df") out <- do.call(data.frame,c(lapply(out,identity),stringsAsFactors=F))
    ## if(debug) browser()
    out <- setNames(out,colnames)

    out
}
