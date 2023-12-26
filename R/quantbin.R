##' Bin observations by quantiles. Label by bin number or by interval.
##'
##' This is simple stuff, but I can never remember the exact quantile
##' and findInterval/cut commands to use. quantbin finds quantiles
##' using quantile and then assigns bins using either findInterval or
##' cut.
##'
##' @param x The observations
##' @param nbins Number of bins to use
##' @param label label="num" gives a numeric bin number
##'     (findInterval). label="interval" gives a character
##'     representation of the interval (cut).

##' @param ... additional arguments passed to quantile.
##' @details quantbin uses stats::quantile for quantile
##'     estimation. Except for x and probs, all parameters can be
##'     controlled using na.rm and ... arguments. See ?stats::quantile
##'     for details.
##'
##' na.rm na.rm=TRUE is needed for quantile to be able to estimate the
##' distribution if x contains NA's. Notice, if na.rm=T, an NA element
##' in x will still result in an NA element in return. If na.rm=F and
##' there are NA's in x, all elements will be NA in result (quantiles
##' cannot be determined, nor can the binning of x by those
##' quantiles).
##'
##' If data is not continuous, this method may not lead to balanced
##' distributions.
##' 
##' @return If label="num", integers. If label="interval", factors.
##' @import data.table
##' @importFrom stats quantile
##' @examples
##' set.seed(134)
##' library(data.table)
##' ## CRAN requires examples to run on a single thread
##' data.table::setDTthreads(1)
##' dt1 <- data.table(x=rnorm(n=1000))
##' dt1[,bin:=quantbin(x,nbins=4,label="num")]
##' dt1[,int:=quantbin(x,nbins=4,label="interval")]
##' ## perfect - flat distribution
##' dt1[,.N,keyby=.(bin,int)]
##' 
##' dt2 <- data.table(x=c(rnorm(n=100000),NA))
##' dt2[,bin:=quantbin(x,nbins=4,label="num",na.rm=TRUE)]
##' dt2[,int:=quantbin(x,nbins=4,label="interval",na.rm=TRUE)]
##' ## perfect - flat distribution
##' dt2[,.N,keyby=.(bin,int)]
##' unique(dt2[,.(bin,int)])[order(bin)]
##' 
##' 
##' ## we may not get a flat distribution in case of discrete observations
##' dt3 <- data.table(x=c(sample(1:3,100,replace=TRUE)))
##' dt3[,bin:=quantbin(x,nbins=2,label="num",na.rm=TRUE)]
##' dt3[,int:=quantbin(x,nbins=2,label="interval",na.rm=TRUE)]
##' ## Not a flat distribution
##' dt3[,.N,keyby=.(x,bin,int)]

##' @export


quantbin <- function(x,nbins,label="num",...){
    switch(label,
           num=findInterval(x,
                            quantile(x,probs=c(seq(0,1,length.out=nbins+1))[-1],...),
                            rightmost.closed=TRUE)
          ,
           interval=cut(x,
                        quantile(x,probs=c(seq(0,1,length.out=nbins+1)),...),
                        include.lowest=TRUE,right=FALSE)
          ,
           stop("label must be either \"num\" or \"interval\"")
           )
}


