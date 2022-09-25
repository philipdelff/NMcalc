##' Log-scale equidistant sequences
##'
##' Useful for generating sequences to be plotted on log scale. This
##' is really simple - seq is run on from and to after log
##' transformation, then the exponential is reported.
##' @param from start of sequence
##' @param to end of sequence
##' @param length.out length of sequence
##' @return A numeric vector.
##' @export
##' @examples
##' library(ggplot2)
##' df <- data.frame(x=seqlog(1,100,100))
##' df <- transform(df, y=x/(10+x))
##' ## the points are equidistant on the log x scale
##' ggplot(df,aes(x,y))+geom_point()+scale_x_log10()

seqlog <- function(from,to,length.out){
    exp(seq(log(from),log(to),length.out=length.out))
}
