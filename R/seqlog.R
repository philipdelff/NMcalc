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

seqlog <- function(from,to,length.out){
    exp(seq(log(from),log(to),length.out=length.out))
}
