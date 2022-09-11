##' Log-scale equidistant sequences
##'
##' Useful for generating sequences to be plotted on log scale.
##' @param from start of sequence
##' @param to end of sequence
##' @param length.out length of sequence
##' @export

seqlog <- function(from,to,length.out){
    exp(seq(log(from),log(to),length.out=length.out))
}
