context("quantbin")

library(data.table)
data.table::setDTthreads(1)

test_that("continuous values",{
    library(data.table)
    fileRef <- "testReference/quantbin_01.rds"

    set.seed(134)
    dt1 <- data.table(x=rnorm(n=1000))
    dt1[,bin:=quantbin(x,nbins=4,label="num")]
    dt1[,int:=quantbin(x,nbins=4,label="interval")]
    ## perfect - flat distribution
    res1 <- dt1[,.N,keyby=.(bin,int)]

    expect_equal_to_reference(res1,fileRef)

})


test_that("with NAs",{

    fileRef <- "testReference/quantbin_02.rds"
    set.seed(134)

    dt2 <- data.table(x=c(rnorm(n=100000),NA))
    dt2[,bin:=quantbin(x,nbins=4,label="num",na.rm=T)]
    dt2[,int:=quantbin(x,nbins=4,label="interval",na.rm=T)]
    ## perfect
    res1 <- dt2[,.N,keyby=.(bin,int)]

    expect_equal_to_reference(res1,fileRef)

    ## If missing values, we need na.rm=TRUE for quantiles to evaluate
    expect_error( dt2[,int:=quantbin(x,nbins=4,label="interval",na.rm=F)])

})


test_that("discrete data",{

    fileRef <- "testReference/quantbin_03.rds"
    set.seed(134)

    ## we may not get a flat distribution in case of discrete observations
    dt3 <- data.table(x=c(sample(1:3,100,replace=T)))
    dt3[,bin:=quantbin(x,nbins=2,label="num",na.rm=T)]
    dt3[,int:=quantbin(x,nbins=2,label="interval",na.rm=T)]
    ## Not correct 
    res1 <- dt3[,.N,keyby=.(x,bin,int)]

    expect_equal_to_reference(res1,fileRef)
    
})
