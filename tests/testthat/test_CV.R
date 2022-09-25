context("CV")

test_that("normal",{

    fileRef <- "testReference/CV1.rds"
    
    set.seed(11)

    x <- rnorm(n=100,mean=10,sd=1)
    cv1 <- CV(x,log=F)
    expect_equal_to_reference(cv1,fileRef)

})

test_that("log",{

    fileRef <- "testReference/CV2.rds"
    
    set.seed(11)

    x <- rnorm(n=100,mean=10,sd=1)
    cv1 <- CV(x,log=T)
    expect_equal_to_reference(cv1,fileRef)

})
