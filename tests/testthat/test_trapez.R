context("trapez")

test_that("simple - no NA's",{

    fileRef <- "testReference/trapez1.rds"

    x <- 1:10
    y <- x^2-x+2
    
    tr1 <- trapez(x,y)
    expect_equal_to_reference(tr1,fileRef)

})


test_that("simple - no NA's - cumulative",{

    fileRef <- "testReference/trapez2.rds"

    x <- 1:10
    y <- x^2-x+2
    
    tr1 <- trapez(x,y,cum=T)
    expect_equal_to_reference(tr1,fileRef)

})
