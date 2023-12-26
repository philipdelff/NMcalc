context("logit")
library(data.table)
data.table::setDTthreads(1)

test_that("basic",{

    fileRef <- "testReference/logit_01.rds"
    
    res <- logit(seq(0,1,.1))
    res    
    expect_equal_to_reference(res,fileRef)

})

context("invlogit")
test_that("basic",{

    fileRef <- "testReference/invlogit_01.rds"

    x <- seq(0,1,.1)
    lres <- logit(x)
    res <- invlogit(lres)
    res    
    expect_equal(res,x)

})
