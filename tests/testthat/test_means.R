context("means")

test_that("point estimate - geometric",{

    fileRef <- "testReference/means1.rds"

    x <- 1:10
    
  ## > gm1
  ##  [1] 4.528729
    gm1 <- means(x,type="geometric")
    expect_equal_to_reference(gm1,fileRef)
})


test_that("with confidence interval - geometric",{

    fileRef <- "testReference/means2.rds"
    
    x <- 1:10
    
    gm2 <- means(x, ci=TRUE,type="geometric")
    expect_equal_to_reference(gm2,fileRef)

})


test_that("point estimate - arithmetic",{
  
  fileRef <- "testReference/means3.rds"
  
  x <- 1:10
  
  am1 <- means(x,type="arithmetic")
  expect_equal_to_reference(am1,fileRef)
})


test_that("with confidence interval - arithmetic",{
  
  fileRef <- "testReference/means4.rds"
  
  x <- 1:10
  
  am2 <- means(x, type="arithmetic", ci=TRUE)
  expect_equal_to_reference(am2,fileRef)
  
})

test_that("with confidence interval - median",{
  
  fileRef <- "testReference/means5.rds"
  
  x <- 1:10
  
  md1 <- means(x, type="median", ci=TRUE)
  expect_equal_to_reference(md1,fileRef)
  
})
