context("signif2")

test_that("Several tests",{

    x <- c(1.24e-4,1.1334e6,1.1,22.00000,10.00,1)
    expect_equal(signif2(x,3),
                 c("0.000124","1130000","1.10","22.0","10.0","1.00")
                 )
##    expect_warning(signif2(x,3,add=FALSE))
    expect_equal(signif2(c(.2,11.84),2),
                 c("0.20","12")
                 )
    expect_error(signif2(3205,-1))
    expect_equal(signif2(0,1),"0")
    expect_equal(signif2(0,3),"0")

})
