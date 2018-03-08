context("MLE function")

test_that("func7 computes MLE likelihood", {
  x1=rgamma(100,3)
  funcga = function(theta, x) dgamma(x, shape = theta, log = TRUE)
  func11 <- function(x, func, interval){

 f11 <- function(theta, x)
  {sum(func(theta, x))}

 oout<- optimize(f11, maximum = TRUE, interval, x=x)
   return(oout$maximum)
}
atest <- func11(x1,funcga,c(0,3))
func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
  expect_identical(func7(x1,func1,c(0,3)) , atest)
})

context("Wrapper for ggplot2")

test_that("ggplot is labeled correctly", {
  df <- data.frame(
    Response = LETTERS[1:5],
    Proportion = c(0.1,0.2,0.1,0.2,0.4)
  )
  aaa <- plotMyData(df)
  expect_identical(aaa$labels$y , "p")
})

