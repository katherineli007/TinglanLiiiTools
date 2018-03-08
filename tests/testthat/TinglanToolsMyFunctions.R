context("MLE function")

test_that("func7 computes MLE correctly", {
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

context("dplyr::count")

test_that("count takes in and counts the margins of a list correctly", {
  df1 <- data.frame(x = 1:10, y = 1:10, z = 1:10)
  bbb <- nrow(df1)
  expect_identical(bbb , nrow(dplyrWrapper(df1)))
})





context("Homework functions")

test_that("func1 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  expect_identical(func1(x), x_list)
})

test_that("func2 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  expect_identical(func2(x), x_list)
  save<-try(func2(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: is.numeric(x) is not TRUE\n")
})

test_that("func5 computes weighted mean, var, sd", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  fff<-list(mean=a,var=b,sd=c)
  expect_identical(func5(d), fff)
})

