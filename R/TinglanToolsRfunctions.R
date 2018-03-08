#' MLE
#'
#' Computes the liklihood of a given distribution for data x
#'
#' @param x vector
#' @param func function, e.g., `function(theta, x) dgamma(x, shape = theta, log = TRUE)`
#' @param interval vector, i.e., interval for optimize function
#'
#' @return scalar
#' @export
#' @examples
#' x1=rgamma(100,3)
#' func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
#' result7_gamma <- func7(x1,func1,c(0,3))
#' result7_gamma
#'
func7 <- function(x, func, interval){

  f7 <- function(theta, x)
  {sum(func(theta, x))}

  oout<- optimize(f7, maximum = TRUE, interval, x=x)
  return(oout$maximum)
}

#' basic- calculate mean, variane, sd
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' func1(rnorm(10))
func1 <- function(x){
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}

#' HW function to calculate Mean, Variane, sd
#'
#' Computes the mean, variance and sd of a vector, but with user checks
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' func2(rnorm(10))
func2 <- function(x){
  stopifnot(is.numeric(x))
  stopifnot(length(x)!=0)
  stopifnot(is.finite(x))
  stopifnot(!is.na(x))
  stopifnot(!is.nan(x))

  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}

#' HW problem to caululate weighted mean, var, sd with stopifnot checkes
#'
#' Computes the weighted mean, var, sd with user checks
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
#' func5(d)
func5 <- function(d){

  stopifnot(is.numeric(d$x))
  stopifnot(is.numeric(d$p))

  stopifnot(length(d$x)!=0)
  stopifnot(length(d$p)!=0)

  stopifnot(is.finite(d$x))
  stopifnot(is.finite(d$p))

  stopifnot(!is.na(d$x))
  stopifnot(!is.na(d$p))

  stopifnot(!is.nan(d$x))
  stopifnot(!is.nan(d$p))

  stopifnot(all.equal(sum(d$p),1))

  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}


#' HW problem to standaridize columns
#'
#' Computes (x−mean(x))/sd(x) for each column
#'
#' @param a data.frame
#'
#' @return list
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
#' newA(a)
newA <- function(x)
{
  stopifnot(nrow(x) > 1)

  aaa <- function(x){
    return((x-mean(x))/sd(x))
  }
  return(apply(x,2,aaa))
}

