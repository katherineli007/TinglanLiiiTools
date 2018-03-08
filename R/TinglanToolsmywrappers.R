#' Wrapper function for ggplot2 for data d
#'
#' Computes a neat plot
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' plotMyData(d)
plotMyData<-function(x){
  library(magrittr)
  x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
}

#' dplyr wrapper function for acquiring marginals using function dplyr
#'
#' manipulate margins and select data
#'
#' @param x data-frame
#'
#' @export
dplyrWrapper<-function(x){
  return(dplyr::count(x,y))
}
