#' Function to split a dataframe into a list
#'
#' This function splits a dataframe into a list of dataframes with at most
#' n columns. Especially useful when working with large dataframes.
#' @param x should be a dataframe. Right now I'm not checking it, but will be fixed in the future.
#' @param n maximum number of columns in the smaller dataframes.
#' @import tibble
#' @export
#'
split_by_ncol <- function(x,n = 25){
  return(tapply(as.list(x), as.factor(c(rep(1:(ncol(x)%/%n),each =
                                              n),rep(1+(ncol(x)%/%n),ncol(x)%%n))), as_tibble))
}
