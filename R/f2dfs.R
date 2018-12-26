#' Apply function to multiple dataframes
#'
#' Use this function to apply any function to multiple dataframes specified by names
#' @param f function
#' @param dfs character vector of names of dataframes, tibbles ideally
#' @export
#'
#' @examples
#' ##Easy example
#' data(iris)
#' data(mtcars)
#' vars <- c("iris","mtcars")
#' f2dfs(nrow,vars)
f2dfs <- function(FUN,dfs = NULL){
  is.function(FUN)
  unlist(lapply(mget(dfs,inherits = T),FUN))
}
