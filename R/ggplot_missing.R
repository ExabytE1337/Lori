#' Function to plot missing map
#'
#' It calls an internal function basic_miss_plot that shouldn't be used outside of this.
#' If split is TRUE then multiple graphs are produced on chunks of dataframe.
#' This might improve visibility in large dataframes
#' @param x should be a dataframe. As of now it is not being checked so use with care.
#' @param split should the dataframe be split into smaller chunks
#' @param  by if split is TRUE we are splitting the dataframe into chunks with by number of columns
#' @export
#' @examples
#' data("airquality")
#' ggplot_missing(airquality)
#'
#' ## splitting into 3 graphs
#' ggplot_missing(airquality,split = TRUE, by = 2)
ggplot_missing <- function(x, split = F, by = 25,...){
   if(split){
     cat("Producing",ncol(x)%/%by + !((ncol(x)%%by) == 0)," missmap graphs. \n")
     lapply(split_by_ncol(x,by),basic_miss_plot, ...)
   }
   else basic_miss_plot(x,...)
}
