#' Cut a "colored" tree
#'
#' This function is used for getting hard clusters by cutting a tree. Use function \code{\link{plot_colours}} on the result of this function to get a plot.
#' @param tree_model model produced by \code{\link{pick_palette_jk}} function.
#' @param k number of clusters.
#' @param fun function used on grouped data. Perhaps it makes sense to use different functions than mean. Median might be a good choice. Input should be character.
#' @import rlang
#' @import pals
#' @import dplyr
#' @export
#' @examples
#' path <- "./man/figures/duck.jpg"
#' tree <- pick_palette_jk(path)
#' colours <- tree2color(tree, k = 10)
#' plot_colours(colours)
#'
#' ## or use something like this
#' color_list <- purrr::map(1:10,~tree2color(tree,.x))
#' do.call(pals::pal.bands,color_list)
tree2color <- function(tree_model,k = 3, fun = "mean"){
  tree <- tree_model[[1]]
  pixels <- tree_model[[2]]
  ind <- cutree(tree, k = k)
  pixels$label <- ind
    vysledok <-
      pixels %>%
      group_by(label) %>%
      summarise_at( c(3:5),sym(fun)) %>%
      select(-1)
  #if(plot) plot_colours(rgb(vysledok))
  return(rgb(vysledok))
}