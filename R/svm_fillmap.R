# Replace colors in SVM fill plot
#'
#' @param g g should be an object output from `plot_svm_jk()`` where fill_plot = T. It's crucial that it's from that function because I embedded some information into the g$s_lim_jk.
#' @param fillmap This should be a diverging color palette. Default is using just `scale_fill_gradient2()`
#' @param s_lim This is the upper limit for the new scale. The lower limit is -s_lim. You can set this to a specific number, but it defaults to the highest number needed for the plot.
#' @export
#' @examples
#' data(df)
#' library(e1071)
#' model <- svm(label ~ x1 + x2, data = df, kernel = "radial", gamma = 1, scale = F, cost = 1.8)
#' g <- plot_svm_jk(df,model,fill_plot = T, plot_contour = F, plot_decision = F)
#' svm_fillmap(g)
svm_fillmap <- function(g,fillmap = NULL,s_lim = NULL){
  if(is.null(s_lim)) s_lim <- g$s_lim_jk
  if(is.null(fillmap)) return(g + scale_fill_gradient2())
  else return(g + scale_fill_gradientn(colours = fillmap, limits = c(-s_lim,s_lim)))
}
