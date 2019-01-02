#' Function to sort colors
#'
#' Warning it is not clear how to sort colors properly.
#' As of now, only one way is implemented here.
#' First we transform the hex values to RGB and then to HSV.
#' Then we apply pca retaining only 1 componen and sort it.
#' @param col color vector in hex format
#' @param method only "PCA_HSV" is implemented
sort_colours <- function(col = NULL, method = "PCA_HSV"){
  color <- col2rgb(col)
  #color <- rgb2hsl(col)
  color <- rgb2hsv(color)
  col[sort(prcomp(color)$rotation[,1], index.return = T)$ix]
}
