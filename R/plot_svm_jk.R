#' Plot the decision boundary of SVM model with 2 features
#'
#' @param df The data to be displayed in this layer.
#' @param xvar Name of the variable in dataframe df to be used in ploting as x-axis.
#' @param yvar Name of the variable in dataframe df to be used in ploting as y-axis.
#' @param grid Size of the grid used for sampling the space. We have to evaluate the decision function f(x) in each point.
#' @param clab Label name for the datapoints.
#' @param svm_model svm model from library e1071 or kernlab
#' @param tolerance when ploting the contours for the decision function f(x) = +-1, how close need the points to be to that line in order to be highlighted.
#' @param color 3 color vector. First two colors used for datapoints, third one used for the contours.
#' @param plot_grid should the grid be plotted. Default value is FALSE.
#' @param plot_contour should the contour for f(x) = +-1 be plotted. Default value is FALSE.
#' @param fill_plot should the plot be filled according to distance to the decision boundary. Classes have different shape. Filled shape means the point is a support vector. Default value is FALSE.
#' @param fillmap colormap used for the fill_plot. Consider using diverging color pallete. Many available in the pals package.
#' @param bins number of bins used for geom_contour in fill_plot.
#' @param contour_color color used for contours in fill_plot.
#' @param longer_legend should the legend be as long as the plot. Default value is TRUE.
#' @import ggplot2
#' @import dplyr
#' @import pals
#' @export
plot_svm_jk <- function(df, xvar = "x1", yvar = "x2",grid = 50,clab = "label",
                        svm_model = NULL, tolerance = 0.005,
                        color = c("#56b4e9","#e69f00","black"),plot_grid = F,
                        plot_1 = F, fill_plot = F, fillmap = pals::ocean.curl(30),
                        bins = 10, contour_color = "white",longer_legend = T){
  require(ggplot2)
  require(dplyr)
  require(pals)
  e1071 <- typeof(svm_model)!="S4"
  xr <- seq(min(df[xvar]),max(df[xvar]),length = grid)
  xr <- c(2*xr[1]-xr[2],xr,xr[length(xr)]+xr[2]-xr[1])
  yr <- seq(min(df[yvar]),max(df[yvar]),length = grid)
  yr <- c(2*yr[1]-yr[2],yr,yr[length(yr)]+yr[2]-yr[1])
  grid_points <- as_tibble(expand.grid(xr,yr))
  colnames(grid_points) <- c(xvar,yvar)
  grid_points$label <- predict(svm_model,grid_points)
  if(e1071 == T) grid_points$f <- attr(predict(svm_model,grid_points,decision.values = T),
                                       "decision.values")
  else grid_points$f <- predict(svm_model,grid_points, type = "decision")
  if(fill_plot == F){
    g <- ggplot(data = df, aes_string(x = xvar, y = yvar, color = clab)) + geom_blank()
    g <- g + scale_color_manual(values=color)
    g <- g + theme_bw() +  theme(axis.title = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.grid.major = element_blank(),
                                 axis.ticks = element_blank(),
                                 axis.text = element_blank(),
                                 plot.title = element_text(hjust = 0.5),
                                 legend.position = "none")
    g <- g + ggtitle("SVM - Visualization")
    if (plot_grid) g <- g + geom_point(data = grid_points, aes(x = x1, y = x2, color = label),cex = 0.3)
    g <- g + geom_point(shape = 1,cex = 3,stroke = 1.5)
    g <- g + geom_contour(data = grid_points, aes( x = x1,y = x2, z = f), cex =1,breaks = 0, color = color[3])
    if(plot_1) g <- g + geom_contour(data = grid_points, aes( x = x1,y = x2, z = f),
                                     cex =0.5,breaks = c(-1,1), color = color[3],lty = "dashed")
    original_points <- (near(model$decision.values,1,tol = tolerance) | near(model$decision.values,-1,tol = tolerance))
    g <- g + geom_point(data = df[original_points,],aes(x = x1, y = x2), color = color[3],cex = 3)
    g
  }
  else{
    g <- ggplot(data = grid_points,aes(x1,x2,z = f)) + geom_raster(interpolate = T,aes(fill = f))+
      scale_fill_gradientn(colours = fillmap)+theme_bw()
    g <- g + scale_y_continuous(expand=c(0, 0)) + scale_x_continuous(expand=c(0, 0)) +
      theme(legend.title=element_blank(),legend.text=element_text(size=12))
    if(longer_legend){
      #fix according to https://stackoverflow.com/questions/19214914/how-can-i-make-the-legend-in-ggplot2-the-same-height-as-my-plot
      panel_height <- unit(1,"npc") - sum(ggplotGrob(g)[["heights"]][-3]) - unit(1,"line")
      g <- g + guides(fill= guide_colorbar(barheight = panel_height))
    }
    df2 <- df
    df2$f <- NA
    # later on we can add the ability to assign shapes according to the true and predicted classes
    # df2$label_model <- predict(svm_model)
    # df2$label_interaction <- interaction(df2$label,df2$label_model)
    df2$sv <- F
    shapes <- c(17,2,1,16)
    if(e1071 == T) svectors <- as.numeric(rownames(model$SV))
    else svectors <- SVindex(model_k)
    df2$sv[svectors] <- T
    df2$sv_class <- interaction(df2$sv,df2$label)
    g <- g + geom_contour(bins = bins,col = contour_color,alpha = 0.2)
    g <- g + geom_point(data = df2,aes(x1,x2,shape = sv_class),show.legend = F) +
      scale_shape_manual(values=shapes)
    g
  }

}
