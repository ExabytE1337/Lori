#' Plot the decision boundary of SVM model with 2 features
#'
#' @param df The data to be displayed in this layer.
#' @param svm_model svm model from library e1071 or kernlab
#' @param grid Size of the grid used for sampling the space. We have to evaluate the decision function f(x) in each point.
#' @param tolerance when ploting the contours for the decision function f(x) = +-1, how close need the points to be to that line in order to be highlighted.
#' @param color 3 color vector. First two colors used for datapoints, third one used for the contours.
#' @param plot_grid should the grid be plotted. Default value is TRUE.
#' @param plot_contour should the contour for f(x) = +-1 be plotted. Default value is TRUE. This setting doesn't affect the contours for fill_plot.
#' @param plot_decision should the decision boundary(the contour for f(x)=0) be plotted. Default value is TRUE.
#' @param plot_data should the original data be plotted? Default value is TRUE.
#' @param fill_plot should the plot be filled according to distance to the decision boundary. Classes have different shape. Filled shape means the point is a support vector. Default value is FALSE.
#' @param fillmap colormap used for the fill_plot. Consider using diverging color pallete. Many available in the pals package.
#' @param bins number of bins used for geom_contour in fill_plot.
#' @param contour_color color used for contours in fill_plot.
#' @param longer_legend should the legend be as long as the plot. Default value is TRUE.
#' @param add_title should the title be added. Default value is TRUE.
#' @param surface_plot should we plot the "decision" function f(x): R^2 -> R. Default value is FALSE.
#' @param title string title for the plot.
#' @param ESL_theme should x and y be without labels and ticks? Default value is FALSE.
#' @param subtitle should default subtitle be added. Default value is TRUE.
#' @param ... additional paramaters when calling the persp_jk function.
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' data(df)
#' library(e1071)
#' model <- svm(label ~ x1 + x2, data = df, kernel = "radial", gamma = 1, scale = F, cost = 1.8)
#' plot_svm_jk(df,model, ESL_theme = T)
#'
#' ## filled plot
#' plot_svm_jk(df,model,fill_plot = T, plot_contour = F, plot_decision = F)
#'
#' ## surface plot
#' plot_svm_jk(df,model,surface_plot = T, theta = 300)
plot_svm_jk <- function(df, svm_model = NULL, grid = 50, tolerance = 0.005,
                        color = c("#56b4e9","#e69f00","black"),plot_grid = T,
                        plot_contour = T, plot_decision = T, fill_plot = F, fillmap = NULL,
                        bins = 10, contour_color = "white",longer_legend = T, add_title = T,
                        surface_plot = F,title = "SVM - Visualization",
                        plot_data = T, ESL_theme = F, subtitle = T, ...){
  if(is.null(fillmap)) fillmap <- c("#151D44", "#182D4D", "#1B3E56", "#1B4E60", "#185E6A", "#136E72", "#197F78", "#278E7D",
                                    "#449C83", "#66AA88", "#85B695", "#A4C2A3", "#BFCFB7", "#D9DDCD", "#F1EDE6", "#F8EAE4",
                                    "#EFD4C7", "#E8BFAC", "#E2A992", "#DC937E", "#D57D6C", "#CA6865", "#BF5460", "#AF4260",
                                    "#9E3260", "#8C2460", "#761B5B", "#601554", "#4A1045", "#340D35")
  abs_max <- NULL
  e1071 <- typeof(svm_model)!="S4"
  if(e1071) variables <- as.character(attr(svm_model$terms,"variables"))[-1]
  else variables <- as.character(attr(svm_model@terms,"variables"))[-1]

  clab <- variables[1]
  xvar <- variables[2]
  yvar <- variables[3]

  xr <- seq(min(df[xvar]),max(df[xvar]),length = grid)
  xr <- c(2*xr[1]-xr[2],xr,xr[length(xr)]+xr[2]-xr[1])
  yr <- seq(min(df[yvar]),max(df[yvar]),length = grid)
  yr <- c(2*yr[1]-yr[2],yr,yr[length(yr)]+yr[2]-yr[1])
  grid_points <- as_tibble(expand.grid(xr,yr))
  colnames(grid_points) <- c(xvar,yvar)
  grid_points$label <- predict(svm_model,grid_points)
  if(e1071 == T) grid_points$f <- as.vector(attr(predict(svm_model,grid_points,decision.values = T),"decision.values"))
  else grid_points$f <- predict(svm_model,grid_points, type = "decision")
  if (surface_plot){
    z <- matrix(grid_points$f,byrow = F,nrow = length(xr))
    return(persp_jk(xr,yr,z,...))
  }
  else{
    if(!fill_plot){
      g <- ggplot(data = df, aes_string(x = xvar, y = yvar, color = clab)) + geom_blank()
      g <- g + scale_color_manual(values=color)
      g <- g + theme_bw()
      if(ESL_theme) g <- g +theme(axis.title = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.grid.major = element_blank(),
                                  axis.ticks = element_blank(),
                                  axis.text = element_blank(),
                                  plot.title = element_text(hjust = 0.5),
                                  legend.position = "none")

      g <- g + ggtitle(title)
      if (plot_grid) g <- g + geom_point(data = grid_points, aes_string(x = xvar, y = yvar, color = "label"),cex = 0.3)

      if(plot_data){
        g <- g + geom_point(shape = 1,cex = 3,stroke = 1.5)
        original_points <- (near(svm_model$decision.values,1,tol = tolerance) | near(svm_model$decision.values,-1,tol = tolerance))
        g <- g + geom_point(data = df[original_points,],aes_string(x = xvar, y = yvar), color = color[3],cex = 3)
      }
    }
    else{#fill_plot==T
      abs_max <- max(abs(grid_points$f))
      g <- ggplot(data = grid_points,aes_string(xvar,yvar,z = "f")) + geom_raster(interpolate = T,aes(fill = f))+
        scale_fill_gradientn(colours = fillmap, limits = c(-abs_max,abs_max))+theme_bw()
      g <- g + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
      g <- g + scale_y_continuous(expand=c(0, 0)) + scale_x_continuous(expand=c(0, 0)) +
        theme(legend.title=element_blank(),legend.text=element_text(size=12))
      if(longer_legend){
        #fix according to https://stackoverflow.com/questions/19214914/how-can-i-make-the-legend-in-ggplot2-the-same-height-as-my-plot
        panel_height <- unit(1,"npc") - sum(ggplotGrob(g)[["heights"]][-3]) - unit(1,"line")
        g <- g + guides(fill= guide_colorbar(barheight = panel_height))
      }
      if(subtitle) g <- g + labs(subtitle = bquote(f(x): R^2 %->% R    ~ "signed distance of feature space to the decision hyperplane"))
      if(plot_data){
        df2 <- df
        df2$f <- NA
        # later on we can add the ability to assign shapes according to the true and predicted classes
        # df2$label_model <- predict(svm_model)
        # df2$label_interaction <- interaction(df2$label,df2$label_model)
        df2$sv <- F
        shapes <- c(2,17,1,16)
        if(e1071) svectors <- as.numeric(rownames(svm_model$SV))
        else svectors <- SVindex(svm_model)
        df2$sv[svectors] <- T
        df2$sv_class <- interaction(df2$sv,df2[[clab]])
        g <- g + geom_point(data = df2,aes_string(xvar,yvar,shape = "sv_class"),show.legend = F) +
          scale_shape_manual(values=shapes)
      }
      g <- g + geom_contour(bins = bins,col = contour_color,alpha = 0.2)
      g <- g + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
    }
  }
  if(plot_contour) {
    g <- g + geom_contour(data = grid_points, aes_string( x = xvar,y = yvar, z = "f"),
                          cex =0.5,breaks = c(-1,1), color = color[3],lty = "dashed")
  }
  if(plot_decision){
    g <- g + geom_contour(data = grid_points, aes_string( x = xvar,y = yvar, z = "f"),
                          cex =1,breaks = 0, color = color[3])
  }
  g <- g + theme(legend.text.align = 1)
  g$s_lim_jk <- abs_max
  return(g)
}
