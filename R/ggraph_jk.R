#' Plot undirected graphs constructed from 2D data
#'
#' @param data The data to be displayed in this layer.
#' @param matrix_g Matrix of edges in calculated graph. Could be a full matrix
#'   or sparse from the Matrix package.
#' @param x Name of the first coordinate.
#' @param y Name of the second coordinate.
#' @param label Name of the column coresponding to ground truth in clustering.
#'   Could be omitted.
#' @param colormap Colormap to color the vertices/edges of a graph.
#' @param colormap Colormap to color the vertices/edges of a graph.
#' @param color A string indicating of what to color, using a colormap. Four
#'   options available: "none", "edge", "degree" and "wdegree". Degree uses
#'   colormap to color points based on the number of edges going to/from the
#'   given vertex. Wdegree calculates the sum of weights of edges in each
#'   vertex.
#' @param plot_points boolean. Should we plot the vertices?
#' @param color_points The chosen color for coloring points.
#' @param color_lines The chosen color for coloring edges.
#' @param cex_p Change the size of points.
#' @param size Change the width of edges.
#' @param matlab boolean. Should we use default MATLAB colors and try to
#'   replicate the ggplot theme?
#' @import Matrix
#' @import dplyr
#' @import ggplot2
#' @import viridisLite
#' @import tidyr
#' @import pals
#' @export
#' @examples
#' ## Edge coloring
#' data(spiraly)
#' data(Wfull)
#' ggraph_jk(spiraly,Wfull)
#'
#' ggraph_jk(spiraly,Wfull,colormap = colormap::colormap(colormap = "hot", n = 256),cex_p = 1.5)
#'
#' ## exporting graph and changing it
#' g <- ggraph_jk(data = spiraly, matrix_g = Wfull, color = "degree",cex_p = 3)
#' g + theme(legend.position = c(0.9,0.2),legend.background=element_blank())
#'
#' ##coloring nodes based on the sum of all weights of edges connected to them.
#' g <- ggraph_jk(data = spiraly, matrix_g = Wfull, color = "wdegree",cex_p = 3,matlab = T,color_lines = "black")
#' g + theme(legend.position = c(0.9,0.2),legend.background=element_blank())
ggraph_jk <- function(data = NULL, matrix_g = NULL,x = colnames(data)[1],
                      y = colnames(data)[2],label = colnames(data)[3],
                      colormap = NULL,
                      color = "edge", plot_points = T, color_points = NULL,
                      color_lines = NULL,cex_p = 3,size = 0.5, matlab = F){
  # Load required packages ---------------------------
  require(Matrix)
  require(ggplot2)
  require(dplyr)
  # Load matlab colors ---------------------------
  matlab_rgb_farby <- matrix(c(0,0.4470,0.7410,0.8500,0.3250,0.0980,0.9290,
                               0.6940,0.1250,0.4940,0.1840,0.5560,0.4660,0.6740,
                               0.1880,0.3010,0.7450,0.9330,0.6350,0.0780,0.1840),ncol = 3,byrow = T)
  m_hex_farby <- rgb(matlab_rgb_farby)
  matlab_theme <- theme(panel.grid = element_blank(),
                        panel.background = element_rect(fill = "white"),
                        axis.line = element_line(colour = "black"),
                        panel.border = element_rect(colour = "black", fill = NA),
                        axis.ticks.x.top = element_line(colour = "black"))
  # Check for sparsity of the graph ---------------------------
  sparse <- is(matrix_g,'sparseMatrix')
  if(!sparse) Wsparse <- Matrix(matrix_g,sparse = T)
  else Wsparse <- matrix_g
  W_df <- as_tibble(summary(Wsparse))
  colnames(W_df) <- c("i","j","value")
  # Find all the rows from data that need to be connected ---------------------------
  df_rows <- W_df %>% gather(link,row,-value,factor_key = T) %>% .$row
  W_point_lines <- data[df_rows,c(x,y)] %>%
    mutate(grp = factor(c(1:(dim(W_df)[1]),1:(dim(W_df)[1]))),value = c(W_df$value,W_df$value))
  g <- ggplot(W_point_lines,aes_string(x = x,y = y))
  g <- g + theme(legend.title.align=0.5)
  # Add matlab theme to plot and colors if not specified ---------------------------
  if(matlab){
    if(is.null(color_points)) color_points <- "#D95319"
    if(is.null(color_lines)) color_lines <- "#0072BD"
    if(is.null(colormap)) colormap <- pals::parula(256)
    g <- g + matlab_theme
  }
  if(is.null(colormap)) colormap <- viridisLite::viridis(256)
  if(is.null(color_points)) color_points <- "black"
  if(is.null(color_lines)) color_lines <- "black"
  if (color != "none"){
    if(color == "edge"){
      # Decide on coloring scheme ---------------------------
      g <- g + scale_color_gradientn(name = "Edge weight",colors = colormap)
      g <- g + geom_line(aes(group = grp,color = value),size = size)
      if(plot_points) g <- g + geom_point(data = data,aes_string(x = x,y = y),color = color_points,cex = cex_p)
    }
    else{
      g<- g + geom_line(aes(group = grp),color = color_lines,size = size)
      if(plot_points){
        if(color =="degree"){
          g <- g + scale_color_gradientn(name = "Vertex degree",colors = colormap)
          point_n <- W_df %>% gather(link,row,-value,factor_key = T) %>% group_by(row) %>% summarise(n = n())
          data2 <- data
          data2$edge_count <- rep(0,dim(data)[1])
          data2$edge_count[point_n$row] <- point_n$n
          g <- g + geom_point(data = data2,aes_string(x = x,y = y,color = "edge_count"),cex = cex_p)
        }
        else{
          if(color =="wdegree"){
            W_point_lines$point_id <- df_rows
            temp <- W_point_lines %>%
              group_by(point_id) %>%
              summarize(w_vertex_degree = sum(value))
            data2 <- data
            data2$w_degree <- rep(0,dim(data)[1])
            data2$w_degree[temp$point_id] <- temp$w_vertex_degree
            g <- g + scale_color_gradientn(name = "Weighted degree",colors = colormap)
            g <- g + geom_point(data = data2,aes_string(x = x,y = y,color = "w_degree"),cex = cex_p)
          }
          else stop("Undefined specification for: color. Please choose one of the following: edge, degree, wdegree, none")
        }
      }
    }
  }
  else{
    # Final plot ---------------------------
    g <- g + geom_line(aes(group = grp),color = color_lines,size = size)
    g <- g + geom_point(data = data,aes_string(x=x,y=y),color = color_points,cex = cex_p)
  }
  return(g)
}
