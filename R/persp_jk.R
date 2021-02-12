#' Plot 3d surfaces, similar to matlab surf function
#'
#' @param x,y locations of grid lines at which the values in z are measured. These must be in ascending order. By default, equally spaced values from 0 to 1 are used. If x is a list, its components x$x and x$y are used for x and y, respectively.
#' @param z a matrix containing the values to be plotted (NAs are allowed).
#' @param colormap colormap used for the z-values
#' @param theta,phi angles defining the viewing direction. theta gives the azimuthal direction and phi the colatitude.
#' @param expand a expansion factor applied to the z coordinates. Often used with 0 < expand < 1 to shrink the plotting box in the z direction.
#' @param ticktype haracter: "simple" draws just an arrow parallel to the axis to indicate direction of increase; "detailed" draws normal ticks as per 2D plots.
#' @param legend should the legend be drawn. The default is TRUE.
#' @import fields
#' @export
#' @examples
#' data(ari)
#' persp_jk(z = ari)
#'
#' ## changing parameters
#' x <- seq(1,50,1)
#' y <- seq(1,20,1)
#' data(parula64)
#' persp_jk(x,y,ari,colormap = parula64,xlab = "t", ylab= "K neighbors",zlab = "Adjusted Rand Index",legend=F)
#'
#' ## changing border color
#' persp_jk(x,y,ari,colormap = viridisLite::viridis(64,option = "A"),xlab = "t", ylab= "K neighbors",zlab = "Adjusted Rand Index", border = "white")
#'
#' ## generating terrain
#' require(ambient)
#' require(scico)
#' set.seed(4425346)
#' terrain <- noise_simplex(c(100,100))
#' persp_jk(z = terrain, colormap = scico(256, palette = "vik"), border = "NA", legend = F)
persp_jk <- function(x = NULL,y = NULL,z,colormap = viridisLite::viridis(256),theta = -30,phi = 30,
                     expand = 0.5,ticktype = 'detailed',legend = T,sorted = F,xlab = "x", ylab = "y", zlab = "z", ...){
  if("data.frame" %in% class(x)){
    df <- x[,1:3]
    x <- sort(unique(df[[1]]))
    y <- sort(unique(df[[2]]))
    cnames <- colnames(df)
    if(!sorted){
      df <- arrange(df,!!sym(cnames[[1]]), !!sym(cnames[[2]]))
    }
    if(xlab == "x"){
      xlab <- cnames[1]
      ylab <- cnames[2]
      zlab <- cnames[3]
    }
    z <- matrix(data = df[[3]], nrow = length(x), ncol = length(y), byrow = TRUE)
  }else{
   if(is.null(x)) x <- seq(1,dim(z)[1],1)
   if(is.null(y)) y <- seq(1,dim(z)[2],1) 
  }
  
  zfacet <- z[-dim(z)[1],-dim(z)[2]]
  facetcol <- cut(zfacet,length(colormap))
  v <- persp(x,y,z,col = colormap[facetcol],theta = theta,phi = phi,
             ticktype = ticktype,expand = expand,xlab = xlab,ylab = ylab,zlab = zlab, ...)
  if(legend) fields::image.plot(legend.only=T, zlim=range(zfacet), col=colormap,useRaster = F)
}
