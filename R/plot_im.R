plot_im_s  <- function(path = NULL,pixels = NULL, grid = 40, cex = 0.5){
  img <- jpeg::readJPEG(path)
  res = dim(img)[2:1]
  plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(img,1,1,res[1],res[2])

  xgrid <- ceiling(seq(1, dim(img)[1], length.out = grid))
  ygrid <- ceiling(seq(1, dim(img)[2], length.out = grid))
  pixels <- as_tibble(expand.grid(xgrid, ygrid))

  pixels$redVals <- apply(pixels, 1, function(x){return(img[x[1], x[2], ][1])})
  pixels$greenVals <- apply(pixels, 1, function(x){return(img[x[1], x[2], ][2])})
  pixels$blueVals <- apply(pixels, 1, function(x){return(img[x[1] ,x[2], ][3])})
  colnames(pixels) <- c("y","x","r","g","b")
  pixels$hex <- rgb(pixels$r, pixels$g, pixels$b)
  points(x = pixels$x,y = pixels$y,cex = cex, pch = 21, bg= "black", col = "white")

  gpixels <- pixels
  gpixels$y <- rev(gpixels$y)
  g <- ggplot(gpixels, aes(x,y,col = hex)) + geom_point() + scale_color_identity() + theme_void()
  print(g)
  return(pixels)
}
