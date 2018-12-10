require(jpeg)
img <- readJPEG("D:\\Pictures\\gopro-app.jpg")
#funkcia pre kreslenie obrazka
plot_jpeg = function(jpg, add=FALSE)
{
  res = dim(jpg)[2:1] # get the resolution, [x, y]
  if (!add) # initialize an empty plot area if add==FALSE
    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(jpg,1,1,res[1],res[2])
}
plot_jpeg(img)

#podme sa hrat
require(tidyverse)
library(Lori)
xgrid <- ceiling(seq(1, dim(img)[1], length.out = 50))
ygrid <- ceiling(seq(1, dim(img)[2], length.out = 50))
pixels <- as_tibble(expand.grid(xgrid, ygrid))
pixels$redVals <- apply(pixels, 1, function(x){return(img[x[1], x[2], ][1])})
pixels$greenVals <- apply(pixels, 1, function(x){return(img[x[1], x[2], ][2])})
pixels$blueVals <- apply(pixels, 1, function(x){return(img[x[1] ,x[2], ][3])})
colnames(pixels) <- c("x","y","r","g","b")
pixels
library(cluster)
a <- agnes(pixels[,3:5])
ind <- cutree(a, k = 4)
pixels$label <- ind
#ind2 <- kmeans(pixels[,3:5],4)
pixels$label <- ind2$cluster
vysledok <- 
pixels %>% 
  group_by(label) %>%
  summarise( avg_r = mean(r),avg_g = mean(g),avg_b = mean(b)) %>%
  select(-1)
vysledok
plot_colours(rgb(vysledok))
