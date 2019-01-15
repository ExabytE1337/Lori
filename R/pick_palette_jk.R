#' Pick color palette from a picture
#'
#' This function uses clustering on a subset of pixels from given picture. The final output is mean of pixels in each cluster converted to hex.
#' Agnes is computationaly much more intensive than kmeans. Hierarchical clustering is primarily used to construct the whole dendrogram.
#' So we can just ouput the whole tree and let you plot or get colors afterwards. Use function \code{\link{tree2color}} from this package.
#' However if you choose the kmeans algorithm, you can only get one vector of colors.
#' @param path Path used to get the .jpg picture. Before running this function you can run file.choose() function to get the desired path.
#' @param k Desired number of colors in a palette.
#' @param plot_image Should the original image be plotted. The default value is FALSE.
#' @param algorithm Two methods are implemented. The currently available are "agnes", "diana" and "kmeans". The default is "agnes".
#' @param grid The size of grid used for pixel sampling.
#' @param linkage Link used in hierarchical clustering. Default is "single".
#' @param kmax Maximal k wanted in the tree while using diana.
#' @import jpeg
#' @import tibble
#' @import dplyr
#' @import cluster
#' @export
#' @examples
#' path <- "./man/figures/duck.jpg"
#' tree <- pick_palette_jk(path) #keep in mind this can take up to 5 mins
#' colours <- tree2color(tree, k = 10)
#' plot_colours(colours)
#'
#' ## using diana
#' tree <- pick_palette_jk(path,algorithm = "diana")
#' tree2color(tree, k = 10) %>% sort_colours %>% plot_colours
#'
#' ## or use something like this
#' color_list <- purrr::map(1:10,~tree2color(tree,.x))
#' do.call(pals::pal.bands,color_list)
#' #compare this with
#' plot_colours(rgb(pick_palette_jk(path,algorithm = "kmeans",k = 10)))
#' #hierarchical clustering has great potential but is really slow compared to kmeans
#'
#'
pick_palette_jk <- function(path = NULL, k = 3, plot_image = F, algorithm = "agnes", grid = 60, full_object = T, linkage = "single", kmax = 10){
  img <- jpeg::readJPEG(path)
  if(plot_image){
    res = dim(img)[2:1]
    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(img,1,1,res[1],res[2])
  }
  xgrid <- ceiling(seq(1, dim(img)[1], length.out = grid))
  ygrid <- ceiling(seq(1, dim(img)[2], length.out = grid))
  pixels <- as_tibble(expand.grid(xgrid, ygrid))
  pixels$redVals <- apply(pixels, 1, function(x){return(img[x[1], x[2], ][1])})
  pixels$greenVals <- apply(pixels, 1, function(x){return(img[x[1], x[2], ][2])})
  pixels$blueVals <- apply(pixels, 1, function(x){return(img[x[1] ,x[2], ][3])})
  colnames(pixels) <- c("x","y","r","g","b")
  if(algorithm == "kmeans"){
    kmeans_obj <- kmeans(pixels[,3:5],k)
    vysledok <- kmeans_obj$centers
    return(rgb(vysledok))
  }
  if(algorithm == "diana"){
    n_rows <- nrow(pixels[,3:5])
    tree <- cluster::diana(pixels[,3:5],stop.at.k = kmax, keep.data = F, keep.dis = F)
    if(!full_object){
      ind <- cutree(tree, k = k)
      pixels$label <- ind
      vysledok <-
        pixels %>%
        group_by(label) %>%
        summarise( avg_r = mean(r),avg_g = mean(g),avg_b = mean(b)) %>%
        select(-1)
      return(rgb(vysledok))
    }
    else {
      return(list(tree,pixels))
    }
  }
  if(algorithm == "agnes"){
    tree <- cluster::agnes(pixels[,3:5], method = linkage)
    if(!full_object){
      ind <- cutree(tree, k = k)
      pixels$label <- ind
      vysledok <-
        pixels %>%
        group_by(label) %>%
        summarise( avg_r = mean(r),avg_g = mean(g),avg_b = mean(b)) %>%
        select(-1)
      return(rgb(vysledok))
    }
    else {
      return(list(tree = tree, pixels = pixels))
    }
  }
}
