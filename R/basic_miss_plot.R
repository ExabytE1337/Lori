basic_miss_plot <- function(x, col=NULL ,flip = F){
   xna <- is.na(x)
   cislo <- sum(xna)/(dim(x)[1]*dim(x)[2])
   cislo <- round(cislo*100,1)
   suppressMessages(require(ggplot2))
   if(is.null(col)) col <- c("#003f5c","#ffa600")
   if(length(col) > 2) col <- col[1:2]
   col <- rev(col)
  g <-
    ggplot(data = reshape2::melt(xna),aes(x = Var2,y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_manual(values = col,  name = "",labels = c(paste0("Present ",100-cislo,"%"),paste0("Missing ",cislo,"%"))) +
    theme_minimal() +
    theme(axis.text.x  = element_text(angle=90,vjust = 0.5,hjust = 0.99)) +
    labs(x = "Variables in Dataset", y = "Rows / observations")
  if(flip) g <- g + coord_flip()
  return(g)
}
