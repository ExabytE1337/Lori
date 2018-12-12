#' Plot a vector of hex colours.
#'
#' This function is from some other package, I don't know where it's from. But it's super helpful.
#' @param colour_vector vector of colors to plot.
#' @export
#' @examples
#' m <- matlab_pal()
#' plot_colours(m)
#'
#' ##
#' plot_colours(pals::ocean.curl(1000),F)
plot_colours <- function(colour_vector,names = T, background_colour = 'white') {
  if (length(colour_vector) < 1)
    stop('Need at least one colour to plot.')
  colour_df <- data.frame(Colours = factor(colour_vector, levels=rev(unique(colour_vector))))
  colour_df
  g <- ggplot(colour_df, aes_string(x = factor(1), y = 'Colours', fill = 'Colours'))+
    geom_tile() +
    scale_fill_manual(values=levels(colour_df$Colours), guide='none') +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.line=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
  if(names){
    g <- g + geom_text(aes_string(label = 'Colours'), colour='white', vjust=1.1, fontface='bold') +
      geom_text(aes_string(label = 'Colours'), colour='black', vjust=-0.1, fontface='bold')
  }
  g
}

