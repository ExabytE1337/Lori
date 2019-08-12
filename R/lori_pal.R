#' Default Lori palettes
#'
#' @param pal choose wanted palette. So far only 5 ale implemented.
#' @export
#' @examples
#' lori_pal(1)
#' plot_colours(lori_pal(1))
#'
#' library(gridExtra)
#' g <- purrr::map(1:6,~plot_colours(lori_pal(.x)))
#' do.call("gridExtra::grid.arrange", c(g, nrow=1))
lori_pal <- function(pal = 1){
  output_pal <- NULL
  # to make this nice I won't be using if/else
  # why isn't there a case switch statement?
  if(pal == 1){
    matlab_rgb_col <- matrix(c(0,0.4470,0.7410,0.8500,0.3250,0.0980,0.9290,0.6940,0.1250,
                               0.4940,0.1840,0.5560,0.4660,0.6740,0.1880,0.3010,0.7450,
                               0.9330,0.6350,0.0780,0.1840),ncol = 3,byrow = T)
    output_pal <- rgb(matlab_rgb_col)
  }

  if(pal == 2){
    output_pal <- c("#003F5C","#F0B51E","#846437","#D6CBB9","#1A1C1F")
  }

  if(pal == 3){
    output_pal <- c("#3abba7","#f5ca48","#fd5b59","#333333")
  }

  if(pal == 4){
    output_pal <- c("#cc3366","#ffcccc","#6699cc","#66cccc")
  }

  if(pal == 5){
    output_pal <- c("#bf7b9d","#c11269","#b86634","#de8c40","#9c0724","#7b1c22")
  }

  if(pal == 6){
    output_pal <- c("#0394c9","#03305a","#2253bc","#047273","#73c022")
  }

  if(pal == 7){
    output_pal <- c("#5e315b","#ba6156","#f2a65e","#ffe478","#cfff70",
                    "#8fde5d","#3ca370","#3d6e70","#473b78","#4b5bab",
                    "#4da6ff","#e36956","#ffb570","#ff9166","#b0305c","#73275c")
  }
  return(output_pal)
}
