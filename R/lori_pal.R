#' Default matlab palette.
#' @export
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
  return(output_pal)
}
