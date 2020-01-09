#' Default Lori palettes
#' @export
lori_pal <- list(matlab = c("#0072BD","#D95319","#EDB120","#7E2F8E","#77AC30",
                            "#4DBEEE","#A2142F"),
                 sweater = c("#003F5C","#F0B51E","#846437","#D6CBB9","#1A1C1F"),#favorite sweater
                 vimix = c("#3abba7","#f5ca48","#fd5b59","#333333"),#vimix
                 ikea_1 = c("#cc3366","#ffcccc","#6699cc","#66cccc"),#swedish furniture store 1
                 ikea_2 = c("#bf7b9d","#c11269","#b86634","#de8c40","#9c0724",
                            "#7b1c22"),#swedish furniture store 2
                 ikea_3 = c("#0394c9","#03305a","#2253bc","#047273","#73c022"),#swedish furniture store 3
                 nord =  c("#2e3440","#3b4252","#434c5e","#4c566a","#d8dee9",
                           "#e5e9f0","#eceff4", "#8fbcbb","#88c0d0","#81a1c1",
                           "#5e81ac","#bf616a","#d08770","#ebcb8b", "#a3be8c",
                           "#b48ead"),#https://www.nordtheme.com/
                 noidea = c("#5e315b","#ba6156","#f2a65e","#ffe478","#cfff70",
                            "#8fde5d","#3ca370","#3d6e70","#473b78","#4b5bab",
                            "#4da6ff","#e36956","#ffb570","#ff9166","#b0305c",
                            "#73275c"),#adjusted color scheme from somewhere, i'll update this with a reference
                 one_dark_vivid = c("#d55fde","#f44747","#ef596f","#d19a66","#e5c07b",
                                    "#7f848e","#5c6370","#61afef","#89ca78","#2bbac5",
                                    "#ffffff"), #one dark vivid
                 one_dark_classic = c("#c678dd","#f44747","#e06c75","#d19a66","#e5c07b",
                                      "#7f848e","#5c6370","#61afef","#98c379","#56b6c2",
                                      "#ffffff"))#one dark classic

#' Plotting list of colors
#'
#' @export
#' @example
#' list_plotter(lori_pal)
list_plotter <- function(color_list) { #original from EmilHvitfeldt
  par(mar = c(0, 0, 0, 0) + 0.1)

  plot(0, 0, type = "n", axes = FALSE, bty = "n", xlab = "", ylab = "",
       xlim = c(0, 1), ylim = c(-length(color_list)-1, 0))
  for (i in seq_len(length(color_list))) {
    colors_len <- length(color_list[[i]])
    breaks <- seq(from = 0, to = 1, length = colors_len + 1)
    if(!is.null(names(color_list))) text(0, -i, names(color_list)[i], pos = 4)
    rect(xleft = breaks[1:colors_len], xright = breaks[1:colors_len + 1],
         ytop = - 0.15-i, ybottom = -0.8-i,
         col = color_list[[i]], border = NA)
  }
}
