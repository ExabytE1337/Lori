## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(dev='svg')

## ----results='hide', message=FALSE, warning=FALSE------------------------
library(Lori)

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=8-----------
data(spiraly)
data(Wfull)
ggraph_jk(data = spiraly,matrix_g = Wfull,colormap = colormap::colormap(colormap = "hot", n = 256),cex_p = 1.5)

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=7-----------
g <- ggraph_jk(data = spiraly, matrix_g = Wfull, color = "degree",cex_p = 3)
g + theme(legend.position = c(0.9,0.2),legend.background=element_blank())

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=7-----------
g <- ggraph_jk(data = spiraly, matrix_g = Wfull, color = "wdegree",cex_p = 3,matlab = T,color_lines = "black")
g + theme(legend.position = c(0.9,0.2),legend.background=element_blank())

