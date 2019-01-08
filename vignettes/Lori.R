## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(dev='svg')

## ----results='hide', message=FALSE, warning=FALSE------------------------
library(Lori)

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=8-----------
data(spiraly)
data(Wfull)
ggraph_jk(data = spiraly,matrix_g = Wfull,colormap = colormap::colormap(colormap = "hot", n = 256),cex_p = 1.5)

