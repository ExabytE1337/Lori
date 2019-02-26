## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(dev='svg')

## ----results='hide', message=FALSE, warning=FALSE------------------------
library(Lori)
library(ggplot2)
library(tidyverse)
library(e1071)
library(kernlab)
library(gridExtra)

## ------------------------------------------------------------------------
# lori_pal()
p1 <- lori_pal(1) %>% plot_colours()
p2 <- lori_pal(2) %>% plot_colours()
p3 <- lori_pal(3) %>% plot_colours()
grid.arrange(p1, p2, p3, nrow = 1)

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=8-----------
data(spiraly)
data(Wfull)
ggraph_jk(data = spiraly,matrix_g = Wfull,colormap = colormap::colormap(colormap = "hot", n = 256),cex_p = 1.5)

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=7-----------
g <- ggraph_jk(data = spiraly, matrix_g = Wfull, color = "degree",cex_p = 3)
g + ggplot2::theme(legend.position = c(0.9,0.2),legend.background=element_blank())

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=7-----------
g <- ggraph_jk(data = spiraly, matrix_g = Wfull, color = "wdegree",cex_p = 3,matlab = T,color_lines = "black")
g + ggplot2::theme(legend.position = c(0.9,0.2),legend.background=element_blank())

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=7-----------
data(df)
model <- svm(label ~ x1 + x2, data = df, kernel = "radial", gamma = 1, scale = F, cost = 1.8)
plot_svm_jk(df,model, title = "SVM - Radial Kernel in Feature Space")

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=7-----------
model_k <- ksvm(label ~ x2 + x1, data =df,scaled = F, C = 1.8, kpar = list(sigma = 1)) 
plot(model_k, data = df)

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=7-----------
#plot_svm_jk(df,model_k,fill_plot = T, bins =20)

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=8-----------
#plot_svm_jk(df,model,surface_plot = T, theta = 300,colormap = viridisLite::viridis(64,option = "A"))

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=8-----------
data(ari)
persp_jk(z = ari)

## ----message=FALSE, warning=FALSE, fig.height = 6, fig.width=8-----------
data(parula64)
persp_jk(z = ari,colormap = parula64,xlab = "t", ylab= "K neighbors",zlab = "Adjusted Rand Index",legend=F)

