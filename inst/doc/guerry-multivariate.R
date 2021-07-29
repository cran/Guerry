## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  comment = "#>",
  fig.height = 5,
  fig.width = 5
)

## ----load---------------------------------------------------------------------
library(Guerry)
library(car)
library(ggplot2)
library(ggrepel)
data(Guerry)

## ----guerry-more-vars---------------------------------------------------------
names(Guerry)[-(1:9)]

## ----load2--------------------------------------------------------------------
library(Guerry)
library(car)
data(Guerry)

## ----lit-pers-scat0-----------------------------------------------------------
ggplot(aes(x=Literacy, y=Crime_pers/1000), data=Guerry) +
  geom_point(size=2) 


## ----lit-pers-scat------------------------------------------------------------
gdf <- Guerry[, c("Literacy", "Crime_pers", "Department")]
gdf$dsq <- mahalanobis(gdf[,1:2], colMeans(gdf[,1:2]), cov(gdf[,1:2]))

ggplot(aes(x=Literacy, y=Crime_pers/1000, label=Department), data=gdf) +
  geom_point(size=2) +
  stat_ellipse(level=0.68, color="blue", size=1.2) +  
  stat_ellipse(level=0.95, color="gray", size=1, linetype=2) + 
  geom_smooth(method="lm", formula=y~x, fill="lightblue") +
  geom_smooth(method="loess", formula=y~x, color="red", se=FALSE) +
  geom_label_repel(data = gdf[gdf$dsq > 4.6,]) +
  theme_bw()

## ----lit-prop-scat------------------------------------------------------------
gdf <- Guerry[, c("Literacy", "Crime_prop", "Department")]
gdf$dsq <- mahalanobis(gdf[,1:2], colMeans(gdf[,1:2]), cov(gdf[,1:2]))

ggplot(aes(x=Literacy, y=Crime_prop/1000, label=Department), data=gdf) +
  geom_point(size=2) +
  stat_ellipse(level=0.68, color="blue", size=1.2) +  
  stat_ellipse(level=0.95, color="gray", size=1, linetype=2) + 
  geom_smooth(method="lm", formula=y~x, fill="lightblue") +
  geom_smooth(method="loess", formula=y~x, color="red", se=FALSE) +
  geom_label_repel(data = gdf[gdf$dsq > 4.6,]) +
  theme_bw()

