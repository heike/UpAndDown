## ----global options, echo=FALSE-----------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)

## ----fig.width=7, fig.height=6, fig.align='center'----------------------------
library(UpAndDownPlots)
library(tidyverse)
kk <- ud_prep(NIpop, v1="y2011", v2="y2017", levs=c("LGD2014_name", "area_name"), sortLev=c("perc", "perc"))
k1 <- ud_plot(kk, labelvar="LGD2014_name")
k1$uadl

## ----fig.width=7, fig.height=6, fig.align='center'----------------------------
km <- ud_prep(NIpop, v1="y2011", v2="y2017", levs=c("gender", "age", "LGD2014_name"), sortLev=c("perc", "orig", "perc"))
k2 <- ud_plot(km, labelvar="age")
k2$uadl

## ----fig.width=7, fig.height=8, fig.align='center'----------------------------
kp <- ud_prep(NIpop, v1="y2011", v2="y2017", levs=c("age", "LGD2014_name"), sortLev=c("orig", "perc"))
k3 <- ud_plot(kp, labelvar="LGD2014_name")
k3$uadl

## ----fig.width=7, fig.height=8, fig.align='center'----------------------------
kq <- ud_prep(NIpop, v1="y2011", v2="y2017", levs=c("gender", "LGD2014_name", "area_name"), sortLev=c("orig", "perc", "perc"))
k4 <- ud_plot(kq, labelvar="LGD2014_name")
k4$uadl

