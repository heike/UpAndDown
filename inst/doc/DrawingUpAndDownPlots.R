## ----global options, echo=FALSE-----------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)

## ----fig.width=7, fig.height=4, fig.align='center'----------------------------
library(UpAndDownPlots)
library(tidyverse)
yy <- ud_prep(CPIuk, weight="Weight", v1="Aug2017", v2="Aug2018", levs=c("Sector"), sortLev=c("orig"))
y1 <- ud_plot(yy, labelvar="Sector")
y1$uad

## ----fig.width=7, fig.height=6, fig.align='center'----------------------------
yw <- ud_prep(CPIuk, weight="Weight", v1="Aug2017", v2="Aug2018", levs=c("Sector"), sortLev=c("perc"))
y2 <- ud_plot(yw, labelvar="Sector")
y2$uadl

## ----fig.width=7, fig.height=6, fig.align='center'----------------------------
yz <- ud_prep(CPIuk, weight="Weight", v1="Aug2017", v2="Aug2018", levs=c("Sector", "Subsector"), sortLev=c("perc", "perc"))
y3 <- ud_plot(yz, labelvar="Sector")
y3$uadl

