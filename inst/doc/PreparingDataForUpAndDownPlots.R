## ----global options, echo=FALSE-----------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)

## ----fig.width=7, fig.height=8, fig.align='center'----------------------------
library(UpAndDownPlots)
library(tidyverse)
yy <- ud_prep(AutoSales, v1="sales17", v2="sales18", levs=c("Segment"), sortLev=c("perc"))
y1 <- ud_plot(yy, labelvar="Segment")
y1$uadl

## ----eval=FALSE---------------------------------------------------------------
#  yv <- ud_prep(AutoSales, v1="sales17", v2="sales18", levs=c("Manufacturer"), sortLev=c("perc"))

## -----------------------------------------------------------------------------
AutoSales0 <- AutoSales %>% group_by(Manufacturer) %>% summarise(Sales17=sum(sales17)) %>% filter(Sales17==0)
AutoSales0

## ----fig.width=7, fig.height=8, fig.align='center'----------------------------
yv <- ud_prep(AutoSales %>% filter(!(Manufacturer=="ALPINE")), v1="sales17", v2="sales18", levs=c("Manufacturer"), sortLev=c("base"))
y2 <- ud_plot(yv, labelvar="Manufacturer", vscale=c(-20, 20))
y2$uadl

## ----fig.width=7, fig.height=8, fig.align='center'----------------------------
yw <- ud_prep(AutoSalesX, v1="sales17", v2="sales18", levs=c("Segment", "Manufacturer"), sortLev=c("perc", "perc"))
y3 <- ud_plot(yw, labelvar="Segment")
y3$uadl

## -----------------------------------------------------------------------------
arrange(y3$level2, desc(percCh)) %>% top_n(10)

## ----fig.width=7, fig.height=8, fig.align='center'----------------------------
yz <- ud_prep(AutoSalesX %>% filter(Segment %in% c("Off-road", "SUV")), v1="sales17", v2="sales18", levs=c("Manufacturer"), sortLev=c("base"))
y4 <- ud_plot(yz, labelvar="Manufacturer")
y4$uadl

## ----fig.width=7, fig.height=8, fig.align='center'----------------------------
yx <- ud_prep(AutoSalesX %>% filter(Segment=="Compact"), v1="sales17", v2="sales18", levs=c("Manufacturer"), sortLev=c("base"))
y5 <- ud_plot(yx, labelvar="Manufacturer")
y5$uadl

