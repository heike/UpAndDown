## ----global options, echo=FALSE-----------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)

## ----fig.width=7, fig.height=7, fig.align='center'----------------------------
library(UpAndDownPlots)
library(tidyverse)
xs <- sort4(NIpop, gvar="LGD2014_name", v1="y2011", v2="y2017")
gridExtra::grid.arrange(xs$uadl)

## ----fig.width=7, fig.height=7, fig.align='center'----------------------------
NIpopX <- NIpop %>% filter(age=="65+") %>% group_by(LGD2014_name) %>% summarise(s11=sum(y2011), s17=sum(y2017), rp=s17/s11-1)
NIpopX <- NIpopX %>% mutate(LGD=fct_reorder(LGD2014_name, rp))
NIpop1 <- NIpop %>% mutate(LGD2014_name=factor(LGD2014_name, levels=levels(NIpopX$LGD)))
ww <- ud_prep(NIpop1, v1="y2011", v2="y2017", levs=c("age", "LGD2014_name"), sortLev=c("orig", "orig"))
w1 <- ud_plot(ww, labelvar="age")
w1$uad

## ----eval=FALSE---------------------------------------------------------------
#  Data1 <- Data1 %>% mutate(Age=factor(Age, levels=c("child", "young", "middle-aged", "old")))

