# quiets concerns of R CMD check
if(getRversion() >= "2.15.1")  utils::globalVariables(c("base", "c1F",
                                                        "c1base", "c1perc", "c1s", "c1v1", "c1v2", "c2F", "c2base", "c2final", "c2perc", "c2s",
                                                        "c2v1", "c2v2", "c3F", "c3s", "c3v1", "c3v2", "cumX", "mx", "mc1base",
                                                        "mc2base", "mc2perc", "mc3base", "mc3perc", "n2ID", "n3ID", "nID", "perc",
                                                        "totp", "v1", "v2", "wt", "lgv", ".sv_", "INX", "bv", "sv1", "sv2"))

# Main function--------------

dgroup <- function(data, byvars, weight=1, v1, v2) {

  GD <- data.frame(data)

  # Check value variables
  if (!(v1 %in% names(GD)))
    stop(v1, " is not in the dataset", call.=FALSE)
  if (!(v2 %in% names(GD)))
    stop(v2, " is not in the dataset", call.=FALSE)

  GD$v1 <- GD[[v1]]
  GD$v2 <- GD[[v2]]

  # Check byvars
  lb <- length(byvars)
  bb <- intersect(names(GD), byvars)
  if(!setequal(bb, byvars))
    stop("grouping variables not in dataset: ", paste(setdiff(byvars, bb), collapse = ", "))


  gd <- list()
  gw <- list()
  vmin <- vector()
  vmax <- vector()
  vlmin <- vector()
  vlmax <- vector()

  for (i in 1:lb) {
    GD$bv <- GD[[byvars[i]]]
    dx <- GD %>% group_by(bv) %>% summarise(sv1=sum(v1), sv2=sum(v2)) %>% mutate(perc=100*(sv2-sv1)/sv1)
    v1 <- "sv1"
    v2 <- "sv2"
    gd[[i]] <- ud_prep(dx, weight=weight, v1=v1, v2=v2, levs=c("bv"), sortLev=c("orig"))
    gw[[i]] <- ud_plot(gd[[i]], labelvar="bv")
    vmin[i] <- min(dx$perc)
    vmax[i] <- max(dx$perc)
    vlmin[i] <- layer_scales(gw[[i]]$uadl)$y$range$range[1]
    vlmax[i] <- layer_scales(gw[[i]]$uadl)$y$range$range[2]
  }

  vvmin <- floor(min(vmin))
  vvmax <- ceiling(max(vmax))
  vlvmin <- floor(min(vlmin))
  vlvmax <- ceiling(max(vlmax))

  gp <- list()
  gpl <- list()
  for (i in 1:lb) {
    gp[[i]] <- gw[[i]]$uad  + ylim(vvmin, vvmax) + ggtitle(byvars[i]) + theme(plot.title = element_text(hjust = 0.5))
    gpl[[i]] <- gw[[i]]$uadl + ylim(vlvmin, vlvmax) + ggtitle(byvars[i]) + theme(plot.title = element_text(hjust = 0.5))
  }


  a5 <- gridExtra::arrangeGrob(grobs=gp, nrow=ceiling(lb/2))
  b5 <- gridExtra::arrangeGrob(grobs=gpl, nrow=ceiling(lb/2))

  return(list(uadg=a5, uadgl=b5))
}
