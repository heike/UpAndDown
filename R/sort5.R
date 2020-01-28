# quiets concerns of R CMD check
if(getRversion() >= "2.15.1")  utils::globalVariables(c("base", "c1F",
                                                        "c1base", "c1perc", "c1s", "c1v1", "c1v2", "c2F", "c2base", "c2final", "c2perc", "c2s",
                                                        "c2v1", "c2v2", "c3F", "c3s", "c3v1", "c3v2", "cumX", "mx", "mc1base",
                                                        "mc2base", "mc2perc", "mc3base", "mc3perc", "n2ID", "n3ID", "nID", "perc",
                                                        "totp", "v1", "v2", "wt", "lgv", ".sv_", "INX", "bv", "sv1", "sv2"))

# Main function--------------

sort5 <- function(data, gvar, weight=1, v1, v2) {

  FD <- data.frame(data)

  sL <- c("orig", "base", "final", "perc", "abs")
  nL <- c("Original order", "Base", "Final", "Percentage change", "Absolute change")
  xd <- list()
  xw <- list()
  xp <- list()
  xpl <- list()
  for (i in 1:5) {
    xd[[i]] <- ud_prep(FD, weight=weight, v1=v1, v2=v2, levs=c(gvar), sortLev=sL[i])
    xw[[i]] <- ud_plot(xd[[i]], labelvar=gvar)
    xp[[i]] <- xw[[i]]$uad + ggtitle(nL[i]) + theme(plot.title = element_text(hjust = 0.5))
    xpl[[i]] <- xw[[i]]$uadl + ggtitle(nL[i]) + theme(plot.title = element_text(hjust = 0.5))
  }

  a5 <- gridExtra::arrangeGrob(xp[[1]], xp[[2]], xp[[3]], xp[[4]], xp[[5]], nrow=2)

  b5 <- gridExtra::arrangeGrob(xpl[[1]], xpl[[2]], xpl[[3]], xpl[[4]], xpl[[5]], nrow=2)


  return(list(uad=a5, uadl=b5))
}
