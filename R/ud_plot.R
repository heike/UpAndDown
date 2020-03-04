# quiets concerns of R CMD check
if(getRversion() >= "2.15.1")  utils::globalVariables(c("base", "c1F",
                                                        "c1base", "c1perc", "c1s", "c1v1", "c1v2", "c2F", "c2base", "c2final", "c2perc", "c2s",
                                                        "c2v1", "c2v2", "c3F", "c3s", "c3v1", "c3v2", "cumX", "mx", "mc1base",
                                                        "mc2base", "mc2perc", "mc3base", "mc3perc", "n2ID", "n3ID", "nID", "perc",
                                                        "totp", "v1", "v2", "wt", "lgv", ".sv_", "INX", "bv", "sv1", "sv2"))

# Main function--------------

ud_plot <- function(outPrep, b=0, totperc="yes", vscale=NULL, labelvar=NULL,
                    levelColour="none", barColour=levelColour, ud_control=ud_colours()) {

  INX <- outPrep$data
  levs <- outPrep$levs
  lc <- length(levs)
  hx <- outPrep$hx
  lgv <- outPrep$lgv

  # Check totperc
  if (!(totperc %in% c("yes", "no")))
    stop("totperc must be either yes or no", call.=FALSE)

  # Check vertical axis limits
  if(!(length(vscale) %in% c(0,2)))
    stop("vscale should have two limits or none", call.=FALSE)

  lv <- length(labelvar)
  if (lv > 0) {
    if (lv > 1)
      stop("There should be at most one label variable.", call.=FALSE)
    if ((labelvar==levs[1])|(lc > 1 & labelvar==levs[2])) {
      INX$labelvar <- INX[[labelvar]]
    } else {
      stop("labelling variable not one of the top two chosen levels")
    }
  }

  # Set the requested colours
  cy <- ud_control$colours
  gcpal <- ud_control$gcpal

  # Check colour parameters
  if(!(levelColour %in% c(levs, "none"))) warning("levelColour name is not a level variable and will have no effect", call.=FALSE)

  glev <- case_when(
      levelColour==levs[1] ~ 1,
      levelColour==levs[2] ~ 2,
      levelColour==levs[3] ~ 3,
      TRUE ~ 0
    )

  # Check barColour variable and that there are enough colours in the palette for barColour
  if(!(barColour %in% c(names(INX), "none"))) stop("barColour name not in the dataset", call.=FALSE)
  if (!(barColour=="none")) {
      INX$barColour <- INX[[barColour]]
    if (!(class(INX$barColour) %in% c("character", "factor"))) {
      stop("barColour should be of class character or factor", call. = FALSE)
      }
      lgC <- length(unique(INX$barColour))
           if (lgC > length(gcpal)) {
        set.seed(4711)
        #gcpal2 <- randomcoloR::distinctColorPalette(lgC)
        gcpal2 <- sample(colorspace::qualitative_hcl(n = lgC, l=80))
      } else {
      gcpal2 <- gcpal
      }
    }

 # Check, if barColour is not a grouping variable, whether it can be used
    if(glev > 0) {
    txy <- table(INX[, c(levs[glev], "barColour")])
    txyd <- data.frame(txy)
 # You want only 1 entry in each row of the table
    if(sum(txyd$Freq>0) > dim(txy)[1]) {
    stop(" the barColour variable cannot be used for colouring the levelColour level", call. = FALSE)
    }
    }


  # Calculate the cumulatives for the three levels (c1F, c2F, c3F)
  if (!(glev==1)) INX1 <- INX %>% mutate(totp=100*(sum(wt*v2)/sum(wt*v1)-1)) %>% group_by(c1F) %>%
    summarise(base=sum(wt*v1), perc=100*(sum(wt*v2)/base-1), totp=mean(totp)) %>% ungroup() %>%
    mutate(cumX=cumsum(base))
  if (glev==1) INX1 <- INX %>% mutate(totp=100*(sum(wt*v2)/sum(wt*v1)-1)) %>% group_by(c1F) %>%
    summarise(base=sum(wt*v1), perc=100*(sum(wt*v2)/base-1), totp=mean(totp), barColour=unique(barColour)) %>% ungroup() %>%
    mutate(cumX=cumsum(base))
    low1 <- min(INX1$perc)
    high1 <- max(INX1$perc)
  if (lc > 1) {
    if (!(glev==2)) INX2 <- INX %>% group_by(c1F, c2F) %>% summarise(mc2base=sum(wt*v1), mc2perc=100*(sum(wt*v2)/mc2base-1)) %>%
      ungroup() %>% mutate(c2s=cumsum(mc2base))
     if (glev==2) INX2 <- INX %>% group_by(c1F, c2F) %>% summarise(mc2base=sum(wt*v1), mc2perc=100*(sum(wt*v2)/mc2base-1), barColour=unique(barColour)) %>%
      ungroup() %>% mutate(c2s=cumsum(mc2base))
    low2 <- min(INX2$mc2perc)
    high2 <- max(INX2$mc2perc)
  }
  if (lc==3) {
    if (!(glev==3)) INX3 <- INX %>% group_by(c1F, c2F, c3F) %>% summarise(mc3base=sum(wt*v1), mc3perc=100*(sum(wt*v2)/mc3base-1)) %>%
      ungroup() %>% mutate(c3s=cumsum(mc3base))
        if (glev==3) INX3 <- INX %>% group_by(c1F, c2F, c3F) %>% summarise(mc3base=sum(wt*v1), mc3perc=100*(sum(wt*v2)/mc3base-1), barColour=unique(barColour)) %>%
      ungroup() %>% mutate(c3s=cumsum(mc3base))
    low3 <- min(INX3$mc3perc)
    high3 <- max(INX3$mc3perc)
  }

  # Plot
  # If you want a legend   + theme(legend.title=element_blank(), legend.position="bottom")
  h1 <- ggplot(INX1, aes(xmin=lag(cumX, default=0), xmax=cumX, ymin=b, ymax=perc))
  if (glev==1) {
  h1 <- h1 + geom_rect(aes(fill=barColour), col=cy[2]) + theme(legend.title=element_blank(), legend.position="bottom") + scale_fill_manual(values=gcpal2)
      }
  if (!(glev==1)) {
  h1 <- h1 + geom_rect(fill=cy[1], col=cy[2])
  }
  if (length(vscale)==2) h1 <- h1  + coord_cartesian(ylim=c(vscale[1], vscale[2]))
  if (totperc=="yes") h4 <- h1 + geom_hline(yintercept=INX1$totp, linetype="dashed", col=cy[3])
  if (totperc=="no") h4 <- h1
  if (lc==2) {
    if (glev==2) h2 <- h1 + geom_rect(data=INX2, aes(xmin=lag(c2s, default=0), xmax=c2s, ymin=b, ymax=mc2perc, fill=barColour), col=cy[4]) + scale_fill_manual(values=gcpal2) + theme(legend.title=element_blank(), legend.position="bottom")
    if (!(glev==2)) h2 <- h1 + geom_rect(data=INX2, aes(xmin=lag(c2s, default=0), xmax=c2s, ymin=b, ymax=mc2perc), alpha=0, col=cy[4])
    if (totperc=="yes") h4 <- h2 + geom_hline(yintercept=INX1$totp, linetype="dashed", col=cy[3])
    if (totperc=="no") h4 <- h2
  }
  if (lc==3) {
    if (glev==3) h2 <- h1 + geom_rect(data=INX3, aes(xmin=lag(c3s, default=0), xmax=c3s, ymin=b, ymax=mc3perc, fill=barColour), col=cy[5]) + scale_fill_manual(values=gcpal2) + theme(legend.title=element_blank(), legend.position="bottom")
    if (!(glev==3)) h2 <- h1 + geom_rect(data=INX3, aes(xmin=lag(c3s, default=0), xmax=c3s, ymin=b, ymax=mc3perc), alpha=0, col=cy[5])
    if (glev==2) h3 <- h2 + geom_rect(data=INX2, aes(xmin=lag(c2s, default=0), xmax=c2s, ymin=b, ymax=mc2perc, fill=barColour), col=cy[4]) + scale_fill_manual(values=gcpal2) + theme(legend.title=element_blank(), legend.position="bottom")
    if (!(glev==2)) h3 <- h2 + geom_rect(data=INX2, aes(xmin=lag(c2s, default=0), xmax=c2s, ymin=b, ymax=mc2perc), alpha=0, col=cy[4])
    if (totperc=="yes") h4 <- h3 + geom_hline(yintercept=INX1$totp, linetype="dashed", col=cy[3])
    if (totperc=="no") h4 <- h3
  }
  h5 <- h4 + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())

  # With labels
  if (lv > 0) {
    # Max label lengths based on the label variable chosen
    INXlab <- INX %>% summarise(mx=max(nchar(as.character(labelvar))))

    # Axis limits to make space for labels (note condition vmin >= 0 to ensure that labels do not start above b)
    if (lc==1) {
      lowA <- low1
      highA <- high1
    }
    if (lc==2) {
      lowA <- min(low1, low2)
      highA <- max(high1, high2)
    }
    if (lc==3) {
      lowA <- min(low1, low2, low3)
      highA <- max(high1, high2, high3)
    }
    if (length(vscale)==2) {
      lowA <- max(vscale[1], lowA)
      highA <- min(vscale[2], highA)
    }
    vmin <- round(lowA-INXlab$mx/2, -1)
    if (vmin >= min(lowA, b)) vmin <- min(vmin, b) - 10
    vmax <- round(2*(highA+2.5),-1)/2

    # Prepare labelled plot
    if (lc==1) {
      h5l <- h4 + ylim(vmin, vmax) + geom_text(data=INX1, aes(x = 0.5*(lag(cumX, default=0) + cumX), y = vmin, label = c1F), size=3, hjust=0, check_overlap=TRUE, inherit.aes=FALSE) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + coord_flip()
    }
    if (lc > 1) {
      if ((lc==2 & labelvar==levs[1])|(lc==3 & labelvar==levs[1])) {
        h5l <- h4 + ylim(vmin, vmax) + geom_text(data=INX1, aes(x = 0.5*(lag(cumX, default=0) + cumX), y = vmin, label = c1F), size=3, hjust=0, check_overlap=TRUE, inherit.aes=FALSE) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + coord_flip()
      }
      if (lc > 1 & labelvar==levs[2]) {
        h5l <- h4 + ylim(vmin, vmax) + geom_text(data=INX2, aes(x = 0.5*(lag(c2s, default=0) + c2s), y = vmin, label = c2F), size=3, hjust=0, check_overlap=TRUE, inherit.aes=FALSE) + theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + coord_flip()
      }
    }
  }

  # return plots and percentages
  Perc1 <- INX1 %>% select(c1F, perc)
  names(Perc1) <- c(levs[1], "percCh")
  if (lc > 1) {
    Perc2 <- INX2 %>% select(c1F, c2F, mc2perc)
    names(Perc2) <- c(levs[1], levs[2], "percCh")
  }
  if (lc > 2) {
    Perc3 <- INX3 %>% select(c1F, c2F, c3F, mc3perc)
    names(Perc3) <- c(levs[1], levs[2], levs[3], "percCh")
  }
  TotPerc <- INX1$totp[1]
  if (lv > 0) {
    if (lc==1) return(list(uad=h5, uadl=h5l, TotPerc=TotPerc, level1=Perc1))
    if (lc==2) return(list(uad=h5, uadl=h5l, TotPerc=TotPerc, level1=Perc1, level2=Perc2))
    if (lc==3) return(list(uad=h5, uadl=h5l, TotPerc=TotPerc, level1=Perc1, level2=Perc2, level3=Perc3))
  }
  if (lv==0) {
    if (lc==1) return(list(uad=h5, TotPerc=TotPerc, level1=Perc1))
    if (lc==2) return(list(uad=h5, TotPerc=TotPerc, level1=Perc1, level2=Perc2))
    if (lc==3) return(list(uad=h5, TotPerc=TotPerc, level1=Perc1, level2=Perc2, level3=Perc3))
  }
}
