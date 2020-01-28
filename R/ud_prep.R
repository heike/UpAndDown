# quiets concerns of R CMD check
if(getRversion() >= "2.15.1")  utils::globalVariables(c("base", "c1F",
                                                        "c1base", "c1perc", "c1s", "c1v1", "c1v2", "c2F", "c2base", "c2final", "c2perc", "c2s",
                                                        "c2v1", "c2v2", "c3F", "c3s", "c3v1", "c3v2", "cumX", "mx", "mc1base",
                                                        "mc2base", "mc2perc", "mc3base", "mc3perc", "n2ID", "n3ID", "nID", "perc",
                                                        "totp", "v1", "v2", "wt", "lgv", ".sv_", "INX", "bv", "sv1", "sv2"))

# Main function--------------

ud_prep <- function(data, weight=1, v1, v2, levs, sortLev, reverse=c(FALSE, FALSE, FALSE)) {

  IND <- data.frame(data) #in case the dataset is a tibble
  IND <- droplevels(IND) #in case a grouping variable has unused levels (gives an error in ud_prep)

  # Check input parameters are within allowed limits
  # Check weight
  if (weight==1) {
    IND$wt <- 1
  } else {
    if (!(weight %in% names(IND)))
      stop(weight, " is not in the dataset")
    IND$wt <- IND[[weight]]
  }

  if (min(IND$wt) <= 0)
    stop("weights should be positive")

  # Check value variables
  if (!(v1 %in% names(IND)))
    stop(v1, " is not in the dataset", call.=FALSE)
  if (!(v2 %in% names(IND)))
    stop(v2, " is not in the dataset", call.=FALSE)

  IND$v1 <- IND[[v1]]
  IND$v2 <- IND[[v2]]

  if (min(IND$v1) < 0)
    stop("initial values should be non-negative", call.=FALSE)
  if (min(IND$v2) < 0)
    stop("final values should be non-negative", call.=FALSE)

  # Check grouping variables
  # There should be from one to three variables to define the plot levels
  # They should be character or factor
  lc <- length(levs)
  nn <- intersect(names(IND), levs)
  if (!setequal(nn, levs))
    stop("grouping variables not in dataset: ", paste(setdiff(levs, nn), collapse = ", "))
  lcu <- length(unique(levs))
  if (lcu < lc)
    stop("a grouping variable is repeated", call.=FALSE)
  for (i in 1:lc) {
    if (!(class(IND[[levs[i]]]) %in% c("character", "factor")))
      stop("grouping variables should have class character or factor", call. = FALSE)
  }

  # Check orderings
  lo <- length(sortLev)
  if (lo < lc)
    stop("Not enough sortings have been specified.  There should be as many as the number of grouping variables.", call.=FALSE)
  if (lo > lc)
    stop("Too many sortings have been specified.  There should be as many as the number of grouping variables.", call.=FALSE)
  sortings <- c("orig", "base", "final", "perc", "abs")
  ox <- intersect(sortings, sortLev)
  if (!setequal(ox, sortLev))
    stop("unavailable sorting(s) requested: ", paste(setdiff(sortLev, ox), collapse = ", "), call.=FALSE)
  
  # Check reverse (of orderings)
  lr <- length(reverse)
  if (lr < lo)
    stop("Not enough reverse parameters have been specified.  There should be as many as the number of sortings.", call.=FALSE)
  if (!(is.logical(reverse)))
    stop("reverse parameters must be either TRUE or FALSE", call.=FALSE)
 
  # Set up grouping variables as factors and check nos of unique values in each level
  # Use unique so that it applies to all levels (ie numbers of categories at higher levels)
  namesGf <- paste0("c", 1:lc, "F")
  lgv <- vector(length=lc)
  for (i in 1:lc) {
    IND[, namesGf[i]] <- as_factor(IND[[levs[i]]])
    lgv[i] <- length(unique(IND[, namesGf[i]]))
  }

  # Check for zero initial values in lowest level of specified subgroups
  if (lc==1) INDch <- IND %>% group_by(c1F) %>% summarise(sv1=sum(v1))
  if (lc==2) INDch <- IND %>% group_by(c1F, c2F) %>% summarise(sv1=sum(v1))
  if (lc==3) INDch <- IND %>% group_by(c1F, c2F, c3F) %>% summarise(sv1=sum(v1))
  if (!(min(INDch$sv1) > 0))
    stop("one or more initial values are zero", call.=FALSE)


  INX <- IND

  # Single level
  if (lc==1) {
    INX <- INX %>% group_by(c1F) %>%  mutate(nID=as.integer(c1F), c1v1=sum(wt*v1),
                                             c1v2=sum(wt*v2), c1base=c1v1, c1final=c1v2, c1orig=mean(nID), c1abs=c1v2-c1v1, c1perc=100*(c1v2/c1v1-1)) %>% ungroup()
    INX <- fsort(INX, sortLev[1], reverse[1])
    return(list(levs=levs, sortLev=sortLev, reverse=reverse, hx=NULL, lgv=lgv, data = INX))
  }

  # More than one level (lc must be > 1 due to the previous chunk)
  # Check nesting
  hx <- Nest(IND[, levs])
  hxS <- sum(hx$NestL)
  Nesting <- case_when(
    hxS == 0 ~ "noneNested",
    lc > 2 & hxS == 1 ~ "oneNesting",
    lc > 2 & hxS == 2 ~ "doubleNesting",
    TRUE ~ "allNested"
  )

  # If all nested
  if (Nesting=="allNested") {

    # Ordering of grouping variable levels
    if ((lgv[1] > lgv[2]) |(lc > 2 & lgv[2] > lgv[3]))
      stop("check the ordering of your grouping variables")

    # Two levels
    if (lc==2) {
      # Calculate statistics for the two levels, higher then lower, and sort
      INX <- INX %>% group_by(c1F) %>% mutate(nID=as.integer(c1F), c1v1=sum(wt*v1),
                                              c1v2=sum(wt*v2), c1base=c1v1, c1final=c1v2, c1orig=mean(nID), c1abs=c1v2-c1v1, c1perc=100*(c1v2/c1v1-1)) %>% ungroup()
      INX <- INX %>% group_by(c1F, c2F) %>% mutate(n2ID=as.integer(c2F), c2v1=sum(wt*v1),
                                                   c2v2=sum(wt*v2), c2orig=mean(n2ID), c2base=c2v1, c2final=c2v2, c2abs=c2v2-c2v1, c2perc=100*(c2v2/c2v1-1)) %>% ungroup()

      # Order by top level and then by lower level
      INX <- fsort(INX, sortLev[1], reverse[1])
      INX <- fsort2(INX, sortLev[2], reverse[2])
      return(list(levs=levs, sortLev=sortLev, reverse=reverse, hx=hx, lgv=lgv, data = INX))
    }

    # Three levels
    if (lc==3) {
      # Calculate statistics for the three levels in turn and order accordingly
      INX <- INX %>% group_by(c1F) %>% mutate(nID=as.integer(c1F), c1v1=sum(wt*v1),
                                              c1v2=sum(wt*v2), c1base=c1v1, c1final=c1v2, c1orig=mean(nID), c1abs=c1v2-c1v1, c1perc=100*(c1v2/c1v1-1)) %>% ungroup()
      INX <- fsort(INX, sortLev[1], reverse[1])
      INX <- INX %>% group_by(c1F, c2F) %>% mutate(n2ID=as.integer(c2F), c2v1=sum(wt*v1),
                                                   c2v2=sum(wt*v2), c2orig=mean(n2ID), c2base=c2v1, c2final=c2v2, c2abs=c2v2-c2v1, c2perc=100*(c2v2/c2v1-1)) %>% ungroup()
      INX <- fsort2(INX, sortLev[2], reverse[2])
      INX <- INX %>% group_by(c1F, c2F, c3F) %>% mutate(n3ID=as.integer(c2F), c3v1=sum(wt*v1),
                                                        c3v2=sum(wt*v2), c3orig=mean(n3ID), c3base=c3v1, c3final=c3v2, c3abs=c3v2-c3v1, c3perc=100*(c3v2/c3v1-1)) %>% ungroup()
      INX <- fsort3(INX, sortLev[3], reverse[3])

      return(list(levs=levs, sortLev=sortLev, reverse=reverse, hx=hx, lgv=lgv, data = INX))
    }
  }

  # If none nested
  if (Nesting=="noneNested"){
    # Sorting of higher levels
    # Two levels
    if (lc==2) {
      # Calculate statistics for higher level and lower level
      INX <- INX %>% group_by(c1F) %>% mutate(nID=as.integer(c1F), c1v1=sum(wt*v1), c1v2=sum(wt*v2),
                                              c1orig=mean(nID), c1base=c1v1, c1final=c1v2, c1abs=c1v2-c1v1, c1perc=100*(c1v2/c1v1-1)) %>% ungroup()
      INX <- INX %>% group_by(c2F) %>% mutate(n2ID=as.integer(c2F), c2v1=sum(wt*v1), c2v2=sum(wt*v2),
                                              c2orig=mean(n2ID), c2base=c2v1, c2final=c2v2, c2abs=c2v2-c2v1, c2perc=100*(c2v2/c2v1-1)) %>% ungroup()

      # Order by top level and then by lower level
      INX <- fsort(INX, sortLev[1], reverse[1])
      INX <- fsort2(INX, sortLev[2], reverse[2])

      return(list(levs=levs, sortLev=sortLev, reverse=reverse, hx=hx, lgv=lgv, data = INX))
    }

    # Three levels
    if (lc==3) {
      # Calculate statistics for first and second levels
      INX <- INX %>% group_by(c1F) %>% mutate(nID=as.integer(c1F), c1v1=sum(wt*v1), c1v2=sum(wt*v2),
                                              c1orig=mean(nID), c1base=c1v1, c1final=c1v2, c1abs=c1v2-c1v1, c1perc=100*(c1v2/c1v1-1)) %>% ungroup()
      INX <- INX %>% group_by(c2F) %>% mutate(n2ID=as.integer(c2F), c2v1=sum(wt*v1), c2v2=sum(wt*v2),
                                              c2orig=mean(n2ID), c2base=c2v1, c2final=c2v2, c2abs=c2v2-c2v1, c2perc=100*(c2v2/c2v1-1)) %>% ungroup()
      INX <- INX %>% group_by(c3F) %>% mutate(n3ID=as.integer(c3F), c3v1=sum(wt*v1), c3v2=sum(wt*v2),
                                              c3orig=mean(n3ID), c3base=c3v1, c3final=c3v2, c3abs=c3v2-c3v1, c3perc=100*(c3v2/c3v1-1)) %>% ungroup()

      # Order by top level and then by lower level
      INX <- fsort(INX, sortLev[1], reverse[1])
      INX <- fsort2(INX, sortLev[2], reverse[2])
      INX <- fsort3(INX, sortLev[3], reverse[3])

      return(list(levs=levs, sortLev=sortLev, reverse=reverse, hx=hx, lgv=lgv, data = INX))
    }
  }

  # If there is one nesting
  if (Nesting=="oneNesting"){
    INX <- INX %>% group_by(c1F) %>% mutate(nID=as.integer(c1F), c1v1=sum(wt*v1), c1v2=sum(wt*v2),
                                            c1orig=mean(nID), c1base=c1v1, c1final=c1v2, c1abs=c1v2-c1v1, c1perc=100*(c1v2/c1v1-1)) %>% ungroup()
    if (hx$NestL[3]){
      INX <- INX %>% group_by(c2F) %>% mutate(n2ID=as.integer(c2F), c2v1=sum(wt*v1), c2v2=sum(wt*v2),
                                              c2orig=mean(n2ID), c2base=c2v1, c2final=c2v2, c2abs=c2v2-c2v1, c2perc=100*(c2v2/c2v1-1)) %>% ungroup()
      INX <- INX %>% group_by(c2F, c3F) %>% mutate(n3ID=as.integer(c2F), c3v1=sum(wt*v1),
                                                   c3v2=sum(wt*v2), c3orig=mean(n3ID), c3base=c3v1, c3final=c3v2, c3abs=c3v2-c3v1, c3perc=100*(c3v2/c3v1-1)) %>% ungroup()
    }
    if (hx$NestL[1]){
      INX <- INX %>% group_by(c1F, c2F) %>% mutate(n2ID=as.integer(c2F), c2v1=sum(wt*v1),
                                                   c2v2=sum(wt*v2), c2orig=mean(n2ID), c2base=c2v1, c2final=c2v2, c2abs=c2v2-c2v1, c2perc=100*(c2v2/c2v1-1)) %>% ungroup()
      INX <- INX %>% group_by(c3F) %>% mutate(n3ID=as.integer(c3F), c3v1=sum(wt*v1), c3v2=sum(wt*v2),
                                              c3orig=mean(n3ID), c3base=c3v1, c3final=c3v2, c3abs=c3v2-c3v1, c3perc=100*(c3v2/c3v1-1)) %>% ungroup()
    }
    INX <- fsort(INX, sortLev[1], reverse[1])
    INX <- fsort2(INX, sortLev[2], reverse[2])
    INX <- fsort3(INX, sortLev[3], reverse[3])

    return(list(levs=levs, sortLev=sortLev, reverse=reverse, hx=hx, lgv=lgv, data = INX))
  }

  # If there is double nesting
  if (Nesting=="doubleNesting"){

    # Check order of three grouping variables
    if (hx$NestL[1]) {
      stop("the nested variable must be the last one in levs", call.=FALSE)
    }

    INX <- INX %>% group_by(c1F) %>% mutate(nID=as.integer(c1F), c1v1=sum(wt*v1), c1v2=sum(wt*v2),
                                            c1orig=mean(nID), c1base=c1v1, c1final=c1v2, c1abs=c1v2-c1v1, c1perc=100*(c1v2/c1v1-1)) %>% ungroup()
    INX <- INX %>% group_by(c2F) %>% mutate(n2ID=as.integer(c2F), c2v1=sum(wt*v1), c2v2=sum(wt*v2),
                                            c2orig=mean(n2ID), c2base=c2v1, c2final=c2v2, c2abs=c2v2-c2v1, c2perc=100*(c2v2/c2v1-1)) %>% ungroup()
    INX <- INX %>% group_by(c2F, c3F) %>% mutate(n3ID=as.integer(c2F), c3v1=sum(wt*v1),
                                                 c3v2=sum(wt*v2), c3orig=mean(n3ID), c3base=c3v1, c3final=c3v2, c3abs=c3v2-c3v1, c3perc=100*(c3v2/c3v1-1)) %>% ungroup()

    INX <- fsort(INX, sortLev[1], reverse[1])
    INX <- fsort2(INX, sortLev[2], reverse[2])
    INX <- fsort3(INX, sortLev[3], reverse[3])

    return(list(levs=levs, sortLev=sortLev, reverse=reverse, hx=hx, lgv=lgv, data = INX))

  }
}

fsort <- function(data = INX, sortL, rev) {
  data$.sv_ <- data[[paste0("c1", sortL)]]
  if(rev) {
  data %>% arrange(desc(.sv_)) %>% select(-.sv_) %>% mutate(c1F = fct_inorder(c1F))
  } else {
  data %>% arrange(.sv_) %>% select(-.sv_) %>% mutate(c1F = fct_inorder(c1F))
  }
}

fsort2 <- function(data = INX, sortL, rev) {
  data$.sv_ <- data[[paste0("c2", sortL)]]
  if(rev) {
   data %>% arrange(c1F, desc(.sv_)) %>% select(-.sv_) %>% mutate(c2F = fct_inorder(c2F))
  } else {
  data %>% arrange(c1F, .sv_) %>% select(-.sv_) %>% mutate(c2F = fct_inorder(c2F))
}
}

fsort3 <- function(data = INX, sortL, rev) {
  data$.sv_ <- data[[paste0("c3", sortL)]]
  if(rev) {
  data %>% arrange(c1F, c2F, desc(.sv_)) %>% select(-.sv_) %>% mutate(c3F = fct_inorder(c3F))
  } else {  
  data %>% arrange(c1F, c2F, .sv_) %>% select(-.sv_) %>% mutate(c3F = fct_inorder(c3F))
}
}