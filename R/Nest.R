Nest <- function(data) {
  cc <- names(data)
  pairVs <- combinations(n=length(cc), r=2, v=cc)
  PairVs <- data.frame(pairVs, stringsAsFactors=FALSE)
  Nesting <- mapply(DetH, gr1=PairVs$X1, gr2=PairVs$X2, MoreArgs=list(data=data))
  PairVs <- cbind(PairVs, Nesting)
  PV <- PairVs %>% mutate(NestL=ifelse(Nesting=="notNested", FALSE, TRUE), Nests=as.character(Nesting)) %>% select(-Nesting)
  return(PV)
}

DetH <- function(data, gr1, gr2) {
  data$gr1 <- data[[gr1]]
  data$gr2 <- data[[gr2]]
  txy <- with(data, table(gr1, gr2))
  txyd <- data.frame(txy)
  ifelse(sum(txyd$Freq>0) > max(dim(txy)), "notNested", ifelse(dim(txy)[1] > dim(txy)[2], "X1inX2", "X2inX1"))
}

# Thanks to Bill Venables for his original version of gtools::combinations
combinations <- function(n, r, v = seq_len(n)) {
  v0 <- vector(mode(v), 0)
  sub <- function(n, r, v) {
    if (r == 0)
      v0
    else if (r == 1)
      matrix(v, n, 1)
    else if (r == n)
      matrix(v, 1, n)
    else rbind(cbind(v[1], Recall(n - 1, r - 1, v[-1])),
               Recall(n - 1, r, v[-1]))
  }
  sub(n, r, v[seq_len(n)])
}
