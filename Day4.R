PassCount <- function(x = "", v = 1) {
  lines <- strsplit(x,"\n")[[1]]
  n <- 0
  for(l in lines) {
    if(v == 1) {
      n <- n + PassVerify(l)
    } else {
      n <- n + PassVerify2(l)
    }
  }
  n
}

PassVerify <- function(x = "") {
  words <- strsplit(x,"\\s+")[[1]]
  usedwords <- character(0)
  v <- T
  for(w in words) {
    if(length(usedwords[usedwords == w]) != 0) {
      v <- F
      break
    } else {
      usedwords <- append(usedwords, w)
    }
  }
  v
}

PassVerify2 <- function(x = "") {
  words <- strsplit(x,"\\s+")[[1]]
  usedwords <- character(0)
  v <- T
  for(w in words) {
    sw <- paste(sort(unlist(strsplit(w,""))), collapse = "")
    if(length(usedwords[usedwords == sw]) != 0) {
      v <- F
      break
    } else {
      usedwords <- append(usedwords, sw)
    }
  }
  v
}