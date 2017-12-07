checksum <- function(spreadsheet = "", version = 1) {
  out <- 0
  for(line in strsplit(spreadsheet, "\n")[[1]]) {
    if(version == 1) {
      out <- out + linechecksum(line)
    } else {
      out <- out + linechecksum2(line)
    }
  }
  out
}

linechecksum <- function(l = "") {
  nin <- sort(as.numeric(strsplit(l, "\\s+")[[1]]))
  nin[length(nin)] - nin[1]
}

# STEP 2

linechecksum2 <- function(l = "") {
  nin <- as.numeric(strsplit(l, "\\s+")[[1]])
  out <- 0
  for(i in nin) {
    divisible <- nin[(nin %% i) == 0]
    notsame <- divisible[divisible != i]
    if(length(notsame) != 0) {
      out <- notsame[1] / i
      break
    }
  }
  out
}