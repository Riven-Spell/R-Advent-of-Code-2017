jumpMaze <- function(x = "", v = 1) {
  jumps <- as.numeric(unlist(strsplit(x,"\n")))
  stackptr <- 1
  i <- 0
  while(stackptr <= length(jumps)) {
    inc <- if(jumps[stackptr] >= 3 && v == 2) -1 else 1
    jumps[stackptr] <- jumps[stackptr] + inc
    stackptr <- stackptr + jumps[stackptr] - inc
    i <- i + 1
  }
  i
}