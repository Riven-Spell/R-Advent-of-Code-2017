jumpMaze <- function(x = "") {
  jumps <- as.numeric(unlist(strsplit(x,"\n")))
  stackptr <- 1
  i <- 0
  while(stackptr <= length(jumps)) {
    jumps[stackptr] <- jumps[stackptr] + 1
    stackptr <- stackptr + jumps[stackptr] - 1
    i <- i + 1
  }
  i
}