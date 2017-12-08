Redist <- function(x = "0 2 7 0") {
  blocks <- as.numeric(unlist(strsplit(x, "\\s+")))
  previousstates <- list() # list(blocks) %in% previousstates
  poscycle <- numeric(0)
  i <- 0
  while(list(blocks) %in% previousstates == F) {
    previousstates <- append(previousstates, list(blocks))
    poscycle <- append(poscycle, i)
    max <- which.max(blocks)
    redist <- blocks[max]
    blocks[max] <- 0
    for(n in 1:redist) {
      max <- max + 1
      blocks[(max - 1) %% length(blocks) + 1] <- blocks[(max - 1) %% length(blocks) + 1] + 1
      redist <- redist - 1
    }
    i <- i + 1
    print(blocks)
    print(i)
  }
  print(abs(poscycle[match(list(blocks), previousstates)] - i))
  i
}