findClosestRoot <- function(x = 1) {
  root <- sqrt(x) %/% 1
  root <- if(root %% 2 == 0) root - 1 else root
  root
}

findSteps1 <- function(x = 1) {
  root <- findClosestRoot(x)
  min <- root ^ 2
  xdist <- abs((x-min) %% (root + 1) - ((root + 1) / 2))
  print(xdist)
  ydist <- (root + 2) %/% 2
  print(ydist)
  xdist + ydist
}

# STEP 2
findNext <- function(x = 747) {
  cmdList <- list() # Format: x, y, #. Movement, number of times.
  m <- matrix(0,3,3) #Will always have a 1 point gap between operating and outer area
  m[2,2] <- 1
  cursor <- c(2,2)
  root <- 1
  
  while(length(m[m > x]) == 0) {
    if(length(cmdList) == 0) {
      root <- root + 2 # Update the root.
      p <- findPerim(root) / 4 #Find the new perimiter for cmdlist
      cmdList <- list( #Update all the commands
        c(-1,0,p-1),
        c(0,-1,p),
        c(1,0,p),
        c(0,1,p+1)
      )
      ml <- length(m[1,])
      tmpm <- matrix(0,ml+2,ml+2)
      tmpm[2:(2+ml-1),2:(2+ml-1)] <- m
      m <- tmpm
      cursor <- cursor + c(1,2)
      if(root - 2 >= 3) {
        cursor[2] <- cursor[2] - 1
      }
    }
    
    # Perform work at cursor location
    m[cursor[1],cursor[2]] <- sum(m[(cursor[1]-1):(cursor[1]+1),(cursor[2]-1):(cursor[2]+1)]) - m[cursor[1],cursor[2]]
    
    cursor[1:2] <- cursor[1:2] + cmdList[[1]][1:2] # Move cursor
    
    cmdList[[1]][3] <- cmdList[[1]][3] - 1 #Increment cmdList
    if(cmdList[[1]][3] <= 0) {
      if(length(cmdList) != 1) {
        cmdList <- cmdList[2:length(cmdList)]
      } else {
        cmdList <- list()
      }
    }
  }
  
  m[m > x]
}

findPerim <- function(x = 3) {
  (x * 2) + ((x - 2) * 2)
}