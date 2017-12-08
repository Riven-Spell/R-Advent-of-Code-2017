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
# NEEDS FIXING
findNext <- function(x = 747) {
  m <- matrix(0,3,3)
  m[2,2] <- 1
  cursor <- c(2,2)
  i <- 1
  root <- 1
  perim <- findPerim(root)
  actions <- list(
    c(-1,0), # First quarter, move upwards
    c(0,-1), # Second quarter, move left
    c(1,0), # Third quarter, move downwards
    c(0,1), # Final quarter, move right.
    c(0,0) # sanity check
  )
  
  while(length(m[m <= x]) != 1) {
    if(root^2 <= i) { # Iterated root squared times
      ml <- length(m[1,]) # Get size of matrix
      tmpm <- matrix(0,ml + 2,ml + 2) # Create matrix 2 larger
      tmpm[2:(2+ml-1),2:(2+ml-1)] <- m # Place matrix in middle of temp matrix
      m <- tmpm # Set matrix to temp matrix
      cursor <- cursor + c(1,2) # Move cursor to old position + (x + 1)
      root <- root + 2 # Iterate the root upwards one step.
      perim <- findPerim(root)
    }
    
    # Calculate & assign new block.
    m[cursor[1],cursor[2]] <- sum(m[(cursor[1]-1):(cursor[1]+1),(cursor[2]-1):(cursor[2]+1)]) - m[cursor[1],cursor[2]]
    # Move cursor.
    lastroot <- root - 2 # determine last root to find current progression to next root
    tmpi <- i - (lastroot^2) # determine progression to next root
    perimq <- perim / 4 # perimiter into quarters
    
    #debug
    print(m)
    print(cursor)
    print("Action:")
    print(actions[[((tmpi + 1) %/% perimq) + 1]])
    print("Maximum:")
    print(max(unlist(m)))
    print("Iteration:")
    print(i)
    
    
    cursor <- cursor + actions[[(tmpi %/% perimq) + 1]] # mouse performs the closest 25% action.
    i <- i + 1
  }
  m[m <= x]
}

findPerim <- function(x = 3) {
  (x * 2) + ((x - 2) * 2)
}