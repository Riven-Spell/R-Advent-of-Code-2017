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
findNext <- function(x = 1) {
  lastLayer <- c(1)
  thisLayer <- numeric(0)
  layerRoot <- 3
  while(length(thisLayer) == 0 || thisLayer[length(thisLayer)] < x) {
    if(length(thisLayer) > ((layerRoot * 2) + ((layerRoot - 2) * 2))) {
      lastLayer <- thisLayer
      thisLayer <- numeric(0)
      layerRoot <- layerRoot + 2
    }
    print(thisLayer)
    
    if(length(thisLayer) == 0) {
      if(length(lastLayer) != 1) {
        thisLayer <- append(thisLayer, lastLayer[1] + lastLayer[length(lastLayer)])
      } else {
        thisLayer <- append(thisLayer, lastLayer[1])
      }
    } else {
      thisLayer <- append(thisLayer, thisLayer[length(thisLayer)] + sum(lastLayer[lastLayerRange(lastLayer, thisLayer, layerRoot)]))
    }
  }
  thisLayer[length(thisLayer)]
}

# WORKS
lastLayerRange <- function(lastLayer = 1, thisLayer = numeric(0), layerRoot = 3) {
  posTL <- (length(thisLayer) - 1) %% (layerRoot - 1) + 1
  base <- (pmax(1, (posTL - 1)):pmin(layerRoot - 2, (posTL + 1)) - 1)
  tlq <- ((length(thisLayer) - 1) %/% (layerRoot - 1))
  llq <- (length(lastLayer) %/% 4)
  base + (tlq * llq)
}