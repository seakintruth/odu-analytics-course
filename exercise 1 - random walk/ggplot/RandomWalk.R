library(ggplot2)
numRuns <- 30
runDist <- vector(length = numRuns)
dist <- vector(length = 10)
maxSteps <- 100
steps <- seq(10, maxSteps, by = 10)

for ( numSteps in steps) {
  walks.x <- array(dim = c(numRuns, numSteps))
  walks.y <- array(dim = c(numRuns, numSteps))
  for ( i in 1:numRuns) {
    walks.x[i][1] <- walks.y[i][1] <- 0
    for (j in 2:numSteps) {
      angle <- runif(1) * 2 * pi
      deltax <- cos(angle)
      deltay <- sin(angle)
      walks.x[i,j] <- walks.x[i,j-1] + deltax
      walks.y[i,j] <- walks.y[i,j-1] + deltay
    }
    runDist[i] <- sqrt(walks.x[i,numSteps]*walks.x[i,numSteps] + walks.y[i,numSteps]*walks.y[i,numSteps])
  }
  dist[numSteps/10] <- mean(runDist)
}

# plot(walks.x[15,],walks.y[15,],"l")
# plot(steps, dist, col = "blue", xlim = c(0, maxSteps), ylim = c(0,max(sqrt(maxSteps),dist[maxSteps/10])))
# points(steps, sqrt(steps), col = "red")
summary(runDist)

#walks <- data.frame("xval" = as.factor(walks.x[15,]),"yval" = as.factor(walks.y[15,]))
walks <- data.frame("xval" = walks.x[15,], "yval" = walks.y[15,])

#walks <- data.frame(walks.x[15,],walks.y[15,])
ggplot(walks, aes(x = xval, y = yval)) + geom_point() + geom_path()
