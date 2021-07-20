maxSteps <- 10^6
# start the clock!
ptm <- proc.time()

randomStep <- function(step_cord){
  # expects a data frame with three columns "x_step, y_step, random_angle"
  angle <- step_cord[,3]*2*pi
  deltax <- cos(angle)
  deltay <- sin(angle)
  step_cord[,1] <- step_cord[,1] + deltax
  step_cord[,2] <- step_cord[,2] + deltay
  step_cord
}

distance <- function(a,b){
  sqrt(((b[,1] - a[,1])^2 + (b[,2] - a[,2])^2))
}

step_cords <-as.data.frame(matrix(rep(0,maxSteps*2),ncol=2))
colnames(step_cords) <- c("x_step","y_step")
#generate our random numbers (can't do this inside the step function)
step_cords$random <- as.data.frame(runif(nrow(step_cords),0,1))
# build each step
step_cords <- randomStep(step_cords)
step_cords$x_actual <- cumsum(step_cords$x_step)
step_cords$y_actual <- cumsum(step_cords$y_step)

distanceTraveled <- distance(
  cbind(step_cords$x_actual[1],step_cords$y_actual[1]),
  cbind(
    step_cords$x_actual[nrow(step_cords)],
    step_cords$y_actual[nrow(step_cords)]
  )
)

proc.time() - ptm

plot(
  x=step_cords$x_actual,
  y=step_cords$y_actual,
  xlab= "x",ylab="y",
  main="Random Walk",
  sub=paste0(maxSteps," steps with a end distance of: ",round(distanceTraveled,2)), 
  type = 'l'
)
proc.time() - ptm
