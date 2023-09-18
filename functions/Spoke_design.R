#Joshua G. Smith
#June 2016

#Initialize plot surface
plot(x = NA, y = NA, xlim = c(-5, 5), ylim = c(-5, 5), asp = 1)
curve(expr = sqrt(1 - x^2), from = -5, to = 5, add = TRUE)
curve(expr = -sqrt(1 - x^2), from = -5, to = 5, add = TRUE)
curve(expr = sqrt(4 - x^2), from = -5, to = 5, add = TRUE)
curve(expr = -sqrt(4 - x^2), from = -5, to = 5, add = TRUE)
curve(expr = sqrt(9 - x^2), from = -5, to = 5, add = TRUE)
curve(expr = -sqrt(9 - x^2), from = -5, to = 5, add = TRUE)
curve(expr = sqrt(16 - x^2), from = -5, to = 5, add = TRUE)
curve(expr = -sqrt(16 - x^2), from = -5, to = 5, add = TRUE)
curve(expr = sqrt(25 - x^2), from = -5, to = 5, add = TRUE)
curve(expr = -sqrt(25 - x^2), from = -5, to = 5, add = TRUE)
curve(expr = 1 * x, from = -5, to = 5, add = TRUE)
curve(expr = -1 * x, from = -5, to = 5, add = TRUE)
abline(h = 0)
abline(v = 0)

#create matrix
mat <- matrix(data = NA, nrow = 8, ncol = 2)

#generate random stratified points
set.seed(1985)

for (i in 1:8) {
  #define number of transects and number of quadrats per transects
  theta <- 360 / 8 * i * 2 * pi / 360
  #provide transect length and number of quadrats per transect
  rho <- 5 * sqrt(runif(n = 2))
  while (abs(rho[1] - rho[2]) < 1) {
    rho <- 5 * sqrt(runif(n = 2))
  }
  if (i %% 2 == 0) {
    lines(x = rho[1] * cos(theta) - 0.5 + c(0,1,1,0,0),
          y = rho[1] * sin(theta) - 0.5 + c(0,0,1,1,0))
    lines(x = rho[2] * cos(theta) - 0.5 + c(0,1,1,0,0),
          y = rho[2] * sin(theta) - 0.5 + c(0,0,1,1,0))
  }
  #if quadrats overlap, then separate so they don't touch
  if (i %% 2 == 1) {
    lines(x = rho[1] * cos(theta) - c(0,-0.7071068,0,0.7071068,0),
          y = rho[1] * sin(theta) - 0.7071068 + c(0,0.7071068,1.414214,0.7071068,0))
    lines(x = rho[2] * cos(theta) - c(0,-0.7071068,0,0.7071068,0),
          y = rho[2] * sin(theta) - 0.7071068 + c(0,0.7071068,1.414214,0.7071068,0))
  }
  points(x = rho * cos(theta), y = rho * sin(theta))
  mat[i,] <- rho
}
mat


#theta <- runif(n = 1000) * 2 * pi
#rho <- 5 * sqrt(runif(n = 1000))
#points(x = rho * cos(theta), y = rho * sin(theta))
