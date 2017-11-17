library(statmod)
library(boot)



set_10_2 <- matrix(rep(rinvgauss(10, mean = 5, shape = 2), 2000), nrow = 2000)
set_25_2 <- rinvgauss(25, mean = 5, shape = 2)
set_50_2 <- rinvgauss(50, mean = 5, shape = 2)
set_100_2 <- rinvgauss(100, mean = 5, shape = 2)
set_500_2 <- rinvgauss(500, mean = 5, shape = 2)


set_10_4 <- rinvgauss(10, mean = 5, shape = 4)
set_25_4 <- rinvgauss(25, mean = 5, shape = 4)
set_50_4 <- rinvgauss(50, mean = 5, shape = 4)
set_100_4 <- rinvgauss(100, mean = 5, shape = 4)
set_500_4 <- rinvgauss(500, mean = 5, shape = 4)



set_10_8 <- rinvgauss(10, mean = 5, shape = 8)
set_25_8 <- rinvgauss(25, mean = 5, shape = 8)
set_50_8 <- rinvgauss(50, mean = 5, shape = 8)
set_100_8 <- rinvgauss(100, mean = 5, shape = 8)
set_500_8 <- rinvgauss(500, mean = 5, shape = 8)



set_10_12 <- rinvgauss(10, mean = 5, shape = 12)
set_25_12 <- rinvgauss(25, mean = 5, shape = 12)
set_50_12 <- rinvgauss(50, mean = 5, shape = 12)
set_100_12 <- rinvgauss(100, mean = 5, shape = 12)
set_500_12 <- rinvgauss(500, mean = 5, shape = 12)



j <- rep(1,500)
object<-basicglm(rep(1,500), set_500_2, link = 1, random = 6)


((rinvgauss(100, mean = 5, shape = 2))) -> j
k <- matrix(data = j, ncol = 10)
apply(k, MARGIN  = 1, basicglm, link = 1, xmat = rep(1,10), random = 6) ->esters






loglikinvgaus <- function(data, par){
  mu <- par[1]
  lambda <- par[2]
  j <- -(sum(1/2*log(lambda) - 1/2*(log(2*pi))  -3/2*log(data)) - lambda/(2*mu^2)*sum((data-mu)^2/data))}

k <- optim(c(5,2), loglikinvgaus, data = set_500_2, hessian = T)
vcov <- solve(k$hessian)
k$par %*% vcov %*% k$par
k$par[1] + 1.96*vcov[1,1]
