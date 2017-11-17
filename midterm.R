library(statmod)
library(boot)



set_10_2 <- matrix(replicate(2000, rinvgauss(10, mean = 5, shape = 2)), nrow = 2000)
set_25_2 <- rinvgauss(25, mean = 5, shape = 2)
set_50_2 <- rinvgauss(50, mean = 5, shape = 2)
set_100_2 <- matrix(replicate(2000, expr = rinvgauss(100, mean = 5, shape = 2)), nrow = 2000)
set_500_2 <- matrix(replicate(2000, expr = rinvgauss(500, mean = 5, shape = 2)), nrow = 2000)


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
set_100_12 <- matrix(replicate(2000, expr = rinvgauss(100, mean = 5, shape = 12)), nrow = 2000)
set_500_12 <- matrix(replicate(2000, expr = rinvgauss(500, mean = 5, shape = 12)), nrow = 2000)



j <- rep(1,500)
object<-basicglm(rep(1,500), set_500_2, link = 1, random = 6)


((rinvgauss(100, mean = 5, shape = 2))) -> j
k <- matrix(data = j, ncol = 10)
apply(k, MARGIN  = 1, basicglm, link = 1, xmat = rep(1,10), random = 6) ->esters





#The log liklihood of the inverse gaussian
loglikinvgaus <- function(data, par){
  mu <- par[1]
  lambda <- par[2]
  j <- -(sum(1/2*log(lambda) - 1/2*(log(2*pi))  -3/2*log(data)) - lambda/(2*mu^2)*sum((data-mu)^2/data))
  }

correct <- NULL
for(i in c(1:2000)){
  dit <- set_10_2[i,]
#finding MLE optims
k <- optim(c(5,2), loglikinvgaus, data = dit, hessian = T, method = 'L-BFGS-B', lower = c(.5,.5), upper = c(100,100))
vcov <- solve(k$hessian)

#confidence interval
upper <- k$par[1] + 1.96*vcov[1,1]
lower <- k$par[1] - 1.96*vcov[1,1]

correct[i] <- 5 < upper & 5 > lower
}



sum(correct)/2000
