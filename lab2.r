# Laboratoria 2
shell("cls")

# Zad 1
  x = c(11,14,15,18,12,14,15,19)
  y = c(19,15,15,12,11,14,16,12)
  hist(x, breaks=c(11,13,15,17,19), col="blue", prob=TRUE)

# Zad 2
  # x = seq(0,20,by=0.01)
  # y = dnorm(x,10,5)
  curve(dnorm(x,10,5), xlim=c(0,20)) # faster than plot
  x = seq(0,20,by=0.01)
  y = dnorm(x,10,5)
  plot(x,y,type="l")

# Zad 3
  # dbinom(2,7,0.05)
  # pbinom(3,7,0.05) + dbinom(4,7,0.05)
  # pbinom(2,7,0.05)

# Zad 4 - approximate do Poisson n >= 50, p <= 0.05
  # binom -> poiss
  # lambda = n*p
  n = 70
  p = 0.05
  lambda = n*p
  
  # P(X<=a) = F(a) discrete
  # P(X<=b) = P(x<b) = F(b) continous
  # P(a<=X<=b) = F(b) - F(a) continous
  # a
  dbinom(2, n,p)
  dpois(2, n*p)
  
  # b
  1 - pbinom(3, n, p)
  1 - ppois(3, n*p)
  
  pbinom(3, n, p, lower.tail=FALSE)
  ppois(3, n*p, lower.tail=FALSE)
  
  # c
  pbinom(2, n, p)
  ppois(2, n*p)

# Zad 5
  # skip draw function
  # N(2,0.25)
  pnorm(2.25, 2, 0.25) - pnorm(1+5/6, 2, 0.25)
  
# Zad 7
  # mean ~ N(mu, sigma/sqrt(n))
  # S ~ N(n*mu, sigma*sqrt(n))
  
  # N(202, 14/8)
  pnorm(206, 202, 14/8) - pnorm(198, 202, 7/4)
  curve(dnorm(x,202,7/4), xlim = c(196, 208))
  x = seq(198, 206, length=100)
  y = dnorm(x, 202, 7/4)
  polygon(c(x,rev(x)), c(rep(0,times=100),rev(y)), col="burlywood") # x - coordinates from left to right and reverse, y - same
  # colors() for all colors
  
# Zad 10
  x = rnorm(200,0,1)
  hist(x, prob=TRUE)
  curve(dnorm(x,0,1), add=TRUE, col="red") # to add to current device
  length(x[x>0])
  