# Laboratoria 7 - Statystyka Matematyczna
LRT <- c()
mu0 <- 0
n <- 30
iterations <- 200

for (i in 1:iterations) {
  x <- rnorm(n, mu0, 1)
  LRT[i] <- n * ((mean(x) - mu0) ^ 2)
}

hist(LRT, prob = T)
curve(dchisq(x, 1), add = T, col = "red")

q90 <- quantile(LRT, 0.9)
q95 <- quantile(LRT, 0.95)
q99 <- quantile(LRT, 0.99)
licz90 <- 0
licz95 <- 0
licz99 <- 0
mu <- 0

for (i in 1:iterations) {
  x <- rnorm(n, mu, 1)
  LRT[i] <- n * ((mean(x) - mu0) ^ 2)
  if(LRT[i] > q90) {licz90 <- licz90 + 1}
  if(LRT[i] > q95) {licz90 <- licz95 + 1}
  if(LRT[i] > q99) {licz90 <- licz99 + 1}
}

moc <- c(licz90, licz95, licz99) / 200