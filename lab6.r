# Statystyka Matematyczna - Laboratoria 6

# Zad  1
dims <- c(50, 500, 5000)
x <- c()

par(mfrow = c(1, 3))
for(i in dims) {
  x <- rbinom(i, 20, 0.02)
  hist(x, prob = TRUE)
  curve(dnorm(x, 20 * 0.02, sqrt(20 * 0.02 * 0.98)), add = TRUE, col = "Red")
}
par(mfrow = c(1, 1))

# Zad 2
dev.new()
x <- rpois(50, 2)

hist(x, prob = T)
curve(dnorm(x, 2, 2), add = T, col = "Cyan")

# Zad 3
dev.new()
m <- c()

for (i in 1:200) {
  x <- rbinom(30, 20, 0.1)
  m <- c(m, mean(x))
}

hist(m, prob = T)
curve(dnorm(x, 20 * 0.1, sqrt(20 * 0.1 * 0.9 / 30)), add = T, col = "Blue")

# Zad 4
# rozkład średniej N(mu, sigma/sqrt(n))
# rozkład sumy N(n*mu, sigma*sqrt(n))

dev.new()
suma <- c()
for (i in 1:200) {
  x <- rbinom(30, 20, 0.1)
  suma <- c(suma, sum(x))
}

hist(suma, prob = T)
curve(dnorm(x, 30 * 20 * 0.1, sqrt(30 * 20 * 0.1 * 0.9)), add = T, col = "Magenta")

# Zad 5
dev.new()
stan <- c()
for (i in 1:200) {
  x <- rnorm(25, 5, 1)
  stan <- c(stan, (mean(x) - 5) / 1)
}

hist(stan, prob = T)
curve(dnorm(x, 0, 1 / sqrt(25)), add = T, col = "Red")

# Zad 6
dev.new()
stat <- c()
for (i in 1:200) {
  x <- rnorm(20, 2, 5)
  stat <- c(stat, (mean(x) - 2) / sd(x) * sqrt(20))
}

hist(stat, prob = T)
curve(dt(x, 19), add = T, col = "Green")

# Zad 7
dev.new()
stat <- c()
for (i in 1:200) {
  x <- rnorm(20, 2, 5)
  stat <- c(stat, 19 * var(x) / 25) # z twierdzenia Fishera
}

hist(stat, prob = T)
curve(dchisq(x, 19), add = T, col = "Magenta")

# Zad extra
x <- rbinom(100, 200, 0.05)
y <- rpois(100, 10)
hist(x)
hist(y, add= T, col = "Red")