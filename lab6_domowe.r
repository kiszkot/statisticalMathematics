# Zadania dla chętnych Lab 6
# Zadanie 1

x <- c()
y <- c()
iter <- 200
f_value <- c()

for (i in 1:iter) {
 x <- rnorm(50, 5, 2)
 y <- rnorm(30, 4, 1)
 f_value <- c(f_value, var(x) / var(y) * 1 / 4)
 # var(x) * sigmay^2 divided by var(y) * sigmax^2
 # you take sigmas from the normal distribution
}

png("./img/lab6_zad1.png")
hist(f_value, prob = T)
curve(df(x, 50 - 1, 30 - 1), add = T, col = "red")
dev.off()

# Zadanie 2

x <- c()
prop_value <- c()

for (i in 1:iter) {
 x <- rnorm(50, 5, 2)
 prop_value <- c(prop_value, length(x[x > 5]) / 50)
}

png("./img/lab6_zad2.png")
hist(prop_value, prob = T)
curve(dnorm(x, 0.5, sqrt(0.5 * 0.5 / 50)), add = T, col = "red")
dev.off()