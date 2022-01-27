#1
cat("-----\nZadanie 1\n")
x <- c(120.5, 126.9, 124.5, 131.0, 118.8, 123.7, 126.1, 125.3)
png("./img/powtorka_zad1.png")
hist(x, col = "blue",
     breaks = c(min(x), quantile(x, 0.33), quantile(x, 0.66),  max(x)),
     xlab = "Prędkość pobierania danych",
     ylab = "Gęstość")
dev.off()

#2
# Rozkład dwumianowy
cat("-----\nZadanie 2\n")
n <- 4
p <- 0.8
dbinom(0, n, p)
dbinom(1, n, p)
dbinom(2, n, p)
dbinom(3, n, p)
dbinom(4, n, p)

# Prawdopodobieństwo P(X >= 3) = 1 - P(X<3) = 1 - P(X <= 2)
1 - pbinom(2, n, p)

# Prawdopodobieństwo P(X <= 2)
pbinom(2, n, p)

#3
cat("-----\nZadanie 3\n")
# X sr ~ N(mu, sigma / sqrt(n))
mu <- 184
sigma <- 3
n <- 49
mu_sr <- mu
sigma_sr <- sigma / sqrt(n)

# P(x sr <= 183)
pnorm(183, mu_sr, sigma_sr)

png("./img/powtorka_zad3.png")
curve(dnorm(x, mu_sr, sigma_sr),
            xlim = c(mu_sr - 3 * sigma_sr, mu_sr + 3 * sigma_sr))
dev.off()

#4
library("TeachingDemos")
cat("-----\nZadanie 4\n")
x <- c(2.87, 2.74, 2.71, 2.92, 2.98, 2.83, 2.80, 2.77, 2.89)

#a
# t.test - średnia (sigma nieznana)
# z.test - średnia (sigma znana) / BSDA / TeachingDemos
# prop.test - struktura
# sigma.test - wariancja (I pop) / TeachingDemos
# var.test - wariancja (II pop)
t.test(x, conf.level = 0.95)

# Z 95% ufnościa możemy stwierdzić, że przedział ufności
# (2.77, 2.90) [m/s], pokrywa nieznaną średnią prędkość maksymalną samochodzików

#b
sigma.test(x, conf.level = 0.95)

# Z 95% ufnościa możemy stwierdzić, że przedział ufności
# (0.0036, 0.0287) [m/s]^2, pokrywa nieznaną wariancje
# prędkość maksymalną samochodzików

#5
cat("-----\nZadanie 5\n")
x <- c(77, 74, 81, 78, 79, 75)

# Hipoteza H0: mu <= 75
# Hipoteza H1: mu > 75
z.test(x, mu = 75, stdev = 2, alternative = "greater", conf.level = 0.9)

# p value = 0.0021 < 0.1
# Na poziomie istotności 0.1 odrzucamy H0 na korzyść H1.
# Zatem średni czas pracy odkurzacza jest zadowalający.

#6
cat("-----\nZadanie 6\n")
m1 <- c(3.5, 3.1, 3.3, 3.2, 3.0, 2.9)
m2 <- c(4.0, 3.5, 3.7, 3.8, 3.7, 3.8)

# Hipoteza H0 : sigma1^2 / sigma2^2 == 1
# Hipoteza H1 : sigma1^2 / sigma2^2 ~= 1

var.test(m1, m2, alternative = "two.sided", conf.level = 0.99)

# p value = 0.56 > 0.01
# Na poziomie istotności 0.01, nie mamy podstaw do odrzucenia H0.

#7
cat("-----\nZadanie 7\n")
intel <- c(1.5, 1.6, 1.3, 1.5, 1.7)
amd <- c(1.7, 1.7, 1.5, 1.6, 1.4)
shakti <- c(1.7, 1.8, 1.7, 1.9, 1.9)

#a
# Hipoteza H0 : sigma_intel^2 = sigma_amd^2 = sigma_shakti^2
# Hipoteza H1 : Co najmniej jedna różni się od posoztałych
dane <- c(intel, amd, shakti)
nazwy <- rep(c("intel", "AMD", "shakti"), each = length(amd))
bartlett.test(dane~nazwy)

# p value = 0.76 > 0.05
# Na poziomie istotności 0.05 nie mamy podstaw do odrzucenia H0

#b
# Hipoteza H0 : mu_intel = mu_amd = mu_shakti
# Hipoteza H1 : Co najmniej jedna różni się od posoztałych
summary(aov(dane~nazwy))

# p value = 0.0114 < 0.05
# Na poziomie istotności 0.05 odrzucamy H0 na korzyść H1.
# Co jajmniej jedna średnia czasu uruchomienia komputera
# różni się od pozostałych

#c
# Hipoteza H0 : mu_intel == mu_shakti
# Hipoteza H1 : mu_intel ~= mu_shakti
TukeyHSD(aov(dane~nazwy))

# p value 0.012 < 0.05
# Na poziomie istotności 0.05 odrzucamy H0 na korzyść H1
# Średni czas uruchomienia komputera jest różny
# dla procesorów inter i shakti

#8
cat("-----\nZadanie 8\n")
x <- c(11, 18, 22, 27, 30, 34)
y <- c(37, 25, 20, 12, 8, 3)

#a
png("./img/powtorka_zad8.png")
plot(x, y)

# Z wykresu możemy się sposziewać liniowej korelacji
# Bedzie to relacja ujemna

#b
cor(x, y)

# -0.99, zatem silny związek liniowy.

#c
b1 <- cov(x, y) / var(x)
b0 <- mean(y) - b1 * mean(x)
cat("y =", b0, " + ", b1, " x\n")

# funkcja y = 52.46415  +  -1.477358  x
# Zatem jeżeli temperatura wzrośnie o 1 C
# to czas roztopu zmaleje o 1.4 minut

#d
summary(lm(x~y))

# hipoteza H0 : b1 = 0
# hipoteza H1 : b1 ~= 0
# p-value 3.37e-6 < alpha = 0.05 odrzucamy H0
# Zatem współczynnik b1 jest istotny.
# p-value dla b0 też jest mniejsze od alpha
# zatem b0 też jest istotne.

#e
curve(52.46415 - 1.477358 * x, add = T, col = "red")
dev.off()

#f
# 15.5302
52.46415 - 1.477358 * 25

#g
# 0.9970021 zatem bardzo dobre dopasowanie regresji do danych
cor(x, y) ^ 2

#9
cat("-----\nZadanie 9\n")
s <- c()
for (i in 1:150) {
 x <- rnorm(50, 7, 4)
 s[i] <- length(x[x > 7]) / 50
}
png("./img/powtorka_zad9.png")
hist(s, prob = T)
curve(dnorm(x, 0.5, sqrt(0.5 * 0.5 / 50)), add = T, col = "red")
dev.off()