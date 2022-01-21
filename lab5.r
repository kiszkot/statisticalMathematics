#1
cat("Zadanie 1 : \n")
w <- c(4.07, 4.09, 4.06, 4.09, 4.08)
k <- c(4.06, 4.08, 4.07, 4.07, 4.07)
g <- c(4.08, 4.09, 4.08, 4.10, 4.09)

temperature <- c(w, k, g)
miasto <- rep(c("w", "k", "g"), each = length(w))

bartlett.test(temperature~miasto)

# test p-value = 0.4717 > alpha
# nie ma podstaw do odrzucenia H0
# zatem wariancje mogą być jednorodne

# test bartetta ręcznie
n <- 15
k_num <- 3
ni <- 5
sp2 <- 4 * (var(w) + var(k) + var(g)) / (n - k_num)
numerator <- (n - k_num) * log(sp2) - 4 * (log(var(w) * var(k) * var(g)))
denominator <- 1 + 1 / (3 * (k_num - 1)) * (3 / 4 - 1 / (n - k_num))
t_stat <- numerator / denominator
cat("T = ", t_stat, "\n")

qchisq(1 - 0.05, k_num - 1)

#b
a <- aov(temperature~miasto)
summary(a)

# p-value 0.0413 < alpha = 0.05 zatem na poziomie istotności 0.05
# odrzucamy H0 na korzyśc H1.
# Co najmniej jedno miasto różni się średnią temperaturą

#c
TukeyHSD(a)

# warszaw - kraków
# p-value 0.28 > alpha = 0.05 zatem nie mamy podstaw
# do odrzucenia H0.

# kraków - gdańsk
# p-value 0.03 < alpha = 0.05 zatem odrzucami H0
# temperatury średnie w mieście Kraków i Gdańsk różnią się istotnie

#2
cat("-----------------------------\n")
cat("Zadanie 2 : \n")
l <- c(28,	26,	29,	30,	28,	31,	26,	32,	25,	29)
m <- c(30, 29, 30, 30,	28,	32,	29,	32,	28,	30)
s <- c(31,	29,	33,	33,	29,	33,	28,	32,	27,	32)
h <- c(29,	27,	30,	31,	27,	32,	27,	32,	27, 30)

#3
k1 <- c(6.5, 7.8, 6.9, 6.4)
k2 <- c(7.2, 8.5, 7.3, 7.0)
k3 <- c(7.2, 7.5, 7.1, 7.5)
k4 <- c(7.1, 7.0, 7.1, 7.2)
k5 <- c(7.2, 6.6, 7.4, 7.5)

#4
np <- c(69, 52, 71, 58, 59, 65)
lp <- c(91, 72, 81, 67, 95, 84)
sp <- c(55, 60, 78, 58, 62, 66)
dp <- c(66, 81, 70, 77, 57, 79)

#5
cat("-----------------------------\n")
cat("Zadanie 5 : \n")
x <- c(145, 260, 405, 600, 620, 763)
y <- c(4.5, 9, 14.4, 17.5, 24, 26.7)

#a
plot(x, y)

#b
cor(x, y)

# Wraz ze wzrostem długości programu rośnie czas kopilacji
# 0 - brak zwiąsku
# +/- 0-0.2 - bardzo słaby związek liniowy
# +/- 0.2-0.4 - słaby związek liniowy
# +/- 0.4-0.6 - umiarkowany związek liniowy
# +/- 0.6-0.8 - silny związek liniowy
# +/- 0.8-1 - bardzo silny związek liniowy
# 1 - związek liniowy

#c
b1 <- cov(x, y) / var(x)
b0 <- mean(y) - b1 * mean(x)
cat("y =", b0, " + ", b1, " x\n")

# długość programu wzorśnie o 100 linijek
# czas kompilacji wzrośnie o 3.5 minut

summary(lm(x~y))

#d
# hipoteza H0 : b1 = 0
# hipoteza H1 : b1 ~= 0
# p-value 0.000874< alpha = 0.05 odrzucamy H0
# Zatem współczynnik b1 jest istotny.

#e
-0.3998279  +  0.03526637 * 500

#f
curve(-0.3998279  +  0.03526637 * x, add = TRUE, col = "red")

#g
# to jest R^2 = r^2
cor(x, y) ^ 2

# 0.9 - 1, bardzo dobre dopasowanie regresji do danych
# 0.8 - 0.9 dobre
# 0.6 - 0.8 zadowalające
# < 0.6 nie można zastosować

#6
x <- c(5, 10, 15, 20, 25, 30, 35)
y <- c(14.1, 13.8, 12.7, 12.3, 11.5, 11.0, 10.3)

#7
x <- c(1, 2, 3, 5, 6)
y <- c(8, 13, 15, 24, 25)