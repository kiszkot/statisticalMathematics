# Zadania na zajęciach:
#   Zad 1
#   Zad 10
#   Zad 11
# Zadania domowe:
#   reszta

# Zad 1
  # Populacja - Wrzystkie diamenty syntetyczne
  # Próba - 6 diamentów systetycznych
  # Badana zmienna - waga diamentu

  x <- c(0.46, 0.61, 0.52, 0.48, 0.57, 0.54)
  n <- length(x)
  m <- mean(x)
  var(x) # w karatach do kwadratu
  
  s <- sd(x)
  alpha <- 0.05 # poziom istotno�ci, 1-aplha wsp�czynnik istotno�ci
  t <- qt(1 - alpha / 2, n - 1)
  m - t * s / sqrt(n)
  m + t * s / sqrt(n)
  
  # Interpretacja:
  # Z 95% ufności możemy stwierdzić, źe przedział ufności (0.47, 0.59) [karaty]
  # POKRYWA nieznan� �redni� wag� diament�w produkowanych t� metod�.
  
  t.test(x, conf.level = 1 - alpha)
  t.test(x, conf.level = 0.99) # przedzia� si� rozszerza
  
  s2 <- s^2
  chi1 <- qchisq(1 - alpha / 2, n - 1)
  chi2 <- qchisq(alpha / 2, n - 1)
  (n - 1) * s2 / chi1
  (n - 1) * s2 / chi2
  # Interpretacja:
  # Z 95% ufno�ci� mo�emy stwierdzi�, �e przedzia� ufno�ci (0.0012, 0.0188) [karaty^2] # nolint
  # POKRYWA nieznan� wariacj� wagi diament�w produkowanych t� metod�.
  
  library("TeachingDemos")
  sigma.test(x, conf.level = 1 - alpha) # variancja, w pakiecjie TeachingDemos

# Zad 2 - Domowe
  n <- 365
  alpha <- 0.02
  m <- 102 # hl
  s2 <- 81 # hl2
  s <- 9 # hl
  t <- qt(1 - alpha / 2, n - 1)
  l <- m - t * s / sqrt(n)
  r <- m + t * s / sqrt(n)
  cat("Z 98 procent ufności możemy stwiedzić,\n",
      "że przedział ufności (", l, ",", r, ") [hl]\n",
      "pokrywa nieznaną średnią dziennego zużycia wody w frabyce\n\n")

# Zad 3 - Domowe
  x <- c(1211, 1206, 1201, 1212, 1200, 1209)
  alpha <- 0.05
  m <- mean(x)
  sigma <- 2.5
  z <- qnorm(1 - alpha / 2, 0, 1)
  l <- m - z * sigma / sqrt(n)
  r <- m + z * sigma / sqrt(n)
  cat("Z 95 procent ufności możemy stwiedzić,\n",
      "że przedział ufności (", l, ",", r, ") [h]\n",
      "pokrywa nieznaną średnią godziń pracy żarówki\n\n")
  
# Zad 4 - Domowe
  x <- c(4.28,  3.3,  4.22,  2.77,  2.75,  2.93,  3.86,  3.05,  4.12,
        2.88,  3.94,  4.99, 2.08,  4.35,  2.7,  4.09,  2.81,  2.82)

  m <- mean(x)
  s2 <- var(x)
  s <- sqrt(s2)
  cat("Punktowa ocena śrendiej i warianci to: ", m, s2, "\n")

  alpha <- 0.1
  n <- length(x)
  t <- qt(1 - alpha / 2, n - 1)
  l <- m - t * s / sqrt(n)
  r <- m + t * s / sqrt(n)
  cat("Z 90 procent ufności możemy stwiedzić,\n",
      "że przedział ufności (", l, ",", r, ") [-]\n",
      "pokrywa nieznaną średnią zawartości białka w 50kg wodorostów\n\n")

  chi1 <- qchisq(1 - alpha / 2, n - 1)
  chi2 <- qchisq(alpha / 2, n - 1)
  l <- (n - 1) * s2 / chi1
  r <- (n - 1) * s2 / chi2
  cat("Z 90 procent ufności możemy stwiedzić,\n",
      "że przedział ufności (", l, ",", r, ") [-]\n",
      "pokrywa nieznaną wariancje zawartości białka w 50kg wodorostów\n\n")

# Zad 5 - Domowe
  # Wzór na minimalną liczebność próby
  # wariancja znana n = ceiling( z(1-alpha/2) * sigma^2 / d^2)
  # wariancja nieznana n = ceiling( z(1-alpha/2) * s^2 / d^2)
  # d jest błędem estymacji

  alpha <- 0.05
  sigma2 <- 25
  d <- 1
  z <- qnorm(1 - alpha/2, 0, 1)

  n <- ceiling(z * sigma2 / d^2)
  cat("Aby z pewnościa 95 procent błąd estymacji nie przekroczył 1, \n",
      "należy pobrać próbe o minimum", n, "czasów mieszania betonu\n\n")
  
# Zad 6 - Domowe
  n <- 10
  sigma <- 15
  alpha <- 0.1
  z <- qnorm(1 - alpha / 2, 0, 1)
  l <- m - z * sigma / sqrt(n)
  r <- m + z * sigma / sqrt(n)
  cat("Z 90 procent ufności możemy stwiedzić,\n",
      "że przedział ufności (", l, ",", r, ") [kG/mm2]\n",
      "pokrywa nieznaną średnią twardości stali\n\n")

# Zad 7 - Domowe
  lo <- c(14, 28, 36, 28, 14)
  n <- sum(lo)
  alpha <- 0.05
  p1 <- sum(lo[1:3]) / n
  p2 <- sum(lo[1:4]) / n
  z <- qt(1 - alpha / 2, n)

  l <- p1 - z * sqrt(p1 * (1 - p1) / n)
  r <- p1 + z * sqrt(p1 * (1 - p1) / n)
  cat("Z 95 procent ufności możemy stwiedzić,\n",
      "że przedział ufności (", l, ",", r, ") [-]\n",
      "pokrywa nieznaną liczbę osób pijących do 2 filiżanek dziennie\n\n")
  
  l <- p2 - z * sqrt(p2 * (1 - p2) / n)
  r <- p2 + z * sqrt(p2 * (1 - p2) / n)
  cat("Z 95 procent ufności możemy stwiedzić,\n",
      "że przedział ufności (", l, ",", r, ") [-]\n",
      "pokrywa nieznaną liczbę osób pijących do 3 filiżanek dziennie\n\n")
  
# Zad 8 - Domowe
  x <- c(10, 20, 16, 18, 30, 24, 20, 17, 25)
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  alpha <- 0.05
  t <- qt(1 - alpha / 2, n - 1)

  l <- m - t * s / sqrt(n)
  r <- m + t * s / sqrt(n)
  cat("Z 95 procent ufności możemy stwiedzić,\n",
      "że przedział ufności (", l, ",", r, ") [s]\n",
      "pokrywa nieznaną średnią czasu mocowania detali\n\n")
  
# Zad 9 - Domowe
  x <- c(16.0, 12.5, 13.2, 17.1, 14.7, 11.9)
  n <- length(x)

  a <- t.test(x, conf.level = 1 - alpha)
  b <- unlist(a)
  l <- round(as.numeric(b[4]), 4)
  r <- round(as.numeric(b[5]), 4)
  cat("Z 95 procent ufności możemy stwiedzić,\n",
      "że przedział ufności (", l, ",", r, ") [g]\n",
      "pokrywa nieznaną średnią wagę detalu mocowanego na silniku\n\n")

# Zad 10
  x <- c(5, 7, 5, 6, 9, 8, 11)
  
  m <- mean(x)
  n <- length(x)
  sigma <- 2
  alpha <- 0.05
  
  z <- qt(1 - alpha / 2, Inf)
  
  m - z * sigma / sqrt(n)
  m + z * sigma / sqrt(n)
  # Interpretacja:
  # Z 95% ufno�ci� mo�emy stwierdzi�, �e przedzia� ufno�ci (5.80, 9.77) [m]
  # POKRYWA nieznan� �redni� d�ugo�� trasy pokonywanej przez robota.
  
  z.test(x, stdev = sigma, conf.level = 1 - alpha) # w pakiecie TeachingDemos
  # z.test(x, sigma.x <- sigma, conf.level <- 1-alpha) w pakiecie BSDA
  
# Zad 11
  alpha <- 0.05
  n <- 500
  n_t <- 330
  p <- n_t / n
  z <- qt(1 - alpha / 2, Inf)
  
  p - z * sqrt(p * (1 - p) / n)
  p + z * sqrt(p * (1 - p) / n)
  # Interpretacja:
  # Z 95% ufno�ci� mo�emy stwierdzi�, �e przedzia� ufno�ci (0.62, 0.70) [-]
  # POKRYWA nieznany odsetek os�b zadowolonych z cz�onkostwa w UE
  
  # Dla proporcji : prop.test(T, n, conf.level <- 1-alpha)
  prop.test(n_t, n, conf.level = 1 - alpha)