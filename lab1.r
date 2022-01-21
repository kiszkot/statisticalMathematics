# 1
6^3 * log(34) + sin(pi/4)

# 2
x = seq(1,10,by=1)

# 3
y = seq(102,999,by=3)
length(y)

# 4
x = c(2,5,-1)
y = rep(x,3)
z = rep(x,each=4) # kazdy element 4 razy

# 5
a = c(1,3,6,2,7,4)
length(a)
a[3]
b = a[-4] # usuwa element nr 4
sum(a)
sum(b)
c = a[a>4]

# 6
r1 = c(1,2,4)
r2 = c(3,1,5)
r3 = c(0,4,4)
M = rbind(r1,r2,r3)
det(M)
T = t(M)
O = solve(M)
M%*%O # * muliplication by elements, %*% matrix multiplication
a = diag(M) # diag on matrix gives vector, diag on vector gives diagonal matrix
b = M[,2]
eigen(M) # eigenvalues = wartosci wlasne
A = M[-1,-3]

# 7
n = 10
x = c()
x = c(seq(4,n+3)^2) # lub z for jak ponizej
for (i in 1:10) {
  x[i] = (i+3)^2
}

# 8
x = seq(1,5)
sr = mean(x)
x3 = x[3]
if (sr == x3) {
  cat("równe")
} else if (sr > x3) { # warning, place else if on same line as closing bracket '}'
  cat("srednia")
} else {
  cat("trzeci")
}

# 9
x = c(2,-3,6,8,-1,4)
ind = c()
for (i in 1:length(x)) {
  if (x[i]<0) {
    x[i] = -x[i]
    ind = c(ind, i)
  }
}
