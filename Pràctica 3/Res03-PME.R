library(MASS)

# Exemple 1
g = function(x) {
  exp(exp(x))
}
n = 10 ^ 4
c = exp(exp(1)) #fita
x = runif(n)
y = c * runif(n)
Imonte = c * sum(y < g(x)) / n # estimació de Monte Carlo
Inum = area(g, 0, 1) # càlcul numeric
error = abs(Inum - Imonte)
error

# PROBLEMA 1

f1 = function(x) {
  (1 - x ^ 2) ^ (3 / 2)
} # el maxim està en f(0)=1.
x = runif(n)
y = runif(n)
Imonte = sum(y < f1(x)) / n
Imonte # estimació de Monte Carlo
Inum = area(f1, 0, 1)
Inum # càlcul numeric
error = abs(Inum - Imonte)
error

# Exemple 2
h = function(x) {
  x ^ 2 * (x > 5)
}
g = function(x) {
  x ^ 2 * exp(-x)
}
n = 10 ^ 4
Imonte = sum(h(rexp(n))) / n
Imonte
Inum = integrate(g, 5, Inf)
Inum # utilitzem aquesta instrucció en lloc d’àrea
error = abs(Inum$value - Imonte)
error

# PROBLEMA 3
h1 = function(x) {
  exp(-5) * (x + 5) ^ 2
}
g = function(x) {
  x ^ 2 * exp(-x)
}
n = 10 ^ 4
Imonte = sum(h1(rexp(n))) / n
Imonte
Inum = integrate(g, 5, Inf)
Inum # utilitzem aquesta instrucció en lloc d’àrea
error = abs(Inum$value - Imonte)
error

# PROBLEMA 4
Imonte10_f = replicate(10, sum(h(rexp(n))) / n)
Imonte10_f
Imonte10_f1 = replicate(10, sum(h1(rexp(n))) / n)
Imonte10_f1
zero = rep(0, 10)
zero
plot(Imonte10_f,
     zero,
     col = "red",
     xlab = "",
     ylab = "")
points(Imonte10_f1, zero, col = "cyan")
points(Inum$value, 0, col = "black")
legend(
  'top',
  c('Densiat f', 'Denistat f1', 'Integral I'),
  lty = c(1, 1, 1),
  col = c("red", "cyan", "black")
)

# subapartat PROBLEMA 3
int1 = function(x) {
  h(x) * g(x)
} # x^4*exp(-x) = (x^2*exp(-x))^2/exp(-x)
int2 = function(x) {
  h1(x - 5) * g(x)
} # x^2*(x+5)^2*exp(-x)*exp(-5) = (x^2*exp(-x))^2/(exp(-x+5)*(x>5))
of = integrate(int1, 5, Inf)$value - Inum$value ^ 2
of
of1 = integrate(int2, 5, Inf)$value - Inum$value ^ 2
of1

# PROBLEMA 5
f5 = function(x) {
  exp(exp(x))
}
n = 10 ^ 4
Iimp_samp = sum(f5(x = runif(n))) / n
Iimp_samp
Inum = area(f5, 0, 1)
Inum # càlcul numeric
error = abs(Inum - Iimp_samp)
error
