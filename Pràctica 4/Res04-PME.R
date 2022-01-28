# Exemple 1
plot(0,
     0,
     xlim = c(0, 1),
     ylim = c(0, 1),
     type = "n")
mostra = runif(min = 0, max = 1, n = 100)
rug(mostra)

# PROBLEMA 1
mostra1 = rnorm(mean = 0, sd = 1, n = 100)
hist(
  mostra1,
  breaks = seq(-4, 4, 0.1),
  freq = FALSE,
  col = 'green',
  xlab = "",
  ylab = "",
  main = "Histograma 1"
)

lines(seq(-4, 4, 0.1), dnorm(
  seq(-4, 4, 0.1),
  mean = 0,
  sd = 1,
  log = FALSE
), col = 'red')

rug(mostra1, col = 'blue')
legend(
  'topright',
  c('Densiat de la normal estàndard'),
  lty = c(1),
  col = c('red')
)


# Exemple 2
set.seed(27092015)

print(rexp(rate = 3, n = 5))

set.seed(27092015)

print(rexp(rate = 3, n = 7))

print(rexp(rate = 3, n = 7))


# Exemple 2
u <- runif(min = 0, max = 1, n = 100)

x <- cut(u, breaks = c(0, 0.2, 0.5, 1), labels = c(1, 2, 3))

print(as.integer(x))

x <- as.integer(x)

pobservades <- c(sum(x == 1), sum(x == 2), sum(x == 3)) / length(x)

print(pobservades)


u <- runif(min = 0, max = 1, n = 100)

p <- c(0.2, 0.3, 0.5)

F <- c(0, cumsum(p))

x <- 0

for (i in 1:3) {
  x <- x + ((F[i] < u) & (u <= F[i + 1])) * i
}

print(x)

pobservades <- c(sum(x == 1), sum(x == 2), sum(x == 3)) / length(x)

print(pobservades)


# PROBLEMA 2
u <- runif(min = 0, max = 1, n = 100)
x <- 0
p <- 0.3
F <- function(i) {
  1 - p ^ i
}
i <- 0
assolits <- 0
while (assolits < 100) {
  x <- x + ((F(i) < u) & (u <= F(i + 1))) * (i + 1)
  assolits <- assolits + sum(x == i + 1)
  i <- i + 1
}
print(x)
prob_real <- p ^ {
  c(1:max(x) - 1)
} * (1 - p)
prob_real
prob_empirica <- rep(0, max(x) - 1)
for (i in 1:max(x)) {
  prob_empirica[i] <- sum(x == i) / length(x)
}

prob_empirica
print(abs(prob_real - prob_empirica))

# Exemple 3
dens.triangular = function(x) {
  if (x <= 0 || 2 <= x)
    return (0)
  if (0 < x && x <= 1)
    return (x)
  if (1 < x && x <= 2)
    return (2 - x)
}
acceptacio = function() {
  while (TRUE) {
    u = runif(1)
    v = runif(min = 0, max = 2, n = 1)
    if (u <= dens.triangular(v) / (1 / 2 * 2))
      return (v)
  }
}
mida <- 100000
mostra <- rep(0, mida)
for (i in 1:mida) {
  mostra[i] <- acceptacio()
}
hist(
  mostra,
  breaks = seq(0, 2, 0.1),
  freq = FALSE,
  ylim = c(0, 1.5)
)

points <- seq(0, 2, 0.01)
valors <- c()
for (x in points) {
  valors <- c(valors, dens.triangular(x))
}
lines(points, valors, col = "red")


# Problema 3
## Apartat 1
p = 0.35
options(warn = -1)
simulacio = function(n) {
  # fins la generació n
  individus = rep(0, n + 1) # aquí emmagatzerem la població en cada generació
  for (i in 0:n) {
    if (i == 0)
      individus[i + 1] = 1
    else
      individus[i + 1] = sum(rgeom(individus[i], p)) # suma de individus[i] v.a. Geo(p)
  }
  return(individus)
}
experiments = function(m, n) {
  # m simulacions fins generació n.
  repliques <- c()
  for (i in 1:m) {
    repliques <- c(repliques, simulacio(n))
  }
  mat = matrix(repliques, nrow = n + 1)
  return(mat)
}
experiments(10, 10)
## Apartat 2
N = 10000
mat2 = experiments(N, 10)
sum(mat2[11, ] == 0) / N
## Apartat 3
expe = function(gen) {
  # gen = genereació que pot prendre valors a {1,2,...,10}
  N = 1000
  mostres = experiments(N, 10)
  mostres_gen = mostres[gen + 1, ]
  maxim = max(mostres_gen)
  E = 0
  for (j in 0:maxim) {
    x = sum(mostres_gen == j)
    E = E + j * (x / N)
  }
  E
}
vari = function(gen) {
  # gen = genereació que pot prendre valors a {1,2,...,10}
  N = 1000
  mostres = experiments(N, 10)
  mostres_gen = mostres[gen + 1, ]
  maxim = max(mostres_gen)
  E = expe(gen)
  V = 0
  for (j in 0:maxim) {
    x = sum(mostres_gen == j)
    V = V + (j - E) ^ 2 * (x / N)
  }
  V
}
expe(5)
expe(10)
vari(5)
vari(10)
## Apartat 4
hist(mat2[6, ], breaks = seq(0, max(mat2[6, ]), 1), freq = FALSE)

hist(mat2[11, ], breaks = seq(0, max(mat2[11, ]), 1), freq = FALSE)

## Apartat 5
p = 1 / 2
mat5 = experiments(N, 10)
sum(mat5[11, ] == 0) / N
## Apartat 6
hist(mat5[11, ], breaks = seq(0, max(mat5[11, ]), 1), freq = FALSE)
