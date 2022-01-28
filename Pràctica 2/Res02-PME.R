# proves
sample(1:6, size = 4, replace = T)
prob = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
sample(1:6, size = 4, replace = T, prob)
pertany6 = function(x) {
  sum(x == 6) > 0
}
prob1 = function(x) {
  sum(replicate(x, pertany6(sample(1:6, 4, replace = TRUE)))) / x
}
(1 - (35 / 36) ^ 24)


# PROBLEMES

n = 10 ^ 5

# Problema 1
pertany36 = function(x) {
  sum(x == 36) > 0
} # cal posar "sum(x==36)>0" i no "sum(x==36)" perquè si surten més d'un 36 en una simulació, aleshores es compten més resultats.
prob2 = function(x) {
  sum(replicate(x, pertany36(sample(1:36, 24, replace = TRUE)))) / x
}
prob2(n)

# Problema 2
errorMere6 = function(x) {
  c(abs(prob1(x) - (1 - (5 / 6) ^ 4)), 1 / sqrt(x))
} # retorna una parella de valors: (error obtingut, cota del error)
errorMere36 = function(x) {
  c(abs(prob2(x) - (1 - (35 / 36) ^ 24)), 1 / sqrt(x))
} # retorna una parella de valors: (error obtingut, cota del error)
errorMere6(10 ^ 4)
errorMere6(10 ^ 5)
errorMere6(10 ^ 6)
errorMere36(10 ^ 4)
errorMere36(10 ^ 5)
errorMere36(10 ^ 6)

# Problema 3
estroben = function(x) {
  unifa = 60 * runif(x)
  unifb = 60 * runif(x)
  sum(abs(unifa - unifb) < 10)
}
estroben(n)
estroben(n) / n

# Problema 4
agullatalla = function(n) {
  l = 1
  a = 2
  x = a / 2 * runif(n)
  o = pi * runif(n)
  sum(x <= l / 2 * sin(o))
}
p1 = agullatalla(10 ^ 4) / 10 ^ 4
p1
p2 = agullatalla(10 ^ 5) / 10 ^ 5
p2

# Problema 5
pi1 = 1 / p1
pi1
pi2 = 1 / p2
pi2
for (i in 4:8) {
  aprox = agullatalla(10 ^ i) / 10 ^ i
  print(1 / aprox)
}

# Problema 6
dinsdelcercle = function(m) {
  x = -1 + 2 * runif(m) # número entre -1 i 1 (eix X)
  y = -1 + 2 * runif(m) # número entre -1 i 1 (eix Y)
  sum(x ^ 2 + y ^ 2 <= 1)
}
p6 = dinsdelcercle(n) / n
p6
pi_6 = p6 * 4
pi_6

# Problema 7
estriangle = function(m) {
  u = runif(m)
  v = runif(m)
  sum(pmax(u, v) > 1 / 2 &
        pmin(u, v) < 1 / 2 &
        pmax(u, v) - pmin(u, v) < 1 / 2) # pmax fa el màxim de cada component; pmin, el mínim.
}
p7 = estriangle(n) / n
p7
