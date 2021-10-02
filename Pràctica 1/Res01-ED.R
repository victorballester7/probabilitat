# Exercici 1

a = c(2, 1, 4, 7)
b = c(4, 0,-1, 7)

c = 2 * a + 5 * b
c
d = a * b
d

# Exercici 2

Var = function(m, n) {
  prod(m:(m - n + 1))
}
Var(365, 10)

# Exercici 3

p = function(N) {
  choose(50, 3) * choose(N - 50, 47) / choose(N, 50)
}
N = 50:2000
plot(N, p(N), type = "l") # gràfic
N[p(N) == max(p(N))] # N que maximitza la funció

# Exercici 4

A = matrix(c(2, 3, 4, 4, 0,-1), 2, byrow = T)
A
B = matrix(c(0, 1, 2,-3, 2, 1), 2, byrow = T)
B
C = A + B
C
D = 2 * A
D
E = A %*% t(B)
E
F = matrix(c(2, 3, 4, -1), 2, byrow = T)
F
G = solve(F)
G
sum(a * b)

# Exercici 5

probabilitat1 = (choose(5, 2) + choose(3, 2)) / choose(8, 2) # fet amb les tècniques habituals
probabilitat1 
urna = c(1, 2, 3, 4, 5,-1,-2,-3)
CP = combn(urna, 2)
CP
b = CP[1, ] * CP[2, ] # producte de les dues files de CP
b 
a = b[b > 0]
a
probabilitat2 = length(a) / length(b)
probabilitat2 # en efecte, surt el mateix resultat que abans
