#### Exemplo 1 ####

face_do_dado <- c(1, 2, 3, 4, 5, 6)

nr_vezes_saiu_face <- c(46, 35, 25, 19, 40, 45)

(tabela_exemplo1 <- data.frame(
  xi = face_do_dado,
  oi = nr_vezes_saiu_face
))

#### Exemplo 2 ####

# Valores na Amostra
(xi <- c(0, 1, 2, 3, 4))

# Nº de Linhas na tabela
(k <- length(xi))

# Frequências Observadas / Absolutas
(Oi <- c(0, 5, 12, 19, 14))

# Dimensãoda Amostra
(n <- sum(Oi))

# Probabilidades Necessárias para as Frequências Espradas
(pi <- c(dbinom(0, 4, 0.6), dbinom(1, 4, 0.6), dbinom(2, 4, 0.6), dbinom(3, 4, 0.6), dbinom(4, 4, 0.6)))

# Nível de Significância
# α = 0.05

# Nº de Parametros Estimados
r <- 0

# Graus de Liberdade da Distribuição Qui-Quadrado
(gl <- k-1-r)

# Teste de Ajustamento do Qui-Quadrado
chisq.test(
  x = Oi,
  p = pi
)

#### Exemplo 3 ####

##### 1) #####

# P(2.1) ?

# Valores na Amostra
(xi <- c(0, 1, 2, 3, 4, 5))

# Nº de Linhas na tabela
(k <- length(xi))

# Frequências Observadas / Absolutas
(Oi <- c(14, 22, 18, 15, 10, 9))

# Dimensãoda Amostra
(n <- sum(Oi))

# Probabilidades Necessárias para as Frequências Espradas
(pi <- c(
  dpois(0, 2.1),
  dpois(1, 2.1),
  dpois(2, 2.1),
  dpois(3, 2.1),
  dpois(4, 2.1),
  1 - ppois(4, 2.1)
))
# OU
(pi <- dpois(xi, 2.1))
(pi[k] <- 1 - ppois(xi[k]-1, 2.1))

# Nível de Significância
# α = 0.05

# Nº de Parametros Estimados
r <- 0

# Graus de Liberdade da Distribuição Qui-Quadrado
(gl <- k-1-r)

# Teste de Ajustamento do Qui-Quadrado
chisq.test(
  x = Oi,
  p = pi
)

##### 2) #####

# P(λ) ?
# α = 0.01

# Valores na Amostra
(xi <- c(0, 1, 2, 3, 4, 5))

# Nº de Linhas na tabela
(k <- length(xi))

# Frequências Observadas / Absolutas
(Oi <- c(14, 22, 18, 15, 10, 9))

# Dimensãoda Amostra
(n <- sum(Oi))

# Amostra
(amostra_32 <- c(rep(xi, Oi)))

# Méda Estimada
mean(amostra_32)

# Probabilidades Necessárias para as Frequências Espradas
(pi <- dpois(xi, mean(amostra_32)))
(pi[k] <- 1 - ppois(xi[k]-1, mean(amostra_32)))

# Nível de Significância
# α = 0.01

# Nº de Parametros Estimados
r <- 1

# Graus de Liberdade da Distribuição Qui-Quadrado
(gl <- k-1-r)

# Teste de Ajustamento do Qui-Quadrado
chisq.test(
  x = Oi,
  p = pi
)


#### Exemplo 4 ####

# P(λ) ?
# α = 0.01

# Valores na Amostra
(xi <- c(0:5))

# Nº de Linhas na tabela
(k <- length(xi))

# Frequências Observadas / Absolutas
(Oi <- c(6, 14, 10, 7, 2, 1))

# Dimensãoda Amostra
(n <- sum(Oi))

# Amostra
(amostra_4 <- c(rep(xi, Oi)))

# Méda Estimada
mean(amostra_4)

# Probabilidades Necessárias para as Frequências Espradas
(pi <- dpois(xi, mean(amostra_4)))
(pi[k] <- 1 - ppois(xi[k]-1, mean(amostra_4)))

# Nível de Significância
# α = 0.01

# Nº de Parametros Estimados
r <- 1

# Graus de Liberdade da Distribuição Qui-Quadrado
(gl <- k-1-r)

# Teste de Ajustamento do Qui-Quadrado
(chisq4 <- chisq.test(
  x = Oi,
  p = pi
))

chisq4$expected






