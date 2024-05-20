#### Capítulo 4 - Exercícios ####

"----------------------------------------------------------------------"

#### Exercicio 4.1 ####

##### 1) #####

## I.C. para a μ a 99%

# I.C.: ] x̅ - (z_(1 - (α/2))) * (σ / sqrt(n)) , x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) [

# α = 1 - confiança = 1 - 0.99 = 0.01
# x̅ = 78.3
# z_(1 - (α/2)) = z(1 - (0.01/2)) = z_(0.995) = qnorm(0.995) = 2.5758
# (σ / sqrt(n)) = (2 / sqrt(25)) = 0.4
# z_(1 - (α/2)) * (σ / sqrt(n)) = 2.5758 * 0.4 = 1.0303

# I.C.: ] 78.3 - 1.0303, 78.3 + 1.0303 [ = ] 77.2697, 79.3303 [

##### 2) #####

## Amplitude = max - min = 79.3303 - 77.2697 = 2.0606

##### 3) #####

## Margem de Erro = Ampitude / 2 = 2.0606 / 2 = 1.0303

##### 4) #####

## Amplitude de IC a 99% < 0.1

## L_superior - L_inferior =
## (x̅ + (z_(1 - (α/2))) * (σ / sqrt(n))) - (x̅ - (z_(1 - (α/2))) * (σ / sqrt(n))) =
## x̅ - (z_(1 - (α/2))) * (σ / sqrt(n)) - x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) =
## 2 * (z_(1 - (α/2))) * (σ / sqrt(n))

## 2 * (z_(1 - (α/2))) * (σ / sqrt(n)) < 0.1 <=>
## 2 * 2.5758 * (2/sqrt(n)) < 0.1 <=>
## (2 * 2.5758 * 2) / sqrt(n) < 0.1 <=>
## 10.3032 / sqrt(n) < 0.1 <=>
## 10.3032 / 0.1 < sqrt(n) <=>
## sqrt(n) > 10.3032 / 0.1 <=>
## sqrt(n) > 103.032 <=>
## n > 103.032^2 <=>
## n > 10615.59

## n tem de ser >= 10616

##### 5) #####

## I.C. para a μ a 95%

# I.C.: ] x̅ - (z_(1 - (α/2))) * (σ / sqrt(n)) , x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) [

# α = 1 - confiança = 1 - 0.95 = 0.05
# x̅ = 78.3
# z_(1 - (α/2)) = z(1 - (0.05/2)) = z_(0.975) = qnorm(0.975) = 1.96
# (σ / sqrt(n)) = (2 / sqrt(25)) = 0.4
# z_(1 - (α/2)) * (σ / sqrt(n)) = 1.96 * 0.4 = 0.784

# I.C.: ] 78.3 - 0.784, 78.3 + 0.784 [ = ] 77.516, 79.084 [

##### 6) #####

# Diminuir o grau de confiança (mantendo o nº de elementos da amostra)
# faz diminuir a amplitude.

# Aumentar o nº de elementos da amostra mantendo o grau de
# confiança faz diminuir a amplitude.

##### 7) #####

# . . . . . . . . . .

"----------------------------------------------------------------------"

#### Exercicio 4.2 ####

# População:
## Normal
## σ = 12g

# Amostra:
amostra_42 <- c(983, 992, 1011, 976, 997, 1000, 1004, 983, 998)
length(amostra_42) # = 9
## n = 9

##### 1) #####

mean(amostra_42) # x̅ = 993.7778

##### 2) #####

# I.C.: ] x̅ - (z_(1 - (α/2))) * (σ / sqrt(n)) , x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) [

BSDA::z.test(
  x = amostra_42,    # Vetor com a amostra
  sigma.x = 12,      # Desvio Padrão da População
  conf.level = 0.90  # Grau de Confiança para o teste/intervalo
)

BSDA::z.test(
  x = amostra_42,    # Vetor com a amostra
  sigma.x = 12,      # Desvio Padrão da População
  conf.level = 0.95  # Grau de Confiança para o teste/intervalo
)

BSDA::z.test(
  x = amostra_42,    # Vetor com a amostra
  sigma.x = 12,      # Desvio Padrão da População
  conf.level = 0.99  # Grau de Confiança para o teste/intervalo
)

##### 3) #####

# . . . . . . . . . .

##### 4) #####

# . . . . . . . . . .

"----------------------------------------------------------------------"

#### Exercicio 4.3 ####

# X - Distância percorrida por um avião em milhares de metros,
#     desde o contacto com o solo até à imobilização total.
# X ~ N(μ, σ)
# Amostra: n = 31.
# Σ(xi) = 54.3
# Σ(xi^2) = 95.57

##### 1) #####

# x̅ = ?

# x̅ = (1/n) * Σ(xi)
# = (1/31) * 54.3
# = 1.7516 milhares de metros

# S^2 = ?

# S^2 = (1/n) * (Σ(xi^2) - n * x̅^2)
# = (1/31) * (95.57 - (31 * (1.7516^2)))
# = 0.0148 milhares de metros^2

##### 2) #####

# 99% de Confiança

# I.C.: ] x̅ - (t_(1 - (α/2)); n-1) * (s / sqrt(n)) , x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n)) [

# x̅ = 1.7516
# α = 0.01
# S = sqrt(S^2) = sqrt(0.0148) = 0.1217
# (t_(1 - (α/2)); n-1) = t_(0.995; 30) = qt(0.995, 30) = 2.75
# (s / sqrt(n)) = 0.1217 / sqrt(31) = 0.0219
# (t_(1 - (α/2)); n-1) * (s / sqrt(n)) = 2.75 * 0.0219 = 0.0602

# I.C. a 99% = ] 1.7516 - 0.0602, 1.7516 + 0.0602 [
#            = ] 1.6914, 1.8118 [

# Não é possível, pois como indicado no intervalo de confiança elaborado,
# verifica-se que a distância segura varia entre 1.6914 e 1.8118 milhares
# de metros, pelo que 1500 metros se encontra fora desse intervalo
# (para 99% de confiança).

# OU

# Com 99% de confiança, não parece ser possível efetuar uma aterragem segura.

"----------------------------------------------------------------------"

#### Exercicio 4.4 ####

(amostra_44 <- c(
  33.1, 32.1, 40.9, 37.1, 37.7, 35.1, 30.2, 45.6, 27.8, 37.3
))

# População:
## Normal;
## Parâmetros Desconhecidos.

# Amostra:
## n = 10.

##### 1) #####

# x̅ = ?
# S = ?

# x̅ = mean(amostra_44) = 35.69 kg
# S = sd(amostra_44) = 5.2314 kg

##### 2) #####

# 99% de Confiança

# I.C.: ] x̅ - (t_(1 - (α/2)); n-1) * (s / sqrt(n)) , x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n)) [

# x̅ = 35.69
# α = 0.01
# S = 5.2314
# (t_(1 - (α/2)); n-1) = t_(0.995; 9) = qt(0.995, 9) = 3.2498
# (s / sqrt(n)) = 5.2314 / sqrt(10) = 1.6543
# (t_(1 - (α/2)); n-1) * (s / sqrt(n)) = 3.2498 * 1.6543 = 5.3761

# I.C. a 99% = ] 35.69 - 5.3761, 35.69 + 5.3761 [
#            = ] 30.3139, 41.0661 [

# OU

t.test(
  x = amostra_44,    # Vetor com a amostra
  mu = 35.69,        # Média
  conf.level = 0.99  # Grau de Confiança para o teste/intervalo
)

##### 3) #####

# Para diminuir a amplitude do intervalo, podemos fazer um dos seguintes pontos:
# -- Diminuir o grau de confiança (mantendo o nº de elementos da amostra);
# -- Aumentar o nº de elementos da amostra (mantendo o grau de confiança).

##### 4) #####

# n = ?

# I.C.: ] x̅ - (t_(1 - (α/2)); n-1) * (s / sqrt(n)) , x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n)) [

# Amplitude = max - min
# x̅ = 35.69
# α = 0.01
# S = 5.2314

# x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n)) - (x̅ - (t_(1 - (α/2)); n-1) * (s / sqrt(n))) = 3 <=>
# x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n)) - x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n))) = 3 <=>
# (t_(1 - (α/2)); n-1) * (s / sqrt(n)) + (t_(1 - (α/2)); n-1) * (s / sqrt(n))) = 3 <=>
# 2 * (t_(1 - (α/2)); n-1) * (s / sqrt(n)) = 3 <=>
# 2 * (t_(1 - (0.01/2)); n-1) * (5.2314 / sqrt(n)) = 3 <=>
# 2 * t_(0.995; n-1) * (5.2314 / sqrt(n)) = 3 <=>
# t_(0.995; n-1) * (10.4628 / sqrt(n)) = 3 <=>
# t_(0.995; n-1) * 10.4628 = 3 / sqrt(n) <=>
# 3 / sqrt(n) = t_(0.995; n-1) * 10.4628 <=>
# sqrt(n) = 3 / (t_(0.995; n-1) * 10.4628) <=>
# . . . . . . . .

"----------------------------------------------------------------------"

#### Exercicio 4.5 ####

# População:
## Normal;
## Parâmetros Desconhecidos.

# Amostra:
## n = 16.

# I.C. para a Média: ] 7.05, 12.95 [

##### 1) #####

# x̅ = ?
# x̅ = (7.05 + 12.95) / 2 = 10

##### 2) #####

# S = 4
# α = ?

# I.C.: ] x̅ - (t_(1 - (α/2)); n-1) * (s / sqrt(n)) , x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n)) [

# x̅ - (t_(1 - (α/2)); n-1) * (s / sqrt(n)) = 7.05 <=>
# 10 - (t_(1 - (α/2)); 15) * (4 / sqrt(16)) = 7.05 <=>
# 10 - (t_(1 - (α/2)); 15) * 1 = 7.05 <=>
# 10 - (t_(1 - (α/2)); 15) = 7.05 <=>
# 10 - 7.05 = (t_(1 - (α/2)); 15) <=>
# t_((1 - (α/2)); 15) = 10 - 7.05 <=>
# t_((1 - (α/2)); 15) = 2.95 <=>
# qt(1 - (α/2), 15) = 2.95 <=>
# pt(2.95, 15) = 1 - (α/2) <=>
# 0.9950 = 1 - (α/2) <=>
# 1 - (α/2) = 0.9950 <=>
# (α/2) = 1 - 0.9950 <=>
# α = 0.005 * 2 <=>
# α = 0.01

# Grau de Confiança = 1 - α = 1 - 0.01 = 0.99

# 99% de Confiança

##### 3) #####

# σ^2 = 44
# 95% de Confiança
# Amplitude = max - min
# Amplitude <= 3.5
# n = ?

# α = 1 - confiança = 1 - 0.95 = 0.05
# σ = sqrt(44) = 6.6333

# I.C.: ] x̅ - (z_(1 - (α/2))) * (σ / sqrt(n)) , x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) [

# x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) - (x̅ - (z_(1 - (α/2))) * (σ / sqrt(n))) <= 3.5 <=>
# x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) - x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) <= 3.5 <=>
# (z_(1 - (α/2))) * (σ / sqrt(n)) + (z_(1 - (α/2))) * (σ / sqrt(n)) <= 3.5 <=>
# 2 * z_(0.975) * (6.6333 / sqrt(n)) <= 3.5 <=>
# 2 * qnorm(0.975) * (6.6333 / sqrt(n)) <= 3.5 <=>
# 2 * 1.96 * (6.6333 / sqrt(n)) <= 3.5 <=>
# 3.92 * 6.6333 / sqrt(n) <= 3.5 <=>
# 26.0025 / sqrt(n) <= 3.5 <=>
# sqrt(n) <= 26.0025 / 3.5 <=>
# sqrt(n) <= 7.4293 <=>
# n <= 7.4293^2 <=>
# n <= 55.1945

# n <= 55

# n >= 56 ?

"----------------------------------------------------------------------"

#### Exercicio 4.6 ####

# População 1:
## Normal;
## σ1 = 3.

# Amostra 1:
## n1 = 10.

# População 2:
## Normal;
## σ2 = 3.

# Amostra 2:
## n2 = 10.

(amostra1_46 <- c(
  57.9, 66.2, 65.4, 65.2, 62.6, 67.6, 63.7, 67.2, 71.0, 65.4
))

(amostra2_46 <- c(
  66.4, 71.7, 70.3, 69.3, 68.8, 69.6, 68.6, 69.4, 65.3, 68.8
))

##### 1) #####

# As amostras podem ser consideradas como independentes.
# Cada uma referente apenas ao seu próprio catalisador.

##### 2) #####

# I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((σ1^2 / n1) + (σ2^2 / n2))) [

BSDA::z.test(
  x = amostra1_46,   # Primeira Amostra
  sigma.x = 3,       # Desvio Padrão da Amostra 1
  y = amostra2_46,   # Segunda Amostra
  sigma.y = 3,       # Desvio Padrão da Amostra 2
  conf.level = 0.95  # Grau de Confiança
)

# I.C.: ] -6.2296, -0.9704 [

# Com 95% de confiança os dados obtidos pelos dois catalizadores não podem
# ser considerados iguais, uma vez que o 0 não está presente no intervalo
# da diferença de médias.

"----------------------------------------------------------------------"

#### Exercicio 4.7 ####

# Populações:
## Desconhecidas/Quaisquer;
## Parâmetros Desconhecidos.

# Amostra 1 - Homens:
## n1 = 250;
## x̅1 = 33.8;
## S1^2 = 5.7.

# Amostra 2 - Mulheres:
## n2 = 150;
## x̅2 = 31;
## S2^2 = 10.3.

# 99% de Confiança
# α = 1 - 0.99 = 0.01

# I.C. para Diferença de Médias:

# I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((s1^2 / n1) + (s2^2 / n2)) [

# (x̅1 - x̅2) = 33.8 - 31 = 2.8
# z_(1 - (α/2)) = qnorm(1 - (0.01/2)) = 2.5758
# sqrt((s1^2 / n1) + (s2^2 / n2)) = sqrt((5.7 / 250) + (10.3 / 150)) = 0.3024
# z_(1 - (α/2)) * sqrt((s1^2 / n1) + (s2^2 / n2)) = 2.5758 * 0.3024 = 0.7789

# I.C. = ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((s1^2 / n1) + (s2^2 / n2)) [
#      = ] 2.8 - 0.7789, 2.8 + 0.7789 [
#      = ] 2.0211, 3.5789 [

# Com 99% de confiança os dados dos salários obtidos pelos dois géneros
# não podem ser considerados iguais, uma vez que o 0 não está presente
# no intervalo da diferença de médias, pelo que parece existir alguma
# discriminação de género na atribuição de remunerações.

"----------------------------------------------------------------------"

#### Exercicio 4.8 ####



##### 1) #####



##### 2) #####



"----------------------------------------------------------------------"

#### Exercicio 4.9 ####



##### 1) #####



##### 2) #####



"----------------------------------------------------------------------"

#### Exercicio 4.10 ####



##### 1) #####



##### 2) #####



##### 3) #####



"----------------------------------------------------------------------"

#### Exercicio 4.11 ####



##### 1) #####



##### 2) #####



"----------------------------------------------------------------------"

#### Exercicio 4.12 ####



"----------------------------------------------------------------------"

#### Exercicio 4.13 ####



"----------------------------------------------------------------------"

#### Exercicio 4.14 ####



"----------------------------------------------------------------------"

#### Exercicio 4.15 ####



"----------------------------------------------------------------------"

#### Exercicio 4.16 ####



"----------------------------------------------------------------------"

#### Exercicio 4.17 ####



##### 1) #####



##### 2) #####



##### 3) #####

###### a) ######



###### b) ######



"----------------------------------------------------------------------"

#### Exercicio 4.18 ####



##### 1) #####



##### 2) #####

###### a) ######



###### b) ######



###### c) ######



"----------------------------------------------------------------------"

#### Exercicio 4.19 ####



"----------------------------------------------------------------------"

#### Exercicio 4.20 ####



"----------------------------------------------------------------------"

#### Exercicio 4.21 ####



"----------------------------------------------------------------------"

#### Exercicio 4.22 ####



"----------------------------------------------------------------------"

#### Exercicio 4.23 ####



##### 1) #####



##### 2) #####



##### 3) #####



##### 4) #####



##### 5) #####



"----------------------------------------------------------------------"
