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

# Como existirá um crescimento drástico do n e ficará n >= 30,
# deduzido também pela alínia 2), tem-se:

# I.C.: ] x̅ |-+| (z_(1 - (α/2))) * (s / sqrt(n)) [

# Amplitude = max - min
# x̅ = 35.69
# α = 0.01
# S = 5.2314

# x̅ + (z_(1 - (α/2))) * (S / sqrt(n)) - (x̅ - (z_(1 - (α/2))) * (S / sqrt(n))) = 3 <=>
# x̅ + (z_(1 - (α/2))) * (S / sqrt(n)) x̅ + (z_(1 - (α/2))) * (S / sqrt(n))) = 3 <=>
# 2 * (z_(1 - (α/2))) * (S / sqrt(n)) = 3 <=>
# 2 * (z_(1 - (0.01/2))) * (5.2314 / sqrt(n)) = 3 <=>
# 2 * qnorm(0.995) * (5.2314 / sqrt(n)) = 3 <=>
# 2 * 2.5758 * (5.2314 / sqrt(n)) = 3 <=>
# 5.1516 * 5.2314 / sqrt(n) = 3 <=>
# 26.9501 / sqrt(n) = 3 <=>
# 26.9501 / 3 = sqrt(n) <=>
# sqrt(n) = 26.9501 / 3 <=>
# sqrt(n) = 8.9834 <=>
# n = 8.9834^2 <=>
# n = 80.7015

# n >= 81

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
# n = ? >= 56

# α = 1 - confiança = 1 - 0.95 = 0.05
# σ = sqrt(44) = 6.6333

# I.C.: ] x̅ |-+| (z_(1 - (α/2))) * (σ / sqrt(n)) [

# x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) - (x̅ - (z_(1 - (α/2))) * (σ / sqrt(n))) <= 3.5 <=>
# x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) - x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) <= 3.5 <=>
# (z_(1 - (α/2))) * (σ / sqrt(n)) + (z_(1 - (α/2))) * (σ / sqrt(n)) <= 3.5 <=>
# 2 * z_(1 - (α/2)) * (σ / sqrt(n)) <= 3.5 <=>
# 2 * z_(0.975) * (6.6333 / sqrt(n)) <= 3.5 <=>
# 2 * qnorm(0.975) * (6.6333 / sqrt(n)) <= 3.5 <=>
# 2 * 1.96 * (6.6333 / sqrt(n)) <= 3.5 <=>
# 3.92 * 6.6333 / sqrt(n) <= 3.5 <=>
# 26.0025 / sqrt(n) <= 3.5 <=>
# sqrt(n) >= 26.0025 / 3.5 <=>
# sqrt(n) >= 7.4293 <=>
# n >= 7.4293^2 <=>
# n >= 55.1945
# n >= 56

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

# População Normal;
# Amostras Emparelhadas;
# n = 10.

(amostra_48_antes <- c(
  147, 122, 127, 141, 150, 132, 157, 147, 157, 155
))

(amostra_48_depois <- c(
  132, 117, 142, 124, 116, 130, 122, 118, 135, 117
))

##### 1) #####

# As amostras são sobre a mesma referência (mesmos pacientes),
# apenas retratam estados diferentes, logo, são emparelhadas.

##### 2) #####

t.test(
  x = amostra_48_depois,  # Primeira Amostra
  y = amostra_48_antes,   # Segunda Amostra
  paired = TRUE,          # As Amostras são Emparelhadas?
  conf.level = 0.95       # Grau de Confiança
)

# I.C. => ] -30.3718, -6.0282 [

# Com 95% de certeza, o programa de reabilitação parece ser eficaz.

"----------------------------------------------------------------------"

#### Exercicio 4.9 ####

# População Normal;
# Amostras Emparelhadas;
# n = 8.

(amostra_49_antes <- c(
  4, 10, 8, 13, 7, 3, 15, 7
))

(amostra_49_depois <- c(
  4, 16, 11, 17, 17, 4, 18, 11
))

##### 1) #####

# Não, pois as amostras são sobre a mesma referência (mesmos utentes),
# apenas retratam estados diferentes, logo, são emparelhadas.

##### 2) #####

t.test(
  x = amostra_49_depois,  # Primeira Amostra
  y = amostra_49_antes,   # Segunda Amostra
  paired = TRUE,          # As Amostras são Emparelhadas?
  conf.level = 0.99       # Grau de Confiança
)

# I.C. => ] 0.0508,7.6992 [

# Com 99% de certeza, a aplicação da nova prótese influenciou o grau
# de satisfação dos utentes.

"----------------------------------------------------------------------"

#### Exercicio 4.10 ####

# Populações:
## Normais;
# σ1 = σ2;

# Amostra 1:
## n1 = 13;
## x̅1 = 74.5;
## S1^2 = 82.6.

# Amostra 2:
## n2 = 11;
## x̅2 = 71.8;
## Σ(x2i - x̅2)^2 = 1126;
## S2^2 = (1/(n-1)) * Σ(x2i - x̅2)^2 = (1126 / 10) = 112.6.

# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))))
# T ~ t(n1 + (n2 - 2))
# I.C.: ] (x̅1 - x̅2) |-+| t_(1 - (α/2); n1 + (n2 - 2)) * sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))) [

# I.C. => ] -5.635, 11.035 [

##### 1) #####

# (x̅1 - x̅2) + t_(1 - (α/2); n1 + (n2 - 2)) * sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))) = 11.035

## (x̅1 - x̅2) = 74.5 - 71.8 = 2.7
## t_(1 - (α/2); n1 + (n2 - 2)) = t_(1-(α/2); 13 + (11 - 2)) = t_(1-(α/2); 22)
## sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))) = sqrt(((1 / 13) + (1 / 11)) * ((((13 - 1) * 82.6) + ((11 - 1) * (1126/10))) / (13 + (11 - 2)))) = 4.0189

# (x̅1 - x̅2) + t_(1 - (α/2); n1 + (n2 - 2)) * sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))) = 11.035 <=>
# 2.7 + t_(1-(α/2); 22) * 4.0189 = 11.035 <=>
# t_(1-(α/2); 22) * 4.0189 = 11.035 - 2.7 <=>
# t_(1-(α/2); 22) * 4.0189 = 8.335 <=>
# t_(1-(α/2); 22) = 8.335 / 4.0189 <=>
# t_(1-(α/2); 22) = 2.074 <=>
# qt(1-(α/2), 22) = 2.074 <=>
# pt(2.074, 22) = 1-(α/2) <=>
# 0.975 = 1 - (α/2) <=>
# α / 2 = 1 - 0.975 <=>
# α / 2 = 0.025 <=>
# α = 0.025 * 2 <=>
# α = 0.05

# I.C. a 95% de Confiança

##### 2) #####

# 90% de Confiança => α = 0.1

# I.C.: ] (x̅1 - x̅2) |-+| t_(1 - (α/2); n1 + (n2 - 2)) * sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))) [

## (x̅1 - x̅2) = 74.5 - 71.8 = 2.7
## t_(1 - (α/2); n1 + (n2 - 2)) = t_(1-(0.1/2); 13 + (11 - 2)) = qt(0.95, 22) = 1.7171
## sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))) = sqrt(((1 / 13) + (1 / 11)) * ((((13 - 1) * 82.6) + ((11 - 1) * (1126/10))) / (13 + (11 - 2)))) = 4.0189

# ] (x̅1 - x̅2) |-+| t_(1 - (α/2); n1 + (n2 - 2)) * sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))) [ =
# ] 2.7 |-+| 1.7171 * 4.0189 [ =
# ] 2.7 |-+| 6.9009 [ =
# ] 2.7 - 6.9009, 2.7 + 6.9009 [ =
# ] -4.2009, 9.6009 [

# Com 90% de confiança, pode-se dizer que os métodos de ensino podem ser considerados iguais.

##### 3) #####

# P(s1^2 = s2^2) = P(s1^2 / s2^2 = 1) = ?

# I.C. a 90% Confiança
# α = 1 - 0.9 = 0.1

# D.A.: F = ((s1^2 / s2^2) * (σ2^2 / σ1^2)) ~ F(n1 - 1, n2 - 1)
# I.C.: ] ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) , ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) [

# ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) =
# (1 / qf(1 - (0.1/2), 12, 10)) * (82.6 / 112.6) =
# 0.2518

# ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) =
# (1 / qf(0.1/2, 12, 10)) * (82.6 / 112.6) =
# 2.0198

# I.C. => ] 0.2518, 2.0198 [

# Com 90% de confiança, pode-se dizer que as variâncias podem ser iguais
# (uma vez que o 1 pertence ao intervalo).

"----------------------------------------------------------------------"

#### Exercicio 4.11 ####

# Populações Normais;
# σ1 = σ2;
# Amostras Independentes.

(amostra_411_com_aspirina <- c(
  9.6, 9.4, 9.3, 11.2, 11.4, 12.1, 10.4, 9.6, 10.2, 8.8, 13.0
))

(amostra_411_sem_aspirina <- c(
  11.4, 12.1, 10.4, 9.6, 8.5, 9.7, 12.3, 12.4, 10.8, 10.8
))

##### 1) #####

# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))))
# T ~ t(n1 + (n2 - 2))
# I.C.: ] (x̅1 - x̅2) |-+| t_(1 - (α/2); n1 + (n2 - 2)) * sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))) [
t.test(
  x = amostra_411_com_aspirina,   # Primeira Amostra
  y = amostra_411_sem_aspirina,   # Segunda Amostra
  paired = FALSE,                 # As Amostras são Independentes
  var.equal = TRUE,               # As Variâncias são Iguais?
  conf.level = 0.95               # Grau de Confiança
)

# I.C. = ] -1.5380, 0.8471 [

# Com 95% de confiança pode-se dizer que as respostas de ambos os medicamentos
# podem, em média, ser consideradas iguais.

##### 2) #####

# P(s1^2 = s2^2) = P(s1^2 / s2^2 = 1) = ?

# I.C. a 95% Confiança
# α = 1 - 0.95 = 0.05

var.test(
  x = amostra_411_com_aspirina,  # Primeira Amostra
  y = amostra_411_sem_aspirina,  # Segunda Amostra
  conf.level = 0.95              # Grau de Confiança
)

# OU

# I.C.: ] ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) , ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) [

# ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) =
# (1 / qf(1 - (0.05/2), length(amostra_411_com_aspirina) - 1, length(amostra_411_sem_aspirina) - 1)) * (var(amostra_411_com_aspirina) / var(amostra_411_sem_aspirina)) =
# 0.2633

# ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) =
# (1 / qf(0.05/2, length(amostra_411_com_aspirina) - 1, length(amostra_411_sem_aspirina) - 1)) * (var(amostra_411_com_aspirina) / var(amostra_411_sem_aspirina)) =
# 3.9438

# I.C. => ] 0.2633, 3.9438 [

# Com 90% de confiança, pode-se dizer que as variâncias podem ser iguais
# (uma vez que o 1 pertence ao intervalo).

"----------------------------------------------------------------------"

#### Exercicio 4.12 ####

# População:
## Normal.

# Amostra:
## n = 15
## x̅ = 134.5mm
## S = 3.5mm

# I.C. a 95% de Confiança
# α = 1 - 0.95 = 0.05

# I.C.: ] (((n-1) * s^2) / x^2_(1 - (α/2); n-1)) , (((n-1) * s^2) / x^2_(α/2; n-1)) [

# (((n-1) * s^2) / x^2_(1 - (α/2); n-1)) =
# (((15-1) * 3.5^2) / qchisq(1 - (0.05/2), 15-1)) =
# 6.5661mm^2

# (((n-1) * s^2) / x^2_(α/2; n-1)) =
# (((15-1) * 3.5^2) / qchisq(0.05/2, 15-1)) =
# 30.4687mm^2

# I.C. = ] sqrt(6.5661), sqrt(30.4687) [
# I.C. = ] 2.56244, 5.5198 [

"----------------------------------------------------------------------"

#### Exercicio 4.13 ####

# População:
## Normal.

# I.C. a 95% de Confiança
# α = 1 - 0.95 = 0.05

(amostra_413 <- c(
  6.5, 6.6, 6.7, 6.8, 7.1, 7.3, 7.4, 7.7, 7.7, 7.7
))

EnvStats::varTest(
  x = amostra_413,   # Amostra
  conf.level = 0.95  # Grau de Confiança
)

# I.C. = ] 0.1075, 0.7573 [
#      = ] sqrt(0.1075), sqrt(0.7573) [
#      = ] 0.3279, 0.8702 [

"----------------------------------------------------------------------"

#### Exercicio 4.14 ####

# População:
## Normal.

# I.C. a 95% de Confiança
# α = 1 - 0.95 = 0.05

(amostra_414 <- c(
  9, 14, 10, 12, 7, 3, 11, 12
))

EnvStats::varTest(
  x = amostra_414,   # Amostra
  conf.level = 0.99  # Grau de Confiança
)

# I.C. = ] 4.1178, 84.4069 [

"----------------------------------------------------------------------"

#### Exercicio 4.15 ####

# Populações Normais.

# Amostra 1 (Escola A):
## n1 = 16
## S1^2 = 6.62

# Amostra 2 (Escola B):
## n2 = 21
## S2^2 = 3.80

# I.C. a 90% de Confiança
# α = 1 - 0.9 = 0.1

# I.C.: ] ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) , ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) [

# ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) =
# (1 / qf(1 - (0.1/2), 16 - 1, 21 - 1)) * (6.62 / 3.80) =
# 0.7907

# ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) =
# (1 / qf(0.1/2, 16 - 1, 21 - 1)) * (6.62 / 3.80) =
# 4.0548

# I.C. => ] 0.7907, 4.0548 [

# Com 95% de confiança pode-se dizer que a variabilidade das escolas não
# pode ser considerada diferente, pois as variâncias podem ser consideradas iguais.

"----------------------------------------------------------------------"

#### Exercicio 4.16 ####

# Amostra:
## n = 2500

# p* = 850 / 2500 = 85 / 250 = 0.34

"95%"

# I.C. a 95% de Confiança
# α = 1 - 0.95 = 0.05

# I.C.: ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [

# p* = 0.34
# z_(1 - (α/2)) = qnorm(1 - (0.05/2)) = 1.96
# sqrt((p* * q*) / n) = sqrt((0.34 * (1 - 0.34)) / 2500) = 0.0095

# I.C. = ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [
#      = ] 0.34 - 1.96 * 0.0095 , 0.34 + 1.96 * 0.0095 [
#      = ] 0.34 - 0.0186 , 0.34 + 0.0186 [
#      = ] 0.3214 , 0.3586 [

"98%"

# I.C. a 98% de Confiança
# α = 1 - 0.98 = 0.02

# I.C.: ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [

# p* = 0.34
# z_(1 - (α/2)) = qnorm(1 - (0.02/2)) = 2.3263
# sqrt((p* * q*) / n) = sqrt((0.34 * (1 - 0.34)) / 2500) = 0.0095

# I.C. = ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [
#      = ] 0.34 - 2.3263 * 0.0095 , 0.34 + 2.3263 * 0.0095 [
#      = ] 0.34 - 0.0221 , 0.34 + 0.0221 [
#      = ] 0.3179 , 0.3621 [

"----------------------------------------------------------------------"

#### Exercicio 4.17 ####

# Amostra:
## n = 400

# I.C. para a Proporção: ] 0.5114, 0.6086 [

##### 1) #####

# Como o intervalo da proporção é dado por ] 0.5114, 0.6086 [:
# p* = (0.5114 + 0.6086) / 2 = 0.56

# A percentagem de pessoas recetivas a
# um novo tipo de espuma de banho foi de 56%.

##### 2) #####

# I.C.: ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [

# p* - z_(1 - (α/2)) * sqrt((p* * q*) / n) = 0.5114 <=>
# 0.56 - qnorm(1 - (α/2)) * sqrt((0.56 * (1 - 0.56)) / 400) = 0.5114 <=>
# 0.56 - 0.5114 = qnorm(1 - (α/2)) * sqrt((0.56 * (1 - 0.56)) / 400) <=>
# 0.0486 = qnorm(1 - (α/2)) * sqrt((0.56 * (1 - 0.56)) / 400) <=>
# qnorm(1 - (α/2)) * 0.0248 = 0.0486 <=>
# qnorm(1 - (α/2)) = 0.0486 / 0.0248 <=>
# qnorm(1 - (α/2)) = 1.9697 <=>
# 1 - (α/2) = pnorm(1.9697) <=>
# 1 - (α/2) = 0.9756 <=>
# α / 2 = 1 - 0.9756 <=>
# α / 2 = 0.0244 <=>
# α = 0.0244 * 2 <=>
# α = 0.0488

# Grau de Confiança = 1 - α = 1 - 0.0488 = 0.9512 = 95%

##### 3) #####

###### a) ######

# A afirmação é incorreta => 95% corresponde ao grau de confiança do
# I.C., e não à percentagem de pessoas.

###### b) ######

# A afirmação é incorreta => Apesar de 95% estar correta e ser o grau
# de confiança do I.C., 56% não corresponde à quota de mercado, mas sim
# à percentagem de pessoas recetivas ao novo tipo de espuma de banho.

"----------------------------------------------------------------------"

#### Exercicio 4.18 ####

# Amostra:
## n = 40
## casos favoráveis = 10

##### 1) #####

# P* = 10 / 40 = 1 / 4 = 0.25

# I.C. a 95% de Confiança
# α = 1 - 0.95 = 0.05

# I.C.: ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [

# p* = 0.25
# z_(1 - (α/2)) = qnorm(1 - (0.05/2)) = 1.96
# sqrt((p* * q*) / n) = sqrt((0.25 * (1 - 0.25)) / 40) = 0.0685

# I.C. = ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [
#      = ] 0.25 - 1.96 * 0.0685 , 0.25 + 1.96 * 0.0685 [
#      = ] 0.25 - 0.1343 , 0.25 + 0.1343 [
#      = ] 0.1157 , 0.3843 [

##### 2) #####

# Amplitude = (max - min) = (0.3843 - 0.1157) = 0.2686
# Amplitude Desejada (Metade): 0.2686 / 2 = 0.1343

###### a) ######

# Confiança = ?

# L_sup - L_inf = 0.1343 <=>
# p* + z_(1 - (α/2)) * sqrt((p* * q*) / n) - (p* - z_(1 - (α/2)) * sqrt((p* * q*) / n)) = 0.1343 <=>
# z_(1 - (α/2)) * sqrt((p* * q*) / n) + z_(1 - (α/2)) * sqrt((p* * q*) / n)) = 0.1343 <=>
# 2 * z_(1 - (α/2)) * sqrt((p* * q*) / n) = 0.1343 <=>
# 2 * z_(1 - (α/2)) * sqrt((0.25 * (1 - 0.25)) / 40) = 0.1343 <=>
# 2 * z_(1 - (α/2)) * 0.0685 = 0.1343 <=>
# 2 * 0.0685 * z_(1 - (α/2)) = 0.1343 <=>
# 0.137 * z_(1 - (α/2)) = 0.1343 <=>
# z_(1 - (α/2)) = 0.1343 / 0.137 <=>
# 1 - (α/2) = pnorm(0.9803) <=>
# 1 - (α/2) = 0.8365 <=>
# 1 - 0.8365 = (α/2) <=>
# 0.1635 = (α/2) <=>
# α = 0.1635 * 2 <=>
# α = 0.327

# Grau de Confiança = 1 - α = 1 - 0.327 = 0.673 = 67.3%

###### b) ######

# n = ?

# P* = 0.25

# 95% de Confiança
# α = 1 - 0.95 = 0.05

# L_sup - L_inf = 0.1343 <=>
# p* + z_(1 - (α/2)) * sqrt((p* * q*) / n) - (p* - z_(1 - (α/2)) * sqrt((p* * q*) / n)) = 0.1343 <=>
# z_(1 - (α/2)) * sqrt((p* * q*) / n) + z_(1 - (α/2)) * sqrt((p* * q*) / n)) = 0.1343 <=>
# 2 * z_(1 - (α/2)) * sqrt((p* * q*) / n) = 0.1343 <=>
# 2 * qnorm(1 - (0.05/2)) * sqrt((0.25 * (1 - 0.25)) / n) = 0.1343 <=>
# 3.9199 * sqrt((0.25 * (1 - 0.25)) / n) = 0.1343 <=>
# sqrt((0.25 * (1 - 0.25)) / n) = 0.1343 / 3.9199 <=>
# sqrt(0.1875 / n) = 0.0343 <=>
# 0.1875 / n = 0.0343^2 <=>
# 0.1875 / n = 0.0343^2 <=>
# 0.1875 / 0.0343^2 = n <=>
# n = 0.1875 / 0.0343^2 <=>
# n = 159.3724

# n >= 160

###### c) ######

# n = ?

# P* = ? = 0.5

# 95% de Confiança
# α = 1 - 0.95 = 0.05

# L_sup - L_inf = 0.1343 <=>
# p* + z_(1 - (α/2)) * sqrt((p* * q*) / n) - (p* - z_(1 - (α/2)) * sqrt((p* * q*) / n)) = 0.1343 <=>
# z_(1 - (α/2)) * sqrt((p* * q*) / n) + z_(1 - (α/2)) * sqrt((p* * q*) / n)) = 0.1343 <=>
# 2 * z_(1 - (α/2)) * sqrt((p* * q*) / n) = 0.1343 <=>
# 2 * qnorm(1 - (0.05/2)) * sqrt((p* * q*) / n) = 0.1343 <=>
# 2 * 1.96 * sqrt((p* * q*) / n) = 0.1343 <=>
# 2 * 1.96 * sqrt((p* * q*) / n) = 0.1343 <=>
# 3.92 * sqrt((p* * q*) / n) = 0.1343 <=>
# sqrt((p* * q*) / n) = 0.1343 / 3.92 <=>
# sqrt((p* * q*) / n) = 0.0343 <=>
# (p* * q*) / n = 0.0343^2 <=>
# (p* * (1 - p*)) / n = 0.0343^2 <=>
# p* * (1 - p*) = 0.0343^2 * n <=>
# n = (p* * (1 - p*)) / 0.0343^2 <=>
# n = (0.5 * (1 - 0.5)) / 0.0343^2 <=>
# n = 0.25 / 0.0343^2 <=>
# n = 212.4965

# n >= 213

"----------------------------------------------------------------------"

#### Exercicio 4.19 ####

# Para a Proporção

# I.C. a 95% de Confiança
# α = 1 - 0.95 = 0.05

# Como p* = ?, p* = 0.5

# I.C.: ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [

# Margem de Erro = z_(1 - (α/2)) * sqrt((p* * q*) / n) = 0.04
# Amplitude = 0.04 * 2 = 0.08

# z_(1 - (α/2)) * sqrt((p* * q*) / n) = 0.04 <=>
# qnorm(1 - (0.05/2)) * sqrt((p* * q*) / n) = 0.04 <=>
# 1.96 * sqrt((p* * q*) / n) = 0.04 <=>
# sqrt((p* * q*) / n) = 0.04 / 1.96 <=>
# sqrt((p* * q*) / n) = 0.0204 <=>
# (p* * q*) / n = 0.0204^2 <=>
# (0.5 * (1 - 0.5)) / n = 0.0204^2 <=>
# 0.25 / n = 0.0204^2 <=>
# n = 0.25 / 0.0204^2 <=>
# n = 600.7305

# n >= 601

"----------------------------------------------------------------------"

#### Exercicio 4.20 ####

# Amostra:
## n = 28000.

# p* = 216 / 300 = 0.72

# I.C. a 98% de Confiança
# α = 1 - 0.98 = 0.02

# I.C.: ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [

# p* = 0.72
# z_(1 - (α/2)) = qnorm(1 - (0.02/2)) = 2.3263
# sqrt((p* * q*) / n) = sqrt((0.72 * (1 - 0.72)) / 300) = 0.02592

# I.C. = ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [
#      = ] 0.72 - 2.3263 * 0.02592 , 0.72 + 2.3263 * 0.02592 [
#      = ] 0.72 - 0.0603 , 0.72 + 0.0603 [
#      = ] 0.6597 , 0.7803 [

# 0.6597 * 28000 = 18471.6 = 18472
# 0.7803 * 28000 = 21848.4 = 21848

# I.C. para amostra de 300 médicos = ] 0.6597 , 0.7803 [
# I.C. para os 28.000 médicos = ] 18.472 , 21.848 [

"----------------------------------------------------------------------"

#### Exercicio 4.21 ####

# Amostra 1 - 2019:
## n1 = 5000

# Amostra 2 - 2009:
## n2 = 4000

# p1* = 2250 / 5000 = 0.45 (Consumidores)
# p2* = 1750 / 4000 = 0.4375 (Consumidores)

# I.C. a 98% de Confiança
# α = 1 - 0.98 = 0.02

# I.C.: ] (p1* - p2*) |-+| z_(1 - (α/2)) * sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2))) [

# (p1* - p2*) = (0.45 - 0.4375) = 0.0125
# z_(1 - (α/2)) = qnorm(1 - (0.02/2)) = 2.3263
# sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2))) = sqrt(((0.45 * (1 - 0.45)) / 5000) + ((0.4375 * (1 - 0.4375)) / 4000)) = 0.01053677

# I.C.: ] (p1* - p2*) |-+| z_(1 - (α/2)) * sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2))) [
#      = ] 0.0125 - 2.3263 * 0.01053677 , 0.0125 + 2.3263 * 0.01053677 [
#      = ] 0.0125 - 0.02451 , 0.0125 + 0.02451 [
#      = ] -0.01201 , 0.03701 [

# Com 98% pode-se dizer que a afirmação não é correta, uma vez que as
# proporções podem ser consideradas iguais (o 0 pertence ao intervalo).

"----------------------------------------------------------------------"

#### Exercicio 4.22 ####

# Amostra A - Novo Medicamento:
## na = 100
## Curados = 75

# Amostra B - Novo Placebo:
## nb = 100
## Curados = 65

# p1* = 75 / 100 = 0.75
# p2* = 65 / 100 = 0.65

# I.C. a 95% de Confiança
# α = 1 - 0.95 = 0.05

# I.C.: ] (p1* - p2*) |-+| z_(1 - (α/2)) * sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2))) [

# (p1* - p2*) = (0.75 - 0.65) = 0.1
# z_(1 - (α/2)) = qnorm(1 - (0.05/2)) = 1.96
# sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2))) = sqrt(((0.75 * (1 - 0.75)) / 100) + ((0.65 * (1 - 0.65)) / 100)) = 0.06442

# I.C.: ] (p1* - p2*) |-+| z_(1 - (α/2)) * sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2))) [
#      = ] 0.1 - 1.96 * 0.06442 , 0.1 + 1.96 * 0.06442 [
#      = ] 0.1 - 0.1263 , 0.1 + 0.1263 [
#      = ] -0.0263 , 0.2263 [

# Com 95% pode-se dizer que a afirmação não é correta e que o novo
# medicamento não é eficaz, uma vez que as proporções podem ser
# consideradas iguais (o 0 pertence ao intervalo).

"----------------------------------------------------------------------"

#### Exercicio 4.23 ####

View(obesidade)

##### 1) #####

# Var. Altura:
## x̅ = mean(obesidade$Altura) = 1.7017m
## S^2 = var(obesidade$Altura) = 0.008706m^2

##### 2) #####

# I.C. a 99% de Confiança para a média
# α = 1 - 0.99 = 0.01

BSDA::z.test(
  x = obesidade$Altura,           # Vetor com a amostra
  sigma.x = sd(obesidade$Altura), # Desvio Padrão da Amostra
  conf.level = 0.99               # Grau de Confiança para o teste/intervalo
)

# I.C. = ] 1.6964 , 1.7069 [

##### 3) #####

# Margem de Erro = Amplitude / 2
# Amplitude = max - min = 1.7069 - 1.6964 = 0.0105
# Margem de Erro = 0.0105 / 2 = 0.00525m

##### 4) #####

# Amostra 1 - Pesos Femininos:
(amostra1_423 <- obesidade[obesidade$Genero == "Feminino",]$Peso)

# Amostra 2 - Pesos Masculinos:
(amostra2_423 <- obesidade[obesidade$Genero == "Masculino",]$Peso)

# I.C. a 90% de Confiança para a Diferença de Médias
# α = 1 - 0.90 = 0.10

BSDA::z.test(
  x = amostra1_423,            # Primeira Amostra
  sigma.x = sd(amostra1_423),  # Desvio Padrão da Amostra 1
  y = amostra2_423,            # Segunda Amostra
  sigma.y = sd(amostra2_423),  # Desvio Padrão da Amostra 2
  conf.level = 0.90            # Grau de Confiança
)

# I.C. = ] -10.325267 , -6.608962 [

# Com 90% de confiança pode-se dizer que os pesos entre géneros
# feminino e masculino não podem ser considerados iguais.

##### 5) #####

remover_outliers <- function (VARIAVEL) {
  boxplot_outliers_aux <- boxplot(
    VARIAVEL,
    col = "gold",
    horizontal = TRUE,
    main = "Extremos e Quartis - Sem Outliers",
    xlab = "VARIAVEL",
    type = 2,
    range = 1.5
  )
  
  outliers <- boxplot_outliers_aux$out
  
  sem_outliers <- VARIAVEL
  sem_outliers <- sem_outliers[!sem_outliers %in% outliers]
  
  return(sem_outliers)
}

(idade_sem_outliers <- remover_outliers(obesidade$Idade))

###### a) ######

# I.C. a 92% de Confiança para a média
# α = 1 - 0.92 = 0.08

BSDA::z.test(
  x = idade_sem_outliers,           # Vetor com a amostra
  sigma.x = sd(idade_sem_outliers), # Desvio Padrão da Amostra
  conf.level = 0.92                 # Grau de Confiança para o teste/intervalo
)

# I.C. = ] 22.7736 , 23.1156 [

###### b) ######

# I.C. a 95% de Confiança para a média
# α = 1 - 0.95 = 0.05

(idade_comem_sem_outliers <- remover_outliers(obesidade[obesidade$FAVC == 1,]$Idade))
(idade_nao_comem_sem_outliers <- remover_outliers(obesidade[obesidade$FAVC == 0,]$Idade))

# I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((s1^2 / n1) + (s2^2 / n2))) [

BSDA::z.test(
  x = idade_comem_sem_outliers,                # Primeira Amostra
  sigma.x = sd(idade_comem_sem_outliers),      # Desvio Padrão da Amostra 1
  y = idade_nao_comem_sem_outliers,            # Segunda Amostra
  sigma.y = sd(idade_nao_comem_sem_outliers),  # Desvio Padrão da Amostra 2
  conf.level = 0.95                            # Grau de Confiança
)

# I.C. = ] 1.6838, 2.5013 [

# Com 95% de confiança pode-se dizer que, em média, há diferenças nas
# idades das pessoas que comem habitualmente alimentos altamente calóricos
# e das que não comem (para as amostras pedidas - sem outliers).

"----------------------------------------------------------------------"
