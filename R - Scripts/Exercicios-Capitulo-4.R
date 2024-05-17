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

"----------------------------------------------------------------------"
