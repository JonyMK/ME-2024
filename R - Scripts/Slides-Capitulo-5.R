# Capítulo 5 - Testes de Hipóteses Paramétricos ####

"-----------------------------------------------------"

## Exemplo 2 - Slide 23 ####

# População:
## ?

# Amostra:
## n = 1000
## p* = 0.014

"-----------------------"

### 1) ####

# Empresa não adquire se a percentagem de defeitos for superior a 1%.
# p > 0.01

# Passos para os Testes de Hipóteses:

## 1)
### H0: p = 0.01 vs. H1: p > 0.01
### Teste Unilateral Direito

## 2)
### α = 0.05

## 3)
### E.T.: Teste de Hipóteses para a Proporção:
###       Z = ((p* - p) / sqrt(pq / n)) ~ N(0, 1)

## 4)
### Rejeitar H0 se: E.T. E R.C.

### Z_obs = ((p* - p) / sqrt(pq / n))
###       = ((0.014 - 0.01) / sqrt((0.01 * (1 - 0.01)) / 1000))
###       = 1.2713

### R.C. = [ z_(1-0.05) , +∞ [
###      = [ qnorm(0.95) , +∞ [
###      = [ 1.6449, +∞ [

### Como Z_obs = 1.2713 \E/ R.C. = [ 1.6449, +∞ [, não se rejeita H0.

## 5)
### A empresa deve adquirir o novo processo de fabrico.

"-----------------------"

# Criar a amostra no R:

# Proporção de Embalagens com Defeito:
# 0.014
# 0.014 * 1000 = 14 Embalagens
# População Binomial
(amostra_5_2 <- c(rep(1,14), rep(0,(1000 - 14))))

# Teste: Z_obs e P-Value
BSDA::z.test(
  x = amostra_5_2,
  sigma.x = sqrt(0.01 * (1 - 0.01)),
  alternative = "greater",
  mu = 0.01
)

# 1º - ?
# 2º - No caso das proporções pode-se colocar a amostra no R.
# 3º - No caso da proporção, para o teste z.test, o sigma do teste é sqrt(pq) ou sqrt(p*q*).
# 4º - Se não houver exigências na forma de tomar a decisão, a forma mais fácil é através do P-Value.

"-----------------------"

### 2) ####

# Empresa não adquire se a percentagem de defeitos for inferior a 1%.
# p < 0.01

# Passos para os Testes de Hipóteses:

## 1)
### H0: p = 0.01 => A afirmação não é verdadeira.
### vs.
### H1: p < 0.01 => A afirmação é verdadeira.

### Teste Unilateral Direito

## 2)
### α = 0.1

## 3)
### E.T.: Teste de Hipóteses para a Proporção:
###       Z = ((p* - p) / sqrt(pq / n)) ~ N(0, 1)
BSDA::z.test(
  x = amostra_5_2,
  sigma.x = sqrt(0.01 * (1 - 0.01)),
  alternative = "less",
  mu = 0.01
)
### Z_obs = 1.2713
### P-Value = 0.8982

## 4)
### Rejeitar H0 se: P-Value < α

### Como P-Value = 0.8982 > α = 0.1, não se rejeita H0.

## 5)
### Com base na amostra recolhida e com 1% de significância,
### a afirmação não é verdadeira.

"-----------------------"

### 3) ####

# A proporção de embalagens é verdadeira?
# p = 0.01

# Passos para os Testes de Hipóteses:

## 1)
### H0: p = 0.01 => A afirmação é verdadeira.
### vs.
### H1: p != 0.01 => A afirmação não é verdadeira.

### Teste Bilateral

## 2)
### α = 0.05

## 3)
### E.T.: Teste de Hipóteses para a Proporção:
###       Z = ((p* - p) / sqrt(pq / n)) ~ N(0, 1)
BSDA::z.test(
  x = amostra_5_2,
  sigma.x = sqrt(0.01 * (1 - 0.01)),
  alternative = "two.sided",
  mu = 0.01
)
### Z_obs = 1.2713
### P-Value = 0.2036

## 4)
### Rejeitar H0 se: P-Value < α

### Como P-Value = 0.2036 > α = 0.1, não se rejeita H0.

## 5)
### Com base na amostra recolhida e com 5% de significância,
### a afirmação é verdadeira.

"-----------------------------------------------------"

## Exemplo 3 - Slide 40 ####

(amostra_5_3_meninos <- c(rep(64,9), rep(72,16), rep(74,2), rep(90,5)))
(amostra_5_3_meninas <- c(rep(70,8), rep(74,22), rep(76,2), rep(90,4)))

# Amostras Independentes

# Passos para os Testes de Hipóteses:

## 1)
### H0: μ1 - μ2 = 0 => Não há difrenças ao nível da leitura.
### vs.
### H1: μ1 - μ2 != 0 => Há difrenças ao nível da leitura.

### Teste Bilateral

## 2)
### α = 0.01

## 3)
### D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((s1^2 / n1) + (s2^2 / n2))) ~ N(0, 1)
### E.T.: Teste de Hipóteses para a Proporção:
###       Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((s1^2 / n1) + (s2^2 / n2)))
BSDA::z.test(
  x = amostra_5_3_meninos,           # Primeira Amostra
  sigma.x = sd(amostra_5_3_meninos), # Desvio Padrão da Amostra 1
  y = amostra_5_3_meninas,           # Segunda Amostra
  sigma.y = sd(amostra_5_3_meninas), # Desvio Padrão da Amostra 2
  mu = 0,                            # Média
  alternative = "two.sided"          # Tipo de Teste
)
### Z_obs = -1.3137
### P-Value = 0.1889

## 4)
### Rejeitar H0 se: P-Value < α

### Como P-Value = 0.1889 > α = 0.01, não se rejeita H0.

## 5)
### Com base na amostra recolhida e com 1% de significância,
### não há evidência estatística que haja diferenças ao nível
### da leitura entre meninos e meninas do 1º ciclo, em média.

"-----------------------------------------------------"

## Exemplo 5 - Slide 59 ####

# Pretende-se comparar o consumo médio (MPG),
# tendo em conta a transmissão (AM).

# 1º - Criar novas amostras:
## Amostra 1 = Transmissão Manual
## Amostra 2  = Transmissão Automática

# Como não se podem utilizar as opções em que a populão é "qualquer",
# porque o nº de elementos das amostras são inferiores a 30.

# Não sabemos se as populações são normais, mas se forem podemos usar
# uma das opções para populações normais.
