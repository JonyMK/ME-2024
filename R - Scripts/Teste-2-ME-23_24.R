# Pergunta 1: ####

## 1.A) ####

# Tamanho Total da Amostra:
(n_1_a <- nrow(profTI)) # = 114

# Profissionais de TI que Recebem Benefícios Extra Salário:
(n_beneficiarios_1_a <- nrow(profTI[profTI$Be == 1,]))

# Estimativa da Proporção Profissionais de TI que Recebem Benefícios Extra Salário:
(p_asterisco <- round((n_beneficiarios_1_a/n_1_a), 4))

# Percentagem de Profissionais de TI que Recebem Benefícios Extra Salário:
(p_asterisco * 100) # = 64.04%

# I.C.:
# n >= 30.
# D.A.: Z = ((p* - p) / sqrt(pq / n)) ~=~ ((p* - p) / sqrt((p* * q*) / n)) ~ N(0, 1)
# I.C.: ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [

# ] 0.6404 |-+| qnorm(1 - (0.04/2)) * sqrt((0.6404 * (1 - 0.6404)) / 114) [ =
# ] 0.6404 |-+| 2.0537 * 0.0449 [ =
# ] 0.6404 |-+| 0.0922 [ =
# ] 0.6404 - 0.0922 , 0.6404 + 0.0922 [ =
# ] 0.5482 , 0.7326 [

# I.C. para a Proporção a 96% de Confiança: ] 0.5482 , 0.7326 [
# I.C. para a Percentagem a 96% de Confiança: ] 54.82 , 73.26 [

"---------------------------------------"

## 1.B) ####

# X - Salários dos Profissionais de TI Júniores, na amostra.
# I.C. a 99% de Confiança.

(amostra_1_b <- profTI[profTI$Exp == "J",]$In)

# Como se sabe que:
## População Qualquer;
## σ Desconhecido;
## n >= 30.

# Tem-se:
## D.A.: Z = ((x̅ - μ) / (s / sqrt(n))) ~ N(0, 1)
## I.C.: ] x̅ |-+| (z_(1 - (α/2))) * (s / sqrt(n)) [
BSDA::z.test(
  x = amostra_1_b,                   # Vetor com a amostra
  sigma.x = sd(amostra_1_b),        # Desvio Padrão da Amostra
  conf.level = 0.99  # Grau de Confiança para o teste/intervalo
)

# I.C. a 99% pra Salários Iniciais = ] 953.6184 , 1236.4725 [

"---------------------------------------"

## 1.C) ####

# Salário Médio Atual dos Profissionais é Superior à Média dos Salários em Portugal?
# Salário Médio de Portugal: 1439€

# X1 - Salário Atual dos Profissionais de TI.
# X2 - Salário de Portugal.

# Teste de Hipóteses Paramétrico:
# Diferença de Médias:

# 1º Passo:

## H0: μ_ATUAL <= μ_PORTUGAL vs. H1: μ_ATUAL > μ_PORTUGAL
## <=>
## H0: μ_ATUAL - μ_PORTUGAL <= 0 vs. H1: μ_ATUAL - μ_PORTUGAL > 0
## Teste Unilateral Direito

# 2º Passo:

## α = 0.01

# 3º Passo:

## Sabe-se que:
### Populações Quaiquer;
### σ1 e σ2 Desconhecidos;
### Amostras Independentes;
### n1 e n2 >= 30.

## Logo:
## D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((s1^2 / n1) + (s2^2 / n2))) ~ N(0, 1)
## I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((s1^2 / n1) + (s2^2 / n2)) [

(mean(profTI$Salario)) # = 1918.921
(var())

## Z_obs = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((s1^2 / n1) + (s2^2 / n2)))
##       = (((1918.921 - 1439) - 0) / sqrt((s1^2 / n1) + (s2^2 / n2)))

BSDA::z.test(
  x = VARIAVEL_1,                 # Primeira Amostra
  sigma.x = sd(VARIAVEL_1),       # Desvio Padrão da Amostra 1
  y = VARIAVEL_2,                 # Segunda Amostra
  sigma.y = sd(VARIAVEL_2),       # Desvio Padrão da Amostra 2
  conf.level = 0.99  # Grau de Confiança
)
# D.A.
## U_obs = X
## P-Value = X

# 4º Passo:

## R.C. = ?
## Como U_obs = X E R.C., rejeita-se H0.

### OU

## Como P-Value = X <= α = 0.05, rejeita-se H0.

# 5º Passo:

## Com base na amostra e com 5% de significância, pode-se concluir
## que...

"---------------------------------------"

## 1.D) ####



"---------------------------------------"

## 1.E) ####



"---------------------------------------"

# Pergunta 2: ####

## 2.A) ####



"---------------------------------------"

## 2.B) ####



"---------------------------------------"

## 2.C) ####

### 2.C.I) ####



"---------------------------------------"

### 2.C.II) ####



"---------------------------------------"

### 2.C.III) ####



"---------------------------------------"

### 2.C.IV) ####



"---------------------------------------"
