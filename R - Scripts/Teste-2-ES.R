# Pergunta 1: ####

## 1.A) ####

# Média da Amostra: x̅
(mean(distonia2$age)) # = 59.0781

# Desvio Padrão da Amostra: S
(sd(distonia2$age)) # = 11.2282

# Total de Pessoas na BD:
(n_total <- nrow(distonia2)) # = 64

# Total de Pessoas que fizeram o Tratamento com Placebos:
(n_placebos <- nrow(distonia2[distonia2$treat == 1,])) # = 33

# % de Pessoas que fizeram o Tratamento com Placebos:
((n_placebos / n_total) * 100) # = 51.5625%

"----------------------------------------"

## 1.B) ####

# X - Idade dos pacientes, em anos.

# Teste de Hipóteses Não Paramétrico: Teste de Ajustamento:

# 1º Passo:

## H0: X ~ N(57, 15) vs. H1: X !~ N(57, 15)

# 2º Passo:

## α = 0.01

# 3º Passo:

# Para testar se a População segue uma Distribuição Contínua (Completamente Especificada):
# Kolmogorov-Smirnov
ks.test(
  distonia2$age, # Amostra
  "pnorm",       # Distribuição a Testar
  mean = 57,     # Média
  sd = 15        # Desvio Padrão
)
## D_obs = 0.1468
## P-Value = 0.1268

# 4º Passo:

## Como P-Value = 0.1268 > α = 0.01, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 1% de significância, pode-se concluir
## que a afirmação é válida e pode-se considerar a distribuição como
## sendo uma normal de média 57 e desvio padrão 15.

"----------------------------------------"

## 1.C) ####

(pacientes_femininos <- distonia2[distonia2$sex == 1,])

# Teste de Hipóteses Paramétrico: Média:

# 1º Passo:

## H0: μ_age <= 55 vs. H1: μ_age > 55
## Teste Unilateral Direito

# 2º Passo:

## α = 0.03

# 3º Passo:

# População Qualquer;
# σ Desconhecido;
# n >= 30.
# D.A.: Z = ((x̅ - μ) / (s / sqrt(n))) ~ N(0, 1)
# I.C.: ] x̅ |-+| (z_(1 - (α/2))) * (s / sqrt(n)) [
BSDA::z.test(
  x = pacientes_femininos$age,                   # Vetor com a amostra
  sigma.x = sd(pacientes_femininos$age),        # Desvio Padrão da Amostra
  conf.level = 0.97,  # Grau de Confiança para o teste/intervalo
  alternative = "greater",
  mu = 55
)
## t_obs = 2.5165
## P-Value = 0.005927

# 4º Passo:

(-qnorm(1-0.03))
## R.C. = ] -∞ , -qnorm(1-0.03) ] = ] -∞ , -1.8808 ]

## Como Z_obs !E R.C., não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 3% de significância, pode-se concluir
## que a média aparenta ser inferior ou igual a 55.

"----------------------------------------"

## 1.D) ####

(botoxB <- distonia2[distonia2$treat == 2,])

# X - Pontuação Depois do Tratamento.
# Y - Pontuação Antes do Tratamento.
# D = X - Y

# Teste de Hipóteses Não Paramétrico: Diferença de 2 Médias:

# 1º Passo:

#H0 : Depois <= Antes -> Menos sintomas -> Melhorou
#H0 : Depois-Antes <= 0 -> Menos sintomas -> Melhorou
#H0 : D <= 0 -> Menos sintomas -> Melhorou

## H0: X <= Y vs. H1: X > Y <=>
## H0: X - Y <= 0 vs. H1: X - Y > 0 <=>
## H0: M_D <= 0 vs. H1: M_D > 0
## Teste Unilateral Direito

# 2º Passo:

## α = 0.1

# 3º Passo:

# Wilcoxon (Variáveis Emparelhadas -> Antes e Depois de um Tratamento)

wilcox.test(
  x = botoxB$twstrs_2,            # Amostra X
  y = botoxB$twstrs_1,            # Amostra Y
  alternative = "greater",  # Tipo de Teste
  mu = 0,                   # Média de H0
  paired = TRUE             # São Emparelhadas?
)
## V_obs = 143
## P-Value = 0.9478

# 4º Passo:

## Como P-Value = 0.9478 > α = 0.1, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 10% de significância, pode-se concluir
## que os pacientes, em média, melhoraram dos seus sintomas.

"----------------------------------------"

# Pergunta 2: ####

## 2.A) ####

# X - Tempos da tarefa realizada em monitores coloridos, em segundos.
(tempos_tarefa_monitores_coloridos <- c(502, 488, 494, 481, 497, 488, 494, 489))

# Y - Tempos da tarefa realizada em monitores P/B, em segundos.
(tempos_tarefa_monitores_pb <- c(510, 498, 512, 497, 494, 495, 508))

"----------------------------------------"

### 2.A.I) ####

# Teste de Hipóteses Não Paramétrico: Teste de Ajustamento:

# 1º Passo:

## TA1 -> H0: X ~ Normal vs. H1: X !~ Normal
## TA1 -> H0: Y ~ Normal vs. H1: Y !~ Normal

# 2º Passo:

## α = 0.05

# 3º Passo:

# Para testar se a População segue uma Normal (com n < 50):
# Shapiro Wilk
shapiro.test(
  tempos_tarefa_monitores_coloridos  # Amostra
)
## W_obs_X = 0.9703
## P-Value_X = 0.9002

# Para testar se a População segue uma Normal (com n < 50):
# Shapiro Wilk
shapiro.test(
  tempos_tarefa_monitores_pb  # Amostra
)
## W_obs_Y = 0.8468
## P-Value_X = 0.1148

# 4º Passo:

## Como P-Value_X = 0.9002 e Value_Y = 0.1148 > α = 0.05, não se rejeita nenhum H0.

# 5º Passo:

## Com base na amostra e com 5% de significância, pode-se concluir
## que a ambas as variáveis aparentam ser de distribuições normais.

"----------------------------------------"

### 2.A.II) ####

# Teste de Hipóteses Paramétrico: Quociente de Variâncias:

# 1º Passo:

## H0: σ_X = σ_Y vs. H1: σ_X != σ_Y <=>
## H0: σ^2_X = σ^2_Y vs. H1: σ^2_X != σ^2_Y <=>
## H0: σ^2_X/σ^2_Y  = 1 vs. H1: σ^2_X/σ^2_Y != 1
## Teste Bilateral

# 2º Passo:

## α = 0.05

# 3º Passo:

# Populações Normais;
# Amostras Independentes.
# D.A.: F = ((s1^2 / s2^2) * (σ2^2 / σ1^2)) ~ F(n1 - 1, n2 - 1)
# I.C.: ] ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) , 
# I.C.:   ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) [
### Comando para Amostras Conhecidas
var.test(
  x = tempos_tarefa_monitores_coloridos,                 # Primeira Amostra
  y = tempos_tarefa_monitores_pb,                 # Segunda Amostra
  conf.level = 0.95,  # Grau de Confiança
  alternative = "two.sided",
  ratio = 1
)
## F_obs = 0.7116
## P-Value = 0.6617

# 4º Passo:

# R.C. = [ 0 , qf(0.05/2, 7, 6) ] U [ qf(1 - (0.05/2), 7, 6) , +∞ [
#      = [ 0 , 0.1954 ] U [ 5.6955 , +∞ [

## Como F_obs = 0.7116 !E R.C., não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 5% de significância, pode-se concluir
## que os desvios padrões, e consequentemente as variância, de
## ambas as populações aparentam ser iguais.

"----------------------------------------"

### 2.A.III) ####

# Teste de Hipóteses Paramétrico: Diferença de Médias:

# 1º Passo:

## H0: μ_X <= μ_Y vs. H1: μ_X > μ_Y <=>
## H0: μ_X - μ_Y <= 0 vs. H1: μ_X - μ_Y > 0
## Teste Unilateral Direito

# 2º Passo:

## α = ?

# 3º Passo:

# Populações Normais (determinado na alínea 2.A.I);
# σ1 e σ2 Desconhecidos;
# σ1 = σ2 (determinado na alínea 2.A.II);
# Amostras Independentes.
# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))))
# T ~ t(n1 + (n2 - 2))
# I.C.: ] (x̅1 - x̅2) |-+| t_(1 - (α/2); n1 + n2 - 2) * sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))) [
t.test(
  x = tempos_tarefa_monitores_coloridos,  # Primeira Amostra
  y = tempos_tarefa_monitores_pb,         # Segunda Amostra
  paired = FALSE,                         # As Amostras são Independentes
  var.equal = TRUE,                       # As Variâncias são Iguais
  alternative = "greater"
)
## t_obs = -2.8396
## P-Value = 0.993

# 4º Passo:

## Para se rejeitar H0, o P-Value tem de ser <= que α.
## Então tem-se que: α >= 0.993.

# 5º Passo:

## Pretende-se descobrir o α que torne verdade que a velocidade
## da resolução da tarefa em monitores coloridos seja superior
## em relação aquando é feita em monitores P/B, logo, pretende-se
## rejeitar H0, logo P-Value <= α.
## Então tem-se que: α >= 0.993.

"----------------------------------------"

### 2.A.IV) ####

# Sobre os tempos da tarefa em monitores P/B:

# Para a Variância:
# População Normal
# D.A.: X^2 = (((n-1) * s^2) / σ^2) ~ X^2(n-1)
# I.C.: ] (((n-1) * s^2) / x^2_(1 - (α/2); n-1)) , (((n-1) * s^2) / x^2_(α/2; n-1)) [

# Como ] 4.8662, k [ é para o desvio padrão, para a variância tem-se:
# ] 4.8662^2, k^2 [ = ] 23.6799 , k^2 [.

# n = 7
# S^2 = var(tempos_tarefa_monitores_pb) = 59

# ((n-1) * s^2) / x^2_(1 - (α/2); n-1) = 23.6799 <=>
# (6 * 59) / qchisq(1 - (α/2), 6) = 23.6799 <=>
# (6 * 59) / 23.6799 = qchisq(1 - (α/2), 6) <=>
# 14.94939 = qchisq(1 - (α/2), 6) <=>
# qchisq(1 - (α/2), 6) = 14.94939 <=>
# 1 - (α/2) = pchisq(14.94939, 6) <=>
# 1 - (α/2) = 0.979346 <=>
# (α/2) = 1 - 0.979346 <=>
# α / 2 = 0.020654 <=>
# α = 0.020654 * 2 <=>
# α = 0.0413

# Confiança = 1 - α = 1 - 0.0413 = 0.9587

# O α é igual a 0.0413, logo, o IC realizado foi com 95.87% de confiança.

# Lim_Sup = (((n-1) * s^2) / x^2_(α/2; n-1))
# = ((6 * 59) / qchisq(0.0413/2, 6))
# = 308.2165

# Para o Desvio: sqrt(308.2165) = 17.5561.

# Para confirmar:

# Lim_Inf = (((n-1) * s^2) / x^2_(1 - (α/2); n-1))
# = ((6 * 59) / qchisq(1 - (0.0413/2), 6))
# = 23.67909

# Para o Desvio: sqrt(23.67909) = 4.8661.

# Logo:
# I.C. = ] 4.8661 , 17.5561 [
# k = 17.5561.

"----------------------------------------"

## 2.B) ####

# Proporção de Homens Daltónicos:
# p = 0.08

"----------------------------------------"

### 2.B.I) ####

# Amostra:
## n = 250 homens.

# P(p* >= 0.1) = 1 - P(p* < 0.1)

# Se se ponderar num Dist. Binomial com n = 250 >= 30:
# Para a Proporção:
# D.A.: Z = ((p* - p) / sqrt(pq / n)) ~ N(0, 1)

# Z = ((p* - p) / sqrt(pq / n))
#   = ((0.1 - 0.08) / sqrt((0.08 * 0.92) / 250))
#   = 1.1656

# P(p* >= 0.1)
#  = 1 - P(p* < 0.1)
#  = 1 - P(Z < 1.1656) => V. Al. Contínua
#  = 1 - pnorm(1.1656)
#  = 0.1219

"----------------------------------------"

### 2.B.II) ####

# I.C. Para a Proporção:
# I.C.: ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [

# Confiança = 99%
# α = 1 - 0.99 = 0.01

# n = ?

# Margem de Erro <= 2% <= 0.02

# Margem de Erro = Amplitude / 2

# Amplitude = Lim_Sup - Lim_Inf
# = p* + z_(1 - (α/2)) * sqrt((p* * q*) / n) - (p* - z_(1 - (α/2)) * sqrt((p* * q*) / n))
# = p* + z_(1 - (α/2)) * sqrt((p* * q*) / n) - p* + z_(1 - (α/2)) * sqrt((p* * q*) / n)
# = z_(1 - (α/2)) * sqrt((p* * q*) / n) + z_(1 - (α/2)) * sqrt((p* * q*) / n)
# = 2 * z_(1 - (α/2)) * sqrt((p* * q*) / n)
# = 2 * qnorm(1 - (0.01/2)) * sqrt((0.5 * 0.5) / n)
# = 5.1517 * sqrt((0.5 * 0.5) / n)
# = 5.1517 * sqrt(0.25 / n)

# Margem de Erro <= 0.02 <=>
# Amplitude / 2 <= 0.02 <=>
# (5.1517 * sqrt(0.25 / n)) / 2 <= 0.02 <=>
# 5.1517 * sqrt(0.25 / n) <= 0.02 * 2 <=>
# 5.1517 * sqrt(0.25 / n) <= 0.04 <=>
# sqrt(0.25 / n) <= 0.04 / 5.1517 <=>
# 0.25 / n >= (0.04/5.1517)^2 <=>
# 0.25 >= (0.04/5.1517)^2 * n <=>
# n >= 0.25 / (0.04/5.1517)^2 <=>
# n >= 4146.877

# Logo:
# A dimensão da amostra deveria ser n >= 4147.

"----------------------------------------"

### 2.B.III) ####

# Populações Binomiais.

# p1*
(p_asterisco.homens <- (75/850)) # = 8.82%
(amostra_homens <- c(rep(1, 75), rep(0, 850-75)))

# p2*
(p_asterisco.mulheres <- (5/2000)) # = 0.25%
(amostra_mulheres <- c(rep(1, 5), rep(0, 2000-5)))

# Diferença de Proporções:
# Amostras Independentes;
# n1 = 850 >= 30 & n2 = 2000 >= 30.
# I.C.: ] (p1* - p2*) |-+| z_(1 - (α/2)) * sqrt(((p1* * q1*) / n1) + ((p2* * q2*) / n2)) [
# Comando para Amostras Conhecidas:
(res_z.test <- BSDA::z.test(
  x = amostra_homens,
  y = amostra_mulheres,
  sigma.x = sqrt((75/850) * (1 - (75/850))),
  sigma.y = sqrt((5/2000) * (1 - (5/2000))),
  conf.level = 0.90
))

res_z.test$conf.int
# I.C. a 90% de Confiança para as Proporções => ] 0.0696 , 0.1018 [
# I.C. a 90% de Confiança para as % => ] 6.96 , 10.18 [

# Como o 0 não pertence ao I.C., está comprovado, a 90% de
# confiança, que existem diferenças significativas nas percentagens
# de daltonismo entre homens e mulheres.

"----------------------------------------"
