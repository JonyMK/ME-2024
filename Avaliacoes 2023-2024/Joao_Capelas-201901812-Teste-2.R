#' ME - Teste 2 ####

# Pergunta 1: ####

## 1.A) ####

# x̅
(mean(Torgersen$massa_corporal))

# n
(nrow(Torgersen))

# População Qualquer;
# σ Desconhecido;
# n >= 30.
# D.A.: Z = ((x̅ - μ) / (s / sqrt(n))) ~ N(0, 1)
# I.C.: ] x̅ |-+| (z_(1 - (α/2))) * (s / sqrt(n)) [
BSDA::z.test(
  x = Torgersen$massa_corporal,                   # Vetor com a amostra
  sigma.x = sd(Torgersen$massa_corporal),        # Desvio Padrão da Amostra
  conf.level = 0.94  # Grau de Confiança para o teste/intervalo
)

"---------------------------------------"

## 1.B) ####

# Margem de Erro = Amplitude / 2 = 115.1905

# Amplitude = L_Sup - L_Inf
(3822.053 - 3591.672) # = 230.381

(230.381 / 2)

"---------------------------------------"

## 1.C) ####

### 1.C.I) ####

(amostra_1_c_i_femea <- Torgersen[Torgersen$sexo == 2,]$comprimento_bico)
(amostra_1_c_i_macho <- Torgersen[Torgersen$sexo == 1,]$comprimento_bico)

# Teste de Hipóteses Não Paramétrico: Teste de Ajustamento:

# 1º Passo:

## TA1 => H0: X1 ~ Normal vs. H1: X1 !~ Normal
## TA2 => H0: X2 ~ Normal vs. H1: X2 !~ Normal

# 2º Passo:

## α = 0.01

# 3º Passo:

(length(amostra_1_c_i_femea))
(length(amostra_1_c_i_macho))

## n1 = 26 e n2 = 25 < 50.
## Logo:

shapiro.test(
  amostra_1_c_i_femea  # Amostra
)
## W_1_obs = 0.9583
## P-Value_1 = 0.36

shapiro.test(
  amostra_1_c_i_macho  # Amostra
)
## W_2_obs = 0.9795
## P-Value_2 = 0.875

# 4º Passo:

## Como P-Value_1 = 0.36 e P-Value_2 = 0.875 > α = 0.01, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 1% de significância, pode-se concluir
## que as populações podem ser consideradas Normais.

"---------------------------------------"

### 1.C.II) ####

# Teste de Hipóteses Paramétrico: Teste para o Quociente de Variâncias:

# 1º Passo:

## H0: σ1^2/σ2^2 = 1 vs. H1: σ1^2/σ2^2 != 1
## Teste Bilateral

# 2º Passo:

## α = 0.01

# 3º Passo:

## Populações Normais;
## Amostras Independentes.
## D.A.: F = ((s1^2 / s2^2) * (σ2^2 / σ1^2)) ~ F(n1 - 1, n2 - 1)
## I.C.: ] ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) , 
## I.C.:   ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) [
var.test(
  x = amostra_1_c_i_femea,                 # Primeira Amostra
  y = amostra_1_c_i_macho,                 # Segunda Amostra
  conf.level = 0.99,  # Grau de Confiança
  alternative = "two.sided",
  ratio = 1
)
## F_obs = 0.5138
## P-Value = 0.1046

# 4º Passo:

## Como P-Value = 0.1046 > α = 0.01, não se rejeita H0.

"---------------------------------------"

## 1.D) ####

# Teste de Hipóteses Paramétrico: Teste para a Diferença de Médias:

# 1º Passo:

## H0: μ_X1 = μ_X2 vs. H1: μ_X1 != μ_X2
## Teste Bilateral

# 2º Passo:

## α = 0.01

# 3º Passo:

# Populações Normais;
# σ1 e σ2 Desconhecidos;
# σ1 = σ2;
# Amostras Independentes/Emparelhadas.
# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))))
# T ~ t(n1 + (n2 - 2))
t.test(
  x = amostra_1_c_i_femea,                 # Primeira Amostra
  y = amostra_1_c_i_macho,                 # Segunda Amostra
  paired = FALSE,                 # As Amostras são Dependentes?
  var.equal = TRUE,               # As Variâncias são Iguais?
  conf.level = 0.99  # Grau de Confiança
)
## t_obs = -3.96
## P-Value = 0.0002424

# Teste Bilateral: RC = ] -∞ , -qt(1 - (0.01/2), 26+25-2) ] U [ qt(1 - (0.01/2), 26+25-2) , +∞ [
# Teste Bilateral: RC = ] -∞ , -2.68 ] U [ 2.68 , +∞ [

"---------------------------------------"

# Pergunta 2: ####

## 2.A) ####

(150/500)
(-1+0.05)
qnorm(0.95)
(0.3/1.6449)
(0.1824^2)

"---------------------------------------"

## 2.B) ####

(amostra_2_b_antes <- c(2.2, 4.8, 5.8, 6.1, 4.9, 5.7, 5.3, 5.7))
(amostra_2_b_depois <- c(3.0, 5.5, 5.5, 6.3, 5.4, 5.6, 5.6, 6.1))

# X - Peso Depois da Alimentação.
# Y - Peso Antes da Alimntação.
# D = X - Y

# Teste de Hipóteses Não Paramétrico: Diferença de 2 Médias:

# 1º Passo:

#H0 : Depois <= Antes -> Menos Peso
#H0 : Depois-Antes <= 0 -> Menos Peso
#H0 : D <= 0 -> Menos Peso

## H0: X <= Y vs. H1: X > Y <=>
## H0: X - Y <= 0 vs. H1: X - Y > 0 <=>
## H0: M_D <= 0 vs. H1: M_D > 0
## Teste Unilateral Direito

# 2º Passo:

## α = ?

# 3º Passo:

# Wilcoxon (Variáveis Emparelhadas -> Antes e Depois da temporada de alimentação)

wilcox.test(
  x = amostra_2_b_depois,            # Amostra X
  y = amostra_2_b_antes,            # Amostra Y
  alternative = "greater",  # Tipo de Teste
  mu = 0,                   # Média de H0
  paired = TRUE             # São Emparelhadas?
)
## V_obs = 31.5
## P-Value = 0.03418

# 4º Passo:

## Como P-Value = 0.03418, e como se pretende rejeitar H0
## (pois pretende-se que a afirmação verdadeira seja a de que o 
## peso aumentou com a temporada de alimentação), então obtém-se:
## α >= 0.03418.

"---------------------------------------"

# Pergunta 3: ####

# p* = 81/100 = 0.81

"---------------------------------------"

## 3.A) ####

# Teste de Hipóteses Paramétrico: Teste para a Proporção:

# 1º Passo:

## H0: p >= 0.80 vs. H1: p < 0.80
## Teste Unilateral Esquerdo

# 2º Passo:

## α = 0.06

# 3º Passo:

# Proporção de Leitores Interessados:
# p* = 81/100 = 0.81
# 81 Leitores em 100 estão interessados nas publicações
# População Binomial
(amostra_3_a <- c(rep(1, 81), rep(0, (100 - 81))))

# Teste: Z_obs e P-Value
BSDA::z.test(
  x = amostra_3_a,
  sigma.x = sqrt(0.81 * (1 - 0.81)),
  alternative = "less",
  mu = 0.8
)
## Z_obs = 0.2549
## P-Value = 0.6006

# 4º Passo:

## Como P-Value = 0.6006 > α = 0.06, não se rejeita H0.


"---------------------------------------"

## 3.B) ####

(sqrt((0.81 * 0.19) / 100))
(0.81 - 0.7406)
(0.0694 / 0.03923)
(1 - pnorm(1.7691))
(0.03844 * 2)

"---------------------------------------"
