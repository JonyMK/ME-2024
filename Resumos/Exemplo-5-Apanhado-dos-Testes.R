# Exemplo 5 - Apanhado dos Testes ####


#'* Considere a base de dados ”mtcars” disponível no R.    *
#'* Estes dados foram extraídos da revista americana Motor *
#'* Trend de 1974 e referem-se ao consumo de combustível,  *
#'* design e desempenho de 32 modelos de automóveis.       *
#'*                                                        *
#'* Considere as seguintes variáveis:                      *
#'*   mpg: consumo de combustível em milhas/galão          *
#'*        (medida americana)                              *
#'*   vs: motor (0 = em forma de V, 1 = em linha)          *
#'*   am: transmissão (0 = automática, 1 = manual)         *
#'*                                                        *
#'* 1) Teste para um nível de significância de 5%, se, em  *
#'*    média, o consumo de combustível é superior nos      *
#'*    automóveis com transmissão manual em relação aos    *
#'*    automóveis com transmissão automática.              *
#'*                                                        *
#'* 2) Teste para um nível de significância de 5%, se a    *
#'*    variância do consumo de combustível nos automóveis  *
#'*    com motores em forma de V é o dobro da variância do *
#'*    consumo de combustível nos automóveis com motores   *
#'*    em linha.                                           *
#'*                                                        *
#'* 3) Teste para um nível de significância de 5%, se o    *
#'*    desvio padrão do consumo de combustível nos         *
#'*    automóveis com motores em forma de V é inferior a 4 *
#'*    milhas/galão.                                       *


## 1.) ####

# População 1:
## X1 - Consumo dos automóveis com transmissão manual
## μ1 - ?
## σ1 - ?

# População 2:
## X2 - Consumo dos automóveis com transmissão automática
## μ2 - ?
## σ2 - ?

# Amostras Independentes.

# Amostra 1:
## n1 = 13
## x̅1 = 24.3923
## S1 = 6.1665
(amostra_auto_manual <- mtcars[mtcars$am == 1,]$mpg)

# Amostra 2:
## n2 = 19
## x̅2 = 17.1474
## S2 = 3.8340
(amostra_auto_automatica <- mtcars[mtcars$am == 0,]$mpg)

# Teste de Hipóteses Paramétrico para a Diferença de Médias:

# 1º Passo:

## H0: μ1 <= μ2 vs. H1: μ1 > μ2 <=> H0: μ1 - μ2 <= 0 vs. H1: μ1 - μ2 > 0
## Teste Unilateral Direito

# 2º Passo:

## α = 0.05

# 3º Passo:

## Com a informação disponibilizada não é possível escolher uma D.A..

## Como n1 e n2 < 30, é necessário que as populações sejam Normais
## para realizar o teste.
## Deve-se fazer um Teste de Ajustamento para cada população, para
## verificar se se pode considerar que as populações sejam Normais.

"------------------------"
# Teste de Hipóteses Não Paramétrico: Teste de Ajustamento:

# 1º Passo:

## TA1 => H0: X1 ~ Normal vs. H1: X1 !~ Normal
## TA2 => H0: X1 ~ Normal vs. H1: X1 !~ Normal

# 2º Passo:

## n1 e n2 < 50.
## Logo:
shapiro.test(
  amostra_auto_manual  # Amostra
)
## W_1_obs = 0.9458
## P-Value_1 = 0.5363

shapiro.test(
  amostra_auto_automatica  # Amostra
)
## W_2_obs = 0.97677
## P-Value_2 = 0.8987

# 3º Passo:

## Como P-Value_1 e P-Value_2 > α = 0.05, não se rejeita H0.

# 4º Passo:

## Com base nas amostras e com 5% de significância, pode-se concluir
## que as populações podem ser consideradas Normais.
"------------------------"

## Populações Normais.

## Como as populações são Normais e os desvios padrões desconhecidos,
## agora é necessário saber também se podem ou não ser considerados iguais.
## Para isso, recorre-se a um I.C. para o Quociente de Variâncias ou a um
## Teste de Hipóteses Paramétrico para o Quaciente de Variâncias.

"------------------------"
## I.C. a 95% para σ1^2/σ2^2:

var.test(
  x = amostra_auto_manual,      # Primeira Amostra
  y = amostra_auto_automatica,  # Segunda Amostra
  conf.level = 0.95             # Grau de Confiança
)

## I.C. = ] 0.9343 , 8.0404 [

## Como o 1 E I.C., as variâncias podem ser consideradas iguais.
## Ou seja, com 95% de confiança, os desvios padrões também podem
## ser considerados iguais.
"------------------------"

## σ1 = σ2.

# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))))
t.test(
  x = amostra_auto_manual,      # Primeira Amostra
  y = amostra_auto_automatica,  # Segunda Amostra
  paired = FALSE,               # As Amostras são Dependentes?
  var.equal = TRUE,             # As Variâncias são Iguais?
  conf.level = 0.95,            # Grau de Confiança
  alternative = "greater"
)
## t_obs = 4.1061
## P-Value = 0.0001425

# 4º Passo:

## Como P-Value = 0.0001425 <= α = 0.05, rejeita-se H0.

# 5º Passo:

## Com base nas amostras e com 5% de significância, pode-se concluir
## que o consumo de combustível é superior nos automóveis com transmissão
## manual em relação aos automóveis com transmissão automática.

"-----------------------------------------------------------------"

## 2.) ####

# População 1:
## Y1 - Consumo de combustível nos automóveis com motor em forma de V
## μ1 - ?
## σ1 - ?

# População 2:
## Y2 - Consumo de combustível nos automóveis com motor em forma em linha
## μ2 - ?
## σ2 - ?

# Amostras Independentes.

# Amostra 1:
## n1 = 18
## y̅1 = 16.6167
## S1 = 3.8607
(amostra_comb_v <- mtcars[mtcars$vs == 0,]$mpg)

# Amostra 2:
## n2 = 14
## y̅2 = 24.5571
## S2 = 5.3790
(amostra_comb_linha <- mtcars[mtcars$vs == 1,]$mpg)

# Teste de Hipóteses Paramétrico para as Variâncias:

# 1º Passo:

## H0: σ1^2 = 2σ2^2 vs. H1: σ1^2 != 2σ2^2 <=>
## H0: σ1^2/σ2^2 = 2 vs. H1: σ1^2/σ2^2 != 2
## Teste Bilateral

# 2º Passo:

## α = 0.05

# 3º Passo:

## Com a informação disponibilizada não é possível escolher uma D.A..

## Como o teste é ao quociente de variâncias é necessário que as
## populações sejam Normais para se poder realizar o teste.
## Como não se sabe se são, deve-se realizar um Teste de Ajustamento
## para verificar se se pode considerar que as populações sejam Normais.
"------------------------"
# Teste de Hipóteses Não Paramétrico: Teste de Ajustamento:

# 1º Passo:

## TA1 => H0: Y1 ~ Normal vs. H1: Y1 !~ Normal
## TA2 => H0: Y1 ~ Normal vs. H1: Y1 !~ Normal

# 2º Passo:

## n1 e n2 < 50.
## Logo:
shapiro.test(
  amostra_comb_v  # Amostra
)
## W_1_obs = 0.95151
## P-Value_1 = 0.4491

shapiro.test(
  amostra_comb_linha  # Amostra
)
## W_2_obs = 0.91166
## P-Value_2 = 0.1666

# 3º Passo:

## Como P-Value_1 e P-Value_2 > α = 0.05, não se rejeita H0.

# 4º Passo:

## Com base nas amostras e com 5% de significância, pode-se concluir
## que as populações podem ser consideradas Normais.
"------------------------"

## Populações Normais.

# D.A.: F = ((s1^2 / s2^2) * (σ2^2 / σ1^2)) ~ F(n1 - 1, n2 - 1)
var.test(
  x = amostra_comb_v,        # Primeira Amostra
  y = amostra_comb_linha,    # Segunda Amostra
  conf.level = 0.95,         # Grau de Confiança
  alternative = "two.sided",
  ratio = 2
)
## F_obs = 0.25757
## P-Value = 0.01028

# 4º Passo:

## Como P-Value = 0.01028 <= α = 0.05, rejeita-se H0.

# 5º Passo:

## Com base nas amostras e com 5% de significância, pode-se concluir
## que não há evidência estatística de que a variância do consumo de
## combustível nos automóveis com motores em forma de V seja o dobro
## da variância do consumo de combustível nos automóveis com motores
## em linha.

"-----------------------------------------------------------------"

## 3.) ####

# População:
## Y1 - Consumo de combustível nos automóveis com motores em forma de V
## μ1 - ?
## σ1 - ?

# Amostra:
## n1 = 18
## y̅1 = 16.6167
## S1 = 3.8607

# Teste de Hipóteses Paramétrico para a Variância:

# 1º Passo:

## H0: σ1 >= 4 vs. H1: σ1 < 4 <=> H0: σ1^2 >= 16 vs. H1: σ1^2 < 16
## Teste Unilateral Esquerdo

# 2º Passo:

## α = 0.05

# 3º Passo:

## A população é Normal (foi testado na alínea anterior).
## Logo:

# D.A.: X^2 = (((n-1) * s^2) / σ^2) ~ X^2(n-1)
EnvStats::varTest(
  x = amostra_comb_v,                   # Amostra
  conf.level = 0.95,  # Grau de Confiança
  alternative = "less",
  sigma.squared = 16
)
## X^2_obs = 15.83656
## P-Value = 0.4645506

# 4º Passo:

## Como P-Value = 0.4645506 > α = 0.05, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 5% de significância, pode-se concluir
## que não há evidência estatística que o desvio padrão do consumo
## de combustível nos automóveis com motores em forma de V seja
## inferior a 4 milhas/galão.
