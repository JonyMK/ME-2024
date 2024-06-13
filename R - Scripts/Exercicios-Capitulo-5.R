#### Capítulo 5 - Exercícios ####

"----------------------------------------------------------------------"

#### Exercicio 5.1 ####


"-------- Enunciado --------"
#'* Uma firma tem seguido a política de oferecer uma garantia de 2000 *
#'* utilizações para determinado aparelho que comercializa. Este      *
#'* procedimento baseia-se em estudos levados a cabo no período       *
#'* inicial de produção, que indicavam um número médio de utilizações *
#'* possíveis por aparelho de 2060, com uma variabilidade traduzida   *
#'* por σ = 20. Existindo indícios de que presentemente a situação    *
#'* pode ter mudado, pretende-se averiguar se continua a ser 2060 o   *
#'* número médio de utilizações por aparelho. Para o efeito foram     *
#'* selecionados ao acaso e testados pela firma 10 aparelhos, os      *
#'* quais forneceram os seguintes valores:                            *
#'*                                                                   *
#'*   2100, 2025, 2071, 2067, 2150, 2115, 2064, 2088, 1995, 2095      *
#'*                                                                   *
#'* Suponha que o número de utilizações permitidas por aparelho       *
#'* comporta-se de forma aproximadamente normal.                      *
#'* 1.) Como define o teste de hipóteses a efetuar?                   *
#'* 2.) Proceda ao cálculo da região crítica para o teste definido    *
#'*     anteriormente, considerando α = 0.05.                         *
#'* 3.) Face à amostra recolhida, que decisão tomaria quanto a H0?    *
#'*     Porquê?                                                       *
"---------------------------"


# População:
## Normal
## μ = 2060
## σ = 20

# Amostra:
## n = 10

(amostra_5_1 <- c(
  2100, 2025, 2071, 2067, 2150, 2115, 2064, 2088, 1995, 2095
))

#### 1) #####

# Teste de Hipóteses Paramétrico:

# H0: μ = 2060 vs. H1: μ != 2060

# Tipo de Teste: Teste Bilateral

#### 2) #####

# R.C. = ?

# α = 0.05

# R.C. = ] -∞ , -qnorm(1 - (α/2)) ] U [ qnorm(1 - (α/2)) , +∞ [
#      = ] -∞ , -qnorm(1 - (0.05/2)) ] U [ qnorm(1 - (0.05/2)) , +∞ [
#      = ] -∞ , -1.96 ] U [ 1.96 , +∞ [

#### 3) #####

# Estatística de Teste:
## D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)
## D.A.: ((mean(amostra_5_1) - 2060) / (20 / sqrt(10)))
media_caso_1(
  xbarra = mean(amostra_5_1),
  mi = 2060,
  desviopadrao = 20,
  dimensao = 10,
  tipo_calculo = 1
)

# Como Zobs = 2.6879 pertence à R.C., rejeita-se a H0.

"----------------------------------------------------------------------"

#### Exercicio 5.2 ####


"-------- Enunciado --------"
#'* Um fabricante de cabos indicava que os seus cabos apresentavam    *
#'* uma tensão média de rutura de 1800 toneladas, com um desvio       *
#'* padrão de 100 toneladas. Um outro fabricante dizia que tal valor  *
#'* não correspondia á verdade, indicando como média de rutura de     *
#'* 1750 toneladas, com o mesmo desvio padrão. Para decidir qual dos  *
#'* dois dizia a verdade foi recolhida uma amostra de 35 cabos:       *
#'*                                                                   *
#'*   1595.1, 1514.4, 1608.8, 1591.7, 1482.5, 1796.1, 1700.1,         *
#'*   1501.5, 1658.9, 1777.1, 1625.4, 1713.9, 1522.3, 1575.2,         *
#'*   1634.7, 1615.8, 1690.5, 1729.0, 1646.7, 1681.1, 1769.7,         *
#'*   1707.3, 1668.3, 1768.0, 1655.1, 1715.6, 1805.2, 1724.8,         *
#'*   1578.1, 1548.4, 1647.0, 1586.5, 1706.8, 1535.0, 1673.4          *
#'*                                                                   *
#'* Admita que a tensão de rutura segue uma distribuição              *
#'* aproximadamente normal.                                           *
#'* 1.) Realize o teste de hipóteses: H0: µ = 1800 vs. H1: µ = 1750,  *
#'*     e diga qual dos dois fabricantes terá razão para um nível de  *
#'*     significância 5% e recorrendo apenas à região critica.        *
#'* 2.) Realize o teste de hipóteses: H0: µ = 1750 vs. H1: µ = 1800,  *
#'*     e diga qual dos dois fabricantes terá razão para um nível de  *
#'*     significância 10% e recorrendo apenas ao valor−p.             *
"---------------------------"


# População - 1º Forma:
## Normal
## μ = 1800 toneladas
## σ = 100 toneladas

# População - 2ª Forma:
## Normal
## μ = 1750 toneladas
## σ = 100 toneladas

# Amostra:
## n = 35

(amostra_5_2 <- c(
  1595.1, 1514.4, 1608.8, 1591.7, 1482.5,
  1796.1, 1700.1, 1501.5, 1658.9, 1777.1,
  1625.4, 1713.9, 1522.3, 1575.2, 1634.7,
  1615.8, 1690.5, 1729.0, 1646.7, 1681.1,
  1769.7, 1707.3, 1668.3, 1768.0, 1655.1,
  1715.6, 1805.2, 1724.8, 1578.1, 1548.4,
  1647.0, 1586.5, 1706.8, 1535.0, 1673.4
))

#### 1) #####

# Teste de Hipóteses Paramétrico:

# 1º Passo:

## H0: μ = 1800 vs. H1: μ = 1750
## <=>
## H0: μ = 1800 vs. H1: μ < 1800
## Teste Unilateral Esquerdo

# 2º Passo:

## α = 0.05

# 3º Passo:

## Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)
BSDA::z.test(
  x = amostra_5_2, # Vetor com a amostra
  sigma.x = 100,   # Desvio Padrão da População
  mu = 1800,
  alternative = "less"
)
## Z_obs = -8.8741
## P-Value < 0.00000000000000022

# 4º Passo:
## R.C. = ] -∞ , -qnorm(0.05/2) ]
## R.C. = ] -∞ , 1.96 ]

## Como Z_obs = -8.8741 E R.C., não se rejeita H0.

# 5º Passo:
## Com base na amostra e para 5% de significância, o segundo fabricante parece ter razão.

#### 2) #####

# Teste de Hipóteses Paramétrico:

# 1º Passo:

## H0: μ = 1750 vs. H1: μ = 1800
## <=>
## H0: μ = 1750 vs. H1: μ > 1750
## Teste Unilateral Direito

# 2º Passo:

## α = 0.1

# 3º Passo:

## Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)
BSDA::z.test(
  x = amostra_5_2, # Vetor com a amostra
  sigma.x = 100,   # Desvio Padrão da População
  mu = 1750,
  alternative = "greater"
)
## Z_obs = -5.9161
## P-Value = 1

# 4º Passo:
## Como P-Value = 1 > α = 0.1, não se rejeita H0.

# 5º Passo:
## Com base na amostra e para 10% de significância, o primeiro fabricante parece ter razão.

"----------------------------------------------------------------------"

#### Exercicio 5.3 ####


"-------- Enunciado --------"
#'* Um comerciante recebe ovos de um determinado aviário, onde os     *
#'* ovos são classificados consoante o peso, em duas classes A e B.   *
#'* O peso dos ovos de classe A tem distribuição N(50, 8) e o peso    *
#'* dos ovos de classe B tem distribuição N(55, 8). Um comerciante    *
#'* recebe uma remessa de ovos com garantia de serem de classe B e    *
#'* tem um prazo de dois dias para reclamar, caso considere ter       *
#'* havido engano da parte do aviário. Para tomar uma decisão ele     *
#'* analisou 10 ovos cujos pesos são:                                 *
#'*  (44.8, 61.4, 50.3, 46.9, 44.7, 64.2, 61.5, 47.3, 46.5, 62.4)     *
#'* Recorrendo a um teste de hipóteses para uma média e um nível de   *
#'* significância de 0.05, diga qual a atitude que o comerciante deve *
#'* tomar.                                                            *
"---------------------------"


# Classe A:
## Distribuição Normal;
## μ = 50;
## σ = 8.

# Classe B:
## Distribuição Normal;
## μ = 55;
## σ = 8.

# Amostra:
## n = 10.

(amostra_5_3 <- c(44.8, 61.4, 50.3, 46.9, 44.7, 64.2, 61.5, 47.3, 46.5, 62.4))

# Teste de Hipóteses Paramétrico para a Média:

# 1º Passo:

## H0: μ = 55 vs. H1: μ = 50 <=> H0: μ = 55 vs. H1: μ != 55
## Teste Bilateral

# 2º Passo:

## α = 0.05

# 3º Passo:

## Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)
BSDA::z.test(
  x = amostra_5_3, # Vetor com a amostra
  sigma.x = 8,   # Desvio Padrão da População
  mu = 55,
  alternative = "two.sided"
)
## Z_obs = -0.79057
## P-Value = 0.4292

# 4º Passo:
## Como P-Value = 0.4292 <= α = 0.05, rejeita-se H0.

# 5º Passo:
## Com base na amostra e para 5% de significância, o comerciante não deve
## fazer uma reclamação pois os ovos aparentam ser de classe B.

"----------------------------------------------------------------------"

#### Exercicio 5.4 ####


"-------- Enunciado --------"
#'* A especificação de produção de determinada liga, com comportamento *
#'* normal, exige 23.2% de cobre. Uma amostra de 10 análises da liga   *
#'* acusou os seguintes conteúdos de cobre (em percentagem):           *
#'* (23.9, 23.5, 23.8, 23.1, 23.4, 23.6, 23.4, 23.2, 23.6, 23.5)       *
#'* 1.) Calcule estimativas para a média e o desvio padrão do conteúdo *
#'*     de cobre.                                                      *
#'* 2.) Considerando α = 0.01, pode-se concluir que o produto satisfaz *
#'*     as especificações?                                             *
#'* 3.) Considerando α = 0.01, teste a hipótese de que o conteúdo      *
#'*     médio de cobre seja superior ao exigido pelas especificações.  *
"---------------------------"


# População:
## Normal.

# Amostra:
## n = 10.

(amostra_5_4 <- c(23.9, 23.5, 23.8, 23.1, 23.4, 23.6, 23.4, 23.2, 23.6, 23.5))

#### 1) #####

# x̅ = mean(amostra_5_4) = 23.5%
# S = sd(amostra_5_4) = 0.245%

#### 2) #####

# Teste de Hipóteses Paramétrico para a Média:

# 1º Passo:

## H0: μ = 23.2% vs. H1: μ != 23.2%
## Teste Bilateral

# 2º Passo:

## α = 0.01

# 3º Passo:

# D.A.: T = ((x̅ - μ) / (s / sqrt(n))) ~ t(n-1)
t.test(
  x = amostra_5_4,                   # Vetor com a amostra
  mu = 23.2,                     # Média da População
  alternative = "two.sided"
)
## t_obs = 3.873
## P-Value = 0.003772

# 4º Passo:
## Como P-Value = 0.003772 <= α = 0.01, rejeita-se H0.

# 5º Passo:
## Com base na amostra e para 1% de significância, não se pode concluir que
## o produto satisfaça as especificações.

#### 3) #####

# Teste de Hipóteses Paramétrico para a Média:

# 1º Passo:

## H0: μ <= 23.2% vs. H1: μ > 23.2%
## Teste Unilateral Direito

# 2º Passo:

## α = 0.01

# 3º Passo:

# D.A.: T = ((x̅ - μ) / (s / sqrt(n))) ~ t(n-1)
t.test(
  x = amostra_5_4,                   # Vetor com a amostra
  mu = 23.2,                     # Média da População
  alternative = "greater"
)
## t_obs = 3.873
## P-Value = 0.001886

# 4º Passo:
## Como P-Value = 0.001886 <= α = 0.01, rejeita-se H0.

# 5º Passo:
## Com base na amostra e para 1% de significância, pode-se concluir que
## o conteúdo médio de cobre seja superior ao exigido pelas especificações.

"----------------------------------------------------------------------"

#### Exercicio 5.5 ####


"-------- Enunciado --------"
#'* Da produção diária de um dado fertilizante colheram-se 9 amostras *
#'* que se analisaram para calcular a percentagem média de azoto. Os  *
#'* resultados obtidos foram os seguintes:                            *
#'* 6.2; 5.7; 5.8; 5.8; 6.1; 5.9; 6.0; 5.7; 5.9                       *
#'* 1.) Sabendo que o processo de análise utilizado fornece valores   *
#'*     com distribuição Normal, N(µ, 0.24), determine o menor nível  *
#'*     de significância que assegura, com base nestas observações    *
#'*     que a % média de azoto no fertilizante é inferior a 6%.       *
#'* 2.) Sabendo que o processo de análise utilizado fornece valores   *
#'*     com distribuição Normal, determine o menor nível de           *
#'*     significância que assegura, com base nestas observações que a *
#'*     % média de azoto no fertilizante é inferior a 6%.             *
"---------------------------"


(amostra_5_5 <- c(6.2, 5.7, 5.8, 5.8, 6.1, 5.9, 6.0, 5.7, 5.9))

#### 1) #####

# População:
## Normal;
## μ = ?;
## σ = 0.24.

# Teste de Hipóteses Paramétrico para a Média:

# 1º Passo:

## H0: μ >= 6% vs. H1: μ < 6%
## Teste Unilateral Esquerdo

# 2º Passo:

## α = ?

# 3º Passo:

# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)
BSDA::z.test(
  x = amostra_5_5,                   # Vetor com a amostra
  sigma.x = 0.24,        # Desvio Padrão da População
  mu = 6,
  alternative = "less"
)
## Z_obs = -1.25
## P-Value = 0.1056

# 4º Passo:
## Para se rejeitar H0, o P-Value tem de ser <= que α.
## Sabe-se que H1 é verdadeira, logo P-Value <= α.

## Então: α >= 0.1056.

#### 2) #####

# População:
## Normal;
## μ = ?;
## σ = ?.

# Teste de Hipóteses Paramétrico para a Média:

# 1º Passo:

## H0: μ >= 6% vs. H1: μ < 6%
## Teste Unilateral Esquerdo

# 2º Passo:

## α = ?

# 3º Passo:

# D.A.: T = ((x̅ - μ) / (s / sqrt(n))) ~ t(n-1)
t.test(
  x = amostra_5_5,                   # Vetor com a amostra
  mu = 6,                     # Média da População
  alternative = "less"
)
## t_obs = -1.7321
## P-Value = 0.06075

# 4º Passo:
## Para se rejeitar H0, o P-Value tem de ser <= que α.
## Sabe-se que H1 é verdadeira, logo P-Value <= α.

## Então: α >= 0.06075.


"----------------------------------------------------------------------"

#### Exercicio 5.6 ####


"-------- Enunciado --------"
#'* A dona de uma loja de roupa pode escolher entre dois fornecedores *
#'* para a aquisição de calças para senhora. Aparentemente, a única   *
#'* diferença reside no preço. A dona da loja compraria ao primeiro   *
#'* fornecedor (que tem preços mais baixos) a não ser que haja razões *
#'* para crer que o produto é de qualidade inferior (tenha, em média, *
#'* menor resistência). Recolhidas duas amostras, obtiveram-se os     *
#'* seguintes resultados sobre a resistência das calças:              *
#'*                                                                   *
#'* Fornecedor 1: 88.9, 89.2, 87.1, 88.8, 87.5, 92.0,                 *
#'*               91.9, 87.5, 84.4, 91.0, 84.1, 85.4                  *
#'*                                                                   *
#'* Fornecedor 2: 93.0, 88.6, 90.9, 90.2, 99.5, 89.8,                 *
#'*               93.8, 90.1, 90.3                                    *
#'*                                                                   *
#'* Admita que a caraterística em estudo, resistência das calças,     *
#'* segue uma distribuição normal.                                    *
#'* 1.) Recorrendo a um teste de hipóteses e a um nível de            *
#'*     significância de 5%, verifique se as variâncias da            *
#'*     resistência das calças dos dois fornecedores podem ser        *
#'*     consideradas iguais.                                          *
#'* 2.) Diga, justificando, que decisão deve tomar a dona da loja     *
#'*     (considere α = 0.05).                                         *
#'* 3.) Comente quais as consequências para o teste anterior de ser   *
#'*     utilizado um grau de significância de 0.01.                   *
#'* 4.) Suponha que conhece as variâncias das populações, sendo       *
#'*     σ1^2 = 8 e σ2^2 = 9. Elabore novamente um teste de hipóteses, *
#'*     indicando que decisão deve tomar a dona da loja (considere    *
#'*     α = 0.10).                                                    *
"---------------------------"


(amostra_5_6_fornecedor1 <- c(88.9, 89.2, 87.1, 88.8, 87.5, 92.0, 91.9, 87.5, 84.4, 91.0, 84.1, 85.4))
(amostra_5_6_fornecedor2 <- c(93.0, 88.6, 90.9, 90.2, 99.5, 89.8, 93.8, 90.1, 90.3))

# População:
## Normal.

#### 1) #####

# Teste de Hipóteses Paramétrico para o Quociente de Variâncias:

# 1º Passo:

## H0: σ1/σ2 = 1 vs. H1: σ1/σ2 != 1
## Teste Bilateral

# 2º Passo:

## α = 0.05

# 3º Passo:

## D.A.: F = ((s1^2 / s2^2) * (σ2^2 / σ1^2)) ~ F(n1 - 1, n2 - 1)
var.test(
  x = amostra_5_6_fornecedor1,  # Primeira Amostra
  y = amostra_5_6_fornecedor2,  # Segunda Amostra
  alternative = "two.sided"
)
## F_obs = 0.65736
## P-Value = 0.5084

# 4º Passo:

## Como P-Value = 0.5084 > α = 0.05, não se rejeita H0.

# 5º Passo:

## Com base nas amostras e com 5% de significância, é possível afirmar
## que as variâncias das resistências das calças de ambos os fornecedores
## podem ser iguais.

#### 2) #####

# Teste de Hipóteses Paramétrico para a Média:

# 1º Passo:

## H0: μ1 <= μ2 vs. H1: μ1 > μ2 <=> H0: μ1 - μ2 <= 0 vs. H1: μ1 - μ2 > 0
## Teste Unilateral Direito

# 2º Passo:

## α = 0.05

# 3º Passo:

# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))))
t.test(
  x = amostra_5_6_fornecedor1,                 # Primeira Amostra
  y = amostra_5_6_fornecedor2,                 # Segunda Amostra
  paired = FALSE,                 # As Amostras são Dependentes?
  var.equal = TRUE,               # As Variâncias são Iguais?
  alternative = "greater"
)
## t_obs = -2.7958
## P-Value = 0.9942

# 4º Passo:

## Como P-Value = 0.9942 > α = 0.05, não se rejeita H0.

# 5º Passo:

## Com base nas amostras e com 5% de significância, pode-se afirmar
## que o fornecedor 1 tem uma média de resistência menor que o fornecedor 2,
## logo a dona da loja deve optar pelo fornecedor 2.

#### 3) #####

# A alteração do nível de significância não implica uma alteração ao
# teste realizado nem aos resultados obtidos, apenas poderia alterar a
# decisão final. Como o P-Value obtido continuaria a ser superior ao novo
# nível de significância (0.01), a decisão final também se manteria igual,
# não implicando assim qualquer alteração.

#### 4) #####

# σ1^2 = 8
# σ2^2 = 9

# σ1 = 2.8284
# σ2 = 3

# Teste de Hipóteses Paramétrico para o Quociente de Variâncias:

# 1º Passo:

## H0: μ1 <= μ2 vs. H1: μ1 > μ2 <=> H0: μ1 - μ2 <= 0 vs. H1: μ1 - μ2 > 0
## Teste Unilateral Direito

# 2º Passo:

## α = 0.1

# 3º Passo:

## D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2))) ~ N(0, 1)
BSDA::z.test(
  x = amostra_5_6_fornecedor1,                 # Primeira Amostra
  sigma.x = sd(amostra_5_6_fornecedor1),       # Desvio Padrão da Amostra 1
  y = amostra_5_6_fornecedor2,                 # Segunda Amostra
  sigma.y = sd(amostra_5_6_fornecedor2),       # Desvio Padrão da Amostra 2
  alternative = "greater"
)
## Z_obs = -2.71
## P-Value = 0.9966

# 4º Passo:

## Como P-Value = 0.9966 > α = 0.1, não se rejeita H0.

# 5º Passo:

# Com base nas amostras e com 10% de significância, pode-se afirmar
# que o fornecedor 1 tem uma média de resistência menor ou igual que
# o fornecedor 2, logo a dona da loja deve optar pelo fornecedor 2.

"----------------------------------------------------------------------"

#### Exercicio 5.7 ####


"-------- Enunciado --------"
#'* Numa experiência, dois grupos de ratos fêmeas foram alimentados   *
#'* com dietas apresentando alto e baixo conteúdo de proteína. A      *
#'* tabela seguinte fornece, para cada rato, o ganho de peso, em      *
#'* gramas, entre o 28º e o 84º dia de vida:                          *
#'*                                                                   *
#'*  Alto Conteúdo de Proteína: 123, 134, 146, 104, 119, 124, 161,    *
#'*                             107,  83, 113,  97, 129               *
#'*                                                                   *
#'* Baixo Conteúdo de Proteína: 70, 118, 101, 85, 107, 132, 94        *
#'*                                                                   *
#'* Suponha que o ganho de peso, com alto ou baixo conteúdo de        *
#'* proteína, segue uma distribuição normal com a mesma variância.    *
#'* 1.) Ao nível de significância de 1%, há evidência estatística de  *
#'*     que a dieta com alto conteúdo de proteína aumenta o ganho de  *
#'*     peso?                                                         *
#'* 2.) Calcule o valor−p associado ao teste realizado na alínea 1.   *
"---------------------------"


# Populações:
## Normais;
## Variâncias Desconhecidas;
## Variâncias Iguais.

# Amostras:
## Independentes.

(amostra_5_7_alto <- c(123, 134, 146, 104, 119, 124, 161, 107,  83, 113,  97, 129))
(amostra_5_7_baixo <- c(70, 118, 101, 85, 107, 132, 94))

#### 1) #####

# Teste de Hipóteses Paramétrico para a Diferença de Médias:

# 1º Passo:

## H0: μ1 <= μ2 vs. H1: μ1 > μ2 <=> H0: μ1 - μ2 <= 0 vs. H1: μ1 - μ2 > 0
## Teste Unilateral Direito

# 2º Passo:

## α = 0.01

# 3º Passo:

## D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))))
t.test(
  x = amostra_5_7_alto,                 # Primeira Amostra
  y = amostra_5_7_baixo,                 # Segunda Amostra
  paired = FALSE,                 # As Amostras são Dependentes?
  var.equal = TRUE,               # As Variâncias são Iguais?
  alternative = "greater"
)
## t_obs = 1.8914
## P-Value = 0.03787

# 4º Passo:

## Como P-Value = 0.03787 > α = 0.01, não se rejeita H0.

# 5º Passo:

# Com base nas amostras e com 1% de significância, pode-se afirmar
# que a dieta com alto conteúdo de proteína não aumenta o ganho de peso.

#### 2) #####

# valor-p =
# P(T >= t_obs) =
# 1 - P(T < t_obs) =
# 1 - F(1.8914) =
# 1 - pt(1.8914, length(amostra_5_7_alto) + length(amostra_5_7_baixo) - 2) =
# 0.03787

"----------------------------------------------------------------------"

#### Exercicio 5.8 ####


"-------- Enunciado --------"
#'* Cinco operadores de máquinas são treinados em duas máquinas de    *
#'* diferentes fabricantes, para verificar qual delas apresentava     *
#'* maior facilidade de aprendizagem. Mediu-se o tempo que cada um    *
#'* dos operadores gastou na realização de uma mesma tarefa com cada  *
#'* um dos dois tipos de máquinas. Os resultados foram:               *
#'*                                                                   *
#'* Operador: 1, 2, 3, 4, 5                                           *
#'*                                                                   *
#'* Fabricante 1: 80, 72, 65, 78, 85                                  *
#'*                                                                   *
#'* Fabricante 2: 75, 70, 60, 72, 78                                  *
#'*                                                                   *
#'* Suponha que a variável em estudo segue uma distribuição normal.   *
#'* 1.) Estas amostras podem ser consideradas independentes?          *
#'* 2.) Ao nível de significância de 10% é possível afirmar que a     *
#'*     tarefa realizada na máquina do fabricante 1 demora, em média, *
#'*     mais do que na máquina do fabricante 2?                       *
"---------------------------"


# Populações:
## Normais.

# Amostra Fabricante 1:
(amostra_5_8_fab1 <- c(80, 72, 65, 78, 85))
# Amostra Fabricante 2:
(amostra_5_8_fab2 <- c(75, 70, 60, 72, 78))

#### 1) #####

# As amostras apesar de serem de máquinas de fabricantes diferentes,
# dozem respeito aos tempos dos mesmos operadores. Por este motivo,
# as amostras são consideradas emparelhadas.

#### 2) #####

# Teste de Hipóteses Paramétrico para a Diferença de Médias:

# 1º Passo:

## H0: μ1 <= μ2 vs. H1: μ1 > μ2 <=> H0: μ1 - μ2 <= 0 vs. H1: μ1 - μ2 > 0
## Teste Unilateral Direito

# 2º Passo:

## α = 0.1

# 3º Passo:

## Como as populações são normais e os desvios padrões são desconhecidos,
## é necessário determinar se eles podem ser ou não considerados iguais,
## para se poder proceder à escolha da D.A. certa.

var.test(
  x = amostra_5_8_fab1,  # Primeira Amostra
  y = amostra_5_8_fab2,  # Segunda Amostra
  conf.level = 0.90      # Grau de Confiança
)
## I.C. para as Variâncias: ] 0.1981702 , 8.0872310 [

## Como 1 E I.C., as variâncias podem ser consideradas iguais.
## Logo:

# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))))
t.test(
  x = amostra_5_8_fab1,     # Primeira Amostra
  y = amostra_5_8_fab2,     # Segunda Amostra
  paired = TRUE,            # As Amostras são Dependentes?
  var.equal = TRUE,         # As Variâncias são Iguais?
  alternative = "greater"
)
## t_obs = 5.9761
## P-Value = 0.00197

# 4º Passo:

## Como P-Value = 0.00197 <= α = 0.1, rejeita-se H0.

# 5º Passo:

## Com base nas amostras e com 1% de significância, pode-se concluir
## que a tarefa realizada na máquina do fornecedor 1 demora, em
## média, mais do que na máquina do fornecedor 2.

"----------------------------------------------------------------------"

#### Exercicio 5.9

"-------- Enunciado --------"
#'* Um laboratório possui dois equipamentos de precisão. O diretor    *
#'* do laboratório suspeita que existe uma pequena diferença de       *
#'* calibração entre os dois (ele não sabe em qual deles) de modo     *
#'* que um tende a dar leituras um pouco maiores do que o outro. Ele  *
#'* propõe testar os dois aparelhos através da leitura das mesmas 10  *
#'* medidas:                                                          *
#'*                                                                   *
#'* Medida: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10                             *
#'*                                                                   *
#'* Aparelho A: 12.2,  12.1, 10.55, 13.33, 11.42,                     *
#'*             10.3, 12.32, 13.27, 11.93, 12.5                       *
#'*                                                                   *
#'* Aparelho B: 12.5,  12.2, 10.57, 13.32, 11.47,                     *
#'*             10.3, 12.36, 13.29, 11.91, 12.61                      *
#'*                                                                   *
#'* Suponha que a variável em estudo segue uma distribuição normal.   *
#'* 1.) Estas amostras podem ser consideradas independentes?          *
#'* 2.) Ao nível de significância de 1% acha que a suspeita do        *
#'*     diretor faz sentido?                                          *
"---------------------------"


# Populações Normais.

(amostra5_9_A <- c(12.2,  12.1, 10.55, 13.33, 11.42, 10.3, 12.32, 13.27, 11.93, 12.5))
(amostra5_9_B <- c(12.5,  12.2, 10.57, 13.32, 11.47, 10.3, 12.36, 13.29, 11.91, 12.61))

#### 1) #####

# Não. Como se tratam das mesmas medidas (foram lidas por
# aparelhos diferentes mas referem-se às mesmas medidas) as
# amostras são emparelhadas.

#### 2) #####

# Teste de Hipóteses Paramétrico para a Diferença de Médias:

# 1º Passo:

## H0: μ1 = μ2 vs. H1: μ1 != μ2 <=> H0: μ1 - μ2 = 0 vs. H1: μ1 - μ2 != 0
## Teste Bilateral

# 2º Passo:

## α = 0.01

# 3º Passo:

## Como as populações são normais e os desvios padrões são desconhecidos,
## é necessário determinar se eles podem ser ou não considerados iguais,
## para se poder proceder à escolha da D.A. certa.

var.test(
  x = amostra5_9_A,  # Primeira Amostra
  y = amostra5_9_B,  # Segunda Amostra
  conf.level = 0.90      # Grau de Confiança
)
## I.C. para as Variâncias: ] 0.3053108 , 3.0852759 [

## Como 1 E I.C., as variâncias podem ser consideradas iguais.
## Logo:

# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))))
t.test(
  x = amostra5_9_A,     # Primeira Amostra
  y = amostra5_9_B,     # Segunda Amostra
  paired = TRUE,            # As Amostras são Dependentes?
  var.equal = TRUE,         # As Variâncias são Iguais?
  mu = 0,
  alternative = "two.sided"
)
## t_obs = -2.0423
## P-Value = 0.0715

# 4º Passo:

## Como P-Value = 0.0715 > α = 0.01, não se rejeita H0.

# 5º Passo:

## Com base nas amostras e com 1% de significância, pode-se concluir
## que a suspeita do diretos não faz sentido, uma vez que as amostras
## parecem ter, em média, dados iguais.

"----------------------------------------------------------------------"

#### Exercicio 5.10 ####


"-------- Enunciado --------"
#'* Uma clínica pretende comparar dois tipos de dietas. Com esse      *
#'* objetivo, escolheu aleatoriamente e independentemente, uma        *
#'* amostra de 100 pacientes com excesso de peso e durante 10 semanas *
#'* metade desses pacientes foram sujeitos à dieta 1 e os restantes à *
#'* dieta 2. Após as 10 semanas, anotou-se o peso perdido (em kg) por *
#'* cada paciente e obteve-se:                                        *
#'*                                                                   *
#'* Paciente:  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,                 *
#'*           11, 12, 13, 14, 15, 16, 17, 18, 19, 20,                 *
#'*           21, 22, 23, 24, 25, 26, 27, 28, 29, 30,                 *
#'*           31, 32, 33, 34, 35, 36, 37, 38, 39, 40,                 *
#'*           41, 42, 43, 44, 45, 46, 47, 48, 49, 50                  *
#'*                                                                   *
#'* Dieta 1: 8.0, 15.2,  6.7, 6.5,  9.1, 8.5, 14.1,  8.9, 10.9,  7.6, *
#'*          9.4,  8.6,  9.4, 6.7, 11.5, 6.8, 10.9,  6.7, 11.0, 10.6, *
#'*          9.8,  6.6, 10.7, 9.4, 13.6, 8.2,  8.0, 12.3, 14.2,  9.1, *
#'*          7.3, 11.3,  6.9, 8.3,  7.2, 7.2, 10.5, 11.1, 14.9, 12.4, *
#'*          7.9,  6.3,  6.2, 6.3,  5.7, 9.0,  9.5,  7.1,  8.0,  6.9  *
#'*                                                                   *
#'* Dieta 2: 4.8, 5.7,  7.8,  7.7, 10.7,  9.1, 6.3, 8.4,  6.2, 12.6,  *
#'*          8.4, 7.7,  9.1, 12.0,  8.8,  6.0, 8.7, 7.2,  6.2, 10.0,  *
#'*          8.6, 6.1,  6.9,  6.2,  7.0, 11.5, 9.0, 7.2, 11.1,  6.0,  *
#'*          6.6, 5.0, 12.4,  7.1,  9.9,  8.8, 5.5, 6.6,  3.8,  8.0,  *
#'*          6.8, 8.8, 17.6,  7.9,  9.4,  6.1, 6.0, 5.8, 16.4,  4.9   *
#'*                                                                   *
#'* Deve-se admitir que as duas dietas têm, em média, o mesmo efeito  *
#'* na perda de peso? Justifique a resposta considerando α = 0.05.    *
"---------------------------"


(amostra_5_10_dieta1 <- c(
  8.0, 15.2,  6.7, 6.5,  9.1, 8.5, 14.1,  8.9, 10.9,  7.6,
  9.4,  8.6,  9.4, 6.7, 11.5, 6.8, 10.9,  6.7, 11.0, 10.6,
  9.8,  6.6, 10.7, 9.4, 13.6, 8.2,  8.0, 12.3, 14.2,  9.1,
  7.3, 11.3,  6.9, 8.3,  7.2, 7.2, 10.5, 11.1, 14.9, 12.4,
  7.9,  6.3,  6.2, 6.3,  5.7, 9.0,  9.5,  7.1,  8.0,  6.9
))

(amostra_5_10_dieta2 <- c(
  4.8, 5.7,  7.8,  7.7, 10.7,  9.1, 6.3, 8.4,  6.2, 12.6,
  8.4, 7.7,  9.1, 12.0,  8.8,  6.0, 8.7, 7.2,  6.2, 10.0,
  8.6, 6.1,  6.9,  6.2,  7.0, 11.5, 9.0, 7.2, 11.1,  6.0,
  6.6, 5.0, 12.4,  7.1,  9.9,  8.8, 5.5, 6.6,  3.8,  8.0,
  6.8, 8.8, 17.6,  7.9,  9.4,  6.1, 6.0, 5.8, 16.4,  4.9
))

# Teste de Hipóteses Paramétrico para a Diferença de Médias:

# 1º Passo:

## H0: μ1 = μ2 vs. H1: μ1 != μ2 <=> H0: μ1 - μ2 = 0 vs. H1: μ1 - μ2 != 0
## Teste Bilateral

# 2º Passo:

## α = 0.05

# 3º Passo:

## As populações são desconhecidas e os desvios padrões
## também são desconhecidos.
## Logo:

# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((s1^2 / n1) + (s2^2 / n2))) ~ N(0, 1)
BSDA::z.test(
  x = amostra_5_10_dieta1,                 # Primeira Amostra
  sigma.x = sd(amostra_5_10_dieta1),       # Desvio Padrão da Amostra 1
  y = amostra_5_10_dieta2,                 # Segunda Amostra
  sigma.y = sd(amostra_5_10_dieta2),       # Desvio Padrão da Amostra 2
  alternative = "two.sided"
)
## Z_obs = 2.0142
## P-Value = 0.04399

# 4º Passo:

## Como P-Value = 0.04399 <= α = 0.05, rejeita-se H0.

# 5º Passo:

## Com base nas amostras e com 5% de significância, pode-se concluir
## que as duas dietas não têm, em média, o mesmo efeito na perda de peso.

"----------------------------------------------------------------------"

#### Exercicio 5.11 ####


"-------- Enunciado --------"
#'* Num exame de Estatística efetuado na 2ª época do ano letivo de    *
#'* 2016/2017 numa Escola Superior, foram avaliados 31 alunos onde se *
#'* obtiveram os seguintes resultados:                                *
#'*  10.1, 10.8, 11.4,  8.4,  7.7, 11.4, 11.1,  9.8, 11.4,  8.9, 7.5, *
#'*   8.9,  8.0, 10.0, 11.5, 10.5, 10.0,  8.6, 10.9,  9.1, 10.2, 8.7, *
#'*  12.3, 10.7, 11.5,  9.3,  9.7, 10.1, 12.3,  8.6, 10.4             *
#'* Considere estes alunos como uma amostra representativa da         *
#'* população dos alunos matriculados na unidade curricular de        *
#'* Estatística.                                                      *
#'* 1.) Mostre, para um nível de significância de 5% que as notas     *
#'*     seguem um comportamento normal.                               *
#'* 2.) Teste a hipótese σ^2 = 5 contra a hipótese σ^2 > 5 para um    *
#'*     α = 0.05.                                                     *
"---------------------------"


# População:
## Qualquer.

# Amostra:
## n = 31.
(amostra_5_11 <- c(
  10.1, 10.8, 11.4,  8.4,  7.7, 11.4, 11.1,  9.8, 11.4,  8.9, 7.5,
   8.9,  8.0, 10.0, 11.5, 10.5, 10.0,  8.6, 10.9,  9.1, 10.2, 8.7,
  12.3, 10.7, 11.5,  9.3,  9.7, 10.1, 12.3,  8.6, 10.4
))

#### 1) #####

# Teste de Hipóteses Não Paramétrico: Teste de Ajustamento:

# 1º Passo:

## H0: X1 ~ Normal vs. H1: X1 !~ Normal

# 2º Passo:

## α = 0.05

# 3º Passo:

## n = 31 < 50.
## Logo:
shapiro.test(
  amostra_5_11  # Amostra
)
## W_1_obs = 0.97009
## P-Value_1 = 0.5215

# 4º Passo:

## Como P-Value = 0.5215 > α = 0.05, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 5% de significância, pode-se concluir
## que a população pode ser considerada Normal.

#### 2) #####

# Teste de Hipóteses Paramétrico: Teste para a Variância:

# 1º Passo:

## H0: σ^2 = 5 vs. H1: σ^2 > 5
## Teste Unilateral Direito

# 2º Passo:

## α = 0.05

# 3º Passo:

# D.A.: X^2 = (((n-1) * s^2) / σ^2) ~ X^2(n-1)
EnvStats::varTest(
  x = amostra_5_11,                   # Amostra
  conf.level = 0.95,  # Grau de Confiança
  alternative = "greater",
  sigma.squared = 5
)
## X^2_obs = 10.4277
## P-Value = 0.9997

# 4º Passo:

## Como P-Value = 0.9997 > α = 0.05, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 5% de significância, pode-se concluir
## que a variância é igual a 5.

"----------------------------------------------------------------------"

#### Exercicio 5.12 ####


"-------- Enunciado --------"
#'* Considere as seguintes amostras:                                  *
#'*   Amostra da População 1: 12.96,  6.19, 14.62, 1.79, 7.40, 4.56,  *
#'*                            1.54, 15.12,  9.09, 7.14, 7.65, 9.33,  *
#'*                            6.93,  9.07,  1.82, 5.44, 0.04, 2.94,  *
#'*                            0.46,  5.07,  5.28                     *
#'*   Amostra da População 2:  7.85,  8.23,  1.75, 7.22, 3.13, 1.12,  *
#'*                            1.57,  5.67,  3.87                     *
#'* 1.) Mostre, para um nível de significância de 5% que se pode      *
#'*     considerar que ambas as amostras foram extraídas de           *
#'*     populações normais.                                           *
#'* 2.) Para α = 0.05, teste a hipótese da variância da população 1   *
#'*     ser superior à da população 2.                                *
"---------------------------"


# Populações Desconhecidas.

# Amostra 1:
## n1 = 21.
(amostra_5_12_1 <- c(
  12.96,  6.19, 14.62, 1.79, 7.40, 4.56,
  1.54, 15.12,  9.09, 7.14, 7.65, 9.33,
  6.93,  9.07,  1.82, 5.44, 0.04, 2.94,
  0.46,  5.07,  5.28
))

# Amostra 2:
## n2 = 9.
(amostra_5_12_2 <- c(
  7.85,  8.23,  1.75, 7.22, 3.13, 1.12,
  1.57,  5.67,  3.87
))

#### 1) #####

# Teste de Hipóteses Não Paramétrico: Teste de Ajustamento:

# 1º Passo:

## TA1 => H0: X1 ~ Normal vs. H1: X1 !~ Normal
## TA2 => H0: X2 ~ Normal vs. H1: X2 !~ Normal

# 2º Passo:

## α = 0.05

# 3º Passo:

## n1 = 21 e n2 = 9 < 50.
## Logo:

shapiro.test(
  amostra_5_12_1  # Amostra
)
## W_1_obs = 0.94824
## P-Value_1 = 0.3155

shapiro.test(
  amostra_5_12_2  # Amostra
)
## W_2_obs = 0.89358
## P-Value_2 = 0.2171

# 4º Passo:

## Como P-Value_1 = 0.3155 e P-Value_2 = 0.2171 > α = 0.05, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 5% de significância, pode-se concluir
## que as populações podem ser consideradas Normais.

#### 2) #####

# Teste de Hipóteses Paramétrico: Teste para o Quociente de Variâncias:

# 1º Passo:

## H0: σ1^2 <= σ2^2 vs. H1: σ1^2 > σ2^2
## <=>
## H0: σ1^2/σ2^2 <= 1 vs. H1: σ1^2/σ2^2 > 1
## Teste Unilateral Direito

# 2º Passo:

## α = 0.05

# 3º Passo:

## D.A.: F = ((s1^2 / s2^2) * (σ2^2 / σ1^2)) ~ F(n1 - 1, n2 - 1)
var.test(
  x = amostra_5_12_1,                 # Primeira Amostra
  y = amostra_5_12_2,                 # Segunda Amostra
  conf.level = 0.95,  # Grau de Confiança
  alternative = "greater",
  ratio = 1
)
## F_obs = 2.3462
## P-Value = 0.1085

# 4º Passo:

## Como P-Value = 0.1085 > α = 0.05, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 5% de significância, pode-se concluir
## que a variância da população 1 parece ser inferior ou igual à
## variância da população 2.

"----------------------------------------------------------------------"

#### Exercicio 5.13 ####


"-------- Enunciado --------"
#'* Suponha que as produções (gramas) de comprimidos, em intervalos   *
#'* de tempo fixos, aleatoriamente selecionados de duas máquinas M1   *
#'* e M2 de um laboratório se podem considerar normais com variâncias *
#'* iguais. Os pesos obtidos em duas amostras foram os seguintes:     *
#'* M1: 10.25; 10.56; 10.23; 10.45; 10.54; 10.21; 10.65; 10.56        *
#'* M2: 11.15; 10.84; 10.69; 10.78; 10.93; 10.87; 11.03; 10.93; 10.30 *
#'* 1.) Mostre, para um nível de significância de 1% que se pode      *
#'*     considerar que ambas as amostras foram extraídas de           *
#'*     populações normais.                                           *
#'* 2.) Verifique se é plausível considerar que a variabilidade, em   *
#'*     gramas, da produção das duas máquinas é idêntica (α = 0.01).  *
#'* 3.) O técnico responsável pelas duas máquinas garante que a       *
#'*     máquina 2 produz, em média, mais quantidade que a máquina 1.  *
#'*     Ao nível de 1%, esta afirmação é consistente com os dados?    *
"---------------------------"


# Populações:
## Normais;
## Variâncias Desconhecidas e Iguais.

# Amostra 1:
## n1 = 8
(amostra_5_13_M1 <- c(10.25, 10.56, 10.23, 10.45, 10.54, 10.21, 10.65, 10.56))

# Amostra 2:
## n1 = 9
(amostra_5_13_M2 <- c(11.15, 10.84, 10.69, 10.78, 10.93, 10.87, 11.03, 10.93, 10.30))

#### 1) #####

# Teste de Hipóteses Não Paramétrico: Teste de Ajustamento:

# 1º Passo:

## TA1 => H0: X1 ~ Normal vs. H1: X1 !~ Normal
## TA2 => H0: X2 ~ Normal vs. H1: X2 !~ Normal

# 2º Passo:

## α = 0.01

# 3º Passo:

## n1 = 8 e n2 = 9 < 50.
## Logo:

shapiro.test(
  amostra_5_13_M1  # Amostra
)
## W_1_obs = 0.85795
## P-Value_1 = 0.1146

shapiro.test(
  amostra_5_13_M2  # Amostra
)
## W_2_obs = 0.90201
## P-Value_2 = 0.2639

# 4º Passo:

## Como P-Value_1 = 0.1146 e P-Value_2 = 0.2639 > α = 0.01, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 1% de significância, pode-se concluir
## que as populações podem ser consideradas Normais.

#### 2) #####

# Teste de Hipóteses Paramétrico: Teste para o Quociente de Variâncias:

# 1º Passo:

## H0: σ1^2 <= σ2^2 vs. H1: σ1^2 > σ2^2
## <=>
## H0: σ1^2/σ2^2 = 1 vs. H1: σ1^2/σ2^2 != 1
## Teste Bilateral

# 2º Passo:

## α = 0.01

# 3º Passo:

## D.A.: F = ((s1^2 / s2^2) * (σ2^2 / σ1^2)) ~ F(n1 - 1, n2 - 1)
var.test(
  x = amostra_5_13_M1,                 # Primeira Amostra
  y = amostra_5_13_M2,                 # Segunda Amostra
  conf.level = 0.99,  # Grau de Confiança
  alternative = "two.sided",
  ratio = 1
)
## F_obs = 0.52755
## P-Value = 0.4144

# 4º Passo:

## Como P-Value = 0.4144 > α = 0.01, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 1% de significância, pode-se concluir
## que as variabilidades da produção de ambas as máquinas parece ser
## idêntica.

#### 3) #####

# Teste de Hipóteses Paramétrico: Teste para a Diferença de Médias:

# 1º Passo:

## H0: μ2 <= μ1 vs. H1: μ2 > μ1
## <=>
## H0: μ1 >= μ2 vs. H1: μ1 < μ2
## <=>
## H0: μ1-μ2 >= 0 vs. H1: μ1-μ2 < 0
## Teste Unilateral Esquerdo

# 2º Passo:

## α = 0.01

# 3º Passo:

## Populações Normais;
## σ1 e σ2 Desconhecidos;
## σ1 = σ2.
## Logo:

# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))))
# T ~ t(n1 + (n2 - 2))
t.test(
  x = amostra_5_13_M1,   # Primeira Amostra
  y = amostra_5_13_M2,   # Segunda Amostra
  paired = FALSE,        # As Amostras são Dependentes?
  var.equal = TRUE,      # As Variâncias são Iguais?
  conf.level = 0.99,     # Grau de Confiança
  alternative = "less",  # Tipo de Teste
  mu = 0
)
## t_obs = -3.9013
## P-Value = 0.0007088

# 4º Passo:

## Como P-Value = 0.0007088 <= α = 0.01, rejeita-se H0.

# 5º Passo:

## Com base na amostra e com 1% de significância, pode-se concluir
## que a máquina 2 produz, em média, mais quantidade que a máquina 1.

"----------------------------------------------------------------------"

#### Exercicio 5.14 ####


"-------- Enunciado --------"
#'* O diretor de produção de uma dada empresa suspeita que uma dada   *
#'* máquina da cadeia de produção está avariada em mais de 10% dos    *
#'* dias de laboração. Tendo recorrido ao histórico de produção nos   *
#'* últimos 180 dias verificou que a máquina esteve avariada em 20    *
#'* desses dias. Através de um teste de hipóteses paramétrico         *
#'* apropriado diga, justificando, se as suspeitas do diretor de      *
#'* produção são estatisticamente aceitáveis para um nível de         *
#'* significância de 5%.                                              *
#'* 1.) Tome a decisão recorrendo à região crítica.                   *
#'* 2.) Tome a decisão recorrendo ao valor-p.                         *
"---------------------------"


# População:
## Desconhecida.

# Amostra:
## n = 180
## p* = 20/180 = 0.1111111

#### 1) #####

# Teste de Hipóteses Paramétrico: Teste para a Proporção:

# 1º Passo:

## H0: p <= 0.10 vs. p > 0.10
## Teste Unilateral Direito

# 2º Passo:

## α = 0.05

# 3º Passo:

## D.A.: Z = ((p* - p) / sqrt(pq / n)) ~=~ ((p* - p) / sqrt((p* * q*) / n)) ~ N(0, 1)

## Z_obs = ((0.1111111 - 0.10) / sqrt((0.10 * 0.90) / 180)) = 0.4969035

# 4º Passo:

## R.C. = [ z_(1-α) , +∞ [ = [ qnorm(1-0.05) , +∞ [ = [ 1.6449 , +∞ [

## Como Z_obs = 0.4969035 !E R.C. = [ 1.6449 , +∞ [, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 5% de significância, pode-se concluir
## que não há evidência estatística para corroborar as suspeitas do
## diretor de produção.

#### 2) #####

# Teste de Hipóteses Paramétrico: Teste para a Proporção:

# 1º Passo:

## H0: p <= 0.10 vs. p > 0.10
## Teste Unilateral Direito

# 2º Passo:

## α = 0.05

# 3º Passo:

## D.A.: Z = ((p* - p) / sqrt(pq / n)) ~=~ ((p* - p) / sqrt((p* * q*) / n)) ~ N(0, 1)

## Z_obs = ((0.1111111 - 0.10) / sqrt((0.10 * 0.90) / 180)) = 0.4969035
## P-Value = P(Z >= Z_obs) = 1 - P(Z < 0.4969035) = 1 - pnorm(0.4969035) = 0.3096

# 4º Passo:

## Como P-Value = 0.3096 > α = 0.05, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 5% de significância, pode-se concluir
## que não há evidência estatística para corroborar as suspeitas do
## diretor de produção.

"----------------------------------------------------------------------"

#### Exercicio 5.15 ####


"-------- Enunciado --------"
#'* O consumidor de um certo produto acusou o fabricante, dizendo que *
#'* mais de 20% das unidades fabricadas apresentam defeito. Para      *
#'* confirmar a sua acusação, o consumidor usou uma amostra de        *
#'* tamanho 50, onde 27% das peças eram defeituosas. Mostre como o    *
#'* fabricante poderia refutar a acusação. Utilize um nível de        *
#'* significância de 5%.                                              *
"---------------------------"


# Amostra:
## n = 50
## p* = 0.27

# Teste de Hipóteses Paramétrico: Teste para a Proporção:

# 1º Passo:

## H0: p <= 0.20 vs. p > 0.20
## Teste Unilateral Direito

# 2º Passo:

## α = 0.05

# 3º Passo:

## D.A.: Z = ((p* - p) / sqrt(pq / n)) ~=~ ((p* - p) / sqrt((p* * q*) / n)) ~ N(0, 1)

## Z_obs = ((0.27 - 0.20) / sqrt((0.20 * 0.80) / 50)) = 1.2374
## P-Value = P(Z >= Z_obs) = 1 - P(Z < 1.2374) = 1 - pnorm(1.2374) = 0.1080

# 4º Passo:

## Como P-Value = 0.1080 > α = 0.05, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 5% de significância, pode-se concluir
## que não existe evidência estatística para corroborar as suspeitas
## do diretor de produção.

"----------------------------------------------------------------------"

#### Exercicio 5.16 ####


"-------- Enunciado --------"
#'* Pretende-se saber a quantidade de nicotina (medida em miligramas) *
#'* existente numa determinada marca de cigarros. Examinaram-se 5     *
#'* cigarros dessa marca, tendo-se obtido os seguintes resultados:    *
#'*  16; 16.5; 19; 15.4; 15.6                                         *
#'* Sabendo que a variável segue uma distribuição normal:             *
#'* 1.) Teste ao nível de significância de 0.1 a hipótese:            *
#'*     H0 : µ = 13.5 contra H1 : µ != 13.5.                          *
#'* 2.) Teste, ao nível de significância de 0.05, a hipótese da       *
#'*     quantidade média de nicotina ser de 14 mg contra a hipótese   *
#'*     alternativa de ser de 13.5 mg.                                *
#'* 3.) Numa amostra de 100 fumadores, 20 contraíram cancro do        *
#'*     pulmão. Será que a percentagem de fumadores que contraem      *
#'*     cancro é superior a 18%? Justifique a sua resposta utilizando *
#'*     um teste de hipóteses adequado, com um nível de significância *
#'*     de 0.05.                                                      *
#'* 4.) Calcule o valor−p associado ao teste de hipóteses realizado   *
#'*     na alínea 3.                                                  *
"---------------------------"


# EX.

#### 1) #####



#### 2) #####



#### 3) #####



#### 4) #####



"----------------------------------------------------------------------"

#### Exercicio 5.17 ####


"-------- Enunciado --------"
#'* A tabela seguinte contém os resultados de um inquérito realizado   *
#'* junto de pessoas de ambos os sexos, acerca da opinião sobre a      *
#'* proibição de fumar nos restaurantes:                               *
#' *                     _____________________________________________ *
#'*                      | Deve ser Proibido | Não Deve ser Proibido | *
#'* |----------------------------------------------------------------- *
#'* | Número de Homens   |         50        |           50          | *
#'* |----------------------------------------------------------------- *
#'* | Número de Mulheres |         60        |           40          | *
#'* ̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅ *
#'* 1.) Um jornal afirma que a percentagem de mulheres a favor desta   *
#'*     proibição se situa acima dos 65%. Verifique a veracidade desta *
#'*     afirmação elaborando um teste de hipóteses adequado            *
#'*     (com α = 0.025).                                               *
#'* 2.) Teste a hipótese da percentagem de pessoas favoráveis à não    *
#'*     proibição ser igual para ambos os sexos (com α = 0.05).        *
"---------------------------"


# EX.

#### 1) #####



#### 2) #####



"----------------------------------------------------------------------"

#### Exercicio 5.18 ####


"-------- Enunciado --------"
#'* Um estudo nutricional detetou numa amostra de 55 hipertensos, 24   *
#'* com dietas pobres em sódio. Paralelamente, numa amostra de 149 não *
#'* hipertensos detetaram-se 36 com dietas pobres em sódio. Poderá     *
#'* concluir-se, para um nível de significância de 0.05, que a         *
#'* proporção de indivíduos sujeitos a dietas pobres em sódio é maior  *
#'* entre hipertensos?                                                 *
"---------------------------"


# EX.

"----------------------------------------------------------------------"

#### Exercicio 5.19 ####


"-------- Enunciado --------"
#'* Uma rutura na coligação que o elegeu levou o Presidente da Câmara  *
#'* Municipal de Vila do Rio a demitir-se e à consequente marcação de  *
#'* eleições intercalares. O Presidente da Câmara demissionário, uma   *
#'* figura muito popular na região, decidiu recandidatar-se ao cargo.  *
#'* Uma rádio local pretende anunciar logo após o fecho das urnas se   *
#'* o presidente demissionário foi ou não reeleito. Para isso decidiu  *
#'* realizar uma sondagem à boca das urnas (isto é, as intenções de    *
#'* voto serão recolhidas logo após os eleitores terem votado)         *
#'* entrevistando 500 eleitores da margem sul do rio selecionados      *
#'* aleatoriamente, tendo obtido que 220 eleitores votaram no          *
#'* presidente demissionário. Uma rádio local concorrente também       *
#'* realizou uma sondagem à boca das urnas, tendo entrevistado 300     *
#'* eleitores da margem norte, tendo registado que 156 votaram no      *
#'* presidente demissionário. A seleção dos eleitores nesta segunda    *
#'* sondagem também foi aleatória e as duas sondagens foram realizadas *
#'* de forma independente. Verifique ao nível de significância de 0.05 *
#'* se os resultados das duas sondagens podem ou não ser considerados  *
#'* diferentes.                                                        *
"---------------------------"


# EX.

"----------------------------------------------------------------------"

#### Exercicio 5.20 ####


"-------- Enunciado --------"
#'* No ficheiro ”Obesidade” tem as respostas a um inquérito efetuado  *
#'* num estudo sobre obesidade a um grupo de indivíduos obesos.       *
#'* 1.) Teste para um nível de significância de 1% se, em média, o    *
#'*     peso de quem come sempre vegetais é inferior ao peso de quem  *
#'*     nunca come vegetais.                                          *
#'* 2.) Teste para um nível de significância de 5% se, em média, o    *
#'*     peso de quem nunca pratica exercício físico é superior ao     *
#'*     peso de quem pratica pelo menos 1 dia exercício físico.       *
#'* 3.) Teste para um nível de significância de 6% se a percentagem   *
#'*     de obesos que come habitualmente alimentos altamente          *
#'*     calóricos é superior a 80%.                                   *
#'* 4.) Teste para um nível de significância de 3% se há diferenças   *
#'*     na percentagem de obesos que nunca comem entre as refeições   *
#'*     principais do género feminino e do género masculino.          *
"---------------------------"


# EX.

#### 1) #####



#### 2) #####



#### 3) #####



#### 4) #####



"----------------------------------------------------------------------"
