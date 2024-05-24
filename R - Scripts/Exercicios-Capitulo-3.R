#### Capítulo 3 - Exercícios ####

"----------------------------------------------------------------------"

#### Exercicio 3.1 ####


"-------- Enunciado --------"
#'* O peso dos indivíduos duma certa espécie de bivalves tem          *
#'* distribuição normal de média 31g e desvio padrão 2.4g.            *
#'* Recolhe-se uma amostra aleatória de 36 indivíduos desta espécie.  *
#'* 1.) Qual a probabilidade da média da amostra ser inferior a 30g?  *
#'* 2.) Qual a probabilidade da média da amostra estar compreendida   *
#'*     entre 30g e 32g?                                              *
"---------------------------"


# População:
## Normal;
## μ = 31g
## σ = 2.4g

# Amostra:
## n = 36

##### 1) #####

# P(x̅ < 30) = ?

# População Normal;
# σ Conhecido.
# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)

# P(Z < ((30 - 31) / (2.4 / sqrt(36)))) =
# P(Z < -2.5) =
# F(-2.5) =
# pnorm(-2.5) =
# 0.0062

##### 2) #####

# P(30 < x̅ < 32) = F(32) - F(30) = P(x̅ <= 32) - P(x̅ <= 30) = ?

# População Normal;
# σ Conhecido.
# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)

# P(Z <= ((32 - 31) / (2.4 / sqrt(36)))) - P(Z <= ((30 - 31) / (2.4 / sqrt(36)))) =
# P(Z <= 2.5) - P(Z <= -2.5) =
# F(2.5) - F(-2.5) =
# pnorm(2.5) - pnorm(-2.5) =
# 0.9876

"----------------------------------------------------------------------"

#### Exercicio 3.2 ####


"-------- Enunciado --------"
#'* O tempo de espera em pista para a descolagem de cada avião no      *
#'* aeroporto de Lisboa é uma variável aleatória com valor médio de    *
#'* 4 minutos e desvio padrão de 2,5 minutos. Suponha que se seleciona *
#'* ao acaso 50 aviões para se registarem os seus correspondentes      *
#'* tempos de espera. Calcule a probabilidade da média dos tempos de   *
#'* espera exceder os 5 minutos.                                       *
"---------------------------"


# População:
## X - Tempo de Descolagem
## μ = 4
## σ = 2.5

# Amostra:
## n = 50

# P(x̅ > 5) = ?

#                           .
# Z = ((x̅ - μ) / (σ / √n)) ~ N(0, 1)

# P(x̅ > 5) =
# P(Z > ((5 - 4) / (2.5 / √50))) =
# P(Z > ((5 - 4) / (2.5 / sqrt(50)))) =
# P(Z > 2.8284) =
# 1 -  P(Z <= 2.8284) =
# 1 - pnorm(2.8284) =
# 0.0023

"----------------------------------------------------------------------"

#### Exercicio 3.3 ####


"-------- Enunciado --------"
#'* Sabe-se que a idade de determinada camada do subsolo segue uma      *
#'* distribuição Normal com média de 0.5 milhões de anos e um desvio    *
#'* padrão de 20000 anos. Selecionadas ao acaso 10 amostras de subsolo, *
#'* calcule a probabilidade da média amostral das suas idades ser       *
#'* superior a 490000 anos.                                             *
"---------------------------"


# População Normal;
## μ = 500 000 anos
## σ =  20 000 anos

# Amostra:
## n = 10

# P(x̅ > 490 000) = ?

# População Normal;
# σ Conhecido.
# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)

# P(Z > ((490000 - 500000) / (20000 / sqrt(10)))) =
# P(Z > -1.5811) =
# 1 - P(Z <= -1.5811) =
# 1 - F(-1.5811) =
# 1 - pnorm(-1.5811) =
# 0.9431

"----------------------------------------------------------------------"

#### Exercicio 3.4 ####


"-------- Enunciado --------"
#'* A duração das chamadas recebidas na central telefônica de uma    *
#'* determinada empresa tem distribuição normal com média de 17      *
#'* minutos e desvio padrão de 5 minutos.                            *
#'* 1.) Determine a probabilidade de numa amostra aleatória de 100   *
#'*     chamadas, a duração média se situar entre os 16 minutos e    *
#'*     os 18 minutos.                                               *
#'* 2.) Se a população não fosse normal, qual seria a probabilidade  *
#'*     da alínea anterior?                                          *
#'* 3.) Qual o tamanho da amostra aleatória a recolher para que não  *
#'*     seja superior a 5% a probabilidade da média da amostra       *
#'*     diferir da média da população por mais de 5 minutos?         *
"---------------------------"


# População:
## Normal;
## μ = 17 minutos
## σ = 5 minutos

##### 1) #####

# Amostra:
## n = 100

# P(16 < x̅ < 18) = F(18) - F(16) = P(x̅ <= 18) - P(x̅ <= 16) = ? = 0.9545

# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)

# P(Z <= ((18 - 17) / (5 / sqrt(100)))) - P(Z <= ((16 - 17) / (5 / sqrt(100)))) =
# P(Z <= 2) - P(Z <= -2) =
# F(2) - F(-2) =
# pnorm(2) - pnorm(-2) =
# 0.9545

##### 2) #####

# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)

# Dado que a fórmula da distribuição amostral é igual,
# o resultado seria o mesmo (0.9545).

##### 3) #####

# Amostra:
## n = ? >= 3

# População Normal;
# σ Conhecido.
# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)

"------------ Primeira Forma ------------"

# P(x̅ - μ > 5) = 5% <=>
# P(x̅ - μ > 5) = 0.05 <=>
# P((x̅ - μ) / (σ / sqrt(n)) > 5 / (σ / sqrt(n))) = 0.05 <=>
# P(Z > 5 / (5 / sqrt(n))) = 0.05 <=>
# 1 - P(Z <= 5 / (5 / sqrt(n))) = 0.05 <=>
# - P(Z <= 5 / (5 / sqrt(n))) = 0.05 - 1 <=>
# - P(Z <= 5 / (5 / sqrt(n))) = -0.95 <=>
# P(Z <= 5 / (5 / sqrt(n))) = 0.95 <=>
# P(Z <= sqrt(n)) = 0.95 <=>
# Φ(sqrt(n)) = 0.95 <=>
# sqrt(n) = z(0.95) <=>
# sqrt(n) = qnorm(0.95) <=>
# sqrt(n) = 1.6449 <=>
# n = 1.6449^2 <=>
# n = 2.7057

# n >= 3

"------------ Segunda Forma ------------"

# P(x̅ - μ > 5) < 5% <=>
# P(x̅ - μ > 5) < 0.05 <=>
# P((x̅ - μ) / (σ / sqrt(n)) > 5 / (σ / sqrt(n))) < 0.05 <=>
# P(Z > 5 / (5 / sqrt(n))) < 0.05 <=>
# P(Z > sqrt(n)) < 0.05

# P(Z > sqrt(n)) = P(Z < -sqrt(n))

# 2 * P(Z > sqrt(n)) = 0.05 <=>
# P(Z > sqrt(n)) = 0.025 <=>
# 1 - Φ(Z <= sqrt(n)) = 0.025 <=>
# - Φ(sqrt(n)) = -0.975 <=>
# Φ(sqrt(n)) = 0.975 <=>
# Φ(sqrt(n)) = 0.975 <=>
# sqrt(n) = Φ^-1(0.975) <=>
# sqrt(n) = qnorm(0.975) <=>
# sqrt(n) = 1.9600 <=>
# n = 1.9600^2 <=>
# n = 3.8416

# n >= 4

"----------------------------------------------------------------------"

#### Exercicio 3.5 ####


"-------- Enunciado --------"
#'* Admita-se uma população Normal com parâmetros desconhecidos.  *
#'* Selecionou-se ao acaso uma amostra de dimensão 20 da qual     *
#'* resultou uma variância igual a 5. Calcule a probabilidade da  *
#'* média amostral ser inferior à média populacional em mais de   *
#'* 1.43 unidades.                                                *
"---------------------------"


# População:
## Normal;
## μ e σ Desconhecidos

# Amostra:
## n = 20
## S^2 = 5
## S = sqrt(5) = 2.2361

# P(x̅ < μ - 1.43) = ?

# D.A.: T = ((x̅ - μ) / (s / sqrt(n))) ~ t(n-1)

# P(x̅ < μ - 1.43) =
# P(T < ((μ - 1.43 - μ) / (2.2361 / sqrt(20)))) =
# P(T < (-1.43 / 0.5)) =
# P(T < -2.86) =
# F(-2.86) =
# pt(-2.86, 19) =
# 0.0050

"----------------------------------------------------------------------"

#### Exercicio 3.6 ####


"-------- Enunciado --------"
#'* O tempo X de reparação de um certo tipo de avaria, numa dada      *
#'* gama de computadores, tem distribuição exponencial de média       *
#'* 10 dias. Para controlar o processo de reparação por forma a       *
#'* melhorar o desempenho e diminuir o tempo de espera dos clientes,  *
#'* foi recolhida uma amostra aleatória de 49 registos de tempos      *
#'* de reparação (X1, . . ., X49). Calcule a probabilidade do tempo   *
#'* de reparação médio da amostra ser superior a 8.                   *
"---------------------------"


# X - Tempo de reparação de um certo tipo de avaria, em dias.
# X ~ Exp(10)

# População:
## Exponencial;
## Paramêtro Conhecido;
## σ = μ = E[X] = 10 dias;
## σ^2 = V[X] = 10^2 = 100 dias.

# Amostra:
## n = 49 (X1, ......, X49)

# P(x̅ > 8) = ?

# População Qualquer;
# σ Conhecido;
# n >= 30.
# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)

# P(x̅ > 8) =
# P(Z > ((x̅ - μ) / (σ / sqrt(n)))) =
# P(Z > ((8 - 10) / (10 / sqrt(49)))) =
# P(Z > (-2 / 1.4286)) =
# P(Z > -1.4) =
# 1 - P(Z <= -1.4) =
# 1 - Φ(-1.4) =
# 1 - pnorm(-1.4) =
# 0.9192

"----------------------------------------------------------------------"

#### Exercicio 3.7 ####


"-------- Enunciado --------"
#'* Sabe-se que o nível de colesterol no sangue está dependente,         *
#'* entre outras coisas, da idade das pessoas. Considere a               *
#'* população desses níveis de colesterol em adultos com idades          *
#'* superiores a 15 anos, que se sabe ter distribuição Normal de         *
#'* valor médio 275 mg/dl de sangue e desvio padrão 100 mg/dl,           *
#'* da qual se vai retirar uma amostra de dimensão 25. Considere         *
#'* ainda a população das crianças com idades inferiores a 15 anos,      *
#'* que se sabe ter uma distribuição Normal de valor médio 180 mg/dl     *
#'* de sangue e desvio padrão 40 mg/dl, da qual se vai retirar uma       *
#'* amostra de dimensão 20, independente da anterior.                    *
#'* Representando por X1 e X2 as médias das amostras atrás indicadas,    *
#'* respetivamente, calcule a probabilidade de:                          *
#'* 1.) X1 ser superior a 250 mg/dl de sangue.                           *
#'* 2.) X1 ser superior a X2.                                            *
#'* 3.) X1 - X2 estar compreendido entre 35 mg/dl e 155 mg/dl de sangue. *
"---------------------------"


# População 1 - Nivel de colestrol no sangue de pessoas com idade > 15.
# População 2 - Nivel de colestrol no sangue de pessoas com idade < 15.
# Amostras Independentes.

# População 1:
## Normal;
## μ1 = 275 mg/dl;
## σ1 = 100 mg/dl.

# População 2:
## Normal;
## μ2 = 180 mg/dl;
## σ2 = 40 mg/dl.

# Amostra 1:
## n1 = 25.

# Amostra 2:
## n2 = 20.

##### 1) #####

# P(x̅1 > 250) = ?

# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)

# P(x̅1 > 250) =
# P(Z > ((250 - 275) / (100 / sqrt(25)))) =
# P(Z > (-25 / 20)) =
# P(Z > -1.25) =
# 1 - Φ(-1.25) =
# 1 - pnorm(-1.25) =
# 0.8944

##### 2) #####

# P(x̅1 > x̅2) = P(x̅1 - x̅2 > 0) = ?

# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2))) ~ N(0, 1)

# P(x̅1 - x̅2 > 0) =
# P(Z > (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2)))) =
# P(Z > ((0 - (275 - 180)) / sqrt((100^2 / 25) + (40^2 / 20)))) =
# P(Z > (-95 / sqrt(400 + 80))) =
# P(Z > (-95 / 21.9089)) =
# P(Z > -4.3361) =
# 1 - P(Z <= -4.3361) =
# 1 - Φ(-4.3361) =
# 1 - pnorm(-4.3361) =
# 1

##### 3) #####

# P(35 < x̅1 - x̅2 < 155) =
# F(155) - F(35) =
# P(x̅1 - x̅2 <= 155) - P(x̅1 - x̅2 <= 35)

# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2))) ~ N(0, 1)

# P(x̅1 - x̅2 <= 155) =
# P(Z <= (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2)))) =
# P(Z <= ((155 - (275 - 180)) / sqrt((100^2 / 25) + (40^2 / 20)))) =
# P(Z <= 2.7386) =
# Φ(2.7386) =
# pnorm(2.7386) =
# 0.9969

# P(x̅1 - x̅2 <= 155) =
# P(Z <= (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2)))) =
# P(Z <= ((35 - (275 - 180)) / sqrt((100^2 / 25) + (40^2 / 20)))) =
# P(Z <= -2.7386) =
# Φ(-2.7386) =
# pnorm(-2.7386) =
# 0.0031

# P(x̅1 - x̅2 <= 155) - P(x̅1 - x̅2 <= 35) =
# 0.9969 - 0.0031 =
# 0.9938

"----------------------------------------------------------------------"

#### Exercicio 3.8 ####


"-------- Enunciado --------"
#'* Suponha que temos duas populações de indivíduos, a população 1      *
#'* e a população 2. A população 1 é composta por clientes de uma       *
#'* agência de um banco na região central de uma cidade e a             *
#'* população 2 é composta por clientes de uma agência do mesmo         *
#'* banco num bairro periférico da cidade. Um executivo do banco        *
#'* está desconfiado de que as duas populações de clientes possuem      *
#'* gastos mensais médios com cartão de crédito diferentes, acha que    *
#'* os clientes da agência central gastam mais que os clientes da       *
#'* agência periférica. Do que o executivo conhece das distribuições    *
#'* de gastos mensais com cartão de crédito dos clientes das duas       *
#'* agências, habitualmente é assumido que elas são aproximadamente     *
#'* normais com médias e desvios padrão idênticos. Com base em duas     *
#'* amostras aleatórias independentes de 15 clientes retirados das      *
#'* duas populações, obtiveram-se como desvios padrão amostrais 182€    *
#'* em relação à amostra da agência central e 165€ em relação à         *
#'* amostra da agência periférica. Qual a probabilidade do executivo    *
#'* obter uma média amostral da agência central que ultrapasse a média  *
#'* amostral da agência da periferia em mais de 130€ ?                  *
"---------------------------"


# População:
## Ambas Normais
## σ1 e σ2 Idênticos e Desconhecidos
## μ1 e μ2 Idênticas e Desconhecidos

# Amostra:
## na = nb = 15
## s1 = 182
## s2 = 165

# P(x̅1 > x̅2 + 130) =
# P(x̅1 - x̅2 > 130) =

# T = (((x̅1 - x̅2) - (μ1 - μ2)) / √(((1/na) + (1/nb)) * ((((na - 1) * s1^2) + ((nb - 1) * s2^2)) / (na + nb - 2))))
# T ~ t(na + nb - 2) ~ t(28)

# P( T > ((130 - 0) / sqrt(((1/15) + (1/15)) * ((((15 - 1) * 182^2) + ((15 - 1) * 165^2)) / 28))) ) =
# P(T > 2.0495) =
# 1 - P(T <= 2.0495) =
# 1 - F(2.0495) =
# 1 - pt(2.0495, 28) =
# 0.0249

"----------------------------------------------------------------------"

#### Exercicio 3.9 ####


"-------- Enunciado --------"
#'* Numa fábrica que produz cabos elétricos, sabe-se que a proporção    *
#'* de cabos defeituosos é de 0.45. Suponha que se pretende selecionar  *
#'* uma amostra aleatória de 500 cabos dessa fábrica. Qual a            *
#'* probabilidade da proporção de cabos defeituosos que vão calhar na   *
#'* amostra exceder 0.5?                                                *
"---------------------------"


# População:
## Desconhecida/Qualquer;
## p = 0.45.

# Amostra:
## n = 500.

# P(p* > 0.5) = ?

# D.A.: Z = ((p* - p) / sqrt(pq / n)) ~=~ ((p* - p) / sqrt((p* * q*) / n)) ~ N(0, 1)

# P(p^ > 0.5) =
# P(Z > ((p* - p) / sqrt(pq / n))) =
# P(Z > ((0.5 - 0.45) / sqrt((0.45 * (1 - 0.45)) / 500))) =
# P(Z > 2.2473) =
# 1 - P(Z <= 2.2473) =
# 1 - Φ(2.2473) =
# 1 - pnorm(2.2473) =
# 0.0123

"----------------------------------------------------------------------"

#### Exercicio 3.10 ####


"-------- Enunciado --------"
#'* Suponha que está em presença de duas populações Binomiais onde    *
#'* p1 = 0.6 e p2 = 0.5. Se se retirar da primeira população uma      *
#'* amostra de 50 observações e da segunda uma amostra com 40         *
#'* observações, qual a probabilidade de que o desvio entre as duas   *
#'* proporções amostrais seja, em valor absoluto, superior a 0.2?     *
"---------------------------"


# População 1:
## Binomial;
## p1 = 0.6.

# População 2:
## Binomial;
## p2 = 0.5.

# Amostra 1:
## n1 = 50.

# Amostra 2:
## n2 = 40.

# P(p1* - p2* > 0.2) = ?

# D.A.: Z = (((p1* - p2*) - (p1 - p2)) / sqrt(((p1 * q1) / (n1)) + ((p2 * q2) / (n2)))) ~ N(0, 1)

# P(p1* - p2* > 0.2) =
# P(Z > (((p1* - p2*) - (p1 - p2)) / sqrt(((p1 * q1) / (n1)) + ((p2 * q2) / (n2))))) =
# P(Z > ((0.2 - (0.6 - 0.5)) / sqrt(((0.6 * 0.4) / 50) + ((0.5 * 0.5) / 40)))) =
# P(Z > 0.9513) =
# 1 - Φ(0.9513) =
# 1 - pnorm(0.9513) =
# 0.1707

"----------------------------------------------------------------------"

#### Exercicio 3.11 ####


"-------- Enunciado --------"
#'* De uma população Normal de variância 64, tomou-se uma amostra   *
#'* aleatória de dimensão 16. Qual a probabilidade da variância     *
#'* amostral exceder 78?                                            *
"---------------------------"


# População:
## Normal;
## σ^2 = 64;
## σ = sqrt(64) = 8.

# Amostra:
## n = 16.

# P(S^2 > 78) = ?

# D.A.: X^2 = (((n-1) * s^2) / σ^2) ~ X^2(n-1) ~ X^2(15)

# P(S^2 > 78) =
# P(X^2 > ((15 * 78) / 64)) =
# P(X^2 > 18.2813) =
# 1 - F(18.2813) =
# 1 - pchisq(18.2813, 15) =
# 0.2482

"----------------------------------------------------------------------"

#### Exercicio 3.12 ####


"-------- Enunciado --------"
#'* Numa população Normal de média desconhecida e desvio padrão 5,     *
#'* calcule a probabilidade da variância de uma amostra aleatória de   *
#'* dimensão 20 dessa população estar compreendida entre 24.1 e 50.8.  *
"---------------------------"


# População:
## Normal;
## μ = Desconhecida;
## σ = 5;
## σ^2 = 5^2 = 25.

# Amostra:
## n = 20.

# P(24.1 < S^2 < 50.8) =
# F(50.8) - F(24.1) =
# P(S^2 <= 50.8) - P(S^2 <= 24.1) = ?

# D.A.: X^2 = (((n-1) * s^2) / σ^2) ~ X^2(n-1) ~ X^2(19)

# P(S^2 <= 50.8) =
# P(X^2 <= ((19 * 50.8) / 25)) =
# P(X^2 <= 38.608) =
# F(38.608) =
# pchisq(38.608, 19) =
# 0.9950

# P(S^2 <= 24.1) =
# P(X^2 <= ((19 * 24.1) / 25)) =
# P(X^2 <= 18.316) =
# F(18.316) =
# pchisq(18.316, 19) =
# 0.4986

# P(S^2 <= 50.8) - P(S^2 <= 24.1) =
# 0.9950 - 0.4986 =
# 0.4964

"----------------------------------------------------------------------"

#### Exercicio 3.13 ####


"-------- Enunciado --------"
#'* Recolheu-se uma amostra de dimensão 6 de uma população Normal         *
#'* com média μ e desvio padrão σ. Determine a probabilidade da           *
#'* variância da amostra ser inferior a 3 vezes a variância da população. *
"---------------------------"


# População:
## Normal;
## μ = Desconhecida;
## σ = Desconhecido.

# Amostra:
## n = 6.

# P(S^2 < 3 * σ^2) = ?

# D.A.: X^2 = (((n-1) * s^2) / σ^2) ~ X^2(n-1) ~ X^2(5)

# P(S^2 < 3 * σ^2) =
# P(X^2 < ((5 * 3 * σ^2) / σ^2)) =
# P(X^2 < 15) =
# F(15) =
# pchisq(15, 5) =
# 0.9896

"----------------------------------------------------------------------"

#### Exercicio 3.14 ####


"-------- Enunciado --------"
#'* Admita a existência de duas populações nas quais são definidas   *
#'* duas variáveis aleatórias X1 ~ N(μ1, 4) e X2 ~ N(μ2, 4) tal que  *
#'* μ1 - μ2 = -2.                                                    *
#'* Considere que se obtêm duas amostras aleatórias independentes,   *
#'* uma de cada população, com 9 e 16 elementos, respectivamente.    *
#'* 1.) Qual a probabilidade da média da segunda amostra exceder a   *
#'*     média da primeira em mais de 3 unidades?                     *
#'* 2.) Qual a probabilidade da variância da primeira amostra        *
#'*     ultrapassar o quádruplo da variância da segunda amostra?     *
"---------------------------"


# População 1:
## Normal;
## μ1 = Desconhecida;
## σ1 = 4.

# População 2:
## Normal;
## μ2 = Desconhecida;
## σ2 = 4.

# μ1 - μ2 = -2

# Amostra 1:
## n1 = 9.

# Amostra 2:
## n2 = 16.

# Amostras Independentes.

##### 1) #####

# P(x̅2 > x̅1 + 3) =
# P(-3 > x̅1 - x̅2) =
# P(x̅1 - x̅2 < -3) = ?

# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2))) ~ N(0, 1)

# P(x̅1 - x̅2 < -3) =
# P(Z < ((-3 - (-2)) / sqrt((4^2 / 9) + (4^2 / 16)))) =
# P(Z < (-1 / sqrt(1.7778 + 1))) =
# P(Z < -0.6) =
# Φ(-0.6) =
# pnorm(-0.6) =
# 0.2743

##### 2) #####

# P(S1^2 > S2^2 * 4) =
# P(S1^2 / S2^2 > 4) = ?

# D.A.: F = ((s1^2 / s2^2) * (σ2^2 / σ1^2)) ~ F(8, 15)

# P(S1^2 / S2^2 > 4) =
# P(F > (4 * (4^2 / 4^2))) =
# P(F > 4) =
# 1 - P(F <= 4) =
# 1 - F(4) =
# 1 - pf(4, 8, 15) =
# 0.01

"----------------------------------------------------------------------"
