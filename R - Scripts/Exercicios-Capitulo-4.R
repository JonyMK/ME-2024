#### Capítulo 4 - Exercícios ####

"----------------------------------------------------------------------"

#### Exercicio 4.1 ####


"-------- Enunciado --------"
#'* Seja X uma população com distribuição normal de média μ e desvio  *
#'* padrão igual a 2. Uma amostra aleatória de dimensão n = 25 foi    *
#'* extraída desta população e revelou uma média x̅ = 78.3.           *
#'* 1.) Calcule o intervalo de confiança para μ a 99%.                *
#'* 2.) Qual a amplitude do intervalo de confiança a 99% para μ?      *
#'* 3.) Qual a margem de erro do intervalo de confiança a 99% para μ? *
#'* 4.) Qual deverá ser a dimensão da amostra para que a amplitude    *
#'*     do intervalo de confiança a 99% para μ não exceda os 0.1?     *
#'* 5.) Calcule o intervalo de confiança a 95% para μ.                *
#'* 6.) Qual o efeito de variar o grau de confiança?                  *
#'* 7.) Qual deverá ser a dimensão da amostra para que a amplitude    *
#'*     do intervalo de confiança a 95% para μ não exceda os 0.1?     *
#'*     E a 99.9% de confiança? Interprete os resultados.             *
"---------------------------"


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


"-------- Enunciado --------"
#'* Certo equipamento de empacotamento automático, encontra-se        *
#'* regulado para encher embalagens de um quilo de certo produto.     *
#'* O seu deficiente funcionamento origina prejuízo para a empresa:   *
#'* se a maioria das embalagens tem peso inferior ao estabelecido,    *
#'* haverá reclamações por parte dos clientes e perda de prestígio;   *
#'* peso excessivo será, por outro lado, antieconômico. Aceita-se da  *
#'* experiência passada que o peso das embalagens se comporta         *
#'* normalmente com desvio padrão de 12 gramas. Para verificar a      *
#'* afinação do equipamento, selecionaram-se em determinada altura,   *
#'* nove embalagens cujos pesos exatos (em gramas) foram anotados:    *
#'* (983, 992, 1011, 976, 997, 1000, 1004, 983, 998).                 *
#'* 1.) Calcule uma estimativa pontual para a média da população.     *
#'* 2.) Construa um intervalo de confiança para μ com os seguintes    *
#'*     graus de confiança: 90%, 95% e 99%. Como varia a precisão do  *
#'*     intervalo (a sua amplitude) com a confiança escolhido?        *
#'* 3.) Com base nos intervalos de confiança calculados na alínea     *
#'*     anterior, acha que a máquina está regulada?                   *
#'* 4.) Num intervalo de confiança a 95% para a média da população,   *
#'*     qual deverá ser o tamanho da amostra a recolher para que a    *
#'*     margem de erro não seja superior a 1?                         *
"---------------------------"


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


"-------- Enunciado --------"
#'* A distância percorrida por um avião, desde o contato com o solo   *
#'* até à imobilização total, é uma variável aleatória X com          *
#'* distribuição normal. Os valores para X, numa série de 31          *
#'* aterrissagens, foram compilados e são apresentados de seguida     *
#'* (valores em milhares de metros):                                  *
#'*  Σ(xi) = 54.3                                                     *
#'*  Σ(xi^2) = 95.57^2                                                *
#'* 1.) Calcule estimativas pontuais para a média e variância da      *
#'*     população.                                                    *
#'* 2.) Determine um intervalo de confiança a 99% para a média.       *
#'*     Acha que é possível efetuar uma aterragem segura numa pista   *
#'*     com menos de 1500 metros? Justifique.                         *
"---------------------------"


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


"-------- Enunciado --------"
#'* Com a finalidade de estimar o peso médio (em quilos) das crianças *
#'* de 15 anos de idade em determinada região geográfica,             *
#'* selecionaram-se aleatoriamente 10 crianças que forneceram os      *
#'* seguintes dados: (33.1, 32.1, 40.9, 37.1, 37.7, 35.1, 30.2, 45.6, *
#'* 27.8, 37.3). Admita a normalidade.                                *
#'* 1.) Calcule estimativas para a média e o desvio padrão do peso    *
#'*     das crianças.                                                 *
#'* 2.) Determine um intervalo de confiança a 99% para o peso médio   *
#'*     de todas as crianças.                                         *
#'* 3.) Considerando que a estimativa para o peso médio não é         *
#'*     suficientemente precisa, dado que o intervalo de confiança    *
#'*     é demasiado amplo, o que sugere fazer para diminuir a         *
#'*     amplitude do intervalo de confiança?                          *
#'* 4.) Considerando que a estimativa para o peso médio não é         *
#'*     suficientemente precisa (dado que o intervalo de confiança    *
#'*     é demasiado grande), qual deve ser a dimensão da amostra a    *
#'*     recolher de modo a obter uma amplitude de 3 quilos com um     *
#'*     grau de confiança de 99%?                                     *
"---------------------------"


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


"-------- Enunciado --------"
#'* Considere uma população normal com parâmetros desconhecidos, de   *
#'* onde se obteve uma amostra aleatória com 16 observações, que      *
#'* permitiu construir o seguinte intervalo de confiança para a       *
#'* média da população: ] 7.05 , 12.95 [.                             *
#'* 1.) Determine a média amostral.                                   *
#'* 2.) Sabendo que, com a informação da amostra, se obteve s = 4,    *
#'*     qual o grau de confiança que pode atribuir ao intervalo       *
#'*     referido?                                                     *
#'* 3.) Suponha que a variância da população é 44. Se pretender       *
#'*     construir um intervalo de confiança a 95% para a média da     *
#'*     população, cuja amplitude não exceda 3.5, qual deverá ser a   *
#'*     dimensão da amostra a considerar?                             *
"---------------------------"


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


"-------- Enunciado --------"
#'* A concentração ativa de um ingrediente num detergente líquido é   *
#'* supostamente afetada pelo catalisador usado no processo. O desvio *
#'* padrão da concentração ativa é 3 gramas/litro independentemente   *
#'* do catalisador utilizado, sendo o comportamento do processo       *
#'* normal. Foram recolhidas 10 observações cada uma com o seu        *
#'* catalisador:                                                      *
#'* Catalisador 1: 57.9, 66.2, 65.4, 65.2, 62.6, 67.6, 63.7, 67.2,    *
#'*                71.0, 65.4                                         *
#'* Catalisador 2: 66.4, 71.7, 70.3, 69.3, 64.8, 69.6, 68.6, 69.4,    *
#'*                65.3, 68.8                                         *
#'* 1.) As amostra são independentes ou emparelhadas?                 *
#'* 2.) Determine um intervalo de confiança a 95% para a diferença    *
#'*     de médias dos dados obtidos pelos dois catalisadores. Em      *
#'*     média, os dados obtidos pelos dois catalisadores podem ser    *
#'*     considerados iguais?                                          *
"---------------------------"


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


"-------- Enunciado --------"
#'* Pretende-se investigar o nível de remuneração salarial dos homens *
#'* e mulheres de certa categoria profissional. De duas amostras      *
#'* obtidas entre dois grupos, destacam-se os seguintes resultados    *
#'* (em unidades monetárias):                                         *
#'* Amostra de 250 homens:    x̅1 = 33.8 | s1^2 = 5.7                 *
#'* Amostra de 150 mulheres:  x̅2 = 31   | s2^2 = 10.3                *
#'* Construa um intervalo de confiança a 99% para as diferenças       *
#'* salariais médias entre os dois géneros e conclua sobre a possível *
#'* existência de discriminação de género na atribuição de            *
#'* remunerações.                                                     *
"---------------------------"


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


"-------- Enunciado --------"
#'* Deseja-se saber se um programa de reabilitação após enfarte de         *
#'* miocárdio diminui a frequência cardíaca de esforçlo. Para tal,         *
#'* 10 doentes com enfarte do miocárdio foram submetidos a uma prova       *
#'* de esforço antes e depois do programa. Os resultados, expressos        *
#'* em batimentos por minuto, estão na tabela seguinte:                    *
#'*  _____________________________________________________________________ *
#'* |Doente: |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |  10 | *
#'* |--------------------------------------------------------------------- *
#'* |Antes : | 147 | 122 | 127 | 141 | 150 | 132 | 157 | 147 | 157 | 155 | *
#'* |--------------------------------------------------------------------- *
#'* |Depois: | 132 | 117 | 142 | 124 | 116 | 130 | 122 | 118 | 135 | 117 | *
#'*  ̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅ *
#'* Suponha que a variável em estudo segue uma distribuição normal.        *
#'* 1.) As amostra são independentes ou emparelhadas?                      *
#'* 2.) Recorrendo a um intervalo de confiança a 95% para a                *
#'*     diferença média, indique se acha que o programa de                 *
#'*     reabilitação foi eficaz.                                           *
"---------------------------"


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


"-------- Enunciado --------"
#'* Foi estudado o grau de satisfação (medido por questionário) de    *
#'* vários utentes de uma clínica dentária antes e depois de lhes     *
#'* ser aplicada uma nova prótese total removível. Os resultados,     *
#'* expressos em grau de satisfação, foram os apresentados na         *
#'* tabela seguinte:                                                  *
#'* Suponha que o grau de satisfação segue uma distribuição normal.   *
#' * _________________________________________________                *
#'* |Utente: |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |                *
#'* |-------------------------------------------------                *
#'* |Antes : |  4 | 10 |  8 | 13 |  7 |  3 | 15 |  7 |                *
#'* |-------------------------------------------------                *
#'* |Depois: |  4 | 16 | 11 | 17 | 17 |  4 | 18 | 11 |                *
#'*  ̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅                *
#'* 1.) Estas amostras podem ser consideradas independentes?          *
#'* 2.) Recorrendo a um intervalo de confiança a 99% para a diferença *
#'*     média, indique se a aplicação da nova prótese influenciou o   *
#'*     grau de satisfação dos utentes.                               *
"---------------------------"


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


"-------- Enunciado --------"
#'* Para comparar a eficiência de dois métodos de ensino, uma turma   *
#'* de 24 alunos foi dividida aleatoriamente em dois grupos. Cada     *
#'* grupo é ensinado de acordo com um método diferente. Os resultados *
#'* no fim do semestre são os seguintes (numa escala de 0 a 100):     *
#'* 1º Grupo: n1 = 13  |  x̅1 = 74.5  |  S1^2 = 82.6                  *
#'* 2º Grupo: n2 = 11  |  x̅2 = 71.8  |  Σ(x2i - x̅2)^2 = 1126        *
#'* Supondo que as populações são normais (com variâncias iguais),    *
#'* obteve-se o seguinte intervalo de confiança para a diferença      *
#'* entre os valores esperados das duas populações: ]−5.635, 11.035[. *
#'* 1.)  Indique qual o grau de confiança utilizado no cálculo deste  *
#'*      intervalo.                                                   *
#'* 2.) Com base num intervalo de confiança a 90% acha que, em média, *
#'*     os métodos de ensino podem ser considerados iguais?           *
#'* 3.) Com base num intervalo de confiança a 90% verifique se a      *
#'*     suposição das variâncias serem iguais é válida.               *
"---------------------------"


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


"-------- Enunciado --------"
#'* Duas marcas de comprimidos, um deles contendo aspirina, são       *
#'* anunciados como fazendo desaparecer a dor de cabeça em tempo      *
#'* recorde. Foram feitas experiências com cada um deles, tendo-se    *
#'* obtido duas amostras aleatórias independentes, cujos resultados   *
#'* (tempo em minutos) foram os seguintes:                            *
#'* Comprimido 1 (com aspirina): 9.6; 9.4; 9.3; 11.2; 11.4; 12.1;     *
#'*                              10.4; 9.6; 10.2; 8.8; 13.0           *
#'* Comprimido 2 (com aspirina): 11.4; 12.1; 10.4; 9.6; 8.5; 9.7;     *
#'*                              12.3; 12.4; 10.8; 10.8               *
#'* Admita que as populações são normais e que os desvios padrão são  *
#'* iguais.                                                           *
#'* 1.) Construa um intervalo de confiança a 95% para a verdadeira    *
#'*     diferença das médias das respostas aos dois medicamentos.     *
#'*     Acha que, em média, as respostas dos dois medicamentos podem  *
#'*     ser consideradas iguais?                                      *
#'* 2.) Com base num intervalo de confiança a 95% verifique se a      *
#'*     suposição dos desvios padrão serem iguais é válida.           *
"---------------------------"


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


"-------- Enunciado --------"
#'* Obtém-se uma amostra de 15 crânios de homens egípcios que viveram *
#'* por volta de 1850ac. Mede-se a largura máxima de cada crânio, e   *
#'* obtiveram-se da amostra uma média de 134.5 mm e um desvio padrão  *
#'* de 3.5 mm (com base em dados de Ancient Races of Thebaid, por     *
#'* Thomson e RandallMaciver ). Suponha que a largura máxima dos      *
#'* crânios tem um comportamento normal. Com esses dados amostrais,   *
#'* construa um intervalo de 95% de confiança para o desvio padrão    *
#'* populacional.                                                     *
"---------------------------"


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


"-------- Enunciado --------"
#'* Pretende-se estudar a variabilidade do tempo de espera (em        *
#'* minutos) de clientes num dado banco, onde os clientes entram      *
#'* numa fila única. Suponha que o tempo de espera segue uma          *
#'* distribuição normal. Construa um intervalo de 95% de confiança    *
#'* para o desvio padrão populacional sabendo que se recolheu a       *
#'* seguinte amostra:                                                 *
#'* (6.5; 6.6; 6.7; 6.8; 7.1; 7.3; 7.4; 7.7; 7.7; 7.7).               *
"---------------------------"


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


"-------- Enunciado --------"
#'* Considere-se a seguinte amostra de uma população cuja             *
#'* distribuição é Normal: (9; 14; 10; 12; 7; 3; 11; 12).             *
#'* Nestas condições, construa o intervalo de confiança a 99% mais    *
#'* adequado para a variância dessa população.                        *
"---------------------------"


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


"-------- Enunciado --------"
#'* Durante uma avaliação de desempenhos das escolas A e B, sugeriu-se *
#'* que a escola A tinha uma maior variabilidade que a escola B em     *
#'* termos das notas finais dos alunos. Fizeram-se 16 registos de      *
#'* classificações para a escola A e 21 registos de classificações     *
#'* para a escola B conduzindo às variâncias de 6.62 e 3.80,           *
#'* respetivamente. Suponha que as populações em estudo têm um         *
#'* comportamento normal. Construa um intervalo de confiança a 90%     *
#'* para a razão das verdadeiras variâncias e diga se a variabilidade  *
#'* das escolas pode ser considerada diferente.                        *
"---------------------------"


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


"-------- Enunciado --------"
#'* Numa região afetada por um surto epidémico, observou-se uma       *
#'* amostra de 2500 indivíduos, tendo-se encontrado 850 contaminados. *
#'* Determine intervalos de confiança a 95% e 98% de confiança para a *
#'* proporção de contaminados na população.                           *
"---------------------------"


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


"-------- Enunciado --------"
#'* Num estudo de mercado efetuado sobre uma amostra aleatória de     *
#'* 400 consumidores, foi encontrado o seguinte intervalo de          *
#'* confiança para a proporção de pessoas recetivas a um novo tipo    *
#'* de espuma de banho a lançar em breve no mercado: ]0.5114, 0.6086[ *
#'* 1.) Em relação à amostra recolhida, qual foi a percentagem de     *
#'*     pessoas recetivas a um novo tipo de espuma de banho?          *
#'* 2.) Mostre que o grau de confiança considerado no intervalo       *
#'*     calculado é de 95%.                                           *
#'* 3.) Comente as seguintes afirmações, indicando se estas lhe       *
#'*     parecem corretas ou incorretas:                               *
#'*  a.) 95% das pessoas vão passar a usar a nova espuma de banho.    *
#'*  b.) A quota de mercado poderá ser, com 95% de confiança, de 56%. *
"---------------------------"


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


"-------- Enunciado --------"
#'* Recolheu-se uma amostra de 40 alunos do 1.o ano da ESTSetúbal     *
#'* tendo-se verificado que 10 destes alunos frequentam os cursos que *
#'* escolheram em primeira opção.                                     *
#'* 1.) Calcule um intervalo de confiança a 95%, para a verdadeira    *
#'*     proporção de estudantes que está no curso que escolheu em     *
#'*     primeira opção.                                               *
#'* 2.) Se pretendesse reduzir a metade a amplitude do intervalo      *
#'*     anterior:                                                     *
#'*  a.) e manter a dimensão da amostra, qual o grau de confiança que *
#'*      deveria utilizar?                                            *
#'*  b.) e manter o grau de confiança, qual a dimensão da amostra que *
#'*      deveria utilizar? Suponha que não há alteração na estimativa *
#'*      da proporção.                                                *
#'*  c.) e manter o grau de confiança, qual a dimensão da amostra que *
#'*      deveria utilizar? Suponha que não conhece estimativas da     *
#'*      proporção.                                                   *
"---------------------------"


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


"-------- Enunciado --------"
#'* Uma repórter da revista Byte deseja fazer uma pesquisa para    *
#'* estimar a verdadeira proporção de todos os universitários      *
#'* que têm computador pessoal. Nos seus resultados a repórter     *
#'* quer ter 95% de confiança e uma margem de erro de 0.04.        *
#'* Quantos universitários devem ser pesquisados?                  *
"---------------------------"


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


"-------- Enunciado --------"
#'* Pretende-se estimar o número total de médicos que trabalham numa  *
#'* certa cidade e estão associados a planos de saúde. Para isso      *
#'* recolheu-se uma amostra aleatória com 300 médicos dessa cidade e  *
#'* apurou-se que entre eles 216 se enquadram nessa condição. Obtenha *
#'* um intervalo de confiança a 98% para a sua estimativa, sabendo    *
#'* que o número total de médicos na cidade é 28000.                  *
"---------------------------"


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


"-------- Enunciado --------"
#'* Dois inquéritos realizados (em 2009 e 2019), relativamente ao     *
#'* consumo de bebidas alcoólicas, em idades entre os 15 e os 35      *
#'* anos, forneceram os seguintes dados:                              *
#' * ____________________________________________________________     *
#'* | Ano: | Nº de Inquiridos | Consumidores | Não Consumidores |     *
#'* |------------------------------------------------------------     *
#'* | 2009 |       4000       |     1750     |       2250       |     *
#'* |------------------------------------------------------------     *
#'* | 2019 |       5000       |     2250     |       2750       |     *
#'*  ̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅̅     *
#'* Através de um intervalo de confiança, a 98%, indique a veracidade *
#'* da afirmação:                                                     *
#'*  ”A percentagem de consumidores de bebidas alcoólicas, em         *
#'*  indivíduos com idades compreendidas entre os 15 e os 35 anos,    *
#'*  registou um grande aumento na década analisada.”                 *
"---------------------------"


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


"-------- Enunciado --------"
#'* Com o objetivo de verificar o efeito de um novo medicamento no     *
#'* tratamento de uma dada doença, dois grupos, A e B, foram formados, *
#'* cada um composto por 100 indivíduos que apresentavam a tal doença, *
#'* estando todos eles no mesmo estágio da mesma. O grupo A recebeu o  *
#'* novo medicamento e o grupo B recebeu um placebo. Curaram-se da     *
#'* doença 75 pessoas no grupo A e 65 no grupo B. É possível afirmar   *
#'* que o novo medicamento é eficaz no tratamento da doença?           *
#'* Justifique a sua resposta recorrendo a um intervalo de confiança   *
#'* a 95%.                                                             *
"---------------------------"


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


"-------- Enunciado --------"
#'* No ficheiro ”Obesidade” tem as respostas a um inquérito efetuado  *
#'* num estudo sobre obesidade a um grupo de indivíduos obesos.       *
#'* 1.) Calcule estimativas pontuais para a altura média e para a     *
#'*     variância das alturas.                                        *
#'* 2.) Calcule um intervalo de confiança a 99% para a altura média.  *
#'* 3.) Qual a margem de erro do intervalo da alínea anterior?        *
#'* 4.) Recorrendo a um intervalo de confiança a 90%, acha que,       *
#'*     em média, os pesos do género feminino podem ser considerados  *
#'*     iguais aos pesos do género masculino? Justifique.             *
#'* 5.) Remova os ”outliers” existentes nos dados referentes à Idade. *
#'*     Com os dados sem ”outliers”:                                  *
#'*  a.) calcule um intervalo de confiança a 92% para a idade média.  *
#'*  b.) recorrendo a um intervalo de confiança a 95%, acha que,      *
#'*      em média, há diferenças na idade das pessoas que comem       *
#'*      habitualmente alimentos altamente calóricos e os que não     *
#'*      comem? Justifique.                                           *
"---------------------------"


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
