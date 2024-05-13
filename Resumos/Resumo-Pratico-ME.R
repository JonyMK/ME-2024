## Resumo dos Comandos no R ####

### Resumo R: Primeiro Teste ####

"-------------------------------------------------------------"

#### Ver Tabela/Dados: ####

View(TABELA)

"-------------------------------------------------------------"

#### Não Escrever Valores em Notação Científica: ####

options(scipen = 999)

"-------------------------------------------------------------"

#### Operadores Lógicos no R: ####

# | --> Ou
# & --> E
# == --> Igual
# != --> Diferente
# > --> Maior
# >= --> Maior Ou Igual
# < --> Menor
# <= --> Menor ou Igual

"-------------------------------------------------------------"

#### Repetição de Dados: ####

rep("VALOR_A_REPETIR", NR_VEZES_A_REPETIR)

"-------------------------------------------------------------"

#### Arredondar Valores: ####

round(VALOR_A_ARREDONDAR, NR_CASAS_DECIMAIS)

"-------------------------------------------------------------"

#### Arrays / Vetores: ####

###### Criar um Array: ######
c(ELEMENTO_1, "ELEMENTO_2")

###### Consultar Tamanho de um Array: ######
length(ARRAY)

###### Criar um Array: ######
c(ELEMENTO_1, "ELEMENTO_2")
###### Criar um Array: ######
c(ELEMENTO_1, "ELEMENTO_2")

"-------------------------------------------------------------"

#### Tabelas: ####

###### Criar uma Tabela: ######
data.frame(
  NOME_COLUNA_1 = ARRAY_DADOS_COLUNA_1,
  NOME_COLUNA_2 = ARRAY_DADOS_COLUNA_2,
  # ......,
  NOME_COLUNA_N = ARRAY_DADOS_COLUNA_N
)

###### Consultar o Nº de Linhas de uma Tabela: ######
nrow(TABELA)

###### Consultar o Nº de Colunas de uma Tabela: ######
ncol(TABELA)

###### Consultar Dimensões de uma Tabela: ######
dim(TABELA)

###### Consultar uma Tabela: ######
TABELA["CONDIÇÃO_PARA_LINHAS", c("COLUNAS_A_APRESENTAR")]

###### Ordenar uma Tabela por uma Coluna: ######
TABELA_ORDENADA <- factor(
  TABELA$COLUNA,
  levels=c("CATEGORIA_1", "CATEGORIA_2")
)

"-------------------------------------------------------------"

#### Tabela de Frequências - Sem Classes: ####

(ni.VARIAVEL <- table(TABELA$VARIAVEL))            # Freq. Absolutas
(fi.VARIAVEL <- round(prop.table(ni.VARIAVEL), 4)) # Freq. Relativas
(Ni.VARIAVEL <- cumsum(ni.VARIAVEL))               # Freq. Absolutas Acumuladas
(Fi.VARIAVEL <- round(cumsum(fi.VARIAVEL), 4))     # Freq. Relativas Acumuladas

(tabela.frequencias.VARIAVEL <- data.frame(
  i = 1:nrow(ni.VARIAVEL),
  xi = names(ni.VARIAVEL),
  ni = as.integer(ni.VARIAVEL),
  fi = as.numeric(fi.VARIAVEL),
  Ni = as.integer(Ni.VARIAVEL),
  Fi = as.numeric(Fi.VARIAVEL)
))

# OU:

DescTools::Freq(TABELA$VARIAVEL)

"-------------------------------------------------------------"

#### Tabela de Frequências - Com Classes: ####

# Mínimo e Máximo dos Dados:
min(TABELA$COLUNA)
max(TABELA$COLUNA)

# Classes - Pelo Enunciado:
(k <- VALOR)

# Amplitude - Pelo Enunciado:
(h <- VALOR)

# Regra de Sturgis:

(n <- nrow(TABELA))

# Classes:
(k <- trunc(1 + log(n)/log(2)))

# Amplitude:
(h <- (max(TABELA$COLUNA) - min(TABELA$COLUNA)) / k)

# Mínimo e Máximo das Classes:
(COLUNA.min <- min(TABELA$COLUNA))
(COLUNA.max <- COLUNA.min + h * k)

# Extremos das Classes:
(COLUNA.cortes <- seq(COLUNA.min, COLUNA.max, by = h))

# Intervalos - Abertos à Esquerda e Fechados à Direita:
(COLUNA.classes <- cut(
  TABELA$COLUNA,
  breaks = COLUNA.cortes,
  right = TRUE,
  include.lowest = TRUE
))

(ni.VARIAVEL <- table(COLUNA.classes))             # Freq. Absolutas
(fi.VARIAVEL <- round(prop.table(ni.VARIAVEL), 4)) # Freq. Relativas
(Ni.VARIAVEL <- cumsum(ni.VARIAVEL))               # Freq. Absolutas Acumuladas
(Fi.VARIAVEL <- round(cumsum(fi.VARIAVEL), 4))     # Freq. Relativas Acumuladas

(tabela.frequencias.VARIAVEL <- data.frame(
  i = 1:nrow(ni.VARIAVEL),
  xi = names(ni.VARIAVEL),
  ni = as.integer(ni.VARIAVEL),
  fi = as.numeric(fi.VARIAVEL),
  Ni = as.integer(Ni.VARIAVEL),
  Fi = as.numeric(Fi.VARIAVEL)
))

# OU:

DescTools::Freq(TABELA$VARIAVEL)

"-------------------------------------------------------------"

#### Dados Omissos: ####

###### Para verificar se existem dados omissos: ######
any(is.na(TABELA))
# OU
anyNA(TABELA)

###### Para retornar a tabela sem dados omissos: ######
na.omit(TABELA)

"-------------------------------------------------------------"

# Variáveis Qualitativas: Só se faz a Medida de Localização Central - Moda!

# Variáveis Quantitativas:
(ni.VARIAVEL <- table(TABELA$COLUNA))

#### Medidas de Localização Central: ####

###### Moda: ######
if(min(ni.VARIAVEL)==max(ni.VARIAVEL)){
  print("Amodal")
} else{
  print(paste("Moda:", names(ni.VARIAVEL)[ni.VARIAVEL==max(ni.VARIAVEL)]))
}
# OU
if(min(ni.VARIAVEL)==max(ni.VARIAVEL)){
  print("Amodal")
} else{
  print(paste("Moda:", DescTools::Mode(ni.VARIAVEL)))
}

###### Média: ######
mean(TABELA$COLUNA)

###### Mediana: ######
median(TABELA$COLUNA, type = 2)
# OU
quantile(TABELA$COLUNA, prob = 0.50, type = 2)

#### Medidas de Localização Não Central: ####

###### 1º, 2º e 3º Quantis: ######
quantile(
  TABELA$COLUNA,
  prob = c(0.25, 0.50, 0.75),
  type = 2
)

#### Medidas de Dispersão Absoluta: ####

###### Amplitude Total: ######
(max(TABELA$COLUNA)-min(TABELA$COLUNA))

###### Amplitude Interquartis: ######
IQR(TABELA$COLUNA, type=2)

###### Variância: ######
var(TABELA$COLUNA)

###### Desvio Padrão: ######
sd(TABELA$COLUNA)

#### Medidas de Dispersão Relativa: ####

###### Coeficiente de variação = CV: ######
((sd(TABELA$COLUNA)/mean(TABELA$COLUNA))*100) # %

# CV => Única Medida de Dispersão para Unidades Diferentes!

"-------------------------------------------------------------"

#### Medidas de Simetria: ####

library(e1071)
e1071::skewness(TABELA$COLUNA)

# Simetria pelas Medidas de Localição Central:
## Média = Mediana => Simétrica
## Média > Mediana => Assimétrica Positiva (à Direita)
## Média < Mediana => Assimétrica Negativa (à Esquerda)

"-------------------------------------------------------------"

#### Nova Janela para Ver Gráficos: ####
# Funciona para Todos (Gráficos + Histogramas + BoxPlots):

dev.new()

# O Comando deve ser executado antes do comando do gráfico,
# para criar a janela de visualização!

"-------------------------------------------------------------"

#### Legenda Para Gráficos: ####
# Funciona para Todos (Gráficos + Histogramas + BoxPlots):

legend(
  "topright",                       # Posição da Legenda
  legend = c("VALOR_1", "VALOR_2"), # Valores da Legenda (Nomes)
  fill = c(1:2),                    # Cores dos Valores
  cex = 1                           # Tamanho do Objeto (Escala)
)

# O Comando da Legenda Deve Ser Executado Depois de Criar o Gráfico!

"-------------------------------------------------------------"

#### Valores sobre as Colunas dos Gráficos de Barras: ####

text(
  x = GRAFICO,          # Gráfico Criado
  y = ni.VARIAVEL + 1,  # Posição Y dos Valores (Acima das Barras)
  labels = ni.VARIAVEL, # Valores das Barras
  pos = 3,              # Posição Relativa ao Texto (3 = acima da linha)
  cex = 0.8,            # Tamanho do Texto
  col = "black",        # Cor do Texto
  font = 2              # Estilo do Texto (2 = Negrito)
)

"-------------------------------------------------------------"

#### Gráfico de Barras de Frequências Absolutas: ####

# Freq. Absolutas:
(ni.VARIAVEL <- table(TABELA$COLUNA))

# Gráfico de Barras:
barplot(
  ni.VARIAVEL,                              # Variável no Gráfico
  main = "TITULO (Frequências Absolutas)",  # Título do Gráfico
  xlab = "VARIAVEL",                        # Título Eixo XX
  ylab = "Frequências Absolutas",           # Título Eixo YY
  col = 1:2,                                # Cores das Barras 
  ylim = c(0, 1000),                        # Limites do Eixo YY
  names.arg = c("BARRA_1", "BARRA_2")       # Nomes das Barras
)

"-------------------------------------------------------------"

#### Gráfico de Barras de Frequências Relativas: ####

# Frequências Absolutas:
(ni.VARIAVEL <- table(TABELA$COLUNA))

# Frequências Relativas:
(fi.VARIAVEL <- round(prop.table(ni.VARIAVEL), 4))

barplot(
  fi.VARIAVEL,                              # Variável no Gráfico
  main = "TITULO (Frequências Relativas)",  # Título do Gráfico
  xlab = "VARIAVEL",                        # Título Eixo XX
  ylab = "Frequências Relativas",           # Título Eixo YY
  col = 1:2,                                # Cores das Barras 
  ylim = c(0, 2.5),                         # Limites do Eixo YY
  names.arg = c("BARRA_1", "BARRA_2")       # Nomes das Barras
)

"-------------------------------------------------------------"

#### Gráfico de Barras de Frequências Relativas Em Percentagem: ####

# SE PRECISO - Colocar as Categorias da Variável por Ordem:
TABELA_ORDENADA <- factor(
  TABELA$COLUNA,
  levels=c("CATEGORIA_1", "CATEGORIA_2")
)

# Frequências Absolutas:
(ni.VARIAVEL <- table(TABELA$COLUNA))
# OU
(ni.VARIAVEL <- table(TABELA_ORDENADA))

# Frequências Relativas:
(fi.VARIAVEL <- round(prop.table(ni.VARIAVEL), 4))

# Gráfico de Barras
barplot(
  fi.VARIAVEL * 100,                    # Variável (%)
  main = "TITULO (%)",                  # Título do Gráfico
  xlab = "VARIAVEL",                    # Título Eixo XX
  ylab = "Frequências Relativas em %",  # Título Eixo YY
  col = 1:2,                            # Cores das Barras 
  ylim = c(0, 100)                      # Limites do Eixo YY
)

"-------------------------------------------------------------"

#### Gráfico Circular: ####

# SE PRECISO - Colocar as Categorias da Variável por Ordem:
TABELA_ORDENADA <- factor(
  TABELA$COLUNA,
  levels=c("CATEGORIA_1", "CATEGORIA_2")
)

# Frequências Absolutas:
(ni.VARIAVEL <- table(TABELA$COLUNA))
# OU
(ni.VARIAVEL <- table(TABELA_ORDENADA))

# Frequências Relativas:
(fi.VARIAVEL <- round(prop.table(ni.VARIAVEL), 4))

# Gráfico:
pie(
  ni.VARIAVEL,
  labels = paste(fi.VARIAVEL * 100, "%"),
  col = 1:2,
  main = "TITULO"
)

"-------------------------------------------------------------"

#### Histograma - Eixo YY - Freq. Absolutas: ####

# Gráfico:
hist(
  TABELA$COLUNA,
  breaks = COLUNA.cortes,
  right = TRUE,
  include.lowest = TRUE,
  freq = TRUE, # TRUE => Freq. Absolutas | FALSE => Freq. Relativas
  main = "Histograma",
  xlab = "VARIAVEL",
  ylab = "Frequências Absolutas",
  col = 1:2,
  xlim = c(COLUNA.min, COLUNA.max),
  ylim = c(0, 100),
  xaxt = "n"  # Para Poder Definir o Eixo XX de Seguida
)

# Valores do Eixo XX:
axis(
  side = 1,
  at = c(
    round(COLUNA.min, 2),
    round(COLUNA.cortes, 2),
    round(COLUNA.max, 2)
  )
)

"-------------------------------------------------------------"

#### Histograma - Eixo YY - fi/h - Comparação entre Qualquer Tipo de Classes: ####

# Gráfico:
hist(
  TABELA$COLUNA,
  breaks = COLUNA.cortes,
  right = TRUE,
  include.lowest = TRUE,
  freq = FALSE, # TRUE => Freq. Absolutas | FALSE => Freq. Relativas
  main = "Histograma",
  xlab = "VARIAVEL",
  ylab = "Frequências Relativas / Amplitude das Classes",
  col = 1:2,
  xlim = c(COLUNA.min, COLUNA.max),
  ylim = c(0, 0.1),
  xaxt = "n"  # para poder definir o eixo dos xx
)

# Valores do Eixo XX:
axis(
  side = 1,
  at = c(
    round(COLUNA.min, 2),
    round(COLUNA.cortes, 2),
    round(COLUNA.max, 2)
  )
)

"-------------------------------------------------------------"

#### Histograma - Eixo YY - fi - Comparação Mesmo Tipo de Classes: ####

# Classes:
(COLUNA.classes <- cut(
  TABELA$COLUNA,
  breaks = COLUNA.cortes,
  right = TRUE,
  include.lowest = TRUE
))

# Frequências Absolutas:
(ni.VARIAVEL <- table(TABELA$COLUNA))

# Frequências Relativas:
(fi.VARIAVEL <- round(prop.table(ni.VARIAVEL), 4))

# Atribuir um nome ao histograma para poder aceder aos seus campos:
graf <- hist(
  TABELA$COLUNA,
  breaks = COLUNA.cortes,
  right = TRUE,
  include.lowest = TRUE
)

graf$density <- fi.VARIAVEL

# Gráfico:
plot(
  graf,
  freq = FALSE, # TRUE => Freq. Absolutas | FALSE => Freq. Relativas
  main = "Histograma",
  xlab = "VARIAVEL",
  ylab = "Frequências Relativas",
  col = 1:2,
  xlim = c(COLUNA.min, COLUNA.max),
  ylim = c(0, 1),
  xaxt = "n"  # para poder definir o eixo dos xx
)

# Valores do Eixo XX:
axis(
  side = 1,
  at = c(
    round(COLUNA.min, 2),
    round(COLUNA.cortes, 2),
    round(COLUNA.max, 2)
  )
)

"-------------------------------------------------------------"

#### Extremos e Quartis: ####

###### Diagrama de Extremos e Quartis - Caixa com Bigodes: ######
boxplot(
  TABELA$COLUNA,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis",
  xlab="VARIAVEL",
  type = 2
)

###### Sem Indicação de Outliers: ######
boxplot(
  TABELA$COLUNA,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Sem Outliers",
  xlab = "VARIAVEL",
  type = 2,
  range = 0
)

###### Outliers A Partir dos Moderados: ######
boxplot(
  TABELA$COLUNA,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Outliers a Partir dos Moderados",
  xlab = "VARIAVEL",
  type = 2,
  range = 1.5
)

###### Ver Quem São os Outliers Moderados: ######
outliers.moderados <- boxplot(
  TABELA$COLUNA,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Outliers Moderados",
  xlab = "VARIAVEL",
  type = 2,
  range = 1.5
)
outliers.moderados$out
length(outliers.moderados$out)

###### Outliers a Partir dos Severos: ######
boxplot(
  TABELA$COLUNA,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Outliers a Partir dos Severos",
  xlab = "VARIAVEL",
  type = 2,
  range = 3
)

###### Ver Quem São os Outliers Severos: ######
outliers.severos <- boxplot(
  TABELA$COLUNA,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Outliers Severos",
  xlab = "VARIAVEL",
  type = 2,
  range = 3
)
outliers.severos$out
length(outliers.severos$out)

###### Nº de Outliers Total: ######
length(outliers.moderados$out)

###### Nº de Outliers Moderados: ######
length(outliers.moderados$out)-length(outliers.severos$out)

###### Nº de Outliers Severos: ######
length(outliers.severos$out)

###### Boxplot entre 2 Variáveis: ######
boxplot(
  VARIAVEL1 ~ VARIAVEL2,
  data = TABELA,
  col = 3:5,
  xlab = "TITULO", 
  ylab = "VARIAVEL",
  type = 2
)

"-------------------------------------------------------------"

#### Escrever um Valor em Fração: ####

library(FRACTION)
FRACTION::fra(VALOR)
# OU
library(MASS)
MASS::fractions(VALOR)

"-------------------------------------------------------------"

#### Calcular Integrais: ####

integral <- function (x) { (x^2 * (x-1)) }
(integrate(integral, lower=1, upper=5)$value)

"-------------------------------------------------------------"

#### Calcular Derivadas: ####

library(Deriv)
derivada <- function (x) { ((x^3/3) + (1/3)) }
(Deriv::Deriv(derivada, "x"))

"-------------------------------------------------------------"

#### Variáveis Aleatórias Discretas: ####

###### Função de Probabilidade: ######
x <- c("X_1", "X_2")
fx <- c("f(X_1)", "f(X_2)")

# Valor E[X]:
valor_ex_discretas <- function () {
  # Nas discretas, o E[X] corresponde ao somatório de x * f(x).
  sum(x * fx)
}

# Valor E[X^2]:
valor_ex2_discretas <- function () {
  # Nas discretas, o E[X^2] corresponde ao somatório de x^2 * f(x).
  sum((x^2) * fx)
}

# Valor V[X]:
valor_vx_discretas <- function () {
  # Nas discretas, o V[X] corresponde à diferença E[X^2] - E^2[X].
  valor_ex2_discretas() - valor_ex_discretas()^2
}

###### E[X]: ######
valor_ex_discretas()

###### E[X^2]: ######
valor_ex2_discretas()

###### V[X]: ######
valor_vx_discretas()

"-------------------------------------------------------------"

#### Variáveis Aleatórias Contínuas: ####

###### Função Densidade de Probabilidade: ######

# Na função f_densidade_probabilidade, cada ifelse representa:
## Condição: Ramo conhecido;
## Verdadeiro: Valor desse ramo;
## Falso: Novo ifelse com outro ramo, ou 0 caso seja o último ramo conhecido.
f_densidade_probabilidade <- function (x) {
  ifelse(
    "LIMITE_INFERIOR_RAMO_1" <= x & x < "LIMITE_SUPERIOR_RAMO_1",
    "VALOR_RAMO_1",
    ifelse(
      "LIMITE_INFERIOR_RAMO_2" <= x & x < "LIMITE_SUPERIOR_RAMO_2",
      "VALOR_RAMO_2",
      0 # SÓ SE FOR O ÚLTIMO RAMO!!
    )
  )
}

# E[X] - Interno:
ex_continuas <- function (x) {
  # Corresponde a: x * f(x)
  f_densidade_probabilidade(x) * x
}

# E[X^2] - Interno:
ex2_continuas <- function (x) {
  # Corresponde a: x^2 * f(x)
  f_densidade_probabilidade(x) * (x^2)
}

# Valor E[X]:
valor_ex_continuas <- function () {
  # Nas contínuas, o E[X] corresponde à integral
  # de - infinito a + infinito de x * f(x).
  integrate(ex_continuas, lower = -Inf, upper = +Inf)$value
}

# Valor E[X^2]:
valor_ex2_continuas <- function () {
  # Nas contínuas, o E[X^2] corresponde à integral
  # de - infinito a + infinito de x^2 * f(x).
  integrate(ex2_continuas, lower = -Inf, upper = +Inf)$value
}

# Valor V[X]:
valor_vx_continuas <- function () {
  # Independentemente de ser ou não contínua, o V[X] corresponde
  # à diferença entre E[X^2] e E^2[X] (os integrais são calculados nos valores
  # esperados).
  valor_ex2_continuas() - (valor_ex_continuas()^2)
}

###### E[X]: ######
valor_ex_continuas()

###### E[X^2]: ######
valor_ex2_continuas()

###### V[X]: ######
valor_vx_continuas()

###### E[AX + B]: ######
# E[AX + B] = A * E[X] + B
(A * valor_ex_continuas() + B)

###### E[AX^2 + B]: ######
# E[AX^2 + B] = A * E[X^2] + B
(A * valor_ex2_continuas() + B)

"-------------------------------------------------------------"

#### Dist. Uniforme Contínua - Descobrir "a" e "b": ####

# Só funciona se se souber a Média, e um F(X) = VALOR !!!

# Média = (a + b) / 2
# a = Média * 2 - b
# b = Média * 2 - a

# Resolve-se primeiro para "a":
# Sabe-se que: (X - a) / (b - a) = prob_conhecida
# Substitui-se "a" por: media * 2 - b
# (X - (media * 2 - b)) / (b - (media * 2 - b)) = prob_conhecida
# Simplificado fica:
# (X - 2media + b) / (2b - 2media) = prob_conhecida

# Depois para "b":

# Função para calcular a probabilidade acumulada dado "a" e "b":
prob_acumulada <- function (a, b, x) {
  (x - a) / (b - a)
}

# Função para verificar se a diferença entre a probabilidade acumulada e
# a probabilidade conhecida está próxima o suficiente de zero:
verificar_prob <- function (a, media, x, prob_conhecida) {
  b <- 2 * media - a
  abs(prob_acumulada(a, b, x) - prob_conhecida)
}

# Função principal que inicializa e termina o processo:
calcular_limites_dominio_uniforme_continua <- function (media, x, prob_conhecida) {
  # Para encontrar o "a" e o "b" utilizando otimização:
  result <- optimize(
    verificar_prob,
    interval = c(0, media),
    media = media,
    x = x,
    prob_conhecida = prob_conhecida
  )
  
  a <- result$minimum
  b <- 2 * media - a
  
  print(paste("Valor Final de a:", round(a, 4)))
  print(paste("Valor Final de b:", round(b, 4)))
}

# Para Utilizar e Calcular:
calcular_limites_dominio_uniforme_continua("MEDIA_CONHECIDA", "X_UTILIZADO", "RESULTADO_Fx")

"-------------------------------------------------------------"

#### Símbolos: ####

# ✓ --> Certo
# ∧ --> Conjunção / E
# ∀x --> Todo e qualquer x
# √ --> Raiz Quadrada
# Σ --> Somatório
# λ --> Lambda
# Φ --> Fi (Dist. Normal Reduzida -> F(X))
# μ --> IU / Média
# σ --> Sigma / Desvio Padrão
# ∫ --> Integral
# ∞ --> Infinito
# [-∞, +∞] --> Intervalo Infinito
# x̅ --> Média da Amostra
# α --> Alfa

"-------------------------------------------------------------"
"-------------------------------------------------------------"
"-------------------------------------------------------------"
"-------------------------------------------------------------"
"-------------------------------------------------------------"

### Resumo R: Segundo Teste ####

"-------------------------------------------------------------"

#### Distribuições Amostrais, Intervalos de COnfiança e Testes de Hipóteses Paramétricos ####

###### Para a Média: ######

# População Normal;
# σ Conhecido.
# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)
# I.C.: ] x̅ - (z_(1 - (α/2))) * (σ / sqrt(n)) , x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) [
BSDA::z.test()

# População Normal;
# σ Desconhecido.
# D.A.: T = ((x̅ - μ) / (s / sqrt(n))) ~ t(n-1)
# I.C.: ] x̅ - (t_(1 - (α/2)); n-1) * (s / sqrt(n)) , x̅ + (t_(1 - (α/2)); n-1) * (s / sqrt(n)) [
t.test()

# População Qualquer;
# σ Conhecido;
# n >= 30.
# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)
# I.C.: ] x̅ - (z_(1 - (α/2))) * (σ / sqrt(n)) , x̅ + (z_(1 - (α/2))) * (σ / sqrt(n)) [
BSDA::z.test()

# População Qualquer;
# σ Desconhecido;
# n >= 30.
# D.A.: Z = ((x̅ - μ) / (s / sqrt(n))) ~ N(0, 1)
# I.C.: ] x̅ - (z_(1 - (α/2))) * (s / sqrt(n)) , x̅ + (z_(1 - (α/2))) * (s / sqrt(n)) [
BSDA::z.test()

"-------------------------------"

###### Para a Diferença de 2 Médias: ######

# Populações Normais;
# σ1 e σ2 Conhecidos;
# Amostras Independentes.
# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2))) ~ N(0, 1)
# I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((σ1^2 / n1) + (σ2^2 / n2))) [
BSDA::z.test()

# Populações Normais;
# σ1 e σ2 Desconhecidos;
# σ1 = σ2;
# Amostras Independentes.
# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))))
# T ~ t(n1 + (n2 - 2))
# I.C.: ] (x̅1 - x̅2) |-+| t_(1 - (α/2); n1 + (n2 - 2)) * sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + (n2 - 2)))) [
t.test()

# Populações Normais;
# σ1 e σ2 Desconhecidos;
# σ1 != σ2;
# Amostras Independentes.
# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((s1^2 / n1) + (s2^2 / n2))) ~ t(gl2)
# I.C.: ] (x̅1 - x̅2) |-+| t_(1 - (α/2); gl2) * sqrt((s1^2 / n1) + (s2^2 / n2))) [
t.test()
## gl2 ~=~ ((s1^2 / n1) + (s2^2 / n2))^2 / ((s1^4 / (n1^2 * (n1 - 1))) + (s2^4 / (n2^2 * (n2 - 1))))
## Para gl2: Considera-se o inteiro mais próximo ou faz-se a correção de Welch-Satterthwaite.

# Populações Quaiquer;
# σ1 e σ2 Conhecidos;
# Amostras Independentes;
# n1 e n2 >= 30.
# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2))) ~ N(0, 1)
# I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((σ1^2 / n1) + (σ2^2 / n2))) [
BSDA::z.test()

# Populações Quaiquer;
# σ1 e σ2 Desconhecidos;
# Amostras Independentes;
# n1 e n2 >= 30.
# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((s1^2 / n1) + (s2^2 / n2))) ~ N(0, 1)
# I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((s1^2 / n1) + (s2^2 / n2))) [
BSDA::z.test()


"-------------------------------"

###### Para a Variância: ######

# População Normal.
# D.A.: X^2 = (((n-1) * s^2) / σ^2) ~ X^2(n-1)
# I.C.: ] (((n-1) * s^2) / x^2_(1 - (α/2); n-1)) , (((n-1) * s^2) / x^2_(α/2; n-1)) [
EnvStats::varTest()

"-------------------------------"

###### Para o Quociente de 2 Variâncias: ######

# Populações Normais;
# Amostras Independentes.
# D.A.: F = ((s1^2 / s2^2) * (σ2^2 / σ1^2)) ~ F(n1 - 1, n2 - 1)
# I.C.: ] ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) , 
# I.C.:   ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) [
var.test()

"-------------------------------"

###### Para a Proporção: ######

# n >= 30.
# D.A.: Z = ((p* - p) / sqrt(pq / n)) ~=~ ((p* - p) / sqrt((p* * q*) / n)) ~ N(0, 1)
# I.C.: ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [
BSDA::z.test()

"-------------------------------"

###### Para a Diferença de 2 Proporções: ######

# Amostras Independentes;
# n1 & n2 >= 30.
# D.A.: Z = (((p1* - p2*) - (p1 - p2)) / sqrt(((p1 * q1) / (n1)) + ((p2 * q2) / (n2))))
# D.A.: Z ~=~ (((p1* - p2*) - (p1 - p2)) / sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2))))
# D.A.: z ~ N(0, 1)
# I.C.: ] (p1* - p2*) |-+| z_(1 - (α/2)) * sqrt(((p1* * q1*) / (n1)) + ((p2* * q2*) / (n2))) [
BSDA::z.test()

"-------------------------------"

###### Testes de Ajustamento: ######

# Distribuição Discreta ou Contínua com Classes:
# Qui-Quadrado
chisq.test()

# Distribuição Contínua Completamente Especificada:
# Kolmogorov-Smirnov
ks.test()

# Normal e n >= 50:
# Lilliefors
nortest::lillie.test()

# Normal e n < 50:
# Shapiro Wilk
shapiro.test()

"-------------------------------------------------------------"
