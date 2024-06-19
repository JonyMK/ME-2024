## Resumo dos Comandos no R ####

"-------------------------------------------------------------"

### LIBRARYs: ####

library(e1071)
library(FRACTION)
library(MASS)
library(Deriv)
library(BSDA)
library(EnvStats)
library(nortest)
library(DescTools)

"-------------------------------------------------------------"

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
"-------------------------------------------------------------"
"-------------------------------------------------------------"
"-------------------------------------------------------------"
"-------------------------------------------------------------"

### Resumo R: Segundo Teste ####

"-------------------------------------------------------------"

#### Testes Paramétricos (D.A. e I.C.): ####

library(BSDA)
library(EnvStats)

# Distribuições Amostrais e Intervalos de Confiança nos Testes Paramétricos

###### Para a Média: ######

# População Normal;
# σ Conhecido.
# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)
# I.C.: ] x̅ |-+| (z_(1 - (α/2))) * (σ / sqrt(n)) [
# BSDA::z.test()
### Comando para Amostras Conhecidas
BSDA::z.test(
  x = VARIAVEL,                   # Vetor com a amostra
  sigma.x = DESVIO_PADRAO,        # Desvio Padrão da População
  conf.level = GRAU_DE_CONFIANCA  # Grau de Confiança para o teste/intervalo
)
### Cálculos para Amostras Desconhecidas
media_caso_1 <- function(
  xbarra = 0,
  desviopadrao = 0,
  dimensao = 0,
  tipo_calculo,
  mi = 0,
  alfa = 0
)
{
  if (tipo_calculo == 1) {
    res <- ((xbarra - mi) / (desviopadrao / sqrt(dimensao)))
    res <- round(res, 4)
    da <- sprintf("DA: Z = %s ~ N(0, 1)", res)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- xbarra - qnorm(1 - (alfa/2)) * (desviopadrao / sqrt(dimensao))
    upper <- xbarra + qnorm(1 - (alfa/2)) * (desviopadrao / sqrt(dimensao))
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

# População Normal;
# σ Desconhecido.
# D.A.: T = ((x̅ - μ) / (s / sqrt(n))) ~ t(n-1)
# I.C.: ] x̅ |-+| (t_(1 - (α/2)); n-1) * (s / sqrt(n)) [
# t.test()
### Comando para Amostras Conhecidas
t.test(
  x = VARIAVEL,                   # Vetor com a amostra
  mu = MEDIA,                     # Média da População
  conf.level = GRAU_DE_CONFIANCA  # Grau de Confiança para o teste/intervalo
)
### Cálculos para Amostras Desconhecidas
media_caso_2 <- function(
  xbarra = 0,
  desviopadrao = 0,
  dimensao = 0,
  tipo_calculo,
  mi = 0,
  alfa = 0
)
{
  if (tipo_calculo == 1) {
    res <- ((xbarra - mi) / (desviopadrao / sqrt(dimensao)))
    res <- round(res, 4)
    da <- sprintf("DA: T = %s ~ t(%s)", dimensao-1)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- xbarra - qt((1 - (alfa/2)), dimensao-1) * (desviopadrao / sqrt(dimensao))
    upper <- xbarra + qt((1 - (alfa/2)), dimensao-1) * (desviopadrao / sqrt(dimensao))
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

# População Qualquer;
# σ Conhecido;
# n >= 30.
# D.A.: Z = ((x̅ - μ) / (σ / sqrt(n))) ~ N(0, 1)
# I.C.: ] x̅ |-+| (z_(1 - (α/2))) * (σ / sqrt(n)) [
# BSDA::z.test()
### Comando para Amostras Conhecidas
BSDA::z.test(
  x = VARIAVEL,                   # Vetor com a amostra
  sigma.x = DESVIO_PADRAO,        # Desvio Padrão da População
  conf.level = GRAU_DE_CONFIANCA  # Grau de Confiança para o teste/intervalo
)
### Cálculos para Amostras Desconhecidas
media_caso_3 <- function(
  xbarra = 0,
  desviopadrao = 0,
  dimensao = 0,
  tipo_calculo,
  mi = 0,
  alfa = 0
)
{
  if (tipo_calculo == 1) {
    res <- ((xbarra - mi) / (desviopadrao / sqrt(dimensao)))
    res <- round(res, 4)
    da <- sprintf("DA: Z = %s ~ N(0, 1)", res)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- xbarra - qnorm(1 - (alfa/2)) * (desviopadrao / sqrt(dimensao))
    upper <- xbarra + qnorm(1 - (alfa/2)) * (desviopadrao / sqrt(dimensao))
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

# População Qualquer;
# σ Desconhecido;
# n >= 30.
# D.A.: Z = ((x̅ - μ) / (s / sqrt(n))) ~ N(0, 1)
# I.C.: ] x̅ |-+| (z_(1 - (α/2))) * (s / sqrt(n)) [
# BSDA::z.test()
### Comando para Amostras Conhecidas
BSDA::z.test(
  x = VARIAVEL,                   # Vetor com a amostra
  sigma.x = DESVIO_PADRAO,        # Desvio Padrão da Amostra
  conf.level = GRAU_DE_CONFIANCA  # Grau de Confiança para o teste/intervalo
)
### Cálculos para Amostras Desconhecidas
media_caso_4 <- function(
  xbarra = 0,
  desviopadrao = 0,
  dimensao = 0,
  tipo_calculo,
  mi = 0,
  alfa = 0
)
{
  if (tipo_calculo == 1) {
    res <- ((xbarra - mi) / (desviopadrao / sqrt(dimensao)))
    res <- round(res, 4)
    da <- sprintf("DA: Z = %s ~ N(0, 1)", res)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- xbarra - qnorm(1 - (alfa/2)) * (desviopadrao / sqrt(dimensao))
    upper <- xbarra + qnorm(1 - (alfa/2)) * (desviopadrao / sqrt(dimensao))
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

"-------------------------------"

###### Para a Diferença de 2 Médias: ######

# Populações Normais;
# σ1 e σ2 Conhecidos;
# Amostras Independentes.
# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2))) ~ N(0, 1)
# I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((σ1^2 / n1) + (σ2^2 / n2)) [
# BSDA::z.test()
### Comando para Amostras Conhecidas
BSDA::z.test(
  x = VARIAVEL_1,                 # Primeira Amostra
  sigma.x = sd(VARIAVEL_1),       # Desvio Padrão da Amostra 1
  y = VARIAVEL_2,                 # Segunda Amostra
  sigma.y = sd(VARIAVEL_2),       # Desvio Padrão da Amostra 2
  conf.level = GRAU_DE_CONFIANCA  # Grau de Confiança
)
### Cálculos para Amostras Desconhecidas
diferenca_medias_caso_1 <- function(
  xb1 = 0,
  xb2 = 0,
  desviopadrao1 = 0,
  desviopadrao2 = 0,
  dimensao1 = 0,
  dimensao2 = 0,
  tipo_calculo,
  mi1 = 0,
  mi2 = 0,
  alfa = 0
)
{
  if (tipo_calculo == 1) {
    res <- (((xb1 - xb2) - (mi1 - mi2)) / sqrt((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2)))
    res <- round(res, 4)
    da <- sprintf("DA: Z = %s ~ N(0, 1)", res)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- (xb1 - xb2) - qnorm(1 - (alfa/2)) * sqrt((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2))
    upper <- (xb1 - xb2) + qnorm(1 - (alfa/2)) * sqrt((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2))
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

# Populações Normais;
# σ1 e σ2 Desconhecidos;
# σ1 = σ2;
# Amostras Independentes/Emparelhadas.
# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))))
# T ~ t(n1 + (n2 - 2))
# I.C.: ] (x̅1 - x̅2) |-+| t_(1 - (α/2); n1 + n2 - 2) * sqrt(((1 / n1) + (1 / n2)) * ((((n1 - 1) * s1^2) + ((n2 - 1) * s2^2)) / (n1 + n2 - 2))) [
# t.test()
### Comando para Amostras Conhecidas
t.test(
  x = VARIAVEL_1,                 # Primeira Amostra
  y = VARIAVEL_2,                 # Segunda Amostra
  paired = FALSE,                 # As Amostras são Dependentes?
  var.equal = TRUE,               # As Variâncias são Iguais?
  conf.level = GRAU_DE_CONFIANCA  # Grau de Confiança
)
### Cálculos para Amostras Desconhecidas
diferenca_medias_caso_2 <- function(
  xb1 = 0,
  xb2 = 0,
  desviopadrao1 = 0,
  desviopadrao2 = 0,
  dimensao1 = 0,
  dimensao2 = 0,
  tipo_calculo,
  mi1 = 0,
  mi2 = 0,
  alfa = 0
)
{
  if (tipo_calculo == 1) {
    res <- (((xb1 - xb2) - (mi1 - mi2)) / sqrt(((1 / dimensao1) + (1 / dimensao2)) * ((((dimensao1 - 1) * desviopadrao1^2) + ((dimensao2 - 1) * desviopadrao2^2)) / (dimensao1 + dimensao2 - 2))))
    res <- round(res, 4)
    da <- sprintf("DA: T = %s ~ t(%s)", res, dimensao1+dimensao2-2)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- (xb1 - xb2) - qt(1 - (alfa/2), n1+n2-2) * sqrt(((1 / dimensao1) + (1 / dimensao2)) * ((((dimensao1 - 1) * desviopadrao1^2) + ((dimensao2 - 1) * desviopadrao2^2)) / (dimensao1 + dimensao2 - 2)))
    upper <- (xb1 - xb2) + qt(1 - (alfa/2), n1+n2-2) * sqrt(((1 / dimensao1) + (1 / dimensao2)) * ((((dimensao1 - 1) * desviopadrao1^2) + ((dimensao2 - 1) * desviopadrao2^2)) / (dimensao1 + dimensao2 - 2)))
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

# Populações Normais;
# σ1 e σ2 Desconhecidos;
# σ1 != σ2;
# Amostras Independentes/Emparelhadas.
# D.A.: T = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((s1^2 / n1) + (s2^2 / n2))) ~ t(gl2)
# I.C.: ] (x̅1 - x̅2) |-+| t_(1 - (α/2); gl2) * sqrt((s1^2 / n1) + (s2^2 / n2)) [
## gl2 ~=~ ((s1^2 / n1) + (s2^2 / n2))^2 / ((s1^4 / (n1^2 * (n1 - 1))) + (s2^4 / (n2^2 * (n2 - 1))))
## Para gl2: Considera-se o inteiro mais próximo ou faz-se a correção de Welch-Satterthwaite.
# t.test()
### Comando para Amostras Conhecidas
t.test(
  x = VARIAVEL_1,                 # Primeira Amostra
  y = VARIAVEL_2,                 # Segunda Amostra
  paired = FALSE,                 # As Amostras são Dependentes?
  var.equal = FALSE,              # As Variâncias são Iguais?
  conf.level = GRAU_DE_CONFIANCA  # Grau de Confiança
)
### Cálculos para Amostras Desconhecidas
diferenca_medias_caso_3 <- function(
  xb1 = 0,
  xb2 = 0,
  desviopadrao1 = 0,
  desviopadrao2 = 0,
  dimensao1 = 0,
  dimensao2 = 0,
  tipo_calculo,
  mi1 = 0,
  mi2 = 0,
  alfa = 0
)
{
  gl2 <- ((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2))^2 / ((desviopadrao1^4 / (dimensao1^2 * (dimensao1 - 1))) + (desviopadrao2^4 / (dimensao2^2 * (dimensao2 - 1))))
  gl2 <- round(gl2, 0)
  
  if (tipo_calculo == 1) {
    res <- (((xb1 - xb2) - (mi1 - mi2)) / sqrt((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2)))
    res <- round(res, 4)
    da <- sprintf("DA: T = %s ~ t(%s)", res, gl2)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- (xb1 - xb2) - qt(1 - (alfa/2), gl2) * sqrt((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2))
    upper <- (xb1 - xb2) + qt(1 - (alfa/2), gl2) * sqrt((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2))
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

# Populações Quaiquer;
# σ1 e σ2 Conhecidos;
# Amostras Independentes;
# n1 e n2 >= 30.
# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((σ1^2 / n1) + (σ2^2 / n2))) ~ N(0, 1)
# I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((σ1^2 / n1) + (σ2^2 / n2)) [
# BSDA::z.test()
### Comando para Amostras Conhecidas
BSDA::z.test(
  x = VARIAVEL_1,                 # Primeira Amostra
  sigma.x = sd(VARIAVEL_1),       # Desvio Padrão da Amostra 1
  y = VARIAVEL_2,                 # Segunda Amostra
  sigma.y = sd(VARIAVEL_2),       # Desvio Padrão da Amostra 2
  conf.level = GRAU_DE_CONFIANCA  # Grau de Confiança
)
### Cálculos para Amostras Desconhecidas
diferenca_medias_caso_4 <- function(
  xb1 = 0,
  xb2 = 0,
  desviopadrao1 = 0,
  desviopadrao2 = 0,
  dimensao1 = 0,
  dimensao2 = 0,
  tipo_calculo,
  mi1 = 0,
  mi2 = 0,
  alfa = 0
)
{
  if (tipo_calculo == 1) {
    res <- (((xb1 - xb2) - (mi1 - mi2)) / sqrt((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2)))
    res <- round(res, 4)
    da <- sprintf("DA: Z = %s ~ N(0, 1)", res)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- (xb1 - xb2) - qnorm(1 - (alfa/2)) * sqrt((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2))
    upper <- (xb1 - xb2) + qnorm(1 - (alfa/2)) * sqrt((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2))
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

# Populações Quaiquer;
# σ1 e σ2 Desconhecidos;
# Amostras Independentes;
# n1 e n2 >= 30.
# D.A.: Z = (((x̅1 - x̅2) - (μ1 - μ2)) / sqrt((s1^2 / n1) + (s2^2 / n2))) ~ N(0, 1)
# I.C.: ] (x̅1 - x̅2) |-+| z_(1 - (α/2)) * sqrt((s1^2 / n1) + (s2^2 / n2)) [
# BSDA::z.test()
### Comando para Amostras Conhecidas
BSDA::z.test(
  x = VARIAVEL_1,                 # Primeira Amostra
  sigma.x = sd(VARIAVEL_1),       # Desvio Padrão da Amostra 1
  y = VARIAVEL_2,                 # Segunda Amostra
  sigma.y = sd(VARIAVEL_2),       # Desvio Padrão da Amostra 2
  conf.level = GRAU_DE_CONFIANCA  # Grau de Confiança
)
### Cálculos para Amostras Desconhecidas
diferenca_medias_caso_5 <- function(
  xb1 = 0,
  xb2 = 0,
  desviopadrao1 = 0,
  desviopadrao2 = 0,
  dimensao1 = 0,
  dimensao2 = 0,
  tipo_calculo,
  mi1 = 0,
  mi2 = 0,
  alfa = 0
)
{
  if (tipo_calculo == 1) {
    res <- (((xb1 - xb2) - (mi1 - mi2)) / sqrt((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2)))
    res <- round(res, 4)
    da <- sprintf("DA: Z = %s ~ N(0, 1)", res)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- (xb1 - xb2) - qnorm(1 - (alfa/2)) * sqrt((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2))
    upper <- (xb1 - xb2) + qnorm(1 - (alfa/2)) * sqrt((desviopadrao1^2 / dimensao1) + (desviopadrao2^2 / dimensao2))
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

"-------------------------------"

###### Para a Variância: ######

# População Normal.
# D.A.: X^2 = (((n-1) * s^2) / σ^2) ~ X^2(n-1)
# I.C.: ] (((n-1) * s^2) / x^2_(1 - (α/2); n-1)) , (((n-1) * s^2) / x^2_(α/2; n-1)) [
# EnvStats::varTest()
### Comando para Amostras Conhecidas
EnvStats::varTest(
  x = VARIAVEL,                   # Amostra
  conf.level = GRAU_DE_CONFIANCA  # Grau de Confiança
)
### Cálculos para Amostras Desconhecidas
variancia_caso_unico <- function(
  desviopadrao_amostra = 0,
  desviopadrao_populacao = 0,
  dimensao = 0,
  tipo_calculo,
  alfa = 0
)
{
  if (tipo_calculo == 1) {
    res <- (((dimensao - 1) * desviopadrao_amostra^2) / desviopadrao_populacao^2)
    res <- round(res, 4)
    da <- sprintf("DA: X^2 = %s ~ X^2(%s)", res, dimensao-1)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- (((dimensao - 1) * desviopadrao_amostra^2) / qchisq(1 - (alfa/2), dimensao - 1))
    upper <- (((dimensao - 1) * desviopadrao_amostra^2) / qchisq(alfa / 2, dimensao - 1))
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

"-------------------------------"

###### Para o Quociente de 2 Variâncias: ######

# Populações Normais;
# Amostras Independentes.
# D.A.: F = ((s1^2 / s2^2) * (σ2^2 / σ1^2)) ~ F(n1 - 1, n2 - 1)
# I.C.: ] ((1 / f_(1 - (α/2); n1 - 1; n2 - 1)) * (s1^2 / s2^2)) , 
# I.C.:   ((1 / f_(α/2; n1 - 1; n2 - 1)) * (s1^2 / s2^2)) [
# var.test()
### Comando para Amostras Conhecidas
var.test(
  x = VARIAVEL_1,                 # Primeira Amostra
  y = VARIAVEL_2,                 # Segunda Amostra
  conf.level = GRAU_DE_CONFIANCA  # Grau de Confiança
)
### Cálculos para Amostras Desconhecidas
quociente_variancias_caso_unico <- function(
  desviopadrao_amostra1 = 0,
  desviopadrao_amostra2 = 0,
  desviopadrao_populacao1 = 0,
  desviopadrao_populacao2 = 0,
  dimensao1 = 0,
  dimensao2 = 0,
  tipo_calculo,
  alfa = 0
)
{
  if (tipo_calculo == 1) {
    res <- ((desviopadrao_amostra1^2 / desviopadrao_amostra2^2) * (desviopadrao_populacao2^2 / desviopadrao_populacao1^2))
    res <- round(res, 4)
    da <- sprintf("DA: F = %s ~ F(%s , %s)", res, dimensao1-1, dimensao2-1)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- ((1 / qf(1 - (alfa/2), dimensao1 - 1, dimensao2 - 1)) * (desviopadrao_amostra1^2 / desviopadrao_amostra2^2))
    upper <- ((1 / qf(alfa / 2, dimensao1 - 1, dimensao2 - 1)) * (desviopadrao_amostra1^2 / desviopadrao_amostra2^2))
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

"-------------------------------"

###### Para a Proporção: ######

# n >= 30.
# D.A.: Z = ((p* - p) / sqrt(pq / n)) ~=~ ((p* - p) / sqrt((p* * q*) / n)) ~ N(0, 1)
# I.C.: ] p* |-+| z_(1 - (α/2)) * sqrt((p* * q*) / n) [

# Nota: Quando p ou p* for desconhecido, pode-se utilizar 0.5 para o seu valor!

### Comando para Amostras Conhecidas
BSDA::z.test()

### Cálculos para Amostras Desconhecidas
proporcao_caso_unico <- function(
  p = 0,
  p_asterisco = 0,
  dimensao = 0,
  tipo_calculo,
  alfa = 0
)
{
  if (tipo_calculo == 0) {
    res <- ((p_asterisco - p) / sqrt(p * (1 - p) / dimensao))
    res <- round(res, 4)
    da <- sprintf("DA: Z = %s ~ N(0, 1)", res)
    print(da)
    return(res)
  } else if (tipo_calculo == 1) {
    res <- ((p_asterisco - p) / sqrt(p_asterisco * (1 - p_asterisco) / dimensao))
    res <- round(res, 4)
    da <- sprintf("DA: Z = %s ~ N(0, 1)", res)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- p_asterisco - qnorm(1 - (alfa/2)) * sqrt((p_asterisco * (1 - p_asterisco)) / dimensao)
    upper <- p_asterisco + qnorm(1 - (alfa/2)) * sqrt((p_asterisco * (1 - p_asterisco)) / dimensao)
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

"-------------------------------"

###### Para a Diferença de 2 Proporções: ######

# Amostras Independentes;
# n1 & n2 >= 30.
# D.A.: Z = (((p1* - p2*) - (p1 - p2)) / sqrt(((p1 * q1) / n1) + ((p2 * q2) / n2)))
# D.A.: Z ~=~ (((p1* - p2*) - (p1 - p2)) / sqrt(((p1* * q1*) / n1) + ((p2* * q2*) / n2)))
# D.A.: z ~ N(0, 1)
# I.C.: ] (p1* - p2*) |-+| z_(1 - (α/2)) * sqrt(((p1* * q1*) / n1) + ((p2* * q2*) / n2)) [

# Nota: Quando p ou p* for desconhecido, pode-se utilizar 0.5 para o seu valor!

### Comando para Amostras Conhecidas
BSDA::z.test()

### Cálculos para Amostras Desconhecidas
proporcao_caso_unico <- function(
  p1 = 0,
  p2 = 0,
  p_asterisco1 = 0,
  p_asterisco2 = 0,
  dimensao1 = 0,
  dimensao2 = 0,
  tipo_calculo,
  alfa = 0
)
{
  if (tipo_calculo == 0) {
    res <- (((p_asterisco1 - p_asterisco2) - (p1 - p2)) / sqrt(((p1 * (1 - p1)) / dimensao1) + ((p2 * (1 - p2)) / dimensao2)))
    res <- round(res, 4)
    da <- sprintf("DA: Z = %s ~ N(0, 1)", res)
    print(da)
    return(res)
  } else if (tipo_calculo == 1) {
    res <- (((p_asterisco1 - p_asterisco2) - (p1 - p2)) / sqrt(((p_asterisco1 * (1 - p_asterisco1)) / dimensao1) + ((p_asterisco2 * (1 - p_asterisco2)) / dimensao2)))
    res <- round(res, 4)
    da <- sprintf("DA: Z = %s ~ N(0, 1)", res)
    print(da)
    return(res)
  } else if (tipo_calculo == 2) {
    lower <- (p_asterisco1 - p_asterisco2) - qnorm(1 - (alfa/2)) * sqrt(((p_asterisco1 * (1 - p_asterisco1)) / dimensao1) + ((p_asterisco2 * (1 - p_asterisco2)) / dimensao2))
    upper <- (p_asterisco1 - p_asterisco2) + qnorm(1 - (alfa/2)) * sqrt(((p_asterisco1 * (1 - p_asterisco1)) / dimensao1) + ((p_asterisco2 * (1 - p_asterisco2)) / dimensao2))
    lower <- round(lower, 4)
    upper <- round(upper, 4)
    ic <- sprintf("IC = ] %s , %s [", lower, upper)
    print(ic)
    return(c(lower, upper))
  } else {
    print("Tipo Desconhecido!")
  }
}

"-------------------------------------------------------------"

#### Testes Não Paramétricos: ####

library(nortest)

###### Testes de Ajustamento: ######

# Para testar se a População segue uma Distribuição Discreta ou Contínua com Classes:
# Qui-Quadrado
## chisq.test()
chisq.test(
  x = Oi,  # Frequências Observadas (Freq. Absolutas)
  p = pi   # Frequências Esperadas
)
RES_CHISQ <- chisq.test(x = Oi, p = pi)
RES_CHISQ$statistic # Qobs
RES_CHISQ$parameter # Graus de Liberdade
RES_CHISQ$p.value   # Valor-P
RES_CHISQ$observed  # Oi
RES_CHISQ$expected  # Ei = npi

# Para testar se a População segue uma Distribuição Contínua (Completamente Especificada):
# Kolmogorov-Smirnov
## ks.test()
ks.test(
  VARIAVEL,       # Amostra
  "DISTRIBUIÇÂO", # Distribuição a Testar
  rate = MEDIA    # Média
)

# Para testar se a População segue uma Normal (com n >= 50, sem a especificar completamente):
# Lilliefors
## nortest::lillie.test()
nortest::lillie.test(
  VARIAVEL  # Amostra
)

# Para testar se a População segue uma Normal (com n < 50):
# Shapiro Wilk
## shapiro.test()
shapiro.test(
  VARIAVEL  # Amostra
)

"-------------------------------"

###### Teste de Independência: ######

# Qui-Quadrado

# Não sai no 2º teste!

# Função -> chisq.test()
# Argumentos da Função: Tabela de Contingência Bidimensional

RES_CHISQ <- chisq.test(
  TABELA_CONTINGENCIA,  # Tabela de Contingência
  correct = FALSE
)

RES_CHISQ$statistic # Qobs
RES_CHISQ$parameter # Graus de Liberdade
RES_CHISQ$p.value   # Valor-P
RES_CHISQ$observed  # Oi = Frequências Observadas
RES_CHISQ$expected  # Ei = Frequências Esperadas

"-------------------------------"

###### Testes à Igualdade de 2 Distribuições: ######

## Tipos de Teste:
### Bilateral => "two.sided"
### Unilateral Direito => "greater"
### Unilateral Esquerdo => "less"

# Wilcoxon
## D = AMOSTRA_Y - AMOSTRA_X
## H0: Mediana de D = 0
## vs.
## H1: Mediana de D > 0
wilcox.test(
  x = AMOSTRA_Y,            # Amostra X
  y = AMOSTRA_X,            # Amostra Y
  alternative = "greater",  # Tipo de Teste
  mu = 0,                   # Média de H0
  paired = TRUE             # São Emparelhadas?
)
RES_WILCOX <- wilcox.test()
RES_WILCOX$statistic   # Tobs
RES_WILCOX$p.value     # valor-p
RES_WILCOX$null.value  # H0: MD = 0
RES_WILCOX$alternative # H1: MD > 0

# Mann-Whitney
## MX - MY
## H0: Mediana de X = Mediana de Y
## vs.
## H1: Mediana de X < Mediana de Y
wilcox.test(
  x = AMOSTRA_X,         # Amostra X
  y = AMOSTRA_Y,         # Amostra Y
  alternative = "less",  # Tipo de Teste
  mu = 0,                # Média de H0
  paired = FALSE         # São Emparelhadas?
)
RES_MANN <- wilcox.test()
RES_MANN$statistic    # Uobs
RES_MANN$p.value      # valor-p
RES_MANN$null.value   # H0: MX - MY = 0
RES_MANN$alternative  # H1: MX - MY < 0

"-------------------------------------------------------------"

#### Regras de Validação do Teste de Ajustamento Qui-Quadrado: ####

###### Dimensão da Amostra Maior que 30: ######
if (DIMENSAO_AMOSTRA > 30) {
  print("Respeita a Regra.")
} else {
  print("Amostra Demasiado Pequena!")
}

###### Todas as Freq. Esperadas >= 1: ######
if (length(which(RES_CHISQ$expected < 1)) > 0) {
  print("Juntar Linhas da Tabela de Frequências!")
} else {
  print("Respeita a Regra.")
}

###### Não Há Mais de 20% das Freq. Esperadas < 5: ######
if (length(which(RES_CHISQ$expected < 5)) > (k * 0.2)) {
  print("Juntar Linhas da Tabela de Frequências!")
} else {
  print("Respeita a Regra.")
}

"-------------------------------------------------------------"

#### Construção de uma Tabela de Contingência: ####

# 1º - Definir a coluna das Freq. Absolutas = Freq. Observadas (Oi = ni):
(Oi <- table(TABELA_OU_VAR_DAS_CLASSES))

# 2º - Estimar Parâmetros (se necessário):
# . . .

# Indicar o Nº de Parâmetros Estimados:
(r <- NR_PARAMETROS_ESTIMADOS)

# Indicar o Nº de Linhas na Tabela / Nº de Elementos no Domínio:
(k <- NR_LINHAS_TABELA)

# Definir os Graus de Liberdade do Qui-Quadrado:
(gl <- k - 1 - r)

# 3º - Definir a coluna das Probabilidades (pi):
## Se xi é um valor: pi = P(X <= xi)
## Se xi é uma classe: pi = P(LIM_INF < X < LIM_SUP)
### Os sinais < > variam consoante a classe é aberta/fechada.

## Código para calcular a coluna pi para Distribuições Uniformes Discretas:
(pi <- rep(1/k, k))
sum(pi) # Tem que dar ~=~ 1, caso contrário está errado!

## Código para calcular a coluna pi para Distribuições Poisson:
(pi = dpois(xi, MEDIA_POISSON))
pi[k] <- 1 - ppois(xi[k-1], MEDIA_POISSON)
sum(pi) # Tem que dar ~=~ 1, caso contrário está errado!
round(pi, 4)

## Código para calcular a coluna pi para Distribuições Binomiais:
for (i in 1:k) {
  pi[i] <- dbinom(DOMINIO_DE_X[i], N_BINOMIAL, P_BINOMIAL)
}
round(pi, 4)
sum(pi) # Tem que dar ~=~ 1, caso contrário está errado!

## Código para calcular a coluna pi para Distribuições Exponencias:
(pi <- pexp(cortes[2:(k+1)], 1/estimativa))
for (i in 2:k) {
  pi[i] <- pexp(cortes[i+1], 1/estimativa) - pexp(cortes[i], 1/estimativa)
}
pi[k] <- 1 - pexp(cortes[k], 1/estimativa)
round(pi, 4)
sum(pi) # Tem que dar ~=~ 1, caso contrário está errado!

# Caso seja preciso juntar linhas, deve-se verificar quais as
# que falham à regra para as juntar.
# Após saber quais juntar, deve-se refazer o processo de
# construção (incluindo as classes, se necessário).

# Caso se estimem parâmetros, apenas o valor da E.T. do teste
# estará correto.
# Para saber o P-Value correto, é necessário calculá-lo.

# Exemplo da Exponencial:
(p.value_correto <- 1 - pchisq(teste_qui_quadrado$statistic, gl))

"-------------------------------------------------------------"

#### Medidas de Associação: ####

library(DescTools)

# Tabela de Contingência de Exemplo (já definida)
# | 24 | 41 |
# |  6 | 11 |

TABELA_CONTINGENCIA <- data.frame(
  coluna1 = c(24, 6),
  coluna2 = c(41, 11)
)

###### Coeficiente de Contingência: ######
ContCoef(TABELA_CONTINGENCIA)

###### Coeficiente V de Crámer: ######
CramerV(TABELA_CONTINGENCIA)

###### Coeficiente Tb de Kendall: ######
KendallTauB(TABELA_CONTINGENCIA)
# OU
KendallTauB(as.matrix(TABELA_CONTINGENCIA))

"-------------------------------------------------------------"

#### Função para Remover os Outliers de Uma Variável: ####

remover_outliers <- function (VARIAVEL) {
  # Utilização do Boxplot (com range a 1.5) para verificar facilmente
  # quais são os dados outliers.
  boxplot_outliers_aux <- boxplot(
    VARIAVEL,
    col = "gold",
    horizontal = TRUE,
    main = "Extremos e Quartis - Sem Outliers",
    xlab = "VARIAVEL",
    type = 2,
    range = 1.5
  )
  
  # Guardar numa variável os dados outliers provenientes do Boxplot.
  outliers <- boxplot_outliers_aux$out
  
  # Fazer uma cópia da variável original, para manipular.
  sem_outliers <- VARIAVEL
  
  # Remover, da variável cópia, todos os dados que estejam presentes
  # na lista de dados outliers.
  sem_outliers <- sem_outliers[!sem_outliers %in% outliers]
  
  # Retornar a nova variável criada, sem os dados outliers.
  return(sem_outliers)
}

"-------------------------------------------------------------"

#### Fórmulas das Regiões Críticas: ####

##### Distribuição Simétrica Normal: ####
# Teste Bilateral: RC = ] -∞ , -qnorm(1 - (α/2)) ] U [ qnorm(1 - (α/2)) , +∞ [
# Teste Unilateral Direito: RC = [ qnorm(1-α) , +∞ [
# Teste Unilateral Esquerdo: RC = ] -∞ , -qnorm(1-α) ]

##### Distribuição Simétrica T-Student: ####
# Teste Bilateral: RC = ] -∞ , -qt(1 - (α/2), df) ] U [ qt(1 - (α/2), df) , +∞ [
# Teste Unilateral Direito: RC = [ qt(1-α, df) , +∞ [
# Teste Unilateral Esquerdo: RC = ] -∞ , -qt(1 - α, df) ]

##### Distribuição Assimétrica Qui-Quadrado: ####
# Teste Bilateral: RC = [ 0 , qchisq(α/2, df) ] U [ qchisq(1 - (α/2), df) , +∞ [
# Teste Unilateral Direito: RC = [ qchisq(1-α, df) , +∞ [
# Teste Unilateral Esquerdo: RC = [ 0 , qchisq(α, df) ]

##### Distribuição Assimétrica F de Snedecor: ####
# Teste Bilateral: RC = [ 0 , qf(α/2, df1, df2) ] U [ qf(1-(α/2), df1, df2) , +∞ [
# Teste Unilateral Direito: RC = [ qf(1-α, df1, df2) , +∞ [
# Teste Unilateral Esquerdo: RC = [ 0 , qf(α, df1, df2) ]

"-------------------------------------------------------------"

#### Passos Para um Teste de Hipóteses: ####

# Teste de Hipóteses Paramétrico:
## OU
# Teste de Hipóteses Não Paramétrico:
# TESTE A FAZER:

# 1º Passo:

## H0: σ_X = σ_Y vs. H1: σ_X != σ_Y
## Teste Bilateral

# 2º Passo:

## α = 0.05

# 3º Passo:

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

"-------------------------------------------------------------"

# Símbolos: ####

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
# θ --> Theta

"-------------------------------------------------------------"
