################## Resumo dos Comandos no R ##################

############# Resumo R: Primeiro Teste #############

"-------------------------------------------------------------"

#### Ver Tabela/Dados: ####

View(TABELA)

"-------------------------------------------------------------"

#### Consultar uma Tabela: ####

TABELA["CONDIÇÃO_PARA_LINHAS", c("COLUNAS_A_APRESENTAR")]

"-------------------------------------------------------------"

#### Ordenar uma Tabela por uma Coluna: ####

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

#### Para verificar se existem dados omissos: ####

any(is.na(TABELA))

# OU

anyNA(TABELA)

"-------------------------------------------------------------"

#  Variáveis Qualitativas: Só se faz a Medida de Localização Central - Moda!

# Variáveis Quantitativas:
(ni.VARIAVEL <- table(TABELA$COLUNA))

#### Medidas de Localização Central: ####

###### Moda: ######
if(min(ni.VARIAVEL)==max(ni.VARIAVEL)){
  print("Amodal")
} else{
  print(paste("Moda: ", names(ni.VARIAVEL)[ni.VARIAVEL==max(ni.VARIAVEL)]))
}
# OU
if(min(ni.VARIAVEL)==max(ni.VARIAVEL)){
  print("Amodal")
} else{
  print(paste("Moda: ", DescTools::Mode(ni.VARIAVEL)))
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

###### Coeficiente de Variância = CV: ######
((sd(TABELA$COLUNA)/mean(TABELA$COLUNA))*100)

"-------------------------------------------------------------"

#### Legenda Para Gráficos: ####
# Funciona para Tudos (Gráficos + Histogramas + BoxPlots):

legend(
  "topright",                       # Posição da Legenda
  legend = c("VALOR_1", "VALOR_2"), # Valores da Legenda (Nomes)
  fill = c(1:2),                    # Cores dos Valores
  cex = 1                           # Tamanho do Objeto (Escala)
)

### O Comando da Legenda Deve Ser Executado Depois de Criar o Gráfico!

"-------------------------------------------------------------"

#### Gráfico de Barras de Frequências Absolutas: ####

## Freq. Absolutas:
(ni.VARIAVEL <- table(TABELA$COLUNA))

## Gráfico de Barras:
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

## Frequências Absolutas:
(ni.VARIAVEL <- table(TABELA$COLUNA))

## Frequências Relativas:
(fi.VARIAVEL <- round(prop.table(ni.VARIAVEL), 4))

barplot(
  fi.VARIAVEL,                              # Variável no Gráfico
  main = "TITULO (Frequências Absolutas)",  # Título do Gráfico
  xlab = "VARIAVEL",                        # Título Eixo XX
  ylab = "Frequências Relativas",           # Título Eixo YY
  col = 1:2,                                # Cores das Barras 
  ylim = c(0, 2.5),                         # Limites do Eixo YY
  names.arg = c("BARRA_1", "BARRA_2")       # Nomes das Barras
)

"-------------------------------------------------------------"

#### Gráfico de Barras de Frequências Relativas Em Percentagem: ####

# Colocar as Categorias da Variável por Ordem:
TABELA_ORDENADA <- factor(
  TABELA$COLUNA,
  levels=c("CATEGORIA_1", "CATEGORIA_2")
)

## Frequências Absolutas:
(ni.VARIAVEL <- table(TABELA$COLUNA))

## Frequências Relativas:
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

## SE PRECISO - Colocar as Categorias da Variável por Ordem:
TABELA_ORDENADA <- factor(
  TABELA$COLUNA,
  levels=c("CATEGORIA_1", "CATEGORIA_2")
)

## Frequências Absolutas:
(ni.VARIAVEL <- table(TABELA$COLUNA))
# OU
(ni.VARIAVEL <- table(TABELA_ORDENADA))

## Frequências Relativas:
(fi.VARIAVEL <- round(prop.table(ni.VARIAVEL), 4))

## Gráfico:
pie(
  ni.VARIAVEL,
  labels=paste(fi.VARIAVEL * 100, "%"),
  col=c("red", "blue"),
  main="TITULO"
)

"-------------------------------------------------------------"

#### Histograma - Eixo YY - Freq. Absolutas: ####

hist(
  TABELA$COLUNA,
  breaks = COLUNA.cortes,
  right = TRUE,
  include.lowest = TRUE,
  freq = TRUE,
  main = "Histograma",
  xlab = "VARIAVEL",
  ylab = "Frequências Absolutas",
  col = 2,
  xlim = c(0,70),
  ylim = c(0,1200),
  xaxt = "n"  # Para Poder Definir o Eixo XX de Seguida
)

axis(
  side = 1,
  at = c(0, COLUNA.cortes, 70)
)  # Define os Valores para o Eixo XX Igual Às Classes

"-------------------------------------------------------------"

#### Histograma - Eixo YY - fi/h - Comparação entre Qualquer Tipo de Classes: ####

hist(
  TABELA$COLUNA,
  breaks = COLUNA.cortes,
  right = TRUE,
  include.lowest = TRUE,
  freq = FALSE,
  main = "Histograma",
  xlab = "VARIAVEL",
  ylab = "Frequências Relativas / Amplitude das Classes",
  col = 2,
  xlim = c(0,70),
  ylim = c(0,0.1),
  xaxt = "n"  # para poder definir o eixo dos xx
)

axis(
  side = 1,
  at = c(0, COLUNA.cortes, 70)
)  # Define os Valores para o Eixo XX Igual Às Classes

"-------------------------------------------------------------"

#### Histograma - Eixo YY - fi - Comparação Mesmo Tipo de Classes: ####

## Classes:
(COLUNA.classes <- cut(
  TABELA$COLUNA,
  breaks = COLUNA.cortes,
  right = TRUE,
  include.lowest = TRUE
))

## Frequências Absolutas:
(ni.VARIAVEL <- table(TABELA$COLUNA))

## Frequências Relativas:
(fi.VARIAVEL <- round(prop.table(ni.VARIAVEL), 4))

# Atribuir um nome ao histograma para poder aceder aos seus campos:
graf <- hist(
  TABELA$COLUNA,
  breaks = COLUNA.cortes,
  right = TRUE,
  include.lowest = TRUE
)

graf$density <- fi.VARIAVEL

plot(
  graf,
  freq = FALSE,
  main = "Histograma",
  xlab = "VARIAVEL",
  ylab = "Frequências Relativas",
  col = 2,
  xlim = c(0, 70),
  ylim = c(0, 0.5),
  xaxt = "n"  # para poder definir o eixo dos xx
)

axis(
  side = 1,
  at = c(0, COLUNA.cortes, 70)
)  # Define os Valores para o Eixo XX Igual Às Classes

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

"-------------------------------------------------------------"

#### Calcular Integrais: ####

f1 <- function (x) { (x^2 * (x-1)) }

(integrate(f1, lower=1, upper=5)$value)

"-------------------------------------------------------------"

#### Calcular Derivadas: ####

library(Deriv)

expr <- function (x) { ((x^3/3) + (1/3)) }
(Deriv::Deriv(expr, "x"))

"-------------------------------------------------------------"

#### Variáveis Aleatórias Contínuas: ####

###### Função Densidade de Probabilidade: ######
pdf_cust <- function (x) {
  ifelse(-1 <= x & x < 0, (x^2),
    ifelse(0 <= x & x < 1, (x),
      ifelse(1 <= x & x < 3, (1/12), 0)
    )
  )
}

## E[X] - Interno:
media_ex_continuas <- function (x) {
  pdf_cust(x) * x
}

## E[X^2] - Interno:
ex2_continuas <- function (x) {
  pdf_cust(x) * (x^2)
}

## Valor E[X]:
valor_ex_continuas <- function () {
  integrate(media_ex_continuas, lower = -Inf, upper = +Inf)$value
}

## Valor E[X^2]:
valor_ex2_continuas <- function () {
  integrate(ex2_continuas, lower = -Inf, upper = +Inf)$value
}

## Valor V[X]:
valor_vx_continuas <- function () {
  integrate(ex2_continuas, lower = -Inf, upper = +Inf)$value - (integrate(media_ex_continuas, lower = -Inf, upper = +Inf)$value)^2
}

###### E[X]: ######
valor_ex_continuas()

###### E[X^2]: ######
valor_ex_continuas()

###### V[X]: ######
valor_vx_continuas()

###### E[AX + B] = A * E[X] + B ######
(A * valor_ex_continuas() + B)

###### E[AX^2 + B] = A * E[X^2] + B ######
(A * valor_ex2_continuas() + B)

"-------------------------------------------------------------"

#### Dist. Uniforme Contínua - Descobrir "a" e "b": ####

# Só funciona se se souber a Média, e um F(X) = ? !!!

# Média = (a + b)/2
# a = Média * 2 - b
# b = Média * 2 - a

# Resolvendo para "a":
# Sabemos que: (X - a) / (b - a) = prob_conhecida
# Substituindo "a" por: media * 2 - b:
# (X - (media * 2 - b)) / (b - (media * 2 - b)) = prob_conhecida
# Simplificando:
# (X - 2media + b) / (2b - 2media) = prob_conhecida

# Agora para "b":

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
  # Encontrando "a" e "b" utilizando otimização:
  result <- optimize(
    verificar_prob,
    interval = c(0, media),
    media = media,
    x = x,
    prob_conhecida = prob_conhecida
  )
  
  a <- result$minimum
  b <- 2 * media - a
  
  print(paste("Valor de a:", round(a, 4)))
  print(paste("Valor de b:", round(b, 4)))
}

calcular_limites_dominio_uniforme_continua(505.4, 500, 0.05)

"-------------------------------------------------------------"

#### Símbolos: ####

## ✓ --> Certo
## ∧ --> Conjunção / E
## ∀x --> Todo e qualquer x
## √ --> Raiz Quadrada
## Σ --> Somatório
## λ --> Lambda
## σ --> Sigma / Desvio Padrão
## ∫ --> Integral
## ∞ --> Infinito
## [-∞, +∞] --> Intervalo Infinito

"-------------------------------------------------------------"
