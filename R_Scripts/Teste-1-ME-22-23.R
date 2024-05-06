#### Pergunta 1: ####

##### A) #####

64.8/360 # = 0.18
(1 - (0.18 + 0.405 + 0.205)) # = 0.21
210 + 180 + 405 + 205 # = 1000

(tab.frequencias.1a = data.frame(
  i = 1:4,
  xi = c("arábica", "liberia", "excelsa", "robusta"),
  ni = c(
    (1000 * 0.405),
    (1000 * 0.205),
    (1000 * (64.8 / 360)),
    (1000 - ((1000 * 0.405) + (1000 * 0.205) + (1000 * (64.8 / 360))))
  ),
  fi = c(
    0.405,
    0.205,
    (64.8/360),
    (1 - (0.405 + 0.205 + (64.8/360)))
  )
))

##### B) #####

###### i) ######
# Feito da folha.

###### ii) ######

cafe[cafe$bags == max(cafe$bags),] # Produziu mais -> Brasil
cafe[cafe$price == max(cafe$price),] # Produziu mais -> Brasil

###### iii) ######

(ni.month <- table(cafe$month))            # Freq. Absolutas
(fi.month <- round(prop.table(ni.month), 4)) # Freq. Relativas
(Ni.month <- cumsum(ni.month))               # Freq. Absolutas Acumuladas
(Fi.month <- round(cumsum(fi.month), 4))     # Freq. Relativas Acumuladas

(tabela.frequencias.month <- data.frame(
  i = 1:nrow(ni.month),
  xi = c("Abril", "Junho", "Outubro"),
  ni = as.integer(ni.month),
  fi = as.numeric(fi.month),
  Ni = as.integer(Ni.month),
  Fi = as.numeric(Fi.month)
))

# Gráfico Circular:
pie(
  ni.month,
  labels = paste(fi.month * 100, "%"),
  col = 2:4,
  main = "Mês da Colheita"
)

# Legenda:
legend(
  "topright",                       # Posição da Legenda
  legend = tabela.frequencias.month$xi, # Valores da Legenda (Nomes)
  fill = 2:4,                    # Cores dos Valores
  cex = 1                           # Tamanho do Objeto (Escala)
)

###### iv) ######

(TABELA_ORDENADA <- factor(
  cafe$month,
  levels=c("Ãbril", "Junho", "Outubro")
))

# Boxplot entre 2 Variáveis:
boxplot(
  price ~ month,
  data = cafe,
  col = 2:4,
  main = "Preço Pago de Acordo com a Colheita",
  xlab = "Mês da Colheita", 
  ylab = "Preço Pago aos Produtores",
  type = 2
)

###### v) ######

## Coeficiente de variação da VAR. bags:
((sd(cafe$bags)/mean(cafe$bags))*100) # = 299.3971%

## Coeficiente de variação da VAR. month:
((sd(cafe$price)/mean(cafe$price))*100) # = 44.5094%

###### vi) ######

# Tabela de Frequências - Com Classes:

# Regra de Sturgis:

(n <- nrow(cafe))

# Classes:
(k <- trunc(1 + log(n)/log(2)))

# Amplitude:
(h <- (max(cafe$price) - min(cafe$price)) / k)

# Mínimo e Máximo das Classes:
(price.min <- min(cafe$price))
(price.max <- price.min + h * k)

# Extremos das Classes:
(price.cortes <- seq(price.min, price.max, by = h))

# Intervalos - Abertos à Esquerda e Fechados à Direita:
(price.classes <- cut(
  cafe$price,
  breaks = price.cortes,
  right = FALSE,
  include.lowest = TRUE
))

(ni.price <- table(price.classes))             # Freq. Absolutas
(fi.price <- round(prop.table(ni.price), 4)) # Freq. Relativas
(Ni.price <- cumsum(ni.price))               # Freq. Absolutas Acumuladas
(Fi.price <- round(cumsum(fi.price), 4))     # Freq. Relativas Acumuladas

(tabela.frequencias.price <- data.frame(
  i = 1:nrow(ni.price),
  xi = names(ni.price),
  ni = as.integer(ni.price),
  fi = as.numeric(fi.price),
  Ni = as.integer(Ni.price),
  Fi = as.numeric(Fi.price)
))

# Gráfico:
hist(
  cafe$price,
  breaks = price.cortes,
  right = FALSE,
  include.lowest = TRUE,
  freq = TRUE, # TRUE => Freq. Absolutas | FALSE => Freq. Relativas
  main = "Histograma de Freq. Absolutas",
  xlab = "PRICE",
  ylab = "Frequências Absolutas",
  col = 2:7,
  xlim = c(price.min, price.max),
  ylim = c(0, 20),
  xaxt = "n"  # Para Poder Definir o Eixo XX de Seguida
)

# Valores do Eixo XX:
axis(
  side = 1,
  at = c(
    round(price.min, 2),
    round(price.cortes, 2),
    round(price.max, 2)
  )
)

#### Pergunta 2: ####

##### A) #####

# Na função f_densidade_probabilidade, cada ifelse representa:
## Condição: Ramo conhecido;
## Verdadeiro: Valor desse ramo;
## Falso: Novo ifelse com outro ramo, ou 0 caso seja o último ramo conhecido.
f_densidade_probabilidade <- function (x) {
  ifelse(
    0 < x & x < 1,
    x,
    ifelse(
      1 <= x & x < 3,
      (x/8),
      0 # SÓ SE FOR O ÚLTIMO RAMO!!
    )
  )
}

# F(x):

# No ramo X <= 0:
(integrate(f_densidade_probabilidade, lower=-Inf, upper=0)$value)

# No ramo 0 < X < 1:
(integrate(f_densidade_probabilidade, lower=0, upper=1)$value)

# No ramo 1 <= X < 3:
(integrate(f_densidade_probabilidade, lower=1, upper=5)$value)

# No ramo X >= 3:
(integrate(f_densidade_probabilidade, lower=1, upper=5)$value)

##### B) #####



##### C) #####



##### D) #####

###### i) ######



###### ii) ######



#### Pergunta 3: ####

##### A) #####



##### B) #####

###### i) ######



###### ii) ######



##### C) #####


