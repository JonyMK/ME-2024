#### Pergunta 1: ####

##### 1.A) #####

# Dimensão da Amostra
nrow(diamantes)

##### 1.B) #####

# Ordenar da Pior para a Melhor Cor:
cor_ordenada <- factor(
  diamantes$cor,
  levels=c("J", "I", "H", "G", "F", "E", "D")
)

# Frequências Absolutas:
(ni.cor <- table(cor_ordenada))

# Frequências Relativas:
(fi.cor <- round(prop.table(ni.cor), 4))

# Gráfico:
barplot(
  fi.cor,                              # Variável no Gráfico
  main = "Cores dos Diamantes (Freq. Relativas)",  # Título do Gráfico
  xlab = "cor",                        # Título Eixo XX
  ylab = "Frequências Relativas",           # Título Eixo YY
  col = 2:8,                                # Cores das Barras 
  ylim = c(0, 0.3),                         # Limites do Eixo YY
  names.arg = c("J (Pior)", "I", "H", "G", "F", "E", "D (Melhor)")       # Nomes das Barras
)

##### 1.C) #####


# Regra de Sturgis:

(n <- nrow(diamantes))

# Classes:
(k <- trunc(1 + log(n)/log(2)))

# Amplitude:
(h <- (max(diamantes$largura) - min(diamantes$largura)) / k)

# Mínimo e Máximo das Classes:
(largura.min <- min(diamantes$largura))
(largura.max <- largura.min + h * k)

# Extremos das Classes:
(largura.cortes <- seq(largura.min, largura.max, by = h))

# Intervalos - Abertos à Esquerda e Fechados à Direita:
(largura.classes <- cut(
  diamantes$largura,
  breaks = largura.cortes,
  right = TRUE,
  include.lowest = TRUE
))

(ni.largura <- table(largura.classes))             # Freq. Absolutas
(fi.largura <- round(prop.table(ni.largura), 4)) # Freq. Relativas
(Ni.largura <- cumsum(ni.largura))               # Freq. Absolutas Acumuladas
(Fi.largura <- round(cumsum(fi.largura), 4))     # Freq. Relativas Acumuladas

(tabela.frequencias.largura <- data.frame(
  i = 1:nrow(ni.largura),
  xi = names(ni.largura),
  ni = as.integer(ni.largura),
  fi = as.numeric(fi.largura),
  Ni = as.integer(Ni.largura),
  Fi = as.numeric(Fi.largura)
))

# Moda:
if(min(ni.largura)==max(ni.largura)){
  print("Amodal")
} else{
  print(paste("Moda:", names(ni.largura)[ni.largura==max(ni.largura)]))
}

# Mediana:
median(diamantes$largura, type = 2)

##### 1.D) #####

# Quantis:
quantile(
  diamantes$preco,
  prob = c(0.85),
  type = 2
)

# Simetria:
e1071::skewness(diamantes$preco) # b1 > 0 => Assimetria Positiva / à Direita

##### 1.E) #####

# Boxplot entre 2 Variáveis:
boxplot(
  preco ~ corte,
  data = diamantes,
  col = 2:6,
  main = "Comparação entre o Preço com os Tipos de Corte",
  xlab = "Corte", 
  ylab = "Preço",
  type = 2
)

##### 1.F) #####

# CV - preco:
((sd(diamantes$preco)/mean(diamantes$preco))*100) # 101.4402%

# CV - peso:
((sd(diamantes$quilate)/mean(diamantes$quilate))*100) # 59.4044%

"------------------------------------------"

#### Pergunta 2: ####

##### 2.A) #####
# Feito na folha.

##### 2.B) #####

((0.7 - 0.4) / (1 - 0.4)) # = 0.5

##### 2.C) #####

# Não Realizei!

##### 2.D) #####

# Função Densidade de Probabilidade:

# Na função f_densidade_probabilidade, cada ifelse representa:
## Condição: Ramo conhecido;
## Verdadeiro: Valor desse ramo;
## Falso: Novo ifelse com outro ramo, ou 0 caso seja o último ramo conhecido.
f_densidade_probabilidade <- function (x) {
  ifelse(
    0 <= x & x < 1,
    ((2*x) / 3),
    ifelse(
      1 <= x & x < 3,
      (1 - (x/3)),
      0
    )
  )
}

# Funções Auxiliares:

## E[X] - Interno:
ex_continuas <- function (x) {
  # Corresponde a: x * f(x)
  f_densidade_probabilidade(x) * x
}

## E[X^2] - Interno:
ex2_continuas <- function (x) {
  # Corresponde a: x^2 * f(x)
  f_densidade_probabilidade(x) * (x^2)
}

## Valor E[X]:
valor_ex_continuas <- function () {
  # Nas contínuas, o E[X] corresponde à integral
  # de - infinito a + infinito de x * f(x).
  integrate(ex_continuas, lower = -Inf, upper = +Inf)$value
}

## Valor E[X^2]:
valor_ex2_continuas <- function () {
  # Nas contínuas, o E[X^2] corresponde à integral
  # de - infinito a + infinito de x^2 * f(x).
  integrate(ex2_continuas, lower = -Inf, upper = +Inf)$value
}

## Valor V[X]:
valor_vx_continuas <- function () {
  # Independentemente de ser ou não contínua, o V[X] corresponde
  # à diferença entre E[X^2] e E^2[X] (os integrais são calculados nos valores
  # esperados).
  valor_ex2_continuas() - (valor_ex_continuas()^2)
}

# E[X]:
valor_ex_continuas()

# E[X^2]:
valor_ex2_continuas()

# V[X]:
valor_vx_continuas()

((9/16) * valor_vx_continuas()) # = 0.2188

"------------------------------------------"

#### Pergunta 3: ####

##### 3.A) #####

1 - ppois(4, 3) # = 0.1847
1 - ppois(2, 3) # = 0.5768

((1 - ppois(4, 3)) / (1 - ppois(2, 3))) # = 0.3203

##### 3.B) #####

qpois(0.97, 3) # = 7

##### 3.C) #####

1 - ppois(9, 15) # = 0.9301

"------------------------------------------"

#### Pergunta 4: ####

##### 4.A) #####

1 - pnorm(1.5, 1.75, 0.1) # = 0.9938

##### 4.B) #####

pnorm(1.5, 1.75, 0.1) # = 0.0062
pbinom(6, 15, 0.0062) # = 1

##### 4.C) #####

195 * 1.75
sqrt(195 * (0.1^2))
1 - pnorm(335, 341.25, 1.3964) # = 0.9999962 ~ 1.0000

##### 4.D) #####

qnorm(0.95) # = 1.6449
3 / 1.6449 # = 1.8238




