#### Pergunta 1: ####

##### A) #####
# Feito na folha.

##### B) #####

## VAR. treat

(ni.treat <- table(distonia[distonia$sex == 1,]$treat))            # Freq. Absolutas
(fi.treat <- round(prop.table(ni.treat), 4)) # Freq. Relativas
(Ni.treat <- cumsum(ni.treat))               # Freq. Absolutas Acumuladas
(Fi.treat <- round(cumsum(fi.treat), 4))     # Freq. Relativas Acumuladas

(tabela.frequencias.treat.femininos <- data.frame(
  i = 1:nrow(ni.treat),
  xi = names(ni.treat),
  ni = as.integer(ni.treat),
  fi = as.numeric(fi.treat),
  Ni = as.integer(Ni.treat),
  Fi = as.numeric(Fi.treat)
))

(ni.treat <- table(distonia[distonia$sex == 2,]$treat))            # Freq. Absolutas
(fi.treat <- round(prop.table(ni.treat), 4)) # Freq. Relativas
(Ni.treat <- cumsum(ni.treat))               # Freq. Absolutas Acumuladas
(Fi.treat <- round(cumsum(fi.treat), 4))     # Freq. Relativas Acumuladas

(tabela.frequencias.treat.masculinos <- data.frame(
  i = 1:nrow(ni.treat),
  xi = names(ni.treat),
  ni = as.integer(ni.treat),
  fi = as.numeric(fi.treat),
  Ni = as.integer(Ni.treat),
  Fi = as.numeric(Fi.treat)
))

(tabela.frequencias.treat.femininos)
(tabela.frequencias.treat.masculinos)

## VAR. age

(ni.age <- table(distonia$age))            # Freq. Absolutas
(fi.age <- round(prop.table(ni.age), 4)) # Freq. Relativas
(Ni.age <- cumsum(ni.age))               # Freq. Absolutas Acumuladas
(Fi.age <- round(cumsum(fi.age), 4))     # Freq. Relativas Acumuladas

(tabela.frequencias.age <- data.frame(
  i = 1:nrow(ni.age),
  xi = names(ni.age),
  ni = as.integer(ni.age),
  fi = as.numeric(fi.age),
  Ni = as.integer(Ni.age),
  Fi = as.numeric(Fi.age)
))

## VAR. sex

## VAR. twstrs



##### C) #####

min(distonia$age)
max(distonia$age)

# Intervalos - Abertos à Esquerda e Fechados à Direita:
(age.classes <- cut(
  distonia$age,
  breaks = age.cortes,
  right = TRUE,
  include.lowest = TRUE
))

# Freq. Absolutas
(ni.age <- table(age.classes))
# Freq. Relativas
(fi.age <- round(prop.table(ni.age), 4))

# Extremos das Classes:
(age.cortes <- c(26, 45, 60, 83))

# Gráfico:
hist(
  distonia$age,
  breaks = age.cortes,
  right = TRUE,
  include.lowest = TRUE,
  freq = FALSE, # TRUE => Freq. Absolutas | FALSE => Freq. Relativas
  main = "Histograma",
  xlab = "age",
  ylab = "Frequências Relativas",
  col = 1:3,
  xlim = c(26, 83),
  ylim = c(0, 0.04),
  xaxt = "n"  # Para Poder Definir o Eixo XX de Seguida
)

# Valores do Eixo XX:
axis(
  side = 1,
  at = c(
    round(26, 2),
    round(age.cortes, 2),
    round(83, 2)
  )
)

##### D) #####

# Boxplot entre 2 Variáveis:
boxplot(
  twstrs ~ treat,
  data = distonia,
  col = 2:4,
  main = "Pontuação por Tipo de Tratamento",
  xlab = "Tipo de Tratamento", 
  ylab = "Ponuação do Tratamento",
  type = 2
)

##### E) #####

# Moda:
if(min(ni.age)==max(ni.age)){
  print("Amodal")
} else{
  print(paste("Moda:", names(ni.age)[ni.age==max(ni.age)]))
}

# Média:
mean(distonia$age)

# Mediana:
median(distonia$age, type = 2)

# Quantis:
quantile(
  distonia$age,
  prob = c(0.25, 0.50, 0.75),
  type = 2
)

#### Pergunta 2: ####

##### A) #####
# Feito na folha.

##### B) #####

(0.9 - 0.1) / 0.9 # = 0.8889

##### C) #####

# Função de Probabilidade:
x <- c(1, 2, 3, 4, 5)
fx <- c(0.1, 0.3, 0.3, 0.2, 0.1)

##############################
##### Funções Auxiliares #####
##############################

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

##############################
##############################
##############################

# E[X]:
valor_ex_discretas()

# E[X^2]:
valor_ex2_discretas()

# V[X]:
valor_vx_discretas()

FRACTION::fra(((7-2)/3)^2)

FRACTION::fra((25/9) * 1.29) # = 43/12

##### D) #####

f_densidade_probabilidade <- function (x) {
  ifelse(
    0 <= x & x <= 80,
    ((1/40) - (x/3200)),
    0
  )
}

1 - integrate(f_densidade_probabilidade, lower = -Inf, upper = 60)$value # = 0.0625034

#### Pergunta 3: ####

##### A) #####

dpois(3, 10) # = 0.0076

##### B) #####

1 - pexp(2, 1/6) # = 0.7165

##### C) #####

###### i) ######

pnorm(280, 300, 20) # = 0.1587
1 - pbinom(0, 7, 0.1587) # = 0.7017

###### ii) ######

qnorm(0.05, 300, 20) # = 267.1029
pnorm(268.1029, 300, 20) # = 0.0554 -> Aprox. 0.05

###### iii) ######

qnorm(0.2)

40/0.8416 # = 47.5285
