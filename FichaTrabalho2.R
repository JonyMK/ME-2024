#########################################################################
########################## Ficha de Trabalho 2 ##########################
#########################################################################

#######################################################
##################### Exercicio 1 #####################
#######################################################

##############
##### A) #####
##############

##############
##### A) #####
##############

##############
##### A) #####
##############

##############
##### A) #####
##############

##############
##### A) #####
##############

##############
##### A) #####
##############

##############
##### A) #####
##############



#######################################################
##################### Exercicio 2 #####################
#######################################################

##############
##### A) #####
##############

# Represente graficamente as variáveis qualitativas ordinais 
# recorrendo ao gráfico de barras.

##### variável: FCVC

# Se come habitualmente vegetais nas refeições: 
# 1= Nunca, 2=As vezes, 3=Sempre

(ni.FCVC <- table(obesidade$FCVC)) # frequências absolutas

#gráfico de barras das frequências absolutas
barplot(
  ni.FCVC,
  main = "Se come habitualmente vegetais nas refeições", 
  xlab = "FCVC",
  ylab = "Frequências absolutas", 
  col = 3:5,
  ylim = c(0, 1200),
  names.arg = c("Nunca", "Às vezes", "Sempre")
)

(fi.FCVC <- round(prop.table(ni.FCVC),4)) # frequências relativas

#gráfico de barras das frequências absolutas
barplot(
  fi.FCVC, 
  main = "Se come habitualmente vegetais nas refeições", 
  xlab = "FCVC",
  ylab = "Frequências relativas", 
  col = 3:5,
  ylim = c(0, 1), 
  names.arg = c("Nunca", "Às vezes", "Sempre")
)

##### variável CH2O

(ni.CH2O <- table(obesidade$CH2O))          # frequências absolutas
(fi.CH2O <- round(prop.table(ni.CH2O),4))   # frequências relativas

#gráfico de barras das frequências relativas
barplot(
  fi.CH2O, 
  main = "Quantidade de água que bebe diariamente",
  xlab = "CH2O", 
  ylab = "Frequências relativas", 
  col = 13:15,
  ylim = c(0, 0.6), 
  names.arg = c("< 1 litro", "1 - 2 litros", "> 2 litros")
)

##############
##### B) #####
##############

# Represente graficamente as variáveis qualitativas nominais 
# recorrendo ao diagrama circular.

##### variável Género

(ni.G <- table(obesidade$Genero))     # frequências absolutas
(fi.G <- round(prop.table(ni.G), 4))   # frequências relativas

pie(
  ni.G,
  labels = paste(fi.G*100, "%"), 
  col = c("yellow", "lightblue"), 
  main = "Género"
)

legend(
  "topleft", 
  legend = names(ni.G), 
  fill = c("yellow", "lightblue"),
  cex = 0.7
)

##### variável FAVC

(ni.FAVC <- table(obesidade$FAVC))          # frequências absolutas
(fi.FAVC <- round(prop.table(ni.FAVC), 4))   # frequências relativas

pie(
  ni.FAVC,
  labels = paste(fi.FAVC*100, "%"),
  col = 2:3,
  main = "Se come alimentos altamente calóricos habitualmente"
)

legend(
  "topright",
  legend = c("Não", "Sim"),
  fill = 2:3,
  cex = 0.7
)

##### variável MTRANS

# colocar as categorias por outra ordem para o gráfico ficar legível
obesidade$MTRANS <- factor(obesidade$MTRANS, levels=c("Automovel", "Mota", "Transportes_Publicos", "Bicicleta", "A_pe"))

(ni.MTRANS <- table(obesidade$MTRANS))          # frequências absolutas
(fi.MTRANS <- round(prop.table(ni.MTRANS), 4))   # frequências relativas

pie(
  ni.MTRANS,
  labels = paste(fi.MTRANS*100, "%"),
  cex = 0.7,
  col = 2:6, 
  main = "Que tipo de transporte utiliza habitualmente"
)

legend(
  "topleft",
  legend = c("Automóvel", "Mota", "Transportes públicos", "Bicicleta", "Anda a pé"), 
  fill = 2:6,
  cex = 0.7
)

##############
##### C) #####
##############

# Represente graficamente as variáveis quantitativas discretas 
# utilizando as frequências relativas.

##### variável NCP: Número de refeições principais que tem habitualmente: 0, 1, 2, 3, 4,...

(ni.NCP <- table(obesidade$NCP))           # frequências absolutas
(fi.NCP <- round(prop.table(ni.NCP), 4))   # frequências relativas

#gráfico de barras das frequências relativas
barplot(
  fi.NCP, 
  main = "Número de refeições principais que tem habitualmente", 
  xlab = "NCP",
  ylab = "Frequências relativas", 
  col = 4:7,
  ylim = c(0, 1)
)

##############
##### D) #####
##############

########
## i) ##
########

# Em relação à variàvel idade, considere 8 classes, 
# com amplitude 6 anos e a primeira classe a começar nos 14 anos 
# (considere intervalos abertos à esquerda e fechados à direita);

##### variável estatística: Idade

# mínimo e máximo dos dados
min(obesidade$Idade)
max(obesidade$Idade)

k <- 8   # 8 classes
h <- 6   # amplitude 6 anos

# mínimo e máximo das classes
valor.min <- 14   # primeira classe a começar nos 14 anos
(valor.max <- valor.min + h*k)

#extremos das classes
(cortes <- seq(valor.min, valor.max, by=h))

# intervalos abertos à esquerda e fechados à direita
# como o mínimo dos dados = ao primeiro valor da primeira classe,
# a primeira classe tem de ser fechada dos dois lados

# histograma -> eixo dos yy -> frequências absolutas
hist(
  obesidade$Idade,
  breaks = cortes,
  right = TRUE,
  include.lowest = TRUE,
  freq = TRUE,
  main = "Histograma",
  xlab = "Idade",
  ylab = "frequências absolutas",
  col = 2,
  xlim = c(0, 70),
  ylim = c(0, 1200),
  xaxt = "n"  # para poder definir o eixo dos xx como pretendermos
)                 

axis(
  side = 1,
  at = c(0, cortes, 70) # definir os valores para o eixo dos xx igual às classes
)

# INICIO NOTA ------------------------------------------
# NOTA: caso fosse necessário saber qual o ni de cada classe para 
#  colocar no ylim, fariamos:
(classes <- cut(obesidade$Idade, breaks=cortes, right=TRUE, include.lowest=TRUE))

# tabela de frequências
(ni.f <- table(classes))  # frexquências absolutas

# Como o maior ni é 1028, o limite do eixo dos yy foi 
# escolhido entre 0 e 200
# FIM NOTA -----------------------------------------

# histograma -> eixo dos yy -> fi/h  -> permite comparar histogramas com qualquer tipo de classes
hist(
  obesidade$Idade,
  breaks = cortes,
  right = TRUE,
  include.lowest = TRUE,
  freq = FALSE,
  main = "histograma",
  xlab = "Idade",
  ylab = "frequências relativas / amplitude das classes",
  col = 2,
  xlim = c(0, 70),
  ylim = c(0, 0.1),
  xaxt = "n" # para poder definir o eixo dos xx
)

axis(
  side = 1,
  at = c(0, cortes, 70) # definir os valores para o eixo dos xx igual às classes
)

#########
## ii) ##
#########

# Em relação à variàvel altura, resuma a informação 
# nas 3 classes: [1.45,1.60], ]1.60,1.80] e ]1.80,2.00];

# variável estatística: Altura

# mínimo e máximo dos dados
min(obesidade$Altura)
max(obesidade$Altura)

# 3 classes: [1.45,1.60], ]1.60,1.80] e ]1.80,2.00]
#extremos das classes
cortes2 <- c(1.45,1.60,1.80,2.00)

# intervalos abertos à esquerda e fechados à direita
# a primeira classe fechada dos dois lados

#####
# histograma -> eixo dos yy -> frequências absolutas -> só faz se colocar freq=TRUE 
# avisa que está errado

#####
# histograma -> eixo dos yy -> fi/h  -> permite comparar histogramas com qualquer tipo de classes
# como as classes têm amplitudes diferentes, este é o histograma correto
hist(
  obesidade$Altura,
  breaks = cortes2,
  right = TRUE,
  include.lowest = TRUE,
  freq = FALSE,
  main = "histograma",
  xlab = "Altura",
  ylab = "frequências relativas / amplitude das classes",
  col = 3,
  xlim = c(0, 2.5),
  ylim = c(0, 4),
  xaxt = "n"  # para poder definir o eixo dos xx
)

axis(
  side = 1,
  at = c(0, cortes2, 2.5),
  las = 2  # definir os valores para o eixo dos xx igual às classes
)

##########
## iii) ##
##########

# Em relação à variàvel peso, recorra à regra de Sturges para definir as classes 
# (considere intervalos fechados à esquerda e abertos à direita).

# mínimo e máximo do NIVEL DE FERRO
(n <- nrow(obesidade))
(min <- min(obesidade$Peso))
(max <- max(obesidade$Peso))

(k <- trunc(1 + log(n) / log(2)))   # número de classes pela regra Sturges
(h <- (max-min) / k)   # amplitude das classes

# mínimo e máximo das classes
(valor.min <- min)
(valor.max <- min + h*k)

#extremos das classes
(cortes3 <- seq(valor.min, valor.max, by=h))

# intervalos fechados à esquerda e abertos à direita
# como o maximo dos dados é igual ao último valor da última classe
# a última classe tem de ser fechada nos dois lados
#####

# histograma -> eixo dos yy -> frequências  
hist(
  obesidade$Peso,
  breaks = cortes3,
  right = FALSE,
  include.lowest = TRUE,
  freq = TRUE,
  main = "histograma",
  xlab = "Peso",
  ylab = "frequências absolutas",
  col = "yellow",
  xlim = c(0, 180),
  ylim = c(0, 500),
  xaxt = "n" # para poder definir o eixo dos xx
)

axis(
  side = 1,
  at = c(0, round(cortes3, 1), 180),
  las = 2
)


# histograma -> eixo dos yy -> fi/h  -> permite comparar histogramas com qualquer tipo de classes
# como as classes têm amplitudes diferentes, este é o histograma correto
hist(
  obesidade$Peso,
  breaks = cortes3,
  right = FALSE,
  include.lowest = TRUE,
  freq = FALSE,
  main = "histograma",
  xlab = "Peso",
  ylab = "frequências relativas / amplitude das classes",
  col = "magenta",
  xlim = c(0, 180),
  ylim = c(0, 0.02),
  xaxt = "n"  # para poder definir o eixo dos xx
)

axis(
  side = 1,
  at = c(0, round(cortes3, 1), 180),
  las = 2  # definir os valores para o eixo dos xx igual às classes
)


#####
# histograma -> eixo dos yy -> colocar o que se pretende, por exemplo, 
#             fi -> permite comparar histogramas com as mesmas classes

# tabela de frequências
(classes3 <- cut(
  obesidade$Peso,
  breaks = cortes3,
  right = FALSE,
  include.lowest = TRUE
))
(ni.Peso <- table(classes3))                # frequências absolutas
(fi.Peso <- round(prop.table(ni.Peso), 4))   # frequências relativas

# atribuir um nome ao histograma para poder aceder aos seus campos
graf3 <- hist(
  obesidade$Peso,
  breaks = cortes3,
  right = FALSE,
  include.lowest = TRUE
)

graf3$density <- fi.Peso

plot(
  graf3,
  freq = FALSE,
  main = "Histograma",
  xlab = "Peso",
  ylab = "frequências relativas",
  col = 4,
  xlim = c(0, 180),
  ylim = c(0, 0.25),
  xaxt = "n"  # para poder definir o eixo dos xx
)

axis(
  side = 1,
  at  =c(0, round(cortes3, 1), 180),
  las = 2  # definir os valores para o eixo dos xx igual às classes
)



