#########################################################################
########################## Ficha de Trabalho 2 ##########################
#########################################################################

#######################################################
##################### Exercicio 1 #####################
#######################################################

##############
##### A) #####
##############

anyNA(stroke)
stroke_sem_omissos <- na.omit(stroke)
anyNA(stroke_sem_omissos)

##############
##### B) #####
##############

# Freq. Absolutas:
(ni.sex <- table(stroke_sem_omissos$sex))

# Gráfico de Barras:
dev.new()
barplot(
  ni.sex,                                   # Variável no Gráfico
  main = "Género (Frequências Absolutas)",  # Título do Gráfico
  xlab = "SEX",                             # Título Eixo XX
  ylab = "Frequências Absolutas",           # Título Eixo YY
  col = 1:2,                                # Cores das Barras 
  ylim = c(0, 200),                         # Limites do Eixo YY
  names.arg = c("Masculino", "Feminino")    # Nomes das Barras
)

##############
##### C) #####
##############

# Frequências Absolutas:
(ni.coma <- table(stroke_sem_omissos$coma))

# Frequências Relativas:
(fi.coma <- round(prop.table(ni.coma), 4))

dev.new()
barplot(
  fi.coma,                              # Variável no Gráfico
  main = "Percentagem pacientes em coma (Frequências Absolutas)",  # Título do Gráfico
  xlab = "COMA",                        # Título Eixo XX
  ylab = "Frequências Relativas",           # Título Eixo YY
  col = 1:2,                                # Cores das Barras 
  ylim = c(0, 1),                         # Limites do Eixo YY
  names.arg = c("Não", "Sim")       # Nomes das Barras
)

##############
##### D) #####
##############

### VAR. sex

# Frequências Absolutas:
(ni.sex <- table(stroke_sem_omissos$sex))

# Frequências Relativas:
(fi.sex <- round(prop.table(ni.sex), 4))

# Gráfico:
dev.new()
pie(
  ni.sex,
  labels=paste(fi.sex * 100, "%"),
  col=c("red", "blue"),
  main="Género"
)

legend(
  "topright",                            # Posição da Legenda
  legend = c("Masculino", "Feminino"),   # Valores da Legenda (Nomes)
  fill = c("red", "blue"),               # Cores dos Valores
  cex = 1                                # Tamanho do Objeto (Escala)
)

### VAR. coma

# Frequências Absolutas:
(ni.coma <- table(stroke_sem_omissos$coma))

# Frequências Relativas:
(fi.coma <- round(prop.table(ni.coma), 4))

# Gráfico:
dev.new()
pie(
  ni.coma,
  labels=paste(fi.coma * 100, "%"),
  col=c("red", "blue"),
  main="COMA"
)

legend(
  "topright",                  # Posição da Legenda
  legend = c("Não", "Sim"),    # Valores da Legenda (Nomes)
  fill = c("red", "blue"),     # Cores dos Valores
  cex = 1                      # Tamanho do Objeto (Escala)
)

### VAR. diab

# Frequências Absolutas:
(ni.diab <- table(stroke_sem_omissos$diab))

# Frequências Relativas:
(fi.diab <- round(prop.table(ni.diab), 4))

# Gráfico:
dev.new()
pie(
  ni.diab,
  labels=paste(fi.diab * 100, "%"),
  col=c(1:2),
  main="DIAB"
)

legend(
  "topright",                # Posição da Legenda
  legend = c("Não", "Sim"),  # Valores da Legenda (Nomes)
  fill = c(1:2),             # Cores dos Valores
  cex = 1                    # Tamanho do Objeto (Escala)
)

### VAR. dead

# Frequências Absolutas:
(ni.dead <- table(stroke_sem_omissos$dead))

# Frequências Relativas:
(fi.dead <- round(prop.table(ni.dead), 4))

# Gráfico:
dev.new()
pie(
  ni.dead,
  labels=paste(fi.dead * 100, "%"),
  col=c("red", "blue"),
  main="DEAD"
)

legend(
  "topright",                # Posição da Legenda
  legend = c("Não", "Sim"),  # Valores da Legenda (Nomes)
  fill = c("red", "blue"),   # Cores dos Valores
  cex = 1                    # Tamanho do Objeto (Escala)
)

##############
##### E) #####
##############

dgn_ordenada <- factor(
  stroke_sem_omissos[stroke_sem_omissos$coma == 2,]$dgn,
  levels=c("ICH", "INF", "SAH", "ID")
)

# Freq. Absolutas:
(ni.dgn <- table(dgn_ordenada))

# Gráfico de Barras:
dev.new()
barplot(
  ni.dgn,                                   # Variável no Gráfico
  main = "DGN (Frequências Absolutas)",     # Título do Gráfico
  xlab = "DGN",                             # Título Eixo XX
  ylab = "Frequências Absolutas",           # Título Eixo YY
  col = 1:4,                                # Cores das Barras 
  ylim = c(0, 12),                          # Limites do Eixo YY
  names.arg = names(ni.dgn)                 # Nomes das Barras
)

##############
##### F) #####
##############

dgn_ordenada <- factor(
  stroke_sem_omissos[stroke_sem_omissos$coma == 1,]$dgn,
  levels=c("ICH", "INF", "SAH", "ID")
)

# Freq. Absolutas:
(ni.dgn <- table(dgn_ordenada))

# Gráfico de Barras:
dev.new()
(grafico <- barplot(
  ni.dgn,                                   # Variável no Gráfico
  main = "DGN (Frequências Absolutas)",     # Título do Gráfico
  xlab = "DGN",                             # Título Eixo XX
  ylab = "Frequências Absolutas",           # Título Eixo YY
  col = 1:4,                                # Cores das Barras 
  ylim = c(0, 200),                         # Limites do Eixo YY
  names.arg = names(ni.dgn)                 # Nomes das Barras
))
text(
  x = grafico,          # Gráfico Criado
  y = ni.dgn + 1,       # Posição Y dos Valores (Acima das Barras)
  labels = ni.dgn,      # Valores das Barras
  pos = 3,              # Posição Relativa ao Texto (3 = acima da linha)
  cex = 0.8,            # Tamanho do Texto
  col = "black",        # Cor do Texto
  font = 2              # Estilo do Texto (2 = Negrito)
)

##############
##### G) #####
##############

# OK

##############
##### H) #####
##############

# Regra de Sturgis:

(n <- nrow(stroke_sem_omissos))

# Classes:
(k <- trunc(1 + log(n)/log(2)))

# Amplitude:
(h <- (max(stroke_sem_omissos$age) - min(stroke_sem_omissos$age)) / k)

# Mínimo e Máximo das Classes:
(age.min <- min(stroke_sem_omissos$age))
(age.max <- age.min + h * k)

# Extremos das Classes:
(age.cortes <- seq(age.min, age.max, by = h))

# Intervalos - Abertos à Esquerda e Fechados à Direita:
(age.classes <- cut(
  stroke_sem_omissos$age,
  breaks = age.cortes,
  right = TRUE,
  include.lowest = TRUE
))

# Freq. Absolutas
(ni.age <- table(age.classes))

# Gráfico:
dev.new()
hist(
  stroke_sem_omissos$age,
  breaks = age.cortes,
  right = TRUE,
  include.lowest = TRUE,
  freq = TRUE,
  main = "Histograma da Idade",
  xlab = "AGE",
  ylab = "Frequências Absolutas",
  col = c(1:9),
  xlim = c(age.min, age.max),
  ylim = c(0, 100),
  xaxt = "n"  # Para Poder Definir o Eixo XX de Seguida
)

# Valores do Eixo XX:
axis(
  side = 1,
  at = c(
    round(age.min, 2),
    round(age.cortes, 2),
    round(age.max, 2)
  )
)

##############
##### I) #####
##############

(nao_morreram_dia_avc <- stroke_sem_omissos[stroke_sem_omissos$obsmonths != 0.1,])

# Regra de Sturgis:

(n <- nrow(nao_morreram_dia_avc))

# Classes:
(k <- trunc(1 + log(n)/log(2)))

# Amplitude:
(h <- (max(nao_morreram_dia_avc$obsmonths) - min(nao_morreram_dia_avc$obsmonths)) / k)

# Mínimo e Máximo das Classes:
(obsmonths.min <- min(nao_morreram_dia_avc$obsmonths))
(obsmonths.max <- obsmonths.min + h * k)

# Extremos das Classes:
(obsmonths.cortes <- seq(obsmonths.min, obsmonths.max, by = h))

# Intervalos - Abertos à Esquerda e Fechados à Direita:
(obsmonths.classes <- cut(
  nao_morreram_dia_avc$obsmonths,
  breaks = obsmonths.cortes,
  right = FALSE,
  include.lowest = TRUE
))

# Freq. Absolutas
(ni.obsmonths <- table(obsmonths.classes))

# Gráfico:
dev.new()
hist(
  nao_morreram_dia_avc$obsmonths,
  breaks = obsmonths.cortes,
  right = FALSE,
  include.lowest = TRUE,
  freq = TRUE,
  main = "Histograma da OBSMONTHS",
  xlab = "OBSMONTHS",
  ylab = "Frequências Absolutas",
  col = c(1:9),
  xlim = c(obsmonths.min, obsmonths.max),
  ylim = c(0, 120),
  xaxt = "n"  # Para Poder Definir o Eixo XX de Seguida
)

# Valores do Eixo XX:
axis(
  side = 1,
  at = c(
    round(obsmonths.min, 2),
    round(obsmonths.cortes, 2),
    round(obsmonths.max, 2)
  )
)

#######################################################
##################### Exercicio 2 #####################
#######################################################

##############
##### A) #####
##############

### VAR. FCVC

# Freq. Absolutas:
(ni.FCVC <- table(obesidade$FCVC))

# Gráfico de Barras:
dev.new()
text(
  x = barplot(
    ni.FCVC,                                     # Variável no Gráfico
    main = "FCVC (Frequências Absolutas)",       # Título do Gráfico
    xlab = "FCVC",                               # Título Eixo XX
    ylab = "Frequências Absolutas",              # Título Eixo YY
    col = 1:3,                                   # Cores das Barras 
    ylim = c(0, 1200),                           # Limites do Eixo YY
    names.arg = c("Nunca", "Às Vezes", "Sempre") # Nomes das Barras
  ),          # Gráfico Criado
  y = ni.FCVC + 1,  # Posição Y dos Valores (Acima das Barras)
  labels = ni.FCVC, # Valores das Barras
  pos = 3,          # Posição Relativa ao Texto (3 = acima da linha)
  cex = 0.8,        # Tamanho do Texto
  col = "black",    # Cor do Texto
  font = 2          # Estilo do Texto (2 = Negrito)
)

### VAR. CAEC

# Freq. Absolutas:
(ni.CAEC <- table(obesidade$CAEC))

# Gráfico de Barras:
dev.new()
text(
  x = barplot(
    ni.CAEC,                                                     # Variável no Gráfico
    main = "CAEC (Frequências Absolutas)",                       # Título do Gráfico
    xlab = "CAEC",                                               # Título Eixo XX
    ylab = "Frequências Absolutas",                              # Título Eixo YY
    col = 1:3,                                                   # Cores das Barras 
    ylim = c(0, 2000),                                           # Limites do Eixo YY
    names.arg = c("Sempre", "Frequentemente", "Não", "Às Vezes") # Nomes das Barras
  ),          # Gráfico Criado
  y = ni.CAEC + 1,  # Posição Y dos Valores (Acima das Barras)
  labels = ni.CAEC, # Valores das Barras
  pos = 3,          # Posição Relativa ao Texto (3 = acima da linha)
  cex = 0.8,        # Tamanho do Texto
  col = "black",    # Cor do Texto
  font = 2          # Estilo do Texto (2 = Negrito)
)

### VAR. CH20

# Freq. Absolutas:
(ni.CH2O <- table(obesidade$CH2O))

# Gráfico de Barras:
dev.new()
text(
  x = barplot(
    ni.CH2O,                                     # Variável no Gráfico
    main = "CH2O (Frequências Absolutas)",       # Título do Gráfico
    xlab = "CH2O",                               # Título Eixo XX
    ylab = "Frequências Absolutas",              # Título Eixo YY
    col = 1:3,                                   # Cores das Barras 
    ylim = c(0, 1200),                           # Limites do Eixo YY
    names.arg = c("Menos de 1 Litro", "Entre 1 e 2 Litros", "Mais de 2 Litros") # Nomes das Barras
  ),          # Gráfico Criado
  y = ni.CH2O + 1,  # Posição Y dos Valores (Acima das Barras)
  labels = ni.CH2O, # Valores das Barras
  pos = 3,          # Posição Relativa ao Texto (3 = acima da linha)
  cex = 0.8,        # Tamanho do Texto
  col = "black",    # Cor do Texto
  font = 2          # Estilo do Texto (2 = Negrito)
)

### VAR. FAF

# Freq. Absolutas:
(ni.FAF <- table(obesidade$FAF))

# Gráfico de Barras:
dev.new()
text(
  x = barplot(
    ni.FAF,                                     # Variável no Gráfico
    main = "FAF (Frequências Absolutas)",       # Título do Gráfico
    xlab = "FAF",                               # Título Eixo XX
    ylab = "Frequências Absolutas",             # Título Eixo YY
    col = 1:3,                                  # Cores das Barras 
    ylim = c(0, 1000),                          # Limites do Eixo YY
    names.arg = c("Não Pratica", "1 ou 2 Dias", "3 ou 4 Dias", "Mais de 4 Dias") # Nomes das Barras
  ),          # Gráfico Criado
  y = ni.FAF + 1,  # Posição Y dos Valores (Acima das Barras)
  labels = ni.FAF, # Valores das Barras
  pos = 3,          # Posição Relativa ao Texto (3 = acima da linha)
  cex = 0.8,        # Tamanho do Texto
  col = "black",    # Cor do Texto
  font = 2          # Estilo do Texto (2 = Negrito)
)

### VAR. CALC

# Freq. Absolutas:
(ni.CALC <- table(obesidade$CALC))

# Gráfico de Barras:
dev.new()
text(
  x = barplot(
    ni.CALC,                                     # Variável no Gráfico
    main = "CALC (Frequências Absolutas)",       # Título do Gráfico
    xlab = "CALC",                               # Título Eixo XX
    ylab = "Frequências Absolutas",              # Título Eixo YY
    col = 1:3,                                   # Cores das Barras 
    ylim = c(0, 1600),                           # Limites do Eixo YY
    names.arg = c("Sempre", "Frequentemente", "Nunca", "Às Vezes") # Nomes das Barras
  ),          # Gráfico Criado
  y = ni.CALC + 1,  # Posição Y dos Valores (Acima das Barras)
  labels = ni.CALC, # Valores das Barras
  pos = 3,          # Posição Relativa ao Texto (3 = acima da linha)
  cex = 0.8,        # Tamanho do Texto
  col = "black",    # Cor do Texto
  font = 2          # Estilo do Texto (2 = Negrito)
)

##############
##### B) #####
##############

### Género

# Frequências Absolutas:
(ni.Genero <- table(obesidade$Genero))

# Frequências Relativas:
(fi.Genero <- round(prop.table(ni.Genero), 4))

# Gráfico:
dev.new()
pie(
  ni.Genero,
  labels=paste(fi.Genero * 100, "%"),
  col=c("red", "blue"),
  main="Género"
)

legend(
  "topright",                # Posição da Legenda
  legend = names(ni.Genero), # Valores da Legenda (Nomes)
  fill = c("red", "blue"),   # Cores dos Valores
  cex = 1                    # Tamanho do Objeto (Escala)
)

### FAVC

# Frequências Absolutas:
(ni.FAVC <- table(obesidade$FAVC))

# Frequências Relativas:
(fi.FAVC <- round(prop.table(ni.FAVC), 4))

# Gráfico:
dev.new()
pie(
  ni.FAVC,
  labels=paste(fi.FAVC * 100, "%"),
  col=c("red", "blue"),
  main="FAVC"
)

legend(
  "topright",                # Posição da Legenda
  legend = c("Não", "Sim"),  # Valores da Legenda (Nomes)
  fill = c("red", "blue"),   # Cores dos Valores
  cex = 1                    # Tamanho do Objeto (Escala)
)

### Fumar

# Frequências Absolutas:
(ni.Fumar <- table(obesidade$Fumar))

# Frequências Relativas:
(fi.Fumar <- round(prop.table(ni.Fumar), 4))

# Gráfico:
dev.new()
pie(
  ni.Fumar,
  labels=paste(fi.Fumar * 100, "%"),
  col=c("red", "blue"),
  main="Fumar"
)

legend(
  "topright",                # Posição da Legenda
  legend = c("Não", "Sim"),  # Valores da Legenda (Nomes)
  fill = c("red", "blue"),   # Cores dos Valores
  cex = 1                    # Tamanho do Objeto (Escala)
)

### MTRANS

# Frequências Absolutas:
(ni.MTRANS <- table(obesidade$MTRANS))

# Frequências Relativas:
(fi.MTRANS <- round(prop.table(ni.MTRANS), 4))

# Gráfico:
dev.new()
pie(
  ni.MTRANS,
  labels=paste(fi.MTRANS * 100, "%"),
  col=1:5,
  main="MTRANS"
)

legend(
  "topright",                 # Posição da Legenda
  legend = names(ni.MTRANS),  # Valores da Legenda (Nomes)
  fill = 1:5,                 # Cores dos Valores
  cex = 1                     # Tamanho do Objeto (Escala)
)

##############
##### C) #####
##############

### VAR. NCP

# Frequências Absolutas:
(ni.NCP <- table(obesidade$NCP))

# Frequências Relativas:
(fi.NCP <- round(prop.table(ni.NCP), 4))

barplot(
  fi.NCP,                                   # Variável no Gráfico
  main = "NCP - Nº de Refeições Principais (Frequências Relativas)", # Título do Gráfico
  xlab = "NCP",                             # Título Eixo XX
  ylab = "Frequências Relativas",           # Título Eixo YY
  col = 1:2,                                # Cores das Barras 
  ylim = c(0, 0.8),                         # Limites do Eixo YY
  names.arg = names(ni.NCP)                 # Nomes das Barras
)

##############
##### D) #####
##############

##########
### i) ###
##########

### VAR. idade

# Classes - Pelo Enunciado:
(k <- 8)

# Amplitude - Pelo Enunciado:
(h <- 6)

# Mínimo e Máximo das Classes:
(idade.min <- min(obesidade$Idade))
(idade.max <- idade.min + h * k)

# Extremos das Classes:
(idade.cortes <- seq(idade.min, idade.max, by = h))

# Intervalos - Abertos à Esquerda e Fechados à Direita:
(idade.classes <- cut(
  obesidade$Idade,
  breaks = idade.cortes,
  right = TRUE,
  include.lowest = TRUE
))

# Freq. Absolutas
(ni.idade <- table(idade.classes))

# Gráfico:
dev.new
hist(
  obesidade$Idade,
  breaks = idade.cortes,
  right = TRUE,
  include.lowest = TRUE,
  freq = TRUE,
  main = "Histograma",
  xlab = "Idade",
  ylab = "Frequências Absolutas",
  col = 1:8,
  xlim = c(idade.min, idade.max),
  ylim = c(0, 1200),
  xaxt = "n"  # Para Poder Definir o Eixo XX de Seguida
)

# Valores do Eixo XX:
axis(
  side = 1,
  at = c(
    round(idade.min, 2),
    round(idade.cortes, 2),
    round(idade.max, 2)
  )
)

###########
### ii) ###
###########

### VAR. altura

# Extremos das Classes:
(altura.cortes <- c(1.45, 1.60, 1.80, 2.00))

# Intervalos - Abertos à Esquerda e Fechados à Direita:
(altura.classes <- cut(
  obesidade$Altura,
  breaks = altura.cortes,
  right = TRUE,
  include.lowest = TRUE
))

# Freq. Absolutas
(ni.altura <- table(altura.classes))

# Gráfico:
dev.new()
hist(
  obesidade$Altura,
  breaks = altura.cortes,
  right = TRUE,
  include.lowest = TRUE,
  freq = TRUE,
  main = "Histograma",
  xlab = "Altura",
  ylab = "Frequências Absolutas",
  col = 1:3,
  xlim = c(min(obesidade$Altura), max(obesidade$Altura)),
  ylim = c(0, 1500),
  xaxt = "n"  # Para Poder Definir o Eixo XX de Seguida
)

# Valores do Eixo XX:
axis(
  side = 1,
  at = c(
    round(min(obesidade$Altura), 2),
    round(altura.cortes, 2),
    round(max(obesidade$Altura), 2)
  )
)

############
### iii) ###
############

### VAR. peso

# Regra de Sturgis:

(n <- nrow(obesidade))

# Classes:
(k <- trunc(1 + log(n)/log(2)))

# Amplitude:
(h <- (max(obesidade$Peso) - min(obesidade$Peso)) / k)

# Mínimo e Máximo das Classes:
(peso.min <- min(obesidade$Peso))
(peso.max <- peso.min + h * k)

# Extremos das Classes:
(peso.cortes <- seq(peso.min, peso.max, by = h))

# Intervalos - Abertos à Esquerda e Fechados à Direita:
(peso.classes <- cut(
  obesidade$Peso,
  breaks = peso.cortes,
  right = TRUE,
  include.lowest = TRUE
))

# Freq. Absolutas
(ni.peso <- table(peso.classes))

# Gráfico:
dev.new()
hist(
  obesidade$Peso,
  breaks = peso.cortes,
  right = TRUE,
  include.lowest = TRUE,
  freq = TRUE,
  main = "Histograma",
  xlab = "Peso",
  ylab = "Frequências Absolutas",
  col = 1:12,
  xlim = c(peso.min, peso.max),
  ylim = c(0, 500),
  xaxt = "n"  # Para Poder Definir o Eixo XX de Seguida
)

# Valores do Eixo XX:
axis(
  side = 1,
  at = c(
    round(peso.min, 2),
    round(peso.cortes, 2),
    round(peso.max, 2)
  )
)
