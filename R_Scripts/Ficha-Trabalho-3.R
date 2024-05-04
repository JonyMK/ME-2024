#########################################################################
########################## Ficha de Trabalho 3 ##########################
#########################################################################

#######################################################
##################### Exercicio 1 #####################
#######################################################

##############
##### A) #####
##############

(engenheiros <- c(rep("Engenheiro", 32)))
(professores <- c(rep("Professor", 20)))
(analistas <- c(rep("Analista de Dados", 16)))
(alunos <- c(rep("Aluno", 12)))

(seminario <- c(engenheiros, professores, analistas, alunos))

##############
##### B) #####
##############

# Tab. Frequências
(ni.seminario <- table(seminario))                   # Freq. Absolutas
(fi.seminario <- round(prop.table(ni.seminario), 4)) # Freq. Relativas
(Ni.seminario <- cumsum(ni.seminario))               # Freq. Absolutas Acumuladas
(Fi.seminario <- round(cumsum(fi.seminario), 4))     # Freq. Relativas Acumuladas
(obesidade.frequencias.seminario <- data.frame(
  i = 1:nrow(ni.seminario),
  xi = names(ni.seminario),
  ni = as.integer(ni.seminario),
  fi = as.numeric(fi.seminario),
  Ni = as.integer(Ni.seminario),
  Fi = as.numeric(Fi.seminario)
))

# Gráfico
text(
  x = barplot(                                # Gráfico Criado
    ni.seminario,                             # Variável no Gráfico
    main = "Presenças no Seminário",          # Título do Gráfico
    xlab = "Tipo de Presença",                # Título Eixo XX
    ylab = "Frequências Absolutas",           # Título Eixo YY
    col = 1:4,                                # Cores das Barras 
    ylim = c(0, 40),                          # Limites do Eixo YY
    names.arg = names(ni.seminario)           # Nomes das Barras
  ),
  y = ni.seminario + 1,  # Posição Y dos Valores (Acima das Barras)
  labels = ni.seminario, # Valores das Barras
  pos = 3,               # Posição Relativa ao Texto (3 = acima da linha)
  cex = 0.8,             # Tamanho do Texto
  col = "black",         # Cor do Texto
  font = 2               # Estilo do Texto (2 = Negrito)
)

# Medidas
# Dado tratar-se de variáveis qualitativas, apenas se calcula a moda:
if(min(ni.seminario)==max(ni.seminario)){
  print("Amodal")
} else{
  print(paste("Moda:", names(ni.seminario)[ni.seminario==max(ni.seminario)]))
}

#######################################################
##################### Exercicio 2 #####################
#######################################################

##############
##### A) #####
##############

# VAR. Qualitativa Nominal - Género

(ni.Genero <- table(obesidade$Genero))

# Como a var. é qualitativa, só se calcula a moda.

if(min(ni.Genero)==max(ni.Genero)){
  print("Amodal")
} else{
  print(paste("Moda:", names(ni.Genero)[ni.Genero==max(ni.Genero)]))
}

# VAR. Qualitativa Ordinal - FCVC

(ni.FCVC <- table(obesidade$FCVC))

# Como a var. é qualitativa, só se calcula a moda.

if(min(ni.FCVC)==max(ni.FCVC)){
  print("Amodal")
} else{
  print(paste("Moda:", names(ni.FCVC)[ni.FCVC==max(ni.FCVC)]))
}

# VAR. Quantitativa Discreta - NCP

(ni.NCP <- table(obesidade$NCP))

### i)

# Moda:
if(min(ni.NCP)==max(ni.NCP)){
  print("Amodal")
} else{
  print(paste("Moda:", names(ni.NCP)[ni.NCP==max(ni.NCP)]))
}

# Média:
mean(obesidade$NCP)

# Mediana:
median(obesidade$NCP, type = 2)

### ii)

quantile(
  obesidade$NCP,
  prob = c(0.25, 0.50, 0.75),
  type = 2
)

### iii)

quantile(
  obesidade$NCP,
  prob = c(0.9),
  type = 2
)

### iv)

quantile(
  obesidade$NCP,
  prob = c(0.03),
  type = 2
)

### v)

min(obesidade$NCP)
max(obesidade$NCP)

### vi)

# Amplitude Total:
(max(obesidade$NCP)-min(obesidade$NCP))

# Amplitude Interquartis:
IQR(obesidade$NCP, type=2)

# Variância:
var(obesidade$NCP)

# Desvio Padrão:
sd(obesidade$NCP)

### vii)

# Coeficiente de variação = CV:
((sd(obesidade$NCP)/mean(obesidade$NCP))*100) # %

### viii)

library(e1071)
e1071::skewness(obesidade$NCP)

# VAR. Quantitativa Contínua - Idade

(ni.Idade <- table(obesidade$Idade))

### i)

# Moda:
if(min(ni.Idade)==max(ni.Idade)){
  print("Amodal")
} else{
  print(paste("Moda:", names(ni.Idade)[ni.Idade==max(ni.Idade)]))
}

# Média:
mean(obesidade$Idade)

# Mediana:
median(obesidade$Idade, type = 2)

### ii)

quantile(
  obesidade$Idade,
  prob = c(0.25, 0.50, 0.75),
  type = 2
)

### iii)

quantile(
  obesidade$Idade,
  prob = c(0.9),
  type = 2
)

### iv)

quantile(
  obesidade$Idade,
  prob = c(0.03),
  type = 2
)

### v)

min(obesidade$Idade)
max(obesidade$Idade)

### vi)

# Amplitude Total:
(max(obesidade$Idade)-min(obesidade$Idade))

# Amplitude Interquartis:
IQR(obesidade$Idade, type=2)

# Variância:
var(obesidade$Idade)

# Desvio Padrão:
sd(obesidade$Idade)

### vii)

# Coeficiente de variação = CV:
((sd(obesidade$Idade)/mean(obesidade$Idade))*100) # %

### viii)

library(e1071)
e1071::skewness(obesidade$Idade)

##############
##### B) #####
##############


### VAR. NCP

dev.new()

# Sem Indicação de Outliers:
boxplot(
  obesidade$NCP,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Sem Outliers",
  xlab = "NCP",
  type = 2,
  range = 0
)

# Outliers A Partir dos Moderados:
boxplot(
  obesidade$NCP,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Outliers a Partir dos Moderados",
  xlab = "NCP",
  type = 2,
  range = 1.5
)

# Outliers a Partir dos Severos:
boxplot(
  obesidade$NCP,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Outliers a Partir dos Severos",
  xlab = "NCP",
  type = 2,
  range = 3
)

### VAR. Idade

# Sem Indicação de Outliers:
boxplot(
  obesidade$Idade,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Sem Outliers",
  xlab = "Idade",
  type = 2,
  range = 0
)

# Outliers A Partir dos Moderados:
boxplot(
  obesidade$Idade,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Outliers a Partir dos Moderados",
  xlab = "Idade",
  type = 2,
  range = 1.5
)

# Outliers a Partir dos Severos:
boxplot(
  obesidade$Idade,
  col = "gold",
  horizontal = TRUE,
  main = "Extremos e Quartis - Outliers a Partir dos Severos",
  xlab = "Idade",
  type = 2,
  range = 3
)

##############
##### C) #####
##############

# VAR. Idade

### i)

# Simetria pelas Medidas de Localição Central:
## Média = Mediana => Simétrica
## Média > Mediana => Assimétrica Positiva (à Direita)
## Média < Mediana => Assimétrica Negativa (à Esquerda)

# Média:
mean(obesidade$Idade) # = 24.3126

# Mediana:
median(obesidade$Idade, type = 2) # = 22.7779

# Como 24.3 > 22.8, a V. Idade é Assimétrica à Direita (Positiva)

### ii)

e1071::skewness(obesidade$Idade) # = 1.5269
# 1.5269 > 0 => Idade Assimétrica à Direita (Positiva)

##############
##### D) #####
##############

# NF

##############
##### E) #####
##############

# NF

##############
##### F) #####
##############

### Var. Idade

# 8 Classes
(k.idade <- 8)
# Amplitude = 6
(h.idade <- 6)

# Mínimo e Máximo das Classes
(idade.min <- min(obesidade$Idade))
(idade.max <- idade.min + h.idade * k.idade)

# Extremos das Classes
(cortes.idade <- seq(idade.min, idade.max, by = h.idade))

# Intervalos: Abertos à Esquerda e Fechados à Direita
(classes.idade <- cut(
  obesidade$Idade,
  breaks = cortes.idade,
  right = TRUE,
  include.lowest = TRUE
))

# Tabela de Frequências com Classes
(ni.idade <- table(classes.idade))
(fi.idade <- round(prop.table(ni.idade), 4))
(Ni.idade <- cumsum(ni.idade))
(Fi.idade <- round(cumsum(fi.idade), 4))

(tabela.frequencias.idade <- data.frame(
  i=1:nrow(ni.idade),
  xi=names(ni.idade),
  ni=as.integer(ni.idade),
  fi=as.numeric(fi.idade),
  Ni=as.integer(Ni.idade),
  Fi=as.numeric(Fi.idade)
))

### Var. Altura

# Extremos das Classes
(cortes.altura <- c(1.45, 1.60, 1.80, 2.00))

# Intervalos: Abertos à Esquerda e Fechados à Direita
(classes.altura <- cut(
  obesidade$Altura,
  breaks = cortes.altura,
  right = TRUE,
  include.lowest = TRUE
))

# Tabela de Frequências com Classes
(ni.altura <- table(classes.altura))
(fi.altura <- round(prop.table(ni.altura), 4))
(Ni.altura <- cumsum(ni.altura))
(Fi.altura <- round(cumsum(fi.altura), 4))

(tabela.frequencias.altura <- data.frame(
  i=1:nrow(ni.altura),
  xi=names(ni.altura),
  ni=as.integer(ni.altura),
  fi=as.numeric(fi.altura),
  Ni=as.integer(Ni.altura),
  Fi=as.numeric(Fi.altura)
))

### Var. Peso

# Sturges:
(n.peso <- nrow(obesidade))
# Nº de Classes:
(k.peso <- trunc(1 + log(n.peso)/log(2)))
# Amplitude das Classes:
(h.peso <- (max(obesidade$Peso) - min(obesidade$Peso)) / k.peso)

# Mínimo e Máximo das Classes:
(peso.min <- min(obesidade$Peso))
(peso.max <- peso.min + h.peso * k.peso)

# Extremos das Classes
(cortes.peso <- seq(peso.min, peso.max, by = h.peso))

# Intervalos: Fechados à Esquerda e Abertos à Direita
(classes.peso <- cut(
  obesidade$Peso,
  breaks = cortes.peso,
  right = FALSE,
  include.lowest = TRUE
))

# Tabela de Frequências com Classes
(ni.peso <- table(classes.peso))
(fi.peso <- round(prop.table(ni.peso), 4))
(Ni.peso <- cumsum(ni.peso))
(Fi.peso <- round(cumsum(fi.peso), 4))

(tabela.frequencias.peso <- data.frame(
  i=1:nrow(ni.peso),
  xi=names(ni.peso),
  ni=as.integer(ni.peso),
  fi=as.numeric(fi.peso),
  Ni=as.integer(Ni.peso),
  Fi=as.numeric(Fi.peso)
))

### Classe Modal:

# VAR. Idade:

if(min(ni.idade)==max(ni.idade)){
  print("Amodal")
} else{
  print(paste("Moda:", names(ni.idade)[ni.idade==max(ni.idade)]))
}
# OU
# Moda: ]20,26] => Observado na Tabela de Frequências

# VAR. Altura:

if(min(ni.altura)==max(ni.altura)){
  print("Amodal")
} else{
  print(paste("Moda:", names(ni.altura)[ni.altura==max(ni.altura)]))
}
# OU
# Moda: ]1.6,1.8] => Observado na Tabela de Frequências

# VAR. Peso:

if(min(ni.peso)==max(ni.peso)){
  print("Amodal")
} else{
  print(paste("Moda:", names(ni.peso)[ni.peso==max(ni.peso)]))
}
# OU
# Moda: [72.5,83.7[ => Observado na Tabela de Frequências

### Classes dos Quartis:

# VAR. Idade:

# Q1 => Classe [14, 20] => 1ª a Passar 0.25 na Tab. de Frequências
# Q2 => Classe ]20, 26] => 1ª a Passar 0.50 na Tab. de Frequências
# Q3 => Classe ]20, 26] => 1ª a Passar 0.75 na Tab. de Frequências

# VAR. Altura:

# Q1 => Classe ]1.6, 1.8] => 1ª a Passar 0.25 na Tab. de Frequências
# Q2 => Classe ]1.6, 1.8] => 1ª a Passar 0.50 na Tab. de Frequências
# Q3 => Classe ]1.6, 1.8] => 1ª a Passar 0.75 na Tab. de Frequências

# VAR. Peso:

# Q1 => Classe [61.3, 72.5[ => 1ª a Passar 0.25 na Tab. de Frequências
# Q2 => Classe [72.5, 83.7[ => 1ª a Passar 0.50 na Tab. de Frequências
# Q3 => Classe [106, 117[   => 1ª a Passar 0.75 na Tab. de Frequências
