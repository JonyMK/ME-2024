#########################################################################
########################## Ficha de Trabalho 1 ##########################
#########################################################################

#######################################################
##################### Exercicio 2 #####################
#######################################################

##############
##### A) #####
##############

# População => Hotéis da Europa
# Amostra => Hotéis da Europa Listados na Tabela
# Unidade Estatística => Hotéis da Europa
# Variáveis e Dados Estatísticos:
### Var. Nome => V. Qualitativa Nominal => Nomes Escritos
### Var. País => V. Qualitativa Nominal =>  Nomes Escritos
### Var. Preço => V. Qualitativa Ordinal => [ $, $$, $$$, $$$$]
### Var. NumeroQuartos => V. Quantitativa Discreta => 1, 2 ... (Contagens Nºs Inteiros)
### Var. Pontuação => V. Quantitativa Contínua => 8.4, 7.0 ... (Medições Números Reais)

##############
##### B) #####
##############

(tabela.ex2 <- data.frame(
  nome = c(
    "Hotel Ronda", "Villad'Este", "Hotel Lisboa",
    "Hotel Prem", "Hotel d'Europa", "Palace Luzern",
    "Hotel Palace", "Hotel Arts", "Hotel Sacher",
    "Duc de Bourgogne", "Villa Gallice", "Hotel Vila"
  ),
  pais = c(
    "Espanha", "Itália", "Portugal", "Alemanha", "França", "França",
    "Portugal", "Espanha", "Alemanha", "França", "França", "Portugal"
  ),
  preco = c(
    "$$", "$$$$", "$", "$", "$$", "$$", "$$$$", "$$$", "$$$", "$", "$$", "$$"
  ),
  numero.quartos = c(
    18, 166, 81, 54, 47, 326, 185, 45, 120, 10, 22, 233
  ),
  pontuacao = c(
    8.4, 8.6, 8.5, 7.7, 7.6, 8.1, 9.5, 7.3, 8.5, 7.6, 9.0, 9.1
  )
))

##############
##### C) #####
##############

nrow(tabela.ex2)

# Dimensão => 12 Hotéis na Amostra.

##############
##### D) #####
##############

(DescTools::Freq(tabela.ex2$nome))
(DescTools::Freq(tabela.ex2$pais))
(DescTools::Freq(tabela.ex2$preco))

##############
##### E) #####
##############

nrow(tabela.ex2[tabela.ex2$pais=="Espanha"|tabela.ex2$pais=="Portugal",])

# Existem 5 hotéis na península ibérica.

##############
##### F) #####
##############

max(tabela.ex2$preco)
(DescTools::Freq(tabela.ex2$preco)[DescTools::Freq(tabela.ex2$preco)$level==max(tabela.ex2$preco),])

# Da amostra dada, 16.7% dos hotéis são os que têm o preço mais elevado ($$$$).

#######################################################
##################### Exercicio 3 #####################
#######################################################

##############
##### A) #####
##############

# População: Casos de Acidente Vascular Cerebral na Estónia, entre 1991 e 1993.
# Unidade Estatística: Pacientes / AVCs
# Variáveis Estatísticas:
### Var. Sex
### Var. Age
### Var. dgn
### Var. coma
### Var. favc
### Var. dead
### Var. obsmonths

##############
##### B) #####
##############

# Dimensão da População: Todos os casos existentes naquele periodo (829 casos).
nrow(stroke) # = 285
# Dimensão da Amostra: Todos os casos recolhidos no ficheiro "stroke.txt" (285 casos).

##############
##### C) #####
##############

# Var. Sex => V. Qualitativa Nominal
# Var. Age => V. Quantitativa Discreta
# Var. dgn => V. Qualitativa Ordinal
# Var. coma => V. Qualitativa Nominal
# Var. favc => V. Qualitativa Nominal
# Var. dead => V. Qualitativa Nominal
# Var. obsmonths => V. Quantitativa Contínua

##############
##### D) #####
##############

### Var. Sex

(ni.sex <- table(stroke$sex))
(fi.sex <- round(prop.table(ni.sex), 4))
(Ni.sex <- cumsum(ni.sex))
(Fi.sex <- round(cumsum(fi.sex), 4))

(tabela.frequencias.sex <- data.frame(
  i = 1:nrow(ni.sex),
  xi = c("Masculino", "Feminino"),
  ni = as.integer(ni.sex),
  fi = as.numeric(fi.sex),
  Ni = as.integer(Ni.sex),
  Fi = as.numeric(Fi.sex)
))

### Var. dgn

(ni.dgn <- table(stroke$dgn))
(fi.dgn <- round(prop.table(ni.dgn), 4))
(Ni.dgn <- cumsum(ni.dgn))
(Fi.dgn <- round(cumsum(fi.dgn), 4))

(tabela.frequencias.dgn <- data.frame(
  i = 1:nrow(ni.dgn),
  xi = names(ni.dgn),
  ni = as.integer(ni.dgn),
  fi = as.numeric(fi.dgn),
  Ni = as.integer(Ni.dgn),
  Fi = as.numeric(Fi.dgn)
))

### Var. coma

(ni.coma <- table(stroke$coma))
(fi.coma <- round(prop.table(ni.coma), 4))
(Ni.coma <- cumsum(ni.coma))
(Fi.coma <- round(cumsum(fi.coma), 4))

(tabela.frequencias.coma <- data.frame(
  i = 1:nrow(ni.coma),
  xi = c("Não", "Sim"),
  ni = as.integer(ni.coma),
  fi = as.numeric(fi.coma),
  Ni = as.integer(Ni.coma),
  Fi = as.numeric(Fi.coma)
))

### Var. favc

(ni.favc <- table(stroke$favc))
(fi.favc <- round(prop.table(ni.favc), 4))
(Ni.favc <- cumsum(ni.favc))
(Fi.favc <- round(cumsum(fi.favc), 4))

(tabela.frequencias.favc <- data.frame(
  i = 1:nrow(ni.favc),
  xi = names(ni.favc),
  ni = as.integer(ni.favc),
  fi = as.numeric(fi.favc),
  Ni = as.integer(Ni.favc),
  Fi = as.numeric(Fi.favc)
))

### Var. dead

(ni.dead <- table(stroke$dead))
(fi.dead <- round(prop.table(ni.dead), 4))
(Ni.dead <- cumsum(ni.dead))
(Fi.dead <- round(cumsum(fi.dead), 4))

(tabela.frequencias.dead <- data.frame(
  i = 1:nrow(ni.dead),
  xi = names(ni.dead),
  ni = as.integer(ni.dead),
  fi = as.numeric(fi.dead),
  Ni = as.integer(Ni.dead),
  Fi = as.numeric(Fi.dead)
))

##############
##### E) #####
##############

tabela.frequencias.dgn[tabela.frequencias.dgn$xi == "ID", "ni"] # = 70

# Existem 70 pacientes que não tiveram o seu diagnóstico identificado.

##############
##### F) #####
##############

tabela.frequencias.dead[tabela.frequencias.dead$xi == "TRUE", "fi"] * 100 # = 55.44

# A percentagem de pacientes que morreu durante o estudo é de 55.44%.

##############
##### G) #####
##############

nrow(na.omit(stroke[stroke$coma == 2 & stroke$favc == "Yes",])) # = 2

# Existem 2 pacientes que entraram em coma já tendo um histórico de favcetes.

##############
##### H) #####
##############

nrow(na.omit(stroke[stroke$obsmonths == 0.1,]))

# Existem 8 pacientes que morreram no próprio dia do AVC.

#######################################################
##################### Exercicio 4 #####################
#######################################################

##############
##### A) #####
##############

# Var. Género => V. Qualitativa Nominal
# Var. Idade  => V. Quantitativa Contínua
# Var. Altura => V. Quantitativa Contínua
# Var. Peso   => V. Quantitativa Contínua
# Var. FAVC   => V. Qualitativa Nominal
# Var. FCVC   => V. Qualitativa Ordinal
# Var. NCP    => V. Quantitativa Discreta
# Var. CAEC   => V. Qualitativa Ordinal
# Var. Fumar  => V. Qualitativa Nominal
# Var. CH2O   => V. Qualitativa Ordinal
# Var. FAF    => V. Qualitativa Ordinal
# Var. CALC   => V. Qualitativa Ordinal
# Var. MTRANS => V. Qualitativa Nominal

##############
##### B) #####
##############

nrow(obesidade) # = 2111
# A amostra tem uma dimensão de 2111 registos.

##############
##### C) #####
##############

### Var. FAVC

(ni.favc <- table(obesidade$FAVC))
(fi.favc <- round(prop.table(ni.favc), 4))
(Ni.favc <- cumsum(ni.favc))
(Fi.favc <- round(cumsum(fi.favc), 4))

(tabela.frequencias.favc <- data.frame(
  i = 1:nrow(ni.favc),
  xi = names(ni.favc),
  ni = as.integer(ni.favc),
  fi = as.numeric(fi.favc),
  Ni = as.integer(Ni.favc),
  Fi = as.numeric(Fi.favc)
))

tabela.frequencias.favc[tabela.frequencias.favc$xi == 1, "fi"]*100 # = 88.39%

# A percentagem de pessoas inquiridas que come alimentos altamente calóricos habitualmente é de 88.39%.

##############
##### D) #####
##############

### Var. CAEC

(ni.caec <- table(obesidade$CAEC))
(fi.caec <- round(prop.table(ni.caec), 4))
(Ni.caec <- cumsum(ni.caec))
(Fi.caec <- round(cumsum(fi.caec), 4))

(tabela.frequencias.caec <- data.frame(
  i = 1:nrow(ni.caec),
  xi = names(ni.caec),
  ni = as.integer(ni.caec),
  fi = as.numeric(fi.caec),
  Ni = as.integer(Ni.caec),
  Fi = as.numeric(Fi.caec)
))

sum(tabela.frequencias.caec[tabela.frequencias.caec$xi != "N", "fi"])*100 # = 97.58%

## Sem tabela de frequências

round((nrow(obesidade[obesidade$CAEC != "N",]) / nrow(obesidade) * 100), 2) # = 97.58%

# A percentagem de pessoas inquiridas que come entre as refeições principais é de 97.58%.

##############
##### E) #####
##############

### Var. MTRANS

(ni.mtrans <- table(obesidade$MTRANS))
(fi.mtrans <- round(prop.table(ni.mtrans), 4))
(Ni.mtrans <- cumsum(ni.mtrans))
(Fi.mtrans <- round(cumsum(fi.mtrans), 4))

(tabela.frequencias.mtrans <- data.frame(
  i = 1:nrow(ni.mtrans),
  xi = names(ni.mtrans),
  ni = as.integer(ni.mtrans),
  fi = as.numeric(fi.mtrans),
  Ni = as.integer(Ni.mtrans),
  Fi = as.numeric(Fi.mtrans)
))

sum(tabela.frequencias.mtrans[
  tabela.frequencias.mtrans$xi == "Bicicleta" | tabela.frequencias.mtrans$xi == "Transportes_Publicos", "fi"])*100 # = 75.18%

# Sem tabela de frequências

round((nrow(obesidade[obesidade$MTRANS %in% c("Bicicleta", "Transportes_Publicos"),]) / nrow(obesidade) * 100), 2) # = 75.18%

# A percentagem de pessoas inquiridas que utiliza habitualmente bicocleta ou transportes públicos é de 75.18%.

##############
##### F) #####
##############

### Var. NCP

(ni.ncp <- table(obesidade$NCP))
(fi.ncp <- round(prop.table(ni.ncp), 4))
(Ni.ncp <- cumsum(ni.ncp))
(Fi.ncp <- round(cumsum(fi.ncp), 4))

(tabela.frequencias.ncp <- data.frame(
  i = 1:nrow(ni.ncp),
  xi = names(ni.ncp),
  ni = as.integer(ni.ncp),
  fi = as.numeric(fi.ncp),
  Ni = as.integer(Ni.ncp),
  Fi = as.numeric(Fi.ncp)
))

##############
##### G) #####
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

##############
##### H) #####
##############

nrow(na.omit(obesidade[obesidade$Idade > 44,])) # = 17
sum(tabela.frequencias.idade[tabela.frequencias.idade$i > 5, "ni"]) # = 17

# Das pessoas inquiridas, 17 delas têm mais de 44 anos.

##############
##### I) #####
##############

sum(tabela.frequencias.altura[tabela.frequencias.altura$i < 3, "fi"])*100 # = 85.17%

# A percentagem de pessoas inquiridas que tem altura máxima 1.80 metros é de 85.17%.
