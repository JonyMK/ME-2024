#########################################################################
######################## Capítulo 1 - Exercícios ########################
#########################################################################

#######################################################
#################### Exercicio 1.6 ####################
#######################################################

tempos <- c(
  13.9, 14.2, 16.1, 15.5, 14.9, 15.6, 15.3,
  14.7, 13.6, 15.7, 15.8, 14.5, 13.5, 14.3,
  15.1, 14.0, 16.3, 14.2, 15.8, 14.9, 15.5,
  16.3, 14.6, 15.2, 16.3, 14.4, 15.4, 15.0
)

##############
##### 1) #####
##############

# Variável Tempos - tempo de cada participante da corrida dos 100 metros.
# Var. Tempos -> Var. Quantitativa Discreta.

##############
##### 2) #####
##############

(DescTools::Freq(tempos))

# OU

# Tabela de Frequências Com Classes:

# Mínimo e Máximo dos Dados
min(tempos)
max(tempos)

# Tamanho da Amostra:
(n <- length(tempos))

# Nº de Classes:
(k <- trunc(1 + log(n)/log(2)))

# Amplitude das Classes:
(h <- (max(tempos) - min(tempos)) / k)

# Mínimo e Máximo das Classes:
(tempos.min <- min(tempos))
(tempos.max <- tempos.min + h * k)

# Extremos das Classes:
(tempos.cortes <- seq(tempos.min, tempos.max, by = h))

# Intervalos: Abertos à Esquerda e Fechados à Direita:
(tempos.classes <- cut(
  tempos,
  breaks = tempos.cortes,
  right = TRUE,
  include.lowest = TRUE
))

# Tabela de Frequências com Classes
(ni.tempos <- table(tempos.classes))           # Frequências Absolutas
(fi.tempos <- round(prop.table(ni.tempos), 4)) # Frequências Relativas
(Ni.tempos <- cumsum(ni.tempos))               # Frequências Absolutas Acumuladas
(Fi.tempos <- round(cumsum(fi.tempos), 4))     # Frequências Relativas Acumuladas

(tabela.frequencias.tempos <- data.frame(
  i=1:nrow(ni.tempos),
  xi=names(ni.tempos),
  ni=as.integer(ni.tempos),
  fi=as.numeric(fi.tempos),
  Ni=as.integer(Ni.tempos),
  Fi=as.numeric(Fi.tempos)
))
