########################
# CAPÍTULO 6 (PARTE 3) #
########################


#######################################################################################
#######################################################################################
#######################################################################################

#                     Teste de Wilcoxon E Teste de Mann-Whitney                       #

#######################################################################################
#######################################################################################
#######################################################################################

# função -> wilcox.test()
# argumentos da função: amostra X e amostra Y

#######################################################################


###############################
###############################
###############################

#          Exemplo 1          #

###############################
###############################
###############################


# amostras
amostra1.x = c(3.5,3.6,4.1,2.9,3.4,4.2,3.9,4.1)
amostra1.y = c(3.4,3.9,4.5,3.1,3.9,4.4,3.8,4.1)

# D=Y-X

# Teste de Wilcoxon
wilcox.test(x=amostra1.y, y=amostra1.x, alternative = "greater", mu=0, paired=TRUE)

#atribuir um "nome" ao teste para aceder aos campos
exemplo.1 = wilcox.test(x=amostra1.y, y=amostra1.x, alternative = "greater", mu=0, paired=TRUE)
exemplo.1$statistic # T+
exemplo.1$p.value   # valor-p
exemplo.1$null.value # H0: MD=0
exemplo.1$alternative # H1: MD>0


###############################
###############################
###############################

#          Exemplo 2          #

###############################
###############################
###############################

# amostras
amostra2.x = c(82.7,73.2,84.1,84.1,81.6,78.9,85.6,80.2,84.5,73.8)
amostra2.y = c(74.5,73.2,79.1,85.6,81.6,79.6,81.5,80.2,86.9,73.8)

# D=Y-X

# Teste de Wilcoxon
wilcox.test(x=amostra2.y, y=amostra2.x, alternative = "less", mu=0, paired=TRUE)

#atribuir um "nome" ao teste para aceder aos campos
exemplo.2 = wilcox.test(x=amostra2.y, y=amostra2.x, alternative = "less", mu=0, paired=TRUE)
exemplo.2$statistic # T+
exemplo.2$p.value   # valor-p
exemplo.2$null.value # H0: MD=0
exemplo.2$alternative # H1: MD<0


###############################
###############################
###############################

#          Exemplo 3          #

###############################
###############################
###############################

# amostras
amostra3.x = c(10,12,13,14,11,12.4,15,9.8,12.9,12.9)
amostra3.y = c(9.8,11.6,12,14,11,13,16,12,13,13.4)

# D=Y-X

# Teste de Wilcoxon
wilcox.test(x=amostra3.y, y=amostra3.x, alternative = "two.sided", mu=0, paired=TRUE)

#atribuir um "nome" ao teste para aceder aos campos
exemplo.3 = wilcox.test(x=amostra3.y, y=amostra3.x, alternative = "two.sided", mu=0, paired=TRUE)
exemplo.3$statistic # T+
exemplo.3$p.value   # valor-p
exemplo.3$null.value # H0: MD=0
exemplo.3$alternative # H1: MD<>0


###############################
###############################
###############################

#          Exemplo 4          #

###############################
###############################
###############################

# amostras
amostra4.x = c(14.4,14.2,13.8,16.5,14.1,16.6,15.9,15.6,14.1,15.3,15.7,16.7,13.7,15.3,14.0)
amostra4.y = c(17.4,16.2,17.1,17.5,15.0,16.0,16.9,15.0,16.3,16.8)

# MX- MY

# Teste de Mann-Whitney
wilcox.test(x=amostra4.x, y=amostra4.y, alternative = "less", mu=0, paired=FALSE)

#atribuir um "nome" ao teste para aceder aos campos
exemplo.4 = wilcox.test(x=amostra4.x, y=amostra4.y, alternative = "less", mu=0, paired=FALSE)
exemplo.4$statistic # Uobs
exemplo.4$p.value   # valor-p
exemplo.4$null.value # H0: MX-MY=0
exemplo.4$alternative # H1: MX-MY<0


###############################
###############################
###############################

#          Exemplo 5          #

###############################
###############################
###############################

# amostras
amostra5.x = c(1.62,0.51,1.29,0.71,0.52,2.10,0.88,0.99,0.51,1.59)
amostra5.y = c(0.92,1.29,2.81,0.82,4.48,0.71,1.10,0.41)

# MX- MY

# Teste de Mann-Whitney
wilcox.test(x=amostra5.x, y=amostra5.y, alternative = "greater", mu=0, paired=FALSE)

#atribuir um "nome" ao teste para aceder aos campos
exemplo.5 = wilcox.test(x=amostra5.x, y=amostra5.y, alternative = "greater", mu=0, paired=FALSE)
exemplo.5$statistic # Uobs
exemplo.5$p.value   # valor-p
exemplo.5$null.value # H0: MX-MY=0
exemplo.5$alternative # H1: MX-MY>0


###############################
###############################
###############################

#          Exemplo 6          #

###############################
###############################
###############################

# amostras
amostra6.x = c(607.4,809.1,488.8,481.1,592.8,345.4,620.0,407.7,513.3,527.4)
amostra6.y = c(694.5,629.6,676.9,430.3,727.2)

# MX- MY

# Teste de Mann-Whitney
wilcox.test(x=amostra6.x, y=amostra6.y, alternative = "two.sided", mu=0, paired=FALSE)

#atribuir um "nome" ao teste para aceder aos campos
exemplo.6 = wilcox.test(x=amostra6.x, y=amostra6.y, alternative = "two.sided", mu=0, paired=FALSE)
exemplo.6$statistic # Uobs
exemplo.6$p.value   # valor-p
exemplo.6$null.value # H0: MX-MY=0
exemplo.6$alternative # H1: MX-MY<>0
