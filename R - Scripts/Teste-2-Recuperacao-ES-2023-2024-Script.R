#######################################
#  2.º TESTE (Recuperação) 2023-2024  #
#######################################

#EXERCÍCIO 1

# dados
#ler os dados do ficheiro: File -> Import Datset -> From Text

ProtesesMI_T2
str(ProtesesMI_T2)


##############################################################
##############################################################

# 1 (a)

# amostra
ProtesesMI_T2$Afisica

#dimensão da amostra
(n = length(ProtesesMI_T2$Afisica))

# média da amostra
(m = mean(ProtesesMI_T2$Afisica))

# desvio padrão da amostra
(dp = sd(ProtesesMI_T2$Afisica))

# valor do quantil de probabilidade
(z = (m-5.2)/(dp/sqrt(n)))

# probabilidade pretendida
pnorm(z)

# nível de significância
(alfa = 2*(1-pnorm(z)))

# grau de confiança
1-alfa


##############################################################
##############################################################

# 1 (b)

# filtrar os dados
feminino = ProtesesMI_T2[ProtesesMI_T2$genero=="Feminino",]

# estimativa pontual para média do tempo de uso das próteses pelos pacientes do género feminino
mean(feminino$tempo)

#estimativa pontual para a variância do tempo de uso das próteses pelos pacientes do género feminino
var(feminino$tempo)

#estimativa pontual para a percentagem de pacientes do género feminino com amputação transtibial
prop.table(table(feminino$amputacao))[2]*100
#ou
transtibial = feminino[feminino$amputacao==1,]
(nrow(transtibial)/nrow(feminino))*100

##############################################################
##############################################################

# 1 (c)

# feminino
#dimensão da amostra
(nf = nrow(feminino))
# teste de ajustamento
shapiro.test(feminino$tempo)

# masculino
# filtrar os dados
masculino = ProtesesMI_T2[ProtesesMI_T2$genero=="Masculino",]
#dimensão da amostra
(nm = nrow(masculino))
# teste de ajustamento
shapiro.test(masculino$tempo)

##############################################################
##############################################################

# 1 (d)

# Intervalo de confiança para o quociente de variâncias
(IC1c = var.test(x=feminino$tempo, y=masculino$tempo, conf.level=0.90))
IC1c$conf.int

##############################################################
##############################################################

# 1 (e)

#teste de hipóteses para diferença de médias
(TH1e = t.test(x=masculino$tempo, y=feminino$tempo, alternative="less", mu=0, paired=FALSE, var.equal=TRUE))
#valor observado da estatística de teste sob a hipótese H0
TH1e$statistic

#quantil de probabilidade da Distribuição t-Student para a região crítica
qt(0.10,nm+nf-2)


##############################################################
##############################################################

# 1 (f)

# amostra
ProtesesMI_T2$amputacao
#dimensão da amostra
length(ProtesesMI_T2$amputacao)
# a amostra já está com sucesso=1 e insucesso=0
# teste de hipóteses para a porporção
library(BSDA)
(TH1f = z.test(x=ProtesesMI_T2$amputacao, sigma.x=sqrt(0.50*(1-0.50)), alternative="greater", mu=0.50))

#valor-p
TH1f$p.value

#ou

#quantil de probabilidade da Distribuição Normal para a região crítica
qnorm(1-0.07)
# valor observado da estatística de teste sob a hipoótese H0
TH1f$statistic


##############################################################
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

# EXERCÍCIO 2

# 2 (a)
(valor = 20/(50/sqrt(6)))
1-pnorm(20/(50/sqrt(6)))

##############################################################
##############################################################

# 2 (b)

qnorm(0.1)
# valor de n
(-5*qnorm(0.1))^2

##############################################################
##############################################################

# 2 (c)

# amostra
pesoRN = c(3599.000, 3495.655, 3433.602, 3477.521, 3541.972, 3454.400)

#TH para uma variância
library(EnvStats)
(TH2c = varTest(x=pesoRN, alternative="greater", sigma.squared=2500))

#valor-p
TH2c$p.value


##############################################################
##############################################################

# 2 (d)

#amostras
AexcLM = c(2166.13, 2235.77, 2324.14, 2168.85, 1268.05)
AparcLM = c(2831.39, 1838.87, 1836.51, 1800.39, 1776.31)


##############################################################
##############################################################

# 2 (d) (i)

# teste de ajustamento
ks.test(x=AexcLM, "pexp", rate=1/2000)

##############################################################
##############################################################

# 2 (d) (ii)

#TH de Mann-Whitney
(TH2dii = wilcox.test(x=AexcLM, y=AparcLM, alternative="two.sided", mu=0, paired=FALSE))

#valor-p
TH2dii$p.value
