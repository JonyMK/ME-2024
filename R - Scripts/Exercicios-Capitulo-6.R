# Capítulo 6 - Exercícios ####

"----------------------------------------------------------------------"

# Testes de Ajustamento ####

"----------------------------------------------------------------------"

## Exercicio 6.1 ####


"-------- Enunciado --------"
#'* Foram inquiridos 995 indivíduos sobre a preferència em relação a  *
#'* 5 marcas de comprimidos para a gripe, tendo-se obtido os          *
#'* resultados apresentados na tabela seguinte:                       *
#'*                                                                   *
#'* Marca: 1, 2, 3, 4, 5                                              *
#'*                                                                   *
#'* Número de Consumidores: 190, 210, 180, 205, 210                   *
#'*                                                                   *
#'* Verifique, para um nível de significância de 5%, se os dados se   *
#'* ajustam a uma distribuição Uniforme Discreta.                     *
"---------------------------"


# População:
## X - Preferência em relação a 5 marcas de comprimidos para a gripe.

# Amostra:
## Dominio:
(Dx = c(1, 2, 3, 4, 5))
(amostra_6_1 <- c(rep(1, 190), rep(2, 210), rep(3, 180), rep(4, 205), rep(5, 210)))
(n <- length(amostra_6_1)) # = 995

# Teste de Hipóteses Não Paramétrico: Teste de Ajustamento:

# 1º Passo:

## H0: X ~ Uniforme Discreta vs. H1: X !~ Uniforme Discreta

# 2º Passo:

## α = 0.05

# 3º Passo:

## Para testar se a População segue uma Distribuição Discreta ou Contínua com Classes:
##chisq.test

(r <- 0)
(k_6_1 <- length(Dx))
(gl <- k - 1 - r)

(Oi_6_1 <- table(amostra_6_1))
(pi_6_1 <- rep(1/k, k))

(res_cisq_6_1 <- chisq.test(
  x = Oi_6_1,  # Frequências Observadas (Freq. Absolutas)
  p = pi_6_1   # Frequências Esperadas
))
## X^2_obs = 3.6181
## P-Value = 0.4602

# Verificação das Regras:

## Dimensão da Amostra Maior que 30:
if (n > 30) {
  print("Respeita a Regra.")
} else {
  print("Amostra Demasiado Pequena!")
} ## VÁLIDA

## Todas as Freq. Esperadas >= 1:
if (length(which(res_cisq_6_1$expected < 1)) > 0) {
  print("Juntar Linhas da Tabela de Frequências!")
} else {
  print("Respeita a Regra.")
} ## VÁLIDA

## Não Há Mais de 20% das Freq. Esperadas < 5:
if (length(which(res_cisq_6_1$expected < 5)) > (k * 0.2)) {
  print("Juntar Linhas da Tabela de Frequências!")
} else {
  print("Respeita a Regra.")
} ## VÁLIDA

# 4º Passo:

## Como P-Value = 0.4602 > α = 0.05, não se rejeita H0.

# 5º Passo:

## Com base na amostra e com 5% de significância, pode-se concluir
## que a população pode ser considerada como Uniforme Discreta.

"----------------------------------------------------------------------"

## Exercicio 6.2 ####


"-------- Enunciado --------"
#'* O recenseamento de 320 famílias com 5 filhos forneceu os          *
#'* seguintes resultados:                                             *
#'*                                                                   *
#'* Rapazes: 0, 1, 2, 3, 4, 5                                         *
#'*                                                                   *
#'* Famílias: 8, 40, 88, 110, 56, 18                                  *
#'*                                                                   *
#'* Verifique, para um nível de significância de 1%, se estes         *
#'* resultados são compatíveis com a hipótese do número de rapazes    *
#'* por família ser uma variível aleatória com distribuição binomial, *
#'* admitindo a equiprobabilidade dos sexos.                          *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.3 ####


"-------- Enunciado --------"
#'* A procura diária de um certo produto foi, em 140 dias escolhidos  *
#'* ao acaso, a seguinte:                                             *
#'*                                                                   *
#'* Nº de Unidades: 0, 1, 2, 3, 4, 6                                  *
#'*                                                                   *
#'* Nº de Dias: 26, 24, 20, 27, 22, 21                                *
#'*                                                                   *
#'* Será que tais observações foram extraídas de uma população com    *
#'* distribuição de Poisson, isto é, será de admitir que tal procura  *
#'* segue uma distribuição de Poisson? Considere um nível de          *
#'* significância de 5%.                                              *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.4 ####


"-------- Enunciado --------"
#'* O Departamento de Defesa de determinado país supõe que a          *
#'* distribuição de probabilidade do número de avarias em determinado *
#'* equipamento de um submarino, durante a realização de uma dada     *
#'* tarefa, segue uma distribuição de Poisson. Os dados relativos a   *
#'* 500 dessas tarefas foram os seguintes:                            *
#'*                                                                   *
#'* Número de falhas/tarefa: 0, 1, 2, 3, 4                            *
#'*                                                                   *
#'* Número de tarefas: 185, 180, 95, 30, 10                           *
#'*                                                                   *
#'* Analise, para um nível de significância de 5%, a hipótese do      *
#'* citado Departamento.                                              *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.5 ####


"-------- Enunciado --------"
#'* Aceita-se com frequência que o número de gralhas por página de    *
#'* um livro, segue uma distribuição de Poisson. Através de uma       *
#'* amostra de 400 páginas de um livro obtiveram-se os dados          *
#'* seguintes:                                                        *
#'*                                                                   *
#'* Número de gralhas: 0, 1, 2, 3, 4, 5, 6, 7, 8                      *
#'*                                                                   *
#'* Número de páginas: 14, 48, 101, 98, 87, 37, 12, 2, 1              *
#'*                                                                   *
#'* Verifique se a hipótese habitualmente aceite é válida para um     *
#'* nível de significância de 5%.                                     *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.6 ####


"-------- Enunciado --------"
#'* Numa fábrica de motores elétricos, o gerente de produção precisa  *
#'* avaliar o problema de ruído excessivo do motor. Uma das possíveis *
#'* causas está associada com variações no diâmetro do eixo. Assim,   *
#'* o responsável de produção mediu o diâmetro do eixo de 200 motores *
#'* (em milésimos de milímetros) e obteve os resultados apresentados  *
#'* no ficheiro “motores.xlsx”                                        *
#'*                                                                   *
#'* 1.) Faça uma análise descritiva dos dados e sugira uma            *
#'*     distribuição adequada a este conjunto de dados.               *
#'*                                                                   *
#'* 2.) Diga a partir de que nível de significância a distribuição    *
#'*     sugerida não é válida.                                        *
#'*                                                                   *
#'* 3.) Construa classes recorrendo à regra de Sturges e, com base    *
#'*     nessas classes, teste a distribuição sugerida considerando um *
#'*     nível de significância de 5%.                                 *
"---------------------------"


# Ex.

### 1) #####



### 2) #####



### 3) #####



"----------------------------------------------------------------------"

## Exercicio 6.7 ####


"-------- Enunciado --------"
#'* Considere o ficheiro “População.xlsx”.                            *
#'*                                                                   *
#'* 1.) Teste, para um nível de significância de 5%, as seguintes     *
#'*     afirmações:                                                   *
#'*     a) A variável “altura” pode ser modelada por uma distribuição *
#'*        Normal.                                                    *
#'*     b) A variável “peso” pode ser modelada por uma distribuição   *
#'*        Normal.                                                    *
#'*                                                                   *
#'* 2.) Recorra ao teste de ajustamento do Qui-Quadrado para testar   *
#'*     as seguintes afirmações. Caso considere necessário construa   *
#'*     classes recorrendo à regra de Sturges e considere um nível de *
#'*     significância de 5%.                                          *
#'*     a) A variável “altura” pode ser modelada por uma distribuição *
#'*        Normal.                                                    *
#'*     b) A variável “peso” pode ser modelada por uma distribuição   *
#'*       Normal.                                                     *
#'*     c) A variável “tempo (em minutos) que a medicação demora a    *
#'*        fazer efeito” pode ser modelada por uma distribuição       *
#'*        Exponencial.                                               *
"---------------------------"


# Ex.

### 1) #####

#### a) ####



#### b) ####



### 2) #####

#### a) ####



#### b) ####



#### c) ####



"----------------------------------------------------------------------"

## Exercicio 6.8 ####


"-------- Enunciado --------"
#'* Num determinado Instituto Politécnico do país efetuou-se um       *
#'* contrato com uma determinada empresa que ficou responsável pelo   *
#'* abastecimento da carne que compunha as refeições na cantina dessa *
#'* escola. O contrato refere uma média de 290 gramas de carne por    *
#'* refeição, por estudante. No entanto, alguns alunos queixaram-se   *
#'* acerca da comida, em particular acerca da quantidade de carne     *
#'* servida por refeição. Os alunos falaram com o cozinheiro chefe,   *
#'* que lhes disse que a quantidade de carne servida por refeição a   *
#'* cada estudante tinha aproximadamente distribuição normal de média *
#'* 290 gramas com um desvio padrão de 56 gramas. Após esta conversa  *
#'* com o cozinheiro, alguns alunos concordaram em recolher as suas   *
#'* refeições ao longo de vários dias, resultando assim uma amostra   *
#'* de 10 refeições, que foram levadas para um laboratório afim de    *
#'* serem pesados os pedaços de carne nelas contidos. Os dados        *
#'* obtidos são os seguintes:                                         *
#'*                                                                   *
#'* 198, 254, 262, 272, 275, 278, 285, 287, 287, 292.                 *
#'*                                                                   *
#'* Ao nível de significância de 5%, há evidência para rejeitar a     *
#'* hipótese de que o cozinheiro seguia as regras que afirmou em      *
#'* relação à quantidade de carne servida?                            *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.9 ####


"-------- Enunciado --------"
#'* Os níveis de glicose no sangue (mg/100ml) numa amostra aleatória  *
#'* de 18 indivíduos, aparentemente saudáveis são os seguintes:       *
#'*                                                                   *
#'* 75, 92, 80, 80, 84, 72, 84, 77, 81, 77, 75, 81, 80, 92, 72, 77,   *
#'* 78, 76.                                                           *
#'*                                                                   *
#'* Pretende testar-se, para um nível de significância de 5%, se os   *
#'* dados se referem a uma distribuição normal de média 80 mg e       *
#'* desvio padrão 6.91 mg.                                            *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.10 ####


"-------- Enunciado --------"
#'* Suponha que os dados seguintes representam os erros de            *
#'* ajustamento (resíduos), obtidos através de um modelo linear:      *
#'*                                                                   *
#'* −2.7, −1.1, −0.5, 0.4, 1.8, −2.1, −1.0, −0.3, 0.8, 2.0, −1.9,     *
#'* −0.9. 0.1. 1.2. 2.6. −1.7. −0.7. 0.4. 1.3. 2.9.                   *
#'*                                                                   *
#'* Verifique se os resíduos se distribuem normalmente, com média     *
#'* nula e variância 2.5 para um nível de significância de 5%.        *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.11 ####


"-------- Enunciado --------"
#'* No âmbito de um programa de reabilitação, registaram-se os QI’s   *
#'* de 30 jovens dependentes do consumo de drogas:                    *
#'*                                                                   *
#'* 95, 100, 91, 106, 109, 110, 98, 104, 97, 100, 107, 119, 92,       *
#'* 106, 103, 106, 105, 112, 101, 91, 105, 102, 101, 110, 101,        *
#'* 95, 102, 104, 107, 118.                                           *
#'*                                                                   *
#'* Para um nível de significância de 0.05, poder-se-á concluir que   *
#'* os dados provêm de uma população normal com média de QI de 105 e  *
#'* desvio padrão de 10? Determine o valor-p.                         *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.12 ####


"-------- Enunciado --------"
#'* Uma firma tem seguido a política de oferecer uma garantia de 2000 *
#'* utilizações para determinado aparelho que comercializa. Este      *
#'* procedimento baseia-se em estudos levados a cabo no período       *
#'* inicial de produção, que indicavam que o número de utilizações    *
#'* possíveis por aparelho seguia uma distribuição Normal de média    *
#'* 2060, com uma variabilidade traduzida por σ = 20. Existindo       *
#'* indícios de que presentemente a situação pode ter mudado, foram   *
#'* selecionados ao acaso e testados pela firma 10 aparelhos, os      *
#'* quais forneceram os seguintes valores:                            *
#'*                                                                   *
#'* 2100, 2025, 2071, 2067, 2150, 2115, 2064, 2088, 1995, 2095.       *
#'*                                                                   *
#'* Teste, ao nível de significância de 5%, se o número de            *
#'* utilizações permitidas por aparelho comporta-se de forma          *
#'* aproximadamente normal.                                           *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.13 ####


"-------- Enunciado --------"
#'* Um fabricante de cabos pretende saber se os seus cabos            *
#'* apresentavam uma tensão de rutura com comportamento Normal. Para  *
#'* tal foi recolhida uma amostra de 35 cabos:                        *
#'*                                                                   *
#'* 1595.1, 1514.4, 1608.8, 1591.7, 1482.5, 1796.1, 1700.1, 1501.5,   *
#'* 1658.9, 1777.1, 1625.4, 1713.9, 1522.3, 1575.2, 1634.7, 1615.8,   *
#'* 1690.5, 1729.0, 1646.7, 1681.1, 1769.7, 1707.3, 1668.3, 1768.0,   *
#'* 1655.1, 1715.6, 1805.2, 1724.8, 1578.1, 1548.4, 1647.0, 1586.5,   *
#'* 1706.8, 1535.0, 1673.4                                            *
#'*                                                                   *
#'* Teste, ao nível de significância de 1%, se a tensão de rutura     *
#'* segue uma distribuição aproximadamente normal.                    *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.14 ####


"-------- Enunciado --------"
#'* No ficheiro EXCEL ”Obesidade” tem as respostas a um inquérito     *
#'* efetuado num estudo sobre obesidade a um grupo de indivíduos      *
#'* obesos.                                                           *
#'*                                                                   *
#'* 1.) Sugira, justificando, uma distribuição para ”Número de        *
#'*     refeições principais que tem habitualmente” e teste, para um  *
#'*     nível de significância de 10%, a distribuição sugerida.       *
#'*                                                                   *
#'* 2.) Analise os dados referentes à altura quanto à simetria.       *
#'*                                                                   *
#'* 3.) Sugira uma distribuição para os dados Altura e teste, para um *
#'*     nível de significância de 10%, a sua sugestão.                *
#'*                                                                   *
#'* 4.) Pretende-se testar se a idade segue uma distribuição          *
#'*     Exponencial.                                                  *
#'*     a) Analise os dados da idade quanto à simetria e diga se      *
#'*        concorda com a distribuição sugerida.                      *
#'*     b) Teste, para um nível de significância de 1%, a             *
#'*        distribuição indicada.                                     *
#'*     c) Teste, para um nível de significância de 1%, se a idade    *
#'*        segue uma distribuição Exponencial com média 25 anos.      *
#'*                                                                   *
#'* 5.) Remova os ”outliers” existentes nos dados referentes à Idade. *
#'*     Com os dados sem ”outliers”, teste, para um nível de          *
#'*     significância de 1%, se os dados da idade podem vir de uma    *
#'*     população com distribuição Normal de média 25 anos e desvio   *
#'*     padrão 5 anos.                                                *
#'*                                                                   *
#'* 6.) Considere os dados referentes aos não fumadores.              *
#'*     a) Teste, para um nível de significância de 1%, se os dados   *
#'*        da idade podem vir de uma distribuição Exponencial de      *
#'*        média 25 anos.                                             *
#'*     b) Teste, para um nível de significância de 3%, se os dados   *
#'*        do peso podem vir de uma distribuição Normal.              *
#'*                                                                   *
#'* 7.) Considere os dados referentes aos fumadores.                  *
#'*     a) Teste, para um nível de significância de 2%, se os dados   *
#'*        da idade podem vir de uma distribuição Exponencial de      *
#'*        média 30 anos.                                             *
#'*     b) Teste, para um nível de significância de 10%, se os dados  *
#'*        do peso podem vir de uma distribuição Normal.              *
"---------------------------"


# Ex.

### 1) #####



### 2) #####



### 3) #####



### 4) #####

#### a) #####



#### b) #####



#### c) #####



### 5) #####



### 6) #####

#### a) #####



#### b) #####



### 7) #####

#### a) #####



#### b) #####



"----------------------------------------------------------------------"

# Teste à Igualdade de Duas Distribuições ####

"----------------------------------------------------------------------"

## Exercicio 6.20 ####


"-------- Enunciado --------"
#'* Os valores registados na tabela seguinte referem-se a uma amostra *
#'* de 9 indivíduos, classificados de acordo com as respostas a um    *
#'* questionário efetuado antes e depois da população a que           *
#'* pertencem ter recebido um conjunto de instruções sobre regras de  *
#'* higiene alimentar. As pontuações aumentam com a melhoria dos      *
#'* hábitos alimentares.                                              *
#'*                                                                   *
#'* Indivíduo: 1, 2, 3, 4, 5, 6, 7, 8, 9                              *
#'*                                                                   *
#'* Antes: 21, 33, 30, 34, 26, 25, 15, 20, 21                         *
#'*                                                                   *
#'* Depois: 21, 36, 26, 35, 31, 25, 17, 20, 21                        *
#'*                                                                   *
#'* Para um nível de significância de 5%, conclua se se registam      *
#'* progressos nos hábitos alimentares da população.                  *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.21 ####


"-------- Enunciado --------"
#'* Num estudo sobre nutrição, pretendem-se avaliar determinados      *
#'* tipos de dieta, bem como as perdas de peso associadas a cada      *
#'* dieta. Para tal realizou-se a experiência, em que a um conjunto   *
#'* de 10 pessoas que seguiu um determinado tipo de dieta,            *
#'* analisou-se o peso antes e depois do plano de dieta. Os pesos (em *
#'* kg) foram os seguintes:                                           *
#'*                                                                   *
#'* Pessoa: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10                             *
#'*                                                                   *
#'* Antes: 82.7, 73.2, 84.1, 84.1, 81.6, 78.9, 85.6, 80.2, 84.5, 73.8 *
#'*                                                                   *
#'* Depois: 74.5, 73.2, 79.1, 85.6, 81.6, 76.9, 81.5, 80.2, 86.9, 73.8*
#'*                                                                   *
#'* Para um nível de significância de 5%, será que existem diferenças *
#'* significativas no peso antes e depois do plano de dieta?          *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.22 ####


"-------- Enunciado --------"
#'* Num estudo sobre alcoolismo um grupo de investigadores pretendia  *
#'* conhecer o efeito inibidor de um novo medicamento (DHT) e por     *
#'* isso recolheu os dados seguintes referentes a níveis de           *
#'* testosterona (em anmol/lb) em 12 homens saudáveis antes e depois  *
#'* do tratamento com DHT:                                            *
#'*                                                                   *
#'* Homem: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12                      *
#'*                                                                   *
#'* Antes: 21.5, 23.0, 21.0, 21.8, 22.8, 14.7, 21.0, 23.4, 20.0,      *
#'*        29.5, 23.5, 20.5                                           *
#'*                                                                   *
#'* Depois: 9.4, 17.2, 13.0, 6.4, 4.8, 4.5, 10.7, 15.6, 12.5, 7.7,    *
#'*         10.8, 9.2                                                 *
#'*                                                                   *
#'* Para um nível de significância de 1%, poder-se-á concluir com     *
#'* base nestes dados, que o tratamento reduz a concentração de       *
#'* testosterona em homens saudáveis?                                 *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.23 ####


"-------- Enunciado --------"
#'* Uma amostra de 15 doentes asmáticos participou numa experiência   *
#'* que pretendia estudar o efeito de um novo tratamento na função    *
#'* pulmonar. Entre outros registos recolheram-se valores do volume   *
#'* expiratório forçado (VEF) por segundo (em l/s), antes e depois do *
#'* tratamento:                                                       *
#'*                                                                   *
#'* Doente: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15         *
#'*                                                                   *
#'* Antes: 1.69, 2.77, 1.00, 1.66, 3.00, 0.85, 1.42, 2.82, 2.58,      *
#'*        1.84, 1.89, 1.91, 1.75, 2.46, 2.35                         *
#'*                                                                   *
#'* Depois 1.69, 2.22, 3.07, 3.35, 3.00, 2.74, 3.61, 5.14, 2.44,      *
#'*        4.17, 2.42, 2.94, 3.04, 4.62, 4.42                         *
#'*                                                                   *
#'* Para um nível de significância de 5%, poder-se-á concluir com     *
#'* base nestes dados, que o tratamento aumenta o VEF?                *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.24 ####


"-------- Enunciado --------"
#'* Num estudo sobre nutrição, pretendem-se avaliar determinados      *
#'* tipos de dieta, bem como as perdas de peso associadas a cada      *
#'* dieta. Para tal realizou-se a experiência, em que a cada uma de   *
#'* 10 pessoas com excesso de peso atribuiu-se aleatoriamente 1 de    *
#'* 2 planos de dieta. As perdas de peso (em kg) foram as seguintes:  *
#'*                                                                   *
#'* Dieta 1: 2.7, 3.2, 4.1, 4.5, 3.8                                  *
#'*                                                                   *
#'* Dieta 2: 4.5, 3.6, 4.1, 3.9, 3.9                                  *
#'*                                                                   *
#'* Para um nível de significância de 5%, pode admitir que as duas    *
#'* dietas são realmente distintas?                                   *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.25 ####


"-------- Enunciado --------"
#'* É medida a altura de determinada espécie de árvores com igual     *
#'* número de anos em duas localidades diferentes, A e B, pois há     *
#'* suspeitas de que a altura das espécies de árvores da localidade A *
#'* seja diferente da altura das espécies de árvores da localidade B. *
#'* Foram recolhidas algumas amostras aleatórias, registando-se o     *
#'* seguinte:                                                         *
#'*                                                                   *
#'* Localidade A: 8, 14, 11, 9, 10, 12                                *
#'*                                                                   *
#'* Localidade B: 7, 5, 10, 6, 10, 11, 8                              *
#'*                                                                   *
#'* Para um nível de significância de 5%, pretende-se verificar se as *
#'* alturas são realmente distintas.                                  *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.26 ####


"-------- Enunciado --------"
#'* Lebranchu et al. conduziu um estudo em que observou 9 pacientes   *
#'* com Imunodeficiência Variável Comum (IVC), cujos resultados       *
#'* foram confrontados com os de um grupo de controle constituído por *
#'* 12 indivíduos aparentemente normais. Entre os dados recolhidos    *
#'* encontravam-se os seguintes, referentes ao número de células CD4+ *
#'* por mm3 de sangue periférico:                                     *
#'*                                                                   *
#'* Pacientes IVC: 623, 437, 370, 300, 330, 527, 290, 730, 1000       *
#'*                                                                   *
#'* Grupo de controle: 710, 1260, 717, 590, 930, 995, 630, 977,       *
#'*                    530, 710, 1275, 825                            *
#'*                                                                   *
#'* Será possível concluir, com base nestes dados e para α = 0.01,    *
#'* que os pacientes IVC têm um nível reduzido de células CD4+ ?      *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"

## Exercicio 6.27 ####


"-------- Enunciado --------"
#'* Um investigador propôs-se a estudar as modificações ocorridas nas *
#'* vias respiratórias de um grupo de doentes asmáticos após inalação *
#'* de um antigénio. Um grupo de controle constituído por indivíduos  *
#'* não asmáticos, supostamente saudáveis foi sujeito ao mesmo        *
#'* procedimento. Na tabela seguinte apresentam-se dados referentes à *
#'* percentagem de antigénio presente em recolhas resultantes de      *
#'* lavagem bronco-alveolar:                                          *
#'*                                                                   *
#'* Não-Asmático: 70, 55, 63, 68, 73, 77, 67                          *
#'*                                                                   *
#'*     Asmático: 64, 25, 70, 35, 43, 49, 62, 56, 43, 66              *
#'*                                                                   *
#'* Poder-se-à concluir, com base nestes dados e para α = 0.05, que o *
#'* grupo de doentes asmáticos tem uma percentagem inferior de        *
#'* antigénio nas recolhas efetuadas?                                 *
"---------------------------"


# Ex.

"----------------------------------------------------------------------"
