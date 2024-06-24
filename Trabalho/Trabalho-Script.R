# Descrição das Colunas presentes na BD: ####

## ID -> Identificação da Campanha.
## plataforma -> Platataforma onde a campanha foi efetuada (Google | Facebook | Instagram).
## anuncio -> Tipo de anúncio utilizado na campanha (Banner | Vídeo | Texto).
## mercado -> Segmento de mercado alvo da campanha (Tecnologia | Moda | Alimentação).
## investimento -> Valor investido na campanha (em milhares de euros).
## cliques -> Número de cliques recebidos pela campanha.
## conversoes -> Número de conversões geradas pela campanha.

"---------------------------------------------"

# Análises de Variáveis Qualitativas: ####

## Relação: Plataforma - Anúncio: ####

### Para estudar a relação entre 2 variáveis qualitativas,
### usa-se o teste de independência do qui-quadrado.

### Hipóteses:

#### H0: As variáveis plataforma e anuncio não estão relacionadas.
#### vs.
#### H1: As variáveis plataforma e anuncio estão relacionadas.

### Confiança e Significância:

#### Os testes será realizado tendo em conta:
##### Confiaça = 0.95
##### Significância = 0.05

### Dados:

#### 2 Variáveis Qualitativas Nominais: Plataforma e Anúncio.

(valores_plataforma <- table(DadosMarkDig$plataforma))
(length(names(valores_plataforma)))

(valores_anuncio <- table(DadosMarkDig$anuncio))
(length(names(valores_anuncio)))

### Tabela de Contingência:

#### r = c = 3

#### Frequências Observadas (Oi):
(table(DadosMarkDig$plataforma, DadosMarkDig$anuncio))


#### Tabela de Contingência:
(plataformas <- factor(DadosMarkDig$plataforma, levels = c("Facebook", "Google", "Instagram")))
(anuncios <- factor(DadosMarkDig$anuncio, levels = c("Banner", "Texto", "Vídeo")))
(tabela.contingencia.plataforma.anuncio <- table(plataformas, anuncios))

### Teste de Independência do Qui-Quadrado:

(resultado.independencia.plataforma.anuncio <- chisq.test(tabela.contingencia.plataforma.anuncio))

resultado.independencia.plataforma.anuncio$statistic # X^2_obs
resultado.independencia.plataforma.anuncio$parameter # Graus de Liberdade
resultado.independencia.plataforma.anuncio$p.value   # P-Value
resultado.independencia.plataforma.anuncio$observed  # Oi = frequências Observadas
resultado.independencia.plataforma.anuncio$expected  # Ei = frequências Esperadas

### Decisão:

#### Pelo P-Value:

# P-Value = P(Q >= Q_obs) = 1 - F(Q_obs) = 0.2009
(round(
  (1 - pchisq(
    resultado.independencia.plataforma.anuncio$statistic,
    resultado.independencia.plataforma.anuncio$parameter
  )), 4
))

#### Como P-Value = 0.2009 > α = 0.05, não se rejeita H0.

### Conclusão:

#### Com base nas variáveis estudadas e com 5% de significância,
#### é possível afirmar que as variáveis plataforma e anúncio não
#### aparentam ter alguma relação entre si.
#### Com isto pode-se afirmar que os anúncios realizados não são
#### dependentes de uma plataforma em específico.

### Como não se observa uma relação entre as variáveis, não é
### necessário (nem faz sentido) calcular quaisquer medidas de
### associação.

"---------------------"

## Relação: Plataforma - Mercado: ####

### Para estudar a relação entre 2 variáveis qualitativas,
### usa-se o teste de independência do qui-quadrado.

### Hipóteses:

#### H0: As variáveis plataforma e mercado não estão relacionadas.
#### vs.
#### H1: As variáveis plataforma e mercado estão relacionadas.

### Confiança e Significância:

#### Os testes será realizado tendo em conta:
##### Confiaça = 0.95
##### Significância = 0.05

### Dados:

#### 2 Variáveis Qualitativas Nominais: Plataforma e Mercado.

(valores_plataforma <- table(DadosMarkDig$plataforma))
(length(names(valores_plataforma)))

(valores_mercado <- table(DadosMarkDig$mercado))
(length(names(valores_mercado)))

### Tabela de Contingência:

#### r = c = 3

#### Frequências Observadas (Oi):
(table(DadosMarkDig$plataforma, DadosMarkDig$mercado))


#### Tabela de Contingência:
(plataformas <- factor(DadosMarkDig$plataforma, levels = c("Facebook", "Google", "Instagram")))
(mercados <- factor(DadosMarkDig$mercado, levels = c("Alimentação", "Moda", "Tecnologia")))
(tabela.contingencia.plataforma.mercado <- table(plataformas, mercados))

### Teste de Independência do Qui-Quadrado:

(resultado.independencia.plataforma.mercado <- chisq.test(tabela.contingencia.plataforma.mercado))

resultado.independencia.plataforma.mercado$statistic # X^2_obs
resultado.independencia.plataforma.mercado$parameter # Graus de Liberdade
resultado.independencia.plataforma.mercado$p.value   # P-Value
resultado.independencia.plataforma.mercado$observed  # Oi = frequências Observadas
resultado.independencia.plataforma.mercado$expected  # Ei = frequências Esperadas

### Decisão:

#### Pelo P-Value:

# P-Value = P(Q >= Q_obs) = 1 - F(Q_obs) = 0.3423
(round(
  (1 - pchisq(
    resultado.independencia.plataforma.mercado$statistic,
    resultado.independencia.plataforma.mercado$parameter
  )), 4
))

#### Como P-Value = 0.3423 > α = 0.05, não se rejeita H0.

### Conclusão:

#### Com base nas variáveis estudadas e com 5% de significância,
#### é possível afirmar que as variáveis plataforma e mercado não
#### aparentam ter alguma relação entre si.
#### Com isto pode-se afirmar que as plataformas das campanhas não
#### são de um tipo de mercado em específico.

### Como não se observa uma relação entre as variáveis, não é
### necessário (nem faz sentido) calcular quaisquer medidas de
### associação.

"---------------------"

## Relação: Anúncio - Mercado: ####



"---------------------------------------------"

# Anãlises de Variáveis Quantitativas: ####

## relação: Investimento - Cliques: ####



"---------------------"

## Relação: Investimento - Conversões: ####



"---------------------"

## Relação: Cliques - Conversões: ####



"---------------------------------------------"

# Função para Simular Dados ####

simular_dados <- function() {
  # Parâmetros Necessários para Simular os Dados:
  n <- sample(234:254, 1)
  m <- c(10, 500, 50)
  s <- c(2, 150, 20)
  vcr <- c(sample(65:95, 1)/100, sample(65:95, 1)/100, sample(65:95, 1)/100)
  crm <- matrix(c(1, -vcr[1], vcr[2], -vcr[1], 1, -vcr[3], vcr[2], -vcr[3], 1), nrow = 3)
  dd <- MASS::mvrnorm(n, mu = m, Sigma = crm * (s %*% t(s)))
  plat <- sample(c("Google", "Facebook", "Instagram"), n, replace = TRUE, prob = c(0.4, 0.3, 0.3))
  ta <- sample(c("Banner", "Vídeo", "Texto"), n, replace = TRUE, prob = c(0.4, 0.3, 0.3))
  sm <- sample(c("Tecnologia", "Moda", "Alimentação"), n, replace = TRUE, prob = c(0.3, 0.3, 0.4))
  
  # Criar uma Tabela com os Dados
  DadosMarkDig <- data.frame(
    ID = 1:n,
    plataforma = plat,
    anuncio = ta,
    mercado = sm,
    investimento = dd[,1],
    cliques = as.integer(dd[,2]),
    conversoes = as.integer(dd[,3])
  )
  
  # Guardar a Tabela num Ficheiro .TXT
  write.table(
    DadosMarkDig,
    "DadosMarkDig.txt",
    row.names = FALSE,
    quote = FALSE
  )
}

"---------------------------------------------"

# Simulação e Geração da BD: ####

simular_dados()

"---------------------------------------------"
