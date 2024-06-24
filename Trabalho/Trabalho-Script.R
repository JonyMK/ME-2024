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

# P-Value = P(Q >= Q_obs) = 1 - F(Q_obs) = 0.0071
(round(
  (1 - pchisq(
    resultado.independencia.plataforma.anuncio$statistic,
    resultado.independencia.plataforma.anuncio$parameter
  )), 4
))

#### Como P-Value = 0.0071 <= α = 0.05, rejeita-se H0.

### Conclusão:

#### Com base nas variáveis estudadas e com 5% de significância,
#### é possível afirmar que as variáveis plataforma e anúncio
#### aparentam estar relacionadas entre si.
#### Com isto pode-se afirmar que os anúncios realizados aparenta
#### ser dependentes de uma plataforma em específico.

### Medidas de Associação:

#### Como se observa uma relação entre as variáveis, pode-se
#### calcular as medidas de associação, para se perceber se é uma
#### relação mais forte ou mais fraca.

#### Coeficiente de Contingência: 0.2344
(round(DescTools::ContCoef(tabela.contingencia.plataforma.anuncio), 4))
##### Resultado: [ 0.10 , 0.30 [ => Fraca

#### Coeficiente V de Crámer: 0.1705
(round(DescTools::CramerV(tabela.contingencia.plataforma.anuncio), 4))
##### Resultado: [ 0.07 , 0.20 [ => Fraca

#### Coeficiente Tb de Kendall: -0.0268
(round(DescTools::KendallTauB(tabela.contingencia.plataforma.anuncio), 4))
##### Resultado: Muito Próximo de 0 => Fraca

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

# P-Value = P(Q >= Q_obs) = 1 - F(Q_obs) = 0.459
(round(
  (1 - pchisq(
    resultado.independencia.plataforma.mercado$statistic,
    resultado.independencia.plataforma.mercado$parameter
  )), 4
))

#### Como P-Value = 0.459 > α = 0.05, não se rejeita H0.

### Conclusão:

#### Com base nas variáveis estudadas e com 5% de significância,
#### é possível afirmar que as variáveis plataforma e mercado não
#### aparentam ter alguma relação entre si.
#### Com isto pode-se afirmar que as plataformas das campanhas não
#### são de um tipo de mercado em específico.

### Medidas de Associação:

#### Como não se observa uma relação entre as variáveis, não é
#### necessário (nem faz sentido) calcular quaisquer medidas de
#### associação.

"---------------------"

## Relação: Anúncio - Mercado: ####

### Para estudar a relação entre 2 variáveis qualitativas,
### usa-se o teste de independência do qui-quadrado.

### Hipóteses:

#### H0: As variáveis anúncio e mercado não estão relacionadas.
#### vs.
#### H1: As variáveis anúncio e mercado estão relacionadas.

### Confiança e Significância:

#### Os testes será realizado tendo em conta:
##### Confiaça = 0.95
##### Significância = 0.05

### Dados:

#### 2 Variáveis Qualitativas Nominais: Anúncio e Mercado.

(valores_anuncio <- table(DadosMarkDig$anuncio))
(length(names(valores_anuncio)))

(valores_mercado <- table(DadosMarkDig$mercado))
(length(names(valores_mercado)))

### Tabela de Contingência:

#### r = c = 3

#### Frequências Observadas (Oi):
(table(DadosMarkDig$anuncio, DadosMarkDig$mercado))


#### Tabela de Contingência:
(anuncios <- factor(DadosMarkDig$anuncio, levels = c("Banner", "Texto", "Vídeo")))
(mercados <- factor(DadosMarkDig$mercado, levels = c("Alimentação", "Moda", "Tecnologia")))
(tabela.contingencia.anuncio.mercado <- table(anuncios, mercados))

### Teste de Independência do Qui-Quadrado:

(resultado.independencia.anuncio.mercado <- chisq.test(tabela.contingencia.anuncio.mercado))

resultado.independencia.anuncio.mercado$statistic # X^2_obs
resultado.independencia.anuncio.mercado$parameter # Graus de Liberdade
resultado.independencia.anuncio.mercado$p.value   # P-Value
resultado.independencia.anuncio.mercado$observed  # Oi = frequências Observadas
resultado.independencia.anuncio.mercado$expected  # Ei = frequências Esperadas

### Decisão:

#### Pelo P-Value:

# P-Value = P(Q >= Q_obs) = 1 - F(Q_obs) = 0.7977
(round(
  (1 - pchisq(
    resultado.independencia.anuncio.mercado$statistic,
    resultado.independencia.anuncio.mercado$parameter
  )), 4
))

#### Como P-Value = 0.0572 > α = 0.05, não se rejeita H0.

### Conclusão:

#### Com base nas variáveis estudadas e com 5% de significância,
#### é possível afirmar que as variáveis anúncio e mercado não
#### aparentam ter alguma relação entre si.
#### Com isto pode-se afirmar que os anúncios das campanhas não
#### são de um tipo de mercado em específico.

### Medidas de Associação:

#### Como não se observa uma relação entre as variáveis, não é
#### necessário (nem faz sentido) calcular quaisquer medidas de
#### associação.

"---------------------------------------------"

# Anãlises de Variáveis Quantitativas: ####

## relação: Investimento - Cliques: ####



"---------------------"

## Relação: Investimento - Conversões: ####



"---------------------"

## Relação: Cliques - Conversões: ####



"---------------------------------------------"

# Simulação e Geração da BD: ####

simular_dados()

"---------------------"

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
