Financiamento de campanhas e composição das Comissões na Câmara
================
Fernando Meireles e Denisson Silva

Este repo contém o código de replicação do capítulo "O dinheiro fala mais alto? Financiamento de campanhas e composição das Comissões Permanentes na Câmara". Para replicar os resultados, basta dar source nos scripts disponíveis na sequência numérica:

``` r
source("1_descritivas.R")
source("2_multivariada.R")
source("3_instrumental.R")
source("4_anexo.R")
```

Os scripts reproduzirão os resultados e salvarrão as figuras e tabelas usadas no capítulos na pasta `output`, dentro do diretório corrente no `R` (`getwd()` para localizá-lo). Os dados limpos usados no capítulo estão na pasta `data` em formato `Rdata`; para abri-los, use:

``` r
load("data/dados.Rda")
```

Para reproduzir os resultados, também é necessário ter os seguintes pacotes instalados:

``` r
install.packages(c("stargazer", "gridExtra", "ggplot2", "dplyr", "tidyr"))
```

Para usar o mesmo tema do `ggplot2`, é necessário instalar o pacote `fmeireles`, não disponível no CRAN:

``` r
# Checa se o pacote devtools está instalado
if(!require(devtools)) install.packages("devtools")

# Instala o pacote fmeireles do github
devtools::install_github("meirelesff/fmeireles")
```

O capítulo também usa testes não-paramétricos de permutação com *cluster*, que são implementados com a função `permuta_comis`, disponível no script `funcoes.R`.

### Informações da sessão

    ## R version 3.3.2 (2016-10-31)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 15063)
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.1252  LC_CTYPE=Portuguese_Brazil.1252   
    ## [3] LC_MONETARY=Portuguese_Brazil.1252 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] backports_1.0.4 magrittr_1.5    rprojroot_1.2   tools_3.3.2    
    ##  [5] htmltools_0.3.5 yaml_2.1.14     Rcpp_0.12.10    stringi_1.1.5  
    ##  [9] rmarkdown_1.5   knitr_1.15.1    stringr_1.2.0   digest_0.6.12  
    ## [13] evaluate_0.10
