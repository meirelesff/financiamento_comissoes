### ===================
### 4 - TABELA DO ANEXO
### ===================


# Pacotes necessarios
library(stargazer)
library(dplyr)


# Carrega os dados
load("data/dados.Rda")


# Cria e exporta a tabela de estatisticas descritivas da base
dados %>%
  dplyr::select(pos_lista_std, perc_votos_colig, idade, politico, empresario,
                advogado, medico, engenheiro, ens_superior, ens_medio, casado,
                mulher, capadr, cvt, cft, cme, ce, cdeics, perc_total_nacional,
                perc_doacao_capadr, perc_doacao_cvt, perc_doacao_cme,
                perc_doacao_cdeics, perc_doacao_cft, perc_doacao_ce) %>%
  as.data.frame() %>%
  stargazer(type = "html", summary = T, digits = 1, out = "outputs/tab_anexo.html")

rm(list = ls())
