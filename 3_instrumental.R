### ====================
### 3 - VAR INSTRUMENTAL
### ====================


# Pacotes
library(stargazer)
library(fmeireles)
library(dplyr)
library(tidyr)
library(AER)


# Carrega os dados
load("data/dados.Rda")


# Filtra os dados (remove suplentes) e cria algumas variaveis
dados <- dados %>%
  arrange(idecadastro, ano) %>%
  group_by(idecadastro) %>%
  mutate(pres_comissao = ifelse(nomecargo != "Membro", 1, 0),
         lideranca = ifelse(descricaocargolideranca != "Nenhum", 1, 0),
         incumbente = lag(rep(1, n()), n = 1, default = 0),
         doacao_capadr_lag = lag(perc_doacao_capadr, n = 1, default = 0),
         doacao_ce_lag = lag(perc_doacao_ce, n = 1, default = 0),
         doacao_cdeics_lag = lag(perc_doacao_cdeics, n = 1, default = 0),
         doacao_cft_lag = lag(perc_doacao_cft, n = 1, default = 0),
         doacao_cme_lag = lag(perc_doacao_cme, n = 1, default = 0),
         doacao_cvt_lag = lag(perc_doacao_cvt, n = 1, default = 0),
         partido_res = recode_factor(sigla_partido,
                                     `PT` = "PT",
                                     `PMDB` = "PMDB",
                                     `PSDB` = "PSDB",
                                     `DEM` = "DEM",
                                     `PFL` = "DEM",
                                     `PTB` = "PTB",
                                     `PDT` = "PDT",
                                     `PP` = "PP",
                                     .default = "Outros"),
         partido_res_lag = lag(partido_res, n = 1, default = NA),
         capadr_lag = lag(capadr, n = 1)) %>%
  ungroup() %>%
  filter(situacaomandato == "Em Exercício" & ano != "2002")


# CAPADR
mod1 <- dados %>%
  rename(doacao_setor = perc_doacao_capadr) %>%
  ivreg(capadr ~ doacao_setor + ens_superior + ens_medio + 
             idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
             ano + ufeleito + partido_res | 
             doacao_capadr_lag + partido_res_lag + ens_superior + ens_medio + 
             idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
             ano + ufeleito + partido_res, data = .)


est1 <- summary(mod1, diagnostics = T)$diagnostics[, 4] %>%
  round(2)


# CE
mod2 <- dados %>%
  rename(doacao_setor = perc_doacao_ce) %>%
  ivreg(ce ~ doacao_setor + ens_superior + ens_medio + 
          idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
          ano + ufeleito + partido_res | 
          doacao_ce_lag + partido_res_lag + ens_superior + ens_medio + 
          idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
          ano + ufeleito + partido_res, data = .)


est2 <- summary(mod2, diagnostics = T)$diagnostics[, 4] %>%
  round(2)

# CDEICS
mod3 <- dados %>%
  rename(doacao_setor = perc_doacao_cdeics) %>%
  ivreg(cdeics ~ doacao_setor + ens_superior + ens_medio + 
          idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
          ano + ufeleito + partido_res | 
          doacao_cdeics_lag + partido_res_lag + ens_superior + ens_medio + 
          idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
          ano + ufeleito + partido_res, data = .)


est3 <- summary(mod3, diagnostics = T)$diagnostics[, 4] %>%
  round(2)


# CFT
mod4 <- dados %>%
  rename(doacao_setor = perc_doacao_cft) %>%
  ivreg(cft ~ doacao_setor + ens_superior + ens_medio + 
          idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
          ano + ufeleito + partido_res | 
          doacao_cft_lag + partido_res_lag + ens_superior + ens_medio + 
          idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
          ano + ufeleito + partido_res, data = .)


est4 <- summary(mod4, diagnostics = T)$diagnostics[, 4] %>%
  round(2)


# CME
mod5 <- dados %>%
  rename(doacao_setor = perc_doacao_cme) %>%
  ivreg(cme ~ doacao_setor + ens_superior + ens_medio + 
          idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
          ano + ufeleito + partido_res | 
          doacao_cme_lag + partido_res_lag + ens_superior + ens_medio + 
          idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
          ano + ufeleito + partido_res, data = .)


est5 <- summary(mod5, diagnostics = T)$diagnostics[, 4] %>%
  round(2)


# CVT
mod6 <- dados %>%
  rename(doacao_setor = perc_doacao_cvt) %>%
  ivreg(cvt ~ doacao_setor + ens_superior + ens_medio + 
          idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
          ano + ufeleito + partido_res | 
          doacao_cvt_lag + partido_res_lag + ens_superior + ens_medio +
          idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
          ano + ufeleito + partido_res, data = .)


est6 <- summary(mod6, diagnostics = T)$diagnostics[, 4] %>%
  round(2)



stargazer(mod1, mod2, mod3, mod4, mod5, mod6,
          type = "html", keep = c("doacao_setor"), out = "tab_instr.html",
          add.lines = list(c("Instrumento fraco", est1[1], est2[1], est3[1], est4[1], est5[1], est6[1]),
                           c("Wu-Hausman", est1[2], est2[2], est3[2], est4[2], est5[2], est6[2]),
                           c("Sargan", est1[3], est2[3], est3[3], est4[3], est5[3], est6[3])))


