### =======================
### 2- ANALISE MULTIVARIADA
### =======================


# Pacotes
library(stargazer)
library(fmeireles)
library(interplot)
library(ggplot2)
library(dplyr)
library(lme4)


# Carrega os dados
load("data/dados.Rda")


# Filtra os dados (remove suplentes) e cria algumas variaveis
dados <- dados %>%
  arrange(idecadastro, ano) %>%
  group_by(idecadastro) %>%
  mutate(pres_comissao = ifelse(nomecargo != "Membro", 1, 0),
         lideranca = ifelse(descricaocargolideranca != "Nenhum", 1, 0),
         partido_res = recode_factor(sigla_partido,
                                     `PT` = "PT",
                                     `PMDB` = "PMDB",
                                     `PSDB` = "PSDB",
                                     `DEM` = "DEM",
                                     `PFL` = "DEM",
                                     `PTB` = "PTB",
                                     `PDT` = "PDT",
                                     `PP` = "PP",
                                     .default = "Outros")) %>%
  ungroup() %>%
  filter(situacaomandato == "Em Exercício")


### Grafico 3 - Efeito do financiamento setorial na chance de entrar numa determinada comissao
# CAPADR
mod <- glm(capadr ~ perc_doacao_capadr + perc_total_nacional + ens_superior + ens_medio + 
             idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std,
           family = "binomial", data = dados)

mod2 <- glm(capadr ~ perc_doacao_capadr + perc_total_nacional + ens_superior + ens_medio + 
             idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std + 
             ufeleito + ano + partido_res, 
           family = "binomial", data = dados)

mod3 <- glmer(capadr ~ perc_doacao_capadr + perc_total_nacional + ens_superior + ens_medio + 
             mulher + casado + advogado + empresario + pos_lista_std + idade + I(idade^2) +
             (1 | ufeleito) + (1 | ano) + (1 | partido_res), 
           family = "binomial", data = dados)

# Salva tabela A - CAPADR
stargazer(mod, mod2, mod3, omit = c("ano", "ufeleito", "partido_res"), 
          apply.coef = exp, t.auto = F, p.auto = F, report = "vct*",
          out = "outputs/tabA.tex")

x <- broom::tidy(mod, conf.int = T)[c(2, 3), c(1, 2, 6, 7)]
x <- broom::tidy(mod2, conf.int = T)[c(2, 3), c(1, 2, 6, 7)] %>%
  bind_rows(x)
x <- broom::tidy(mod3, conf.int = T)[c(2, 3), c(1, 2, 6, 7)] %>%
  bind_rows(x)%>%
  mutate(modelo = rep(c("Efeitos mistos", "Efeitos fixos", "Pooled"), each = 2),
         comissao = "CAPADR",
         term = recode_factor(term, 
                              `perc_total_nacional` = "Todos os setores",
                              `perc_doacao_capadr` = "Setor afim"))


# CE
mod <- glm(ce ~ perc_doacao_ce + perc_total_nacional + ens_superior + ens_medio + 
             idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std,
           family = "binomial", data = dados)

mod2 <- glm(ce ~ perc_doacao_ce + perc_total_nacional + ens_superior + ens_medio + 
              idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std + 
              ufeleito + ano + partido_res, 
            family = "binomial", data = dados)

mod3 <- glmer(ce ~ perc_doacao_ce + perc_total_nacional + ens_superior + ens_medio + 
                mulher + casado + advogado + empresario + pos_lista_std + idade + I(idade^2) +
                (1 | ufeleito) + (1 | ano) + (1 | partido_res), 
              family = "binomial", data = dados)

# Salva tabela A2 - CE
stargazer(mod, mod2, mod3, omit = c("ano", "ufeleito", "partido_res"), 
          apply.coef = exp, t.auto = F, p.auto = F, report = "vct*", 
          out = "outputs/tabA2.tex")

x2 <- broom::tidy(mod, conf.int = T)[c(2, 3), c(1, 2, 6, 7)]
x2 <- broom::tidy(mod2, conf.int = T)[c(2, 3), c(1, 2, 6, 7)] %>%
  bind_rows(x2)
x2 <- broom::tidy(mod3, conf.int = T)[c(2, 3), c(1, 2, 6, 7)] %>%
  bind_rows(x2)%>%
  mutate(modelo = rep(c("Efeitos mistos", "Efeitos fixos", "Pooled"), each = 2),
         comissao = "CE",
         term = recode_factor(term, 
                              `perc_total_nacional` = "Todos os setores",
                              `perc_doacao_ce` = "Setor afim"))


# CEDEICS
mod <- glm(cdeics ~ perc_doacao_cdeics + perc_total_nacional + ens_superior + ens_medio + 
             idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std,
           family = "binomial", data = dados)

mod2 <- glm(cdeics ~ perc_doacao_cdeics + perc_total_nacional + ens_superior + ens_medio + 
              idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std + 
              ufeleito + ano + partido_res, 
            family = "binomial", data = dados)

mod3 <- glmer(cdeics ~ perc_doacao_cdeics + perc_total_nacional + ens_superior + ens_medio + 
                mulher + casado + advogado + empresario + pos_lista_std + idade + I(idade^2) +
                (1 | ufeleito) + (1 | ano) + (1 | partido_res), 
              family = "binomial", data = dados)

# Salva tabela A3 - CDEICS
stargazer(mod, mod2, mod3, omit = c("ano", "ufeleito", "partido_res"), 
          apply.coef = exp, t.auto = F, p.auto = F, report = "vct*",
          out = "outputs/tabA3.tex")

x3 <- broom::tidy(mod, conf.int = T)[c(2, 3), c(1, 2, 6, 7)]
x3 <- broom::tidy(mod2, conf.int = T)[c(2, 3), c(1, 2, 6, 7)] %>%
  bind_rows(x3)
x3 <- broom::tidy(mod3, conf.int = T)[c(2, 3), c(1, 2, 6, 7)] %>%
  bind_rows(x3)%>%
  mutate(modelo = rep(c("Efeitos mistos", "Efeitos fixos", "Pooled"), each = 2),
         comissao = "CDEICS",
         term = recode_factor(term, 
                              `perc_total_nacional` = "Todos os setores",
                              `perc_doacao_cdeics` = "Setor afim"))


# CFT
mod <- glm(cft ~ perc_doacao_cft + perc_total_nacional + ens_superior + ens_medio + 
             idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std,
           family = "binomial", data = dados)

mod2 <- glm(cft ~ perc_doacao_cft + perc_total_nacional + ens_superior + ens_medio + 
              idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std + 
              ufeleito + ano + partido_res, 
            family = "binomial", data = dados)

mod3 <- glmer(cft ~ perc_doacao_cft + perc_total_nacional + ens_superior + ens_medio + 
                mulher + casado + advogado + empresario + pos_lista_std + idade + I(idade^2) +
                (1 | ufeleito) + (1 | ano) + (1 | partido_res), 
              family = "binomial", data = dados)

# Salva tabela A4 - CFT
stargazer(mod, mod2, mod3, omit = c("ano", "ufeleito", "partido_res"), 
          apply.coef = exp, t.auto = F, p.auto = F, report = "vct*",
          out = "outputs/tabA4.tex")

x4 <- broom::tidy(mod, conf.int = T)[c(2, 3), c(1, 2, 6, 7)]
x4 <- broom::tidy(mod2, conf.int = T)[c(2, 3), c(1, 2, 6, 7)] %>%
  bind_rows(x4)
x4 <- broom::tidy(mod3, conf.int = T)[c(2, 3), c(1, 2, 6, 7)] %>%
  bind_rows(x4)%>%
  mutate(modelo = rep(c("Efeitos mistos", "Efeitos fixos", "Pooled"), each = 2),
         comissao = "CFT",
         term = recode_factor(term, 
                              `perc_total_nacional` = "Todos os setores",
                              `perc_doacao_cft` = "Setor afim"))


# CME
mod <- glm(cme ~ perc_doacao_cme + perc_total_nacional + ens_superior + ens_medio + 
             idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std,
           family = "binomial", data = dados)

mod2 <- glm(cme ~ perc_doacao_cme + perc_total_nacional + ens_superior + ens_medio + 
              idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std + 
              ufeleito + ano + partido_res, 
            family = "binomial", data = dados)

mod3 <- glmer(cme ~ perc_doacao_cme + perc_total_nacional + ens_superior + ens_medio + 
                mulher + casado + advogado + empresario + pos_lista_std + idade + I(idade^2) +
                (1 | ufeleito) + (1 | ano) + (1 | partido_res), 
              family = "binomial", data = dados)

# Salva tabela A5 - CME
stargazer(mod, mod2, mod3, omit = c("ano", "ufeleito", "partido_res"), 
          apply.coef = exp, t.auto = F, p.auto = F, report = "vct*",
          out = "outputs/tabA5.tex")

x5 <- broom::tidy(mod, conf.int = T)[c(2, 3), c(1, 2, 6, 7)]
x5 <- broom::tidy(mod2, conf.int = T)[c(2, 3), c(1, 2, 6, 7)] %>%
  bind_rows(x5)
x5 <- broom::tidy(mod3, conf.int = T)[c(2, 3), c(1, 2, 6, 7)] %>%
  bind_rows(x5)%>%
  mutate(modelo = rep(c("Efeitos mistos", "Efeitos fixos", "Pooled"), each = 2),
         comissao = "CME",
         term = recode_factor(term, 
                              `perc_total_nacional` = "Todos os setores",
                              `perc_doacao_cme` = "Setor afim"))


# CVT
mod <- glm(cvt ~ perc_doacao_cvt + perc_total_nacional + ens_superior + ens_medio + 
             idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std,
           family = "binomial", data = dados)

mod2 <- glm(cvt ~ perc_doacao_cvt + perc_total_nacional + ens_superior + ens_medio + 
              idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std + 
              ufeleito + ano + partido_res, 
            family = "binomial", data = dados)

mod3 <- glmer(cvt ~ perc_doacao_cvt + perc_total_nacional + ens_superior + ens_medio + 
                mulher + casado + advogado + empresario + pos_lista_std + idade + I(idade^2) +
                (1 | ufeleito) + (1 | ano) + (1 | partido_res), 
              family = "binomial", data = dados)

# Salva tabela A6 - CVT
stargazer(mod, mod2, mod3, omit = c("ano", "ufeleito", "partido_res"), 
          apply.coef = exp, t.auto = F, p.auto = F, report = "vct*",
          out = "outputs/tabA6.tex")

x6 <- broom::tidy(mod, conf.int = T)[c(2, 3), c(1, 2, 6, 7)]
x6 <- broom::tidy(mod2, conf.int = T)[c(2, 3), c(1, 2, 6, 7)] %>%
  bind_rows(x6)
x6 <- broom::tidy(mod3, conf.int = T)[c(2, 3), c(1, 2, 6, 7)] %>%
  bind_rows(x6)%>%
  mutate(modelo = rep(c("Efeitos mistos", "Efeitos fixos", "Pooled"), each = 2),
         comissao = "CVT",
         term = recode_factor(term, 
                              `perc_total_nacional` = "Todos os setores",
                              `perc_doacao_cvt` = "Setor afim"))


# Junta as estimativas de cada comissao num mesmo df
z <- bind_rows(list(x, x2, x3, x4, x5, x6))


# Salva o grafico
#pdf("outputs/grafico3.pdf", height = 6, width = 9)
png("outputs/grafico3.png", height = 6, width = 9, unit = "in", res = 500)
ggplot(z, aes(ymin = conf.low, ymax = conf.high, y = estimate, x = factor(term), shape = factor(modelo))) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_pointrange(position = position_dodge(width = 1)) +
  coord_flip() +
  labs(shape = NULL, y = "Estimativa\n(log odds)", x = NULL) +
  facet_wrap(~ comissao, scales = "free_x") +
  theme_cp()
dev.off()

rm(x, x2, x3, x4, x5, x6, z, mod, mod2, mod3)
gc()


# Tabela 2 - Efeito ao longo do tempo
capadr <- glm(capadr ~ perc_doacao_capadr * numlegislatura + perc_total_nacional + ens_superior + ens_medio + 
             idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
             ufeleito + partido_res,
           family = "binomial", data = dados) %>%
  interplot(var1 = "perc_doacao_capadr", var2 = "numlegislatura", plot = F) %>%
  mutate(comissao = "CAPADR")

ce <- glm(capadr ~ perc_doacao_ce * numlegislatura + perc_total_nacional + ens_superior + ens_medio + 
                idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
                ufeleito + partido_res,
              family = "binomial", data = dados) %>%
  interplot(var1 = "perc_doacao_ce", var2 = "numlegislatura", plot = F) %>%
  mutate(comissao = "CE")

cdeics <- glm(capadr ~ perc_doacao_cdeics * numlegislatura + perc_total_nacional + ens_superior + ens_medio + 
            idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
            ufeleito + partido_res,
          family = "binomial", data = dados) %>%
  interplot(var1 = "perc_doacao_cdeics", var2 = "numlegislatura", plot = F) %>%
  mutate(comissao = "CDEICS")

cft <- glm(capadr ~ perc_doacao_cft * numlegislatura + perc_total_nacional + ens_superior + ens_medio + 
                idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
                ufeleito + partido_res,
              family = "binomial", data = dados) %>%
  interplot(var1 = "perc_doacao_cft", var2 = "numlegislatura", plot = F) %>%
  mutate(comissao = "CFT")

cme <- glm(capadr ~ perc_doacao_cme * numlegislatura + perc_total_nacional + ens_superior + ens_medio + 
             idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
             ufeleito + partido_res,
           family = "binomial", data = dados) %>%
  interplot(var1 = "perc_doacao_cme", var2 = "numlegislatura", plot = F) %>%
  mutate(comissao = "CME")

cvt <- glm(capadr ~ perc_doacao_cvt * numlegislatura + perc_total_nacional + ens_superior + ens_medio + 
             idade + I(idade^2) + mulher + casado + advogado + empresario + pos_lista_std +
             ufeleito + partido_res,
           family = "binomial", data = dados) %>%
  interplot(var1 = "perc_doacao_cvt", var2 = "numlegislatura", plot = F) %>%
  mutate(comissao = "CVT")

x <- bind_rows(list(capadr, ce, cdeics, cft, cme, cvt))


# Salva o grafico
#pdf("outputs/grafico4.pdf", height = 6, width = 9)
png("outputs/grafico4.png", height = 6, width = 9, unit = "in", res = 500)
ggplot(x, aes(ymin = lb, ymax = ub, y = coef, x = numlegislatura)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ comissao, scales = "free_y") +
  labs(x = "Legislatura", y = "Efeito marginal do\nfinanciamento de setor afim\n(log odds)") +
  theme_cp()
dev.off()


rm(list = ls())
gc()

