### ============================
### 1 - ESTATISTICAS DESCRITIVAS
### ============================


# Pacotes necessarios
library(stargazer)
library(gridExtra)
library(fmeireles)
library(ggplot2)
library(dplyr)
library(tidyr)


# Carrega as funcoes necessarias
source("funcoes.R")


# Cria um diretorio pra salvar graficos e tabelas
if(!dir.exists("outputs")) dir.create("outputs")


# Carrega os dados
load("data/dados.Rda")


# Define uma seed e o numero de simulacoes
set.seed(123)
sims <- 10000


# Grafico 1 - Doacoes do setor em todas as comissoes por legislatura
png("outputs/grafico1.png", height = 6, width = 9, unit = "in", res = 500)
dados %>%
  dplyr::select(c(ano, capadr, cvt:cft, cme, ce, cdeics, perc_doacao_capadr:perc_doacao_ctur)) %>%
  gather(comissao, membro, -ano, -c(perc_doacao_capadr:perc_doacao_ctur)) %>%
  group_by(ano, comissao, membro) %>%
  summarise_all(funs(mean(., na.rm = T))) %>%
  gather(sigla, media, -ano, -comissao, - membro) %>%
  group_by(ano, comissao, membro) %>%
  filter(grepl(comissao, sigla)) %>%
  ungroup() %>%
  mutate(comissao = toupper(comissao),
         membro = recode_factor(membro, `0` = "Não-membros", `1` = "Membros da comissão")) %>%
  ggplot(aes(x = ano, y = media, fill = membro)) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_wrap(~ comissao, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("white", "gray10")) +
  labs(x = NULL, y = "Média de doações do setor afim à Comissão (%)", fill = NULL) +
  theme_cp(tam.fonte = 13) 
dev.off()


# Grafico 2 - Exemplo de teste de permutacao (CAPADR, todo o periodo)
x <- permuta_comis(dados$perc_doacao_capadr, dados$capadr, dados$sigla_partido, dados$ano, sims)
x2 <- permuta_comis(dados$perc_doacao_cdeics, dados$capadr, dados$sigla_partido, dados$ano, sims)
x3 <- permuta_comis(dados$perc_doacao_cme, dados$capadr, dados$sigla_partido, dados$ano, sims)
x4 <- permuta_comis(dados$perc_doacao_cft, dados$capadr, dados$sigla_partido, dados$ano, sims)
x5 <- permuta_comis(dados$perc_doacao_cvt, dados$capadr, dados$sigla_partido, dados$ano, sims)
x6 <- permuta_comis(dados$perc_doacao_ce, dados$capadr, dados$sigla_partido, dados$ano, sims)

y <- data.frame(doacao = factor(rep(c("Agrícola", "Indústria", "Mineração e Energia", "Finanças", "Viação e Transportes", "Educação"), each = sims)),
           inicial = rep(c(x$inicial, x2$inicial, x3$inicial, x4$inicial, x5$inicial, x6$inicial), each = sims),
           valor = c(x$h_nula, x2$h_nula, x3$h_nula, x4$h_nula, x5$h_nula, x6$h_nula))

p_valores <- c(x$p_valor, x6$p_valor, x4$p_valor, x2$p_valor, x3$p_valor, x5$p_valor) %>%
  round(2) %>%
  ifelse(. < 0.05, paste0(., "*"), .) %>%
  paste0("P-valor =\n", .)

#pdf("outputs/grafico2.pdf", height = 6, width = 9)
png("outputs/grafico2.png", height = 5, width = 8, unit = "in", res = 500)
ggplot(y, aes(x = valor)) + 
  facet_wrap(~ doacao) +
  geom_histogram(fill = "white", color = "black") +
  geom_vline(data = y, aes(xintercept = inicial), linetype = 2, size = 0.6) +
  annotate("text", x = .235, y = 3000, label = p_valores, size = 3.5) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 0.43)) +
  labs(x = "Média de doações do setor afim à Comissão (%)", y = "Frequência") +
  theme_cp()
dev.off()

rm(y, x, x2, x3, x4, x5, x5, x6, p_valores)


# Tabela 1 - Testes de placebo com outros setores
# CAPADR
x <- permuta_comis(dados$perc_doacao_capadr, dados$capadr, dados$sigla_partido, dados$ano, sims)
x2 <- permuta_comis(dados$perc_total_nacional, dados$capadr, dados$sigla_partido, dados$ano, sims)
tab1 <- tab_comis(x, "CAPADR", "Agrícola")
tab1 <- rbind(tab1, tab_comis(x2, "CAPADR", "Todos os setores"))

# CE
x <- permuta_comis(dados$perc_doacao_ce, dados$ce, dados$sigla_partido, dados$ano, sims)
x2 <- permuta_comis(dados$perc_total_nacional, dados$ce, dados$sigla_partido, dados$ano, sims)
tab1 <- rbind(tab1, tab_comis(x, "CE", "Educação"))
tab1 <- rbind(tab1, tab_comis(x2, "CE", "Todos os setores"))


# CDEICS
x <- permuta_comis(dados$perc_doacao_cdeics, dados$cdeics, dados$sigla_partido, dados$ano, sims)
x2 <- permuta_comis(dados$perc_total_nacional, dados$cdeics, dados$sigla_partido, dados$ano, sims)
tab1 <- rbind(tab1, tab_comis(x, "CDEICS", "Indústria"))
tab1 <- rbind(tab1, tab_comis(x2, "CDEICS", "Todos os setores"))


# CFT
x <- permuta_comis(dados$perc_doacao_cft, dados$cft, dados$sigla_partido, dados$ano, sims)
x2 <- permuta_comis(dados$perc_total_nacional, dados$cft, dados$sigla_partido, dados$ano, sims)
tab1 <- rbind(tab1, tab_comis(x, "CFT", "Finanças"))
tab1 <- rbind(tab1, tab_comis(x2, "CFT", "Todos os setores"))


# CME
x <- permuta_comis(dados$perc_doacao_cme, dados$cme, dados$sigla_partido, dados$ano, sims)
x2 <- permuta_comis(dados$perc_total_nacional, dados$cme, dados$sigla_partido, dados$ano, sims)
tab1 <- rbind(tab1, tab_comis(x, "CME", "Mineração e Energia"))
tab1 <- rbind(tab1, tab_comis(x2, "CME", "Todos os setores"))


# CVT
x <- permuta_comis(dados$perc_doacao_cvt, dados$cvt, dados$sigla_partido, dados$ano, sims)
x2 <- permuta_comis(dados$perc_total_nacional, dados$cvt, dados$sigla_partido, dados$ano, sims)
tab1 <- rbind(tab1, tab_comis(x, "CVT", "Viação e Transportes"))
tab1 <- rbind(tab1, tab_comis(x2, "CVT", "Todos os setores"))


# Salva
stargazer(tab1, summary = F, type = "html", rownames = F, digits = 2, out = "outputs/tab1.html")

rm(list = ls())
gc()
