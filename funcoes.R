### ========================================
### FUNCAO PARA RODAR PERMUTACAO COM CLUSTER
### ========================================


# Pacotes
library(dplyr)


# Cria funcao para fazer as permutacoes (dentro de cada partido)
permuta_comis <- function(doacao, membro, partido, ano, sims = 10){
  
  
  # Media inicial
  inicial <- mean(doacao[membro == 1], na.rm = T)
  
  # Simula as medias redistribuindo os membros da comissao
  data <- data.frame(doacao, membro, partido, ano)
  
  simula <- function(data){
    
    data %>%
      group_by(ano, partido) %>%
      filter(row_number() %in% base::sample(1:n(), sum(membro, na.rm = T))) %>%
      ungroup() %>%
      summarise(doacao = mean(doacao, na.rm = T)) %>%
      as.numeric()
  }
  
  h_nula <- sapply(1:sims, function(x) simula(data = data))
  
  
  # Calcula o p-valor do teste
  p_valor <- sum(h_nula > inicial) / sims
  
  
  # Retorna
  out <- list(p_valor = p_valor, h_nula = h_nula, inicial = inicial)
  out
}


# Funcao para resumir resultados em tabela
tab_comis <- function(x, comissao = "comissao", setor = "setor") {
  
  rng <- range(x$h_nula) %>% 
    round(2) %>%
    paste(collapse = ", ") %>%
    paste0("[", ., "]")
  
  observ <- round(x$inicial, 2)
  
  p_valor <- round(x$p_valor, 2) %>%
    ifelse(. < 0.05, paste0(., "*"), .)
  
  data.frame(comissao = comissao, setor = setor, range = rng, observado = observ, p_valor = p_valor, stringsAsFactors = F)
}