library(dplyr)
library(rstatix)
library(tidyverse)
library(readr)
library(ggplot2)
library(gridExtra)

# Data
red_homo <- read_csv("~/Tese/Artigo/RTAD_homo_hard.csv")
outros_homo <- read_csv("~/Tese/Artigo/metodos_homo_hard_sem-RT.csv")
df_homo <- bind_rows(red_homo, outros_homo)

red_hetero <- read_csv("~/Tese/Artigo/RTAD_hetero_hard.csv")
outros_hetero <- read_csv("~/Tese/Artigo/metodos_hetero_hard_sem-RT.csv")
df_hetero <- bind_rows(red_hetero, outros_hetero)

# Calculating effect size
analisa_conjunto <- function(df) {
  metodos <- unique(df$method)
  metodos <- setdiff(metodos, "RTAD")
  
  resultados <- data.frame()
  
  for (m in metodos) {
    series_comuns <- intersect(
      filter(df, method == "RTAD")$serie,
      filter(df, method == m)$serie
    )
    
    extrair_metricas <- function(metrica_nome) {
      red <- filter(df, method == "RTAD", serie %in% series_comuns) %>% arrange(serie) %>% pull(metrica_nome)
      outro <- filter(df, method == m, serie %in% series_comuns) %>% arrange(serie) %>% pull(metrica_nome)
      validos <- !is.na(red) & !is.na(outro)
      list(red = red[validos], outro = outro[validos])
    }
    
    for (metrica in c("F1", "precision", "recall")) {
      dados <- extrair_metricas(metrica)
      
      # Effect size
      p_valor <- wilcox.test(dados$red, dados$outro, alternative = "greater", paired = TRUE)$p.value
      r <- wilcox_effsize(
        data.frame(valor = c(dados$red, dados$outro),
                   metodo = rep(c("RTAD", m), each = length(dados$red))),
        valor ~ metodo, paired = TRUE
      )$effsize
      
      # Median
      med_rtad  <- median(filter(df, method == "RTAD")[[metrica]], na.rm = TRUE)
      med_outro <- median(filter(df, method == m)[[metrica]], na.rm = TRUE)
      mostrar_valor <- med_rtad >= med_outro
      
      resultados <- rbind(resultados, data.frame(
        metodo = m,
        metrica = metrica,
        effect_size = r,
        p_valor = p_valor,
        mostrar_valor = mostrar_valor
      ))
    }
  }
  
  return(resultados)
}

# run
resultados_homo   <- analisa_conjunto(df_homo)   %>% mutate(grupo = "homo")
resultados_hetero <- analisa_conjunto(df_hetero) %>% mutate(grupo = "hetero")

# join
todos_resultados <- bind_rows(resultados_homo, resultados_hetero)

# labels
todos_resultados <- todos_resultados %>%
  mutate(label = case_when(
    mostrar_valor & p_valor < 0.05 ~ paste0("underline(", format(round(effect_size, 2), nsmall = 2), ")"),
    mostrar_valor & p_valor >= 0.05 ~ format(round(effect_size, 2), nsmall = 2),
    !mostrar_valor & p_valor < 0.05 ~ paste0("underline(", format(round(effect_size, 2), nsmall = 2), "*'*')"),
    TRUE ~ paste0(format(round(effect_size, 2), nsmall = 2), "*'*'")
  ))

# range
min_effect <- min(todos_resultados$effect_size, na.rm = TRUE)
max_effect <- max(todos_resultados$effect_size, na.rm = TRUE)

# generate plots
criar_grafico <- function(grupo_nome, titulo) {
  ggplot(filter(todos_resultados, grupo == grupo_nome), aes(x = metrica, y = metodo, fill = effect_size)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), size = 4, parse = TRUE) +
    scale_fill_gradient2(
      low = "white", mid = "grey90", high = "grey40",
      midpoint = 0.3,
      limits = c(min_effect, max_effect)
    ) +
    labs(title = titulo,
         x = "Metric",
         y = "Method") +
    theme_minimal(base_size = 9)
}

grafico_hetero <- criar_grafico("hetero", "Heteroscedastic series")
grafico_homo   <- criar_grafico("homo", "Homoscedastic series")

# show
grid.arrange(grafico_hetero, grafico_homo, ncol = 2)

