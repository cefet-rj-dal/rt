library(tidyverse)
library(readr)
library(patchwork)

# --- prepare date for barplot  ---
prepara_dados <- function(path_com, path_sem, tipo) {
  com_rt <- read_csv(path_com)
  sem_rt <- read_csv(path_sem)
  
  colnames(com_rt) <- c("method", "serie", "F1", "precision", "recall")
  colnames(sem_rt) <- c("method", "serie", "F1", "precision", "recall")
  
  id_col <- c("method", "serie")
  
  com_rt <- com_rt %>% rename_with(~ paste0(., "_com_RT"), -all_of(id_col))
  sem_rt <- sem_rt %>% rename_with(~ paste0(., "_sem_RT"), -all_of(id_col))
  
  full_join(sem_rt, com_rt, by = id_col) %>%
    pivot_longer(
      cols = -all_of(id_col),
      names_to = c("metric", "RT"),
      names_pattern = "(.*)_(sem|com)_RT",
      values_to = "value"
    ) %>%
    mutate(tipo = tipo)
}

# --- file path ---
hetero_com <- "~/Tese/Artigo/metodos_hetero_hard_com-RT.csv"
hetero_sem <- "~/Tese/Artigo/metodos_hetero_hard_sem-RT.csv"
homo_com   <- "~/Tese/Artigo/metodos_homo_hard_com-RT.csv"
homo_sem   <- "~/Tese/Artigo/metodos_homo_hard_sem-RT.csv"

# --- data ---
dados_hetero <- prepara_dados(hetero_com, hetero_sem, "Heterocedástica")
dados_homo   <- prepara_dados(homo_com, homo_sem, "Homocedástica")

# --- function point plot ---
grafico_pontos_f1 <- function(path_com, path_sem, titulo) {
  com_rt <- read_csv(path_com)
  sem_rt <- read_csv(path_sem)
  
  names(com_rt) <- tolower(names(com_rt))
  names(sem_rt) <- tolower(names(sem_rt))
  
  colnames(com_rt)[colnames(com_rt) %in% c("f1", "precision", "recall")] <- paste0(colnames(com_rt)[colnames(com_rt) %in% c("f1", "precision", "recall")], "_com")
  colnames(sem_rt)[colnames(sem_rt) %in% c("f1", "precision", "recall")] <- paste0(colnames(sem_rt)[colnames(sem_rt) %in% c("f1", "precision", "recall")], "_sem")
  
  dados_comp <- inner_join(com_rt, sem_rt, by = c("serie", "method")) %>%
    mutate(melhorou_f1 = ifelse(!is.na(f1_com) & !is.na(f1_sem), f1_com > f1_sem, NA))
  
  resumo_pontos <- dados_comp %>%
    group_by(method) %>%
    summarise(
      f1_melhorias = sum(melhorou_f1, na.rm = TRUE),
      f1_total = sum(!is.na(melhorou_f1)),
      .groups = "drop"
    ) %>%
    mutate(
      percentual = round(100 * f1_melhorias / f1_total, 1),
      destaque = percentual > 50
    )
  
  ordem <- resumo_pontos %>% arrange(percentual) %>% pull(method)
    
  g <- ggplot(resumo_pontos, aes(x = percentual, y = factor(method, levels = ordem))) +
    geom_segment(aes(x = 0, xend = percentual, yend = method), color = "gray70") +
    geom_point(aes(color = destaque), size = 4) +
    geom_text(aes(label = paste0(percentual, "%"), x = percentual + 2), hjust = 0, size = 3.5) +
    scale_color_manual(values = c("TRUE" = "gray20", "FALSE" = "gray70")) +
    labs(
      title = titulo,
      x = "% of time series with improved F1 after RT",
      y = "Method",
      color = "Improved in >50% of the series"
    ) +
    xlim(0, 105) +
    theme_minimal() +
    theme(legend.position = "top")
  
  list(plot = g, ordem = ordem)
}

# --- function barplot ---
grafico_barras_f1 <- function(dados, titulo, ordem) {
  dados %>%
    filter(metric == "F1") %>%
    group_by(method, RT, tipo) %>%
    summarise(
      media = mean(value, na.rm = TRUE),
      erro = sd(value, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(method = factor(method, levels = rev(ordem))) %>%
    ggplot(aes(x = method, y = media, fill = RT)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(
      aes(ymin = media - erro, ymax = media + erro),
      width = 0.2,
      position = position_dodge(width = 0.8)
    ) +
    scale_fill_grey(start = 0.3, end = 0.8, name = "RT") +
    labs(
      title = titulo,
      x = "Method",
      y = "F1"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# --- plot heteroscedastic ---
gp_hetero <- grafico_pontos_f1(hetero_com, hetero_sem, "")
g_barras_hetero <- grafico_barras_f1(dados_hetero, "F1 for heteroscedastic series", gp_hetero$ordem)
g_pontos_hetero <- gp_hetero$plot

# --- plot homoscedastic ---
gp_homo <- grafico_pontos_f1(homo_com, homo_sem, "")
g_barras_homo <- grafico_barras_f1(dados_homo, "F1 for homoscedastic series", gp_homo$ordem)
g_pontos_homo <- gp_homo$plot

# --- combined ---
final_plot <- (g_barras_hetero / g_pontos_hetero) | (g_barras_homo / g_pontos_homo)

final_plot
