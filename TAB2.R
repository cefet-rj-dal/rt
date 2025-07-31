# packages
library(dplyr)
library(ggplot2)
library(readr)
library(rstatix)
library(tidyr)

# reading data
com_red <- read_csv("~/Tese/Artigo/metodos_homo_hard_com-RT.csv")
sem_red <- read_csv("~/Tese/Artigo/metodos_homo_hard_sem-RT.csv")

# ordering
com_red <- com_red %>% arrange(method, serie)
sem_red <- sem_red %>% arrange(method, serie)

# metrics
metricas <- c("F1", "precision", "recall")

# strutured data
tabela_final <- data.frame()

# Loop by method
for (m in unique(com_red$method)) {
  linha <- list(Metodo = m)
  
  for (metrica in metricas) {
    x <- com_red %>% filter(method == m) %>% pull(metrica)
    y <- sem_red %>% filter(method == m) %>% pull(metrica)
    
    # Wilcoxon test
    wilcox <- wilcox.test(x, y, alternative = "greater", paired = TRUE, exact = FALSE)
    
    # Effect size
    df_temp <- data.frame(
      id = 1:length(x),
      x = x,
      y = y
    ) %>%
      pivot_longer(cols = c(x, y), names_to = "grupo", values_to = "valor")
    
    r <- wilcox_effsize(df_temp, valor ~ grupo, paired = TRUE)$effsize
    
    # interpretation
    interpret <- case_when(
      abs(r) < 0.1 ~ "desprezível",
      abs(r) < 0.3 ~ "pequeno",
      abs(r) < 0.5 ~ "médio",
      TRUE ~ "grande"
    )
    
    # add colunms
    linha[[paste0(metrica, "_pval")]] <- round(wilcox$p.value, 4)
    linha[[paste0(metrica, "_effsize")]] <- round(r, 3)
    linha[[paste0(metrica, "_interp")]] <- interpret
  }
  
  tabela_final <- bind_rows(tabela_final, linha)
}

# show table
print(tabela_final)
