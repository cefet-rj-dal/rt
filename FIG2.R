# packages
library(ggplot2)
library(dplyr)
library(readr)

# read CSV
red <- read_csv("Tese/Artigo/RTAD_hetero_hard.csv")
outros <- read_csv("Tese/Artigo/metodos_hetero_hard_sem-RT.csv")

dados <- rbind(red, outros)

dados <- dados %>%
  mutate(method = factor(method, levels = names(sort(tapply(F1, method, median, na.rm = TRUE), decreasing = TRUE))))

# RTAD highlited
dados$destacar <- ifelse(dados$method == "RTAD", "destacado", "outros")

# creating boxplot
ggplot(dados, aes(x = method, y = F1, fill = destacar)) +
  geom_boxplot() +
  scale_fill_manual(values = c("destacado" = "gray95", "outros" = "gray70")) +
  theme_minimal() +
  labs(title = "Set of heterocedastic series",
       x = "Detection Method",
       y = "F1-score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

