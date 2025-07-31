# Packages
library(ggplot2)
library(dplyr)

# Data
dados <- read_csv("Artigo/metrics_dispersion-measures.csv")

# Renaming
dados$method <- gsub("RED_wavelet", "wavelet", dados$method)
dados$method <- gsub("RED_meanad", "median abs dev", dados$method)
dados$method <- gsub("RED_sd", "standard deviation", dados$method)
dados$method <- gsub("RED_realized", "vol realized", dados$method)
dados$method <- gsub("RED_range", "range", dados$method)
dados$method <- gsub("RED_mad", "MAD", dados$method)
dados$method <- gsub("RED_iqr", "IQR", dados$method)
dados$method <- gsub("RED_ewma", "EWMA", dados$method)
dados$method <- gsub("RED_garch", "GARCH", dados$method)
dados$method <- gsub("RED_var", "Variance", dados$method)

# Median calc
dados <- dados %>%
  group_by(method) %>%
  mutate(mediana_F1 = median(F1, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(method = reorder(method, -mediana_F1))  # Ordem decrescente da mediana

# Prepate data
resumo <- dados %>%
  group_by(method) %>%
  summarise(
    mediana = median(F1, na.rm = TRUE),
    q1 = quantile(F1, 0.25, na.rm = TRUE),
    q3 = quantile(F1, 0.75, na.rm = TRUE)
  ) %>%
  mutate(method = reorder(method, -mediana))  # Ordena pela mediana

# plot
ggplot(resumo, aes(x = method, y = mediana)) +
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, color = "gray40") +
  theme_minimal() +
  labs(title = "",
       x = "Rolling dispersion measure",
       y = "F1-score (median and IQR)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
