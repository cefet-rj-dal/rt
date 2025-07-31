library(daltoolbox)
library(daltoolboxdp)
library(harbinger)
hutils <- harutils()

load("~/Tese/datasets/DataHetero.RData")
load("~/Tese/datasets/DataHetero_gab.RData")

# data selection
dataset <- data_hetero
dataset_gab <- data_hetero_gab
n <- ncol(dataset)

set.seed(432)

# methods
metodos <- list(
  hanr_garch = function() hanr_garch(),
  hanr_red = function() hanr_red(),
  hanr_arima = function() hanr_arima(),
  hanr_fbiad = function() hanr_fbiad(),
  hanr_fft = function() hanr_fft(),
  hanr_remd = function() hanr_remd(),
  hanr_wavelet = function() hanr_wavelet(),
  hanr_mlp = function() hanr_ml(ts_mlp(ts_norm_gminmax(), input_size = 3, size = 3, decay = 0)),
  hanr_svm = function() hanr_ml(ts_svm(ts_norm_gminmax(), input_size = 4, kernel = "radial")),
  hanr_lstm = function() hanr_ml(ts_lstm(ts_norm_gminmax(), input_size=4, epochs=10000)),
  hanr_autoencoder = function() hanr_ml(ts_lstm(ts_norm_gminmax(), input_size=4, epochs=10000))
)

# create dataframe with results
resultados <- data.frame(
  method = character(),
  serie = integer(),
  F1 = numeric(),
  precision = numeric(),
  recall = numeric(),
  stringsAsFactors = FALSE
)

# Loop by method
for (nome_metodo in names(metodos)) {
  cat("Executando método:", nome_metodo, "\n")
  
  for (j in 1:n) {
    skip_to_next <- FALSE
    cat("  Série:", j, "\n")
    
    modelo <- tryCatch({
      metodos[[nome_metodo]]()
    }, error = function(e) {
      cat("  Erro ao instanciar o modelo:", e$message, "\n")
      skip_to_next <<- TRUE
      NULL
    })
    if (skip_to_next) next
    
    if (!is.null(modelo)) {
      if (!is.null(modelo$har_outliers)) {
        modelo$har_outliers <- hutils$har_outliers_gaussian
      }
      if (!is.null(modelo$har_distance)) {
        modelo$har_distance <- hutils$har_distance_l2
      }
    }
    
    # Fit
    modelo <- tryCatch({
      fit(modelo, dataset[, j])
    }, error = function(e) {
      cat("  Erro no fit:", e$message, "\n")
      skip_to_next <<- TRUE
      NULL
    })
    if (skip_to_next) next
    
    # Detect
    detection <- tryCatch({
      detect(modelo, dataset[, j])
    }, error = function(e) {
      cat("  Erro na detecção:", e$message, "\n")
      skip_to_next <<- TRUE
      NULL
    })
    if (skip_to_next) next
    
    # Evaluate
    evaluation <- tryCatch({
      evaluate(modelo, detection$event, dataset_gab[, j])
    }, error = function(e) {
      cat("  Erro na avaliação:", e$message, "\n")
      skip_to_next <<- TRUE
      NULL
    })
    if (skip_to_next) next
    
    # saving results
    resultados <- rbind(resultados, data.frame(
      method = nome_metodo,
      serie = j,
      F1 = ifelse(is.nan(evaluation$F1), NA, evaluation$F1),
      precision = ifelse(is.nan(evaluation$precision), NA, evaluation$precision),
      recall = ifelse(is.nan(evaluation$recall), NA, evaluation$recall)
    ))
  }
}

resultados