# RT: Resilient Transformation for Heteroscedastic Time Series

This repository contains the implementation and experimental results of the article **"RT: Resilient Transformation for Heteroscedastic Time Series"**, which proposes a transformation technique (RT) designed to enhance the detection of contextual anomalies in time series with non-constant variance.

## Summary

Heteroscedastic time series exhibit non-constant variance over time, which undermines the effectiveness of classical anomaly detection methods based on fixed thresholds. This issue particularly affects the identification of contextual anomalies, whose detection depends on local variability.

To address this challenge, this work proposes the **RT transformation**, which modifies the original series using CEEMD decomposition, selection of high-frequency components based on roughness, differentiation, and normalization by local dispersion. This transformation equalizes variance and highlights pointwise deviations.

Evaluations using the Yahoo! S5 dataset (via the Harbinger framework) show that RT enhances the performance of classical methods and can be combined with thresholding to form an autonomous detection method, **RTAD**. Sensitivity analyses indicate that the use of standard deviation and a Gaussian threshold yields the best results.

RT can be applied both as a preprocessing step or as a complete detection procedure.

---

## 📁 Repository Structure

### `.R` scripts

- **`cod_RT.R`**: Defines the function that performs the RT transformation on a time series.
- **`cod_dispersion-measures.R`**: Implements methods for computing local volatility (instantaneous dispersion).
- **`cod_exp-eva.R`**: Executes anomaly detection methods on the series and computes evaluation metrics.
- **`hanr_RTAD.R`**: RTAD implementation within the Harbinger framework.
- **`FIG1.R`, `FIG2.R`, `FIG3.R`, `FIG4.R`, `FIG5.R` and `TAB2`**: Scripts used to generate all figures and Table 2 from the article.

### `.RDATA` files

- **`DataHomo`**, **`DataHomo_gab`**, **`DataHomoRT`**: Original, label, and RT-transformed versions of 100 homoscedastic series (Yahoo A3).
- **`DataHetero`**, **`DataHetero_gab`**, **`DataHeteroRT`**: Original, label, and RT-transformed versions of 74 heteroscedastic series (Yahoo A4).

### `.csv` files

- **`metodos_hetero_hard_com-RT.csv`**: Evaluation metrics (hard) for traditional methods applied to heteroscedastic series after RT.
- **`metodos_hetero_hard_sem-RT.csv`**: Evaluation metrics (hard) for traditional methods applied to original heteroscedastic series.
- **`metodos_homo_hard_com-RT.csv`**: Evaluation metrics (hard) for traditional methods applied to homoscedastic series after RT.
- **`metodos_homo_hard_sem-RT.csv`**: Evaluation metrics (hard) for traditional methods applied to original homoscedastic series.
- **`RTAD_hetero_hard.csv`**: Evaluation metrics (hard) of RTAD on heteroscedastic series.
- **`RTAD_homo_hard.csv`**: Evaluation metrics (hard) of RTAD on homoscedastic series.
- **`metrics_dispersion-measures.csv`**: Performance of RTAD with different local dispersion measures.
- **`thresholds_hetero_hard_RTAD.csv`**: Performance of RTAD using different thresholding strategies.

---

## 👥 Authors

- **Laís Baroni**  
  Federal Center for Technological Education of Rio de Janeiro (CEFET/RJ)  
  [lais.baroni@eic.cefet-rj.br](mailto:lais.baroni@eic.cefet-rj.br)
- **Gustavo Guedes**
- **Eduardo Mendes**
- **Carlos Eduardo Mello**
- **Esther Pacitti**
- **Fabio Porto**
- **Rafaelli Coutinho**
- **Eduardo Ogasawara**
