# Multi-Transformer: A new neural network-based architecture for forecasting S&P volatility
Transformer layers have already been successfully applied for NLP purposes. This repository adapts Transfomer layers in order to be used within hybrid volatility forecasting models. Following the intuition of bagging, this repository also introduces Multi-Transformer layers. The aim of this novel architecture is to improve the stability and accurateness of Transformer layers by averaging multiple attention mechanism.

The article collecting theoretical background and empirical results of the proposed model can be downloaded [*here*](https://doi.org/10.3390/math9151794). The stock volatility models based on Transformer and Multi-Transformer (T-GARCH, TL-GARCH, MT-GARCH and MTL-GARCH) overcome the performance of traditional autoregressive algorithms and other hybrid models based on feed forward layers or LSTM units. The following table collects the validation error (RMSE) by year and model.

Model           |  2016  |  2017  |  2018  |  2019  |  2020  | 2016-2020 | 
--------------- | ------ | ------ | ------ | ------ | ------ | --------- |
   GARCH(1,1)   | 0.0058 | 0.0026 | 0.0095 | 0.0073 | 0.1026 |   0.0464  |
  AVGARCH(1,1)  | 0.0053 | 0.0027 | 0.0076 | 0.0056 | 0.0847 |   0.0383  |
  EGARCH(1,1)   | 0.0056 | 0.0028 | 0.0093 | 0.0078 | 0.0880 |   0.0399  |
 GJR-GARCH(1,1) | 0.0090 | 0.0028 | 0.0126 | 0.0068 | 0.1248 |   0.0565  |
  TrGARCH(1,1)  | 0.0074 | 0.0027 | 0.0115 | 0.0058 | 0.1153 |   0.0521  |
  FIGARCH(1,1)  | 0.0062 | 0.0029 | 0.0095 | 0.0066 | 0.1011 |   0.0457  |
  ANN-GARCH     | 0.0042 | 0.0023 | 0.0060 | 0.0044 | 0.0171 |   0.0086  |
  LSTM-GARCH    | 0.0032 | 0.0021 | 0.0043 | 0.0030 | 0.0101 |   0.0054  |
  T-GARCH       | 0.0048 | 0.0029 | 0.0058 | 0.0044 | 0.0117 |   0.0067  |
  TL-GARCH      | 0.0030 | 0.0019 | 0.0033 | 0.0026 | 0.0070 |   0.0040  |
  MT-GARCH      | 0.0036 | 0.0021 | 0.0046 | 0.0033 | 0.0096 |   0.0054  |
  MTL-GARCH     | 0.0030 | 0.0016 | 0.0033 | 0.0026 | 0.0066 |   0.0038  |

The article also evaluates the appropriateness of the equity risk measures obtained with the previous models. Kupiec and Christoffersen tests demonstrate that only the risk estimations made by MTL-GARCH, TL-GARCH and MT-GARCH can be considered as appropriate for the period 2016–2020.

This repository provides the source code to replicate the results obtained in [*Multi-Transformer: A new neural network-based architecture for forecasting S&P volatility*](https://doi.org/10.3390/math9151794).
