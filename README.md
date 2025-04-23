# vars2a 

Note: The package is initially designated vars2; however, it is 
subsequently redesignated vars2a due to the pre-existence of vars2.


## Vector autoregression (a slightly modified version of vars) <img align="right" src="/tools/hex_vars2a.png" width="120">

The R package ‘vars’, created by Bernhard Pfaff, is an extremely powerful tool for time series analysis, and his contribution is truly worthy of praise in the fields of statistics and econometrics. Numerous researchers and practitioners in the fields, myself included, have greatly appreciated it and owe a considerable debt to his contributions over the years.

Recently, I have observed a growing preference among researchers and practitioners for ‘tibble’ over ‘ts’, and ‘ggplot2’ over the base plot. In response to this trend, I am planning to slightly modify vars to utilize tibble and ggplot2.  Needless to say, I respect the original intent and functionality of the ‘vars’.

The modified version of vars is now being tentatively referred to as vars2a. This package is currently in progress and has several incomplete parts. I welcome any suggestions or feedback for improvement

```
devtools::install_github("koiti-yano/vars2")
```

## New features of vars2a
+ Tibbles: The VAR and VARselect functions accept a tibble (including df) as input y and some useful tools are added
+ ggplot2: Impulse response plots with ggplot2 (the ggplot code for vars2 is based on Victor Espinoza's work and my own. See DESCRIPTION)

<img src="tools/figure/plot_irf_with_vars2.png" width="800">

## Koiti' note
2024-04-09: After reaching out with the intention to fork ‘vars’, I received a generous and kind response from Dr. Bernhard Pfaff. I would like to express my gratitude for his significant contributions to the creation of many R packages, including ‘vars’, and I hereby record my appreciation for his great work.
