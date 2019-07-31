
# trf1

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jjesusfilho/trf1?branch=master&svg=true)](https://ci.appveyor.com/project/jjesusfilho/trf1)
[![Travis build
status](https://travis-ci.org/jjesusfilho/trf1.svg?branch=master)](https://travis-ci.org/jjesusfilho/trf1)
<!-- badges: end -->

O objetivo deste pacote é de prover funções para baixar e organizar
decisões de primeira e de segunda instância do Tribunal Regional Federal
da Primeira Região

## Instalação

Instale a versão em desenvolvimento:

``` r
devtools::install_.packages_github("jjesusfilho/trf1")
```

## Utilização

Para baixar acórdãos do TRF1, use a seguinte
função:

``` r
baixar_cjsg_trf1(livre = "agência nacional de vigilância sanitária", data_inicial = "01/07/2019", data_final = "31/07/2019")

df <- ler_cjsg_trf1()
```

Please note that the ‘trf1’ project is released with a [Contributor Code
of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
