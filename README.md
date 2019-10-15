
# trf1

<!-- badges: start -->

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

Para baixar acórdãos do TRF1 com busca livre, use a seguinte
função:

``` r
baixar_cjsg_trf1(livre = "agência nacional de vigilância sanitária", data_inicial = "01/07/2019", data_final = "31/07/2019")

df <- ler_cjsg_trf1()
```

Para baixar processos julgados numa data específica, utilize as funções
abaixo.

``` r

dir.create("trf1")

baixar_por_data_trf1(data = "15/10/2019", diretorio = "trf1")

df  <- ler_por_data_trf1(diretorio = "trf1")

dir.create("trf1/detalhes")

baixar_detalhes_trf1(df, diretorio = "trf1/detalhes")

detalhes <- ler_detalhes_trf1(diretorio = "trf1/detalhes")

partes <- ler_partes_trf1(diretorio =  "trf1/detalhes")

movimentacao <- ler_movimentacao_trf1(diretorio = "trf1/detalhes")
```

Please note that the ‘trf1’ project is released with a [Contributor Code
of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
