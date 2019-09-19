library(purrr)
library(trf1)
load("data-raw/base.RData")

lista1<-append(lista1,1,0)
faltantes<-trf1::codigo_trf1$codigo[lista1] %>% as.numeric()


lista[[lista1[20]]]<-quantidade_trf1(1,50000,ano = 2018,segmento = 4,1,faltantes[20])


