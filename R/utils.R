divisor <- function(x) {x %>%
    as.character() %>%
    nchar() %>%
    exp() %>%
    floor()}


quantidade_trf1 <-
  function(inicio,
           fim,
           ano,
           segmento,
           uf,
           distribuidor) {
    ## Para encontrar o maior n\u00famero do processo do ano, eu usei a l\u00f3gica da busca bin\u00e1ria.
    ## fim pode ser qualquer n\u00famero grande o bastante para ser superior ao n\u00famero total de processos
    ## distribu\u00eddos.


    # O loop abaixo faz requisi\u00e7\u00e3o ao modo de busca bin\u00e1ria. Pode haver uma pequena diferen\u00e7a de 2.

    while (`-`(fim, inicio) > 5) {
      inicio <- mean(c(inicio,fim)) ## Calculo a m\u00e9dia, mas n\u00e3o vejo necessidade de arrendondar.


      # Todas as fun\u00e7\u00f5es para baixar htmls dos processos, de todos os pacotes,
      # possuem um argumento para o vetor de processos (ids) e outro para o
      # diret\u00f3rio ou path. Assim, criamos um diretorio tempor\u00e1rio para guardar
      # os arquivos:


      temporario <- tempdir()

      ## Criamos um intervalo de oito n\u00fameros em torno de y
      ## para assegurar que ao menos um deles existir\u00e1 caso o \u00faltimo seja
      ## superior ou igual a y.
      intervalo <- round(inicio + -2:2) %>%
        range()

      ## aqui eu uso a fun\u00e7\u00e3o cnj_sequencial para criar a numeracao conforme o CNJ,
      ## aplico a fun\u00e7\u00e3o para baixar e verifico se os cinco s\u00e3o simultaneamente nulos,
      ## somando os objetos l\u00f3gicos. Se a soma for cinco, ou seja, TRUE, TRUE, TRUE, TRUE, TRUE
      ## o \u00faltimo processo \u00e9 menor que inicio.

      soma <-
        sequencial_trf1(intervalo[1], intervalo[2], ano, segmento, uf, distribuidor) %>%
        baixar_processo_trf1(diretorio = temporario) %>%
        purrr::map_dbl(trf1_vazio) %>% ## Eu usei NULL como padr\u00e3o porque a requisi\u00e7\u00e3o para o DF retorna nulo,
        # mas isso n\u00e3o se aplica a outros tribunais.
        sum()

      unlink(temporario) ## manda o diret\u00f3rio pro espa\u00e7o.

      ## Se inicio for maior que o \u00faltimo processo, substitu\u00edmos inicio atual pelo y anterior,
      ## e fim se torna o atual inicio, isto \u00e9 a m\u00e9dia entre inicio e fim.
      ## Se o \u00faltimo for maior que inicio, fim \u00e9 preservado e inicio passa a ser
      ## a m\u00e9dia entre inicio e fim.

      if (soma == 5) {
        inicio <- inicio - (fim - inicio)

        fim <- mean(c(inicio,fim))

      }

    }

    return(inicio)
  }


sequencial_trf1 <- function(inicio, fim, ano, segmento, uf, distribuidor)
{
  if (!is.numeric(inicio) | !is.numeric(fim)) {
    stop("inicio e fim devem ser num\u00e9ricos")
  }
  o <- stringr::str_pad(inicio:fim, width = 7, "left", "0")
  uf <- stringr::str_pad(uf, 2, "left", "0")
  distribuidor <- stringr::str_pad(distribuidor, 4, "left",
                                   "0")
  num <- paste0(o, ano, segmento, uf, distribuidor)
  abjutils::calc_dig(num, TRUE)
}


trf1_vazio<-function(x){
  utils::object.size(x) %>%
    `<`(19000)
}
