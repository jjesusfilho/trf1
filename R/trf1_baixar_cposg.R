#' Baixar processos da segunda instância do TRF1
#'
#' @param processo Número do processo
#' @param diretorio Diretório
#' @param partes A partes são baixadas num arquivo separado
#'
#' @return Dois htmls, um com dados e andamentos processuis, outro com as partes.
#' @export
#'
trf1_baixar_cposg <- function(processo = NULL, diretorio = ".", partes = TRUE){


 pb <- progress::progress_bar$new(total = length(processo))

  purrr::walk(processo,  purrr::possibly(~{

    pb$tick()

    .x <- stringr::str_remove_all(.x,"\\D+")


    url1 <- "https://processual.trf1.jus.br/consultaProcessual/processo.php"


    arquivo1 <- file.path(diretorio, paste0("trf1_cposg_",
                                            stringr::str_replace_all(Sys.Date(),"\\D","_"),
                                            "_",.x,".html"))

    body <- list(
      secao= "TRF1",
      opTrf= "proc",
      proc= .x,
      enviar = "Ok"
    )


    httr::POST(url1,body = body, encode = "form",httr::write_disk(arquivo1,overwrite = TRUE))

    if (partes == TRUE) {



      arquivo2 <- file.path(diretorio, paste0("trf1_cposg_parte_",
                                              stringr::str_replace_all(Sys.Date(),"\\D","_"),
                                              "_",.x,".html"))

      query <- list(
        proc = .x,
        secao="TRF1",
        origem = "juris")


      url2  <- "https://processual.trf1.jus.br/consultaProcessual/arquivo/partes.php"

      httr::RETRY("GET",url2,query = query,httr::write_disk(arquivo2,overwrite = TRUE))

    }
  },NULL))

}
