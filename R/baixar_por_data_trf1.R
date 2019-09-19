#' Baixar lista de decisões do TRF1 por data
#'
#' @param data data da decisão. Data de hoje, se não informado.
#' @param diretorio diretório, atual se não informado.
#'
#' @return html
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_por_data_trf1()
#' }
baixar_por_data_trf1<- function(data=NULL,diretorio = "."){

  if (is.null(data)){

    data <- Sys.Date() %>%
      format("%d/%m/%Y")

  }

  data1<-lubridate::dmy(data) %>% format("%Y_%m_%d")
  url <- "https://arquivo.trf1.jus.br/PesquisaPublicacao.php"

  body <-
    list(
      numero_processo = "",
      pA = "",
      pN = "",
      p1 = "",
      orgao = "",
      nome_orgao = "+Todos+",
      data_publicacao = data
    )

  httr::RETRY("POST",
              url,
              body=body,
              encode="form",
              httr::timeout(5),
            httr::write_disk(file.path(diretorio, data1 %>%
stringr::str_replace("$",".html"))))
}


