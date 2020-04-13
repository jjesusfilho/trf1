#' Extrai tabela urls dos documentos com base no número do processo
#'
#' @param processos vetor com números dos processos
#'
#' @return tibble com processo,documento, data,url_documento e extensão
#' @export
#'
extrair_docs_por_processo <- function(processos = NULL){


  purrr::map_dfr(processos,purrr::possibly(purrrogress::with_progress(~{

    processo <- stringr::str_remove_all(.x,"\\D")
    p <- stringr::str_remove(processo,"^0+")

    url <- paste0("https://arquivo.trf1.jus.br/PesquisaMenuArquivo.asp?p1=",processo,"&pA=&pN=",p)

    conteudo <- httr::GET(url) %>%
      httr::content(encoding="UTF-8")

    url_documento <- xml2::xml_find_all(conteudo,"//table[@id='docs-tb']//td[1]/a") %>%
      xml2::xml_attr("href")

    extensao <- stringr::str_extract(url_documento,"\\.\\w+$")

    data  <- xml2::xml_find_all(conteudo,"//table[@id='docs-tb']//td[2]") %>%
      xml2::xml_text()
    documento <- xml2::xml_find_all(conteudo,"//table[@id='docs-tb']//td[1]/a") %>%
      xml2::xml_text()

    tibble::tibble(processo,documento, data, url_documento,extensao)

  }),NULL))
}
