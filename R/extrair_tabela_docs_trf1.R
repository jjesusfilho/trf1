#' Extrai urls dos documentos a partir da tabela criada com ler_por_data
#'
#' @param df tibble com número do processo e url_inteiro_teor
#'
#' @return nova tabela
#' @export
#'
#' @examples
#' \dontrun{
#' df_docs <- extrair_tabela_docs_trf1(df)
#' }
extrair_tabela_docs_trf1 <- function(df=NULL){
  suppressMessages(
  purrr::map2_dfr(df$url_inteiro_teor,df$processo,purrr::possibly(~{


    conteudo <-  httr::RETRY("GET",url=.x) %>%
      httr::content()

    url_doc <- conteudo %>%
      xml2::xml_find_all("//td/a") %>%
      xml2::xml_attr("href") %>%
      unique()

    doc <- conteudo %>%
      xml2::xml_find_all("//td[1]/a") %>%
      xml2::xml_text() %>%
      unique()

    data <- conteudo %>%
      xml2::xml_find_all("//td[2]/a") %>%
      xml2::xml_text() %>%
      #unique() %>%
      lubridate::dmy()

    extensao <- stringr::str_extract(url_doc,"\\.\\w+$")

    tibble::tibble(processo = .y, data,documento = doc, url_documento = url_doc, extensao )

  },NULL))
  )

}
