#' ler detalhes baixados com baixar_url_detalhes_trf1
#'
#' @param arquivos vetor caminhos para arquivos
#' @param diretorio se arquivos for NULL, informar diretorio
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ler_detalhes_stj()
#' }
ler_detalhes_trf1 <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern = "html",full.names = TRUE)

  }

  purrr::map_dfr(arquivos, purrr::possibly(~{
    processo <- stringr::str_extract(.x,"\\d{7,}")

    x <- xml2::read_html(.x)

    dados <- x %>%
      xml2::xml_find_all("//div[@id='aba-processo']/table") %>%
      rvest::html_table() %>%
      `[[`(1) %>%
      tidyr::pivot_wider(names_from = "X1",values_from = "X2") %>%
      janitor::clean_names() %>%
      tibble::add_column(proc=processo,.before =1) %>%
      tidyr::separate(processo_originario,c("processo_origem","origem"),sep="/") %>%
      dplyr::mutate(data_de_autuacao=lubridate::dmy(data_de_autuacao)) %>%
      tidyr::separate(grupo,c("codigo_classe","classe"),sep=" - ") %>%
      tidyr::separate(assunto,c("codigo_assunto","assunto"),sep=" - ")


  }, NULL))
}
