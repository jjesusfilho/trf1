#' ler movimentacao baixada com baixar_url_detalhes_trf1
#'
#' @param arquivos vetor caminhos para arquivos
#' @param diretorio se arquivos for NULL, informar diretorio
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ler_movimentacao_stj()
#' }
ler_movimentacao_trf1 <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern = "html",full.names = TRUE)

  }

  processos <- stringr::str_extract(arquivos,"\\d{7,}")
  purrr::map2_dfr(arquivos,processos, purrr::possibly(~{

    x <- xml2::read_html(.x)

    dados <- x %>%
      xml2::xml_find_all("//div[@id='aba-movimentacao']//table") %>%
      rvest::html_table() %>%
      `[[`(1) %>%
      janitor::clean_names() %>%
      tibble::add_column(processo=.y,.before =1) %>%
      dplyr::mutate(data= lubridate::parse_date_time(data,"dmy HMS"))

  }, NULL))

}
