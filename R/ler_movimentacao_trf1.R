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
#' df <- ler_movimentacao_trf1()
#' }
ler_movimentacao_trf1 <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern = "detalhe",full.names = TRUE)

  }

  pb <- progress::progress_bar(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    processo <- stringr::str_extract(.x,"\\d{7,}")

      xml2::read_html(.x)  %>%
      xml2::xml_find_all("//div[@id='aba-movimentacao']//table") %>%
      rvest::html_table() %>%
      `[[`(1) %>%
      janitor::clean_names() %>%
      tibble::add_column(processo=processo,.before =1) %>%
      dplyr::mutate(data= lubridate::parse_date_time(data,"dmy HMS"))

  }, NULL))

}
