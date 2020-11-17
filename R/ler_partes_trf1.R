#' ler partes do TRF1
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar se vetor de arquivos for NULL
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' ler_partes_trf1()
#' }
ler_partes_trf1 <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)) {

    arquivos <- list.files(diretorio, pattern="partes",full.names=TRUE)

  }


  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    processo <- stringr::str_extract(.x,"\\d{10,}")

    xml2::read_html(.x) %>%
      rvest::html_table() %>%
      `[[`(1) %>%
      `[`(,c(1,4)) %>%
      setNames(c("parte","parte_nome")) %>%
      tibble::add_column(processo=processo)

  },NULL))

}
