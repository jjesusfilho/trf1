#' Ler htmls baixados por baixar_cjsg_trf1
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar se não informar arquivos.
#'
#' @return tibble com informações processuais
#' @export
#'
#' @examples
#' \dontrun{
#' ler_cjsg_trf1(diretorio = ".")
#' }
ler_cjsg_trf1 <- function(arquivos = NULL, diretorio = ".") {

  if (is.null(arquivos)){

  arquivos <- list.files(path = diretorio, pattern = ".html", full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    doc <- xml2::read_html(.x)

    variavel <- doc %>%
      xml2::xml_find_all("//span[@class='label_pontilhada']") %>%
      xml2::xml_text(trim = T)

    valor <- doc %>%
      xml2::xml_find_all("//span[@class='label_pontilhada']/ancestor::tr[1]/following-sibling::tr/td") %>%
      xml2::xml_text(trim = T)

    tibble::tibble(variavel = variavel, valor = valor) %>%
      dplyr::group_by_at(dplyr::vars(-valor)) %>%
      dplyr::mutate(row_id = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::spread(key = variavel, value = valor) %>%
      dplyr::select(-row_id)
  },NULL))
}
