#' Baixar detalhes dos processos lidos com ler_por_data_trf1
#'
#' @param df tibble lido com ler_por_data_trf1
#' @param diretorio diretório
#'
#' @return html
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_url_detalhes_trf1(df)
#' }
baixar_url_detalhes_trf1 <- function(df = NULL,diretorio = "."){

  purrr::walk2(df$url_detalhes,df$processo,purrr::possibly(~{

   arquivo <- paste0("_detalhes_processo_", .y, ".html")

  httr::RETRY("GET",url=.x,httr::write_disk(file.path(diretorio, Sys.time() %>%
  stringr::str_replace_all("\\D+", "_") %>%
  stringr::str_replace("$", arquivo))))

  },NULL))

}
