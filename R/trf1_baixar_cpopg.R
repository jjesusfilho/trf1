#' Baixar processos de primeira instância
#'
#' @param processo Vetor com número dos processos
#' @param secao Seção judiciária
#' @param diretorio Diretório
#'
#' @return Htmls
#' @export
#'
trf1_baixar_cpopg <- function(processo = NULL, secao = NULL, diretorio = "."){


url <- "https://processual.trf1.jus.br/consultaProcessual/processo.php"


pb <- progress::progress_bar$new(total = length(processo))

purrr::walk2(processo, secao, purrr::possibly(~{

  pb$tick()
.x <- stringr::str_remove_all(.x,"\\D+")

query <- list(
proc = .x,
secao=.y)


arquivo <- file.path(diretorio, paste0("trf1_cpopg_",
                                       tolower(.y),
                                       "_",
                                       stringr::str_replace_all(Sys.Date(),"\\D","_"),
                                       "_",.x,".html"))

httr::RETRY("GET",url,query = query,httr::write_disk(arquivo,overwrite = TRUE))


},NULL))

}
