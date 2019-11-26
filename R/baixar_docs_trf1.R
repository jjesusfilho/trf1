#' Baixar documentos listados em extrair_tabela_docs_trf1
#'
#' @param df tibble lido com ler_por_data_trf1
#' @param diretorio diretório
#' @param tudo default para FALSE, ou seja, mantêm somente ementas, decisões,
#'     relatórios     e votos
#'
#' @return docs
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_docs_trf1(df)
#' }
baixar_docs_trf1 <- function(df = NULL,diretorio = ".", tudo = FALSE){

   if (tudo==FALSE){
     df <- df %>%
       dplyr::filter(stringr::str_detect(documento,"(?i)(decis.o|voto|ementa|relat.rio)"))


   }

  purrr::pwalk(list(x=df$url_documento, y=df$data, z=df$processo,w=df$extensao,u=df$documento),function(x,y,z,w,u){

    documento <- stringr::str_replace_all(u,"\\s+","_") %>%
                 stringr::str_remove_all("[:punct:]+") %>%
                 tolower()

    arquivo <- paste0("_",documento,"_",z,w)

    httr::RETRY("GET",url=x,httr::write_disk(file.path(diretorio, y %>%
                                                          stringr::str_replace_all("\\D+", "_") %>%
                                                          stringr::str_replace("$", arquivo)),overwrite=TRUE))

  })

}
