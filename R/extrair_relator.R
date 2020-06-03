#' Extrai nome do relator dos textos dos julgados
#'
#' @param julgado Texto do julgado
#'
#' @return Vetor com os nomes dos relatores, sem acento e em minúsculo
#' @export
#'
extrair_relator <- function(julgado = NULL){
  
  inicio <- stringr::str_sub(julgado,start = 1,300) %>% 
    tolower() %>% 
    stringi::stri_trans_general("latin-ascii") %>% 
    stringr::str_squish() %>% 
    stringr::str_extract("(?<=juiza?|desembargadora?)\\X+?(?=(\\||apelante|relator|\\())")
  
  
  fim <- stringr::str_sub(julgado,-300) %>% 
    tolower() %>% 
    stringi::stri_trans_general("latin-ascii") %>% 
    stringr::str_squish() %>% 
    stringr::str_extract("(?<=juiza?|desembargadora?)\\X+?(?=(\\||apelante|relator|\\())")
  
  dplyr::coalesce(inicio,fim) %>% 
    stringr::str_remove(".+federal") %>% 
    stringr::str_remove_all("[:punct:]") %>% 
    stringr::str_squish()
}
