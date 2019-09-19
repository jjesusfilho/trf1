#' Baixar processos do trf1 pelo número
#'
#' @param x vetor com os números dos processos
#' @param diretorio diretório
#'
#' @return htmls no diretório indicado
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_processo_trf1(x="00010419820184013300",diretorio = ".")
#' }
baixar_processo_trf1<- function(x,diretorio = "."){


x <- x %>% stringr::str_remove_all("\\D+")

codigo <- x %>% stringr::str_extract("\\d{4}$")


httr::set_config(httr::config(ssl_verifypeer = FALSE))



res<-purrr::map2(x,codigo,~{

  url <- "https://processual.trf1.jus.br/consultaProcessual/processo.php"

  sigla <- codigo_trf1 %>%
           dplyr::filter(codigo == .y) %>%
           dplyr::pull("sigla")


  body<-list(secao = sigla,
             opSec = "proc",
             proc = .x,
             enviar =  "Ok")


 resposta <- httr::POST(url,body= body, encode = "form")


    if (resposta$content %>% utils::object.size() > 19000){
    arquivo <-  paste0("_processo_",.x,".html")

    resposta$content %>%
      writeBin(file.path(diretorio, Sys.time() %>%
                  stringr::str_replace_all("\\D+", "_") %>%
                  stringr::str_replace("$", arquivo)))
    }
 resposta$content
  })

}
