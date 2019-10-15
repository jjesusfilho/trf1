#' Baixar detalhes dos processos lidos com ler_por_data_trf1
#'
#' @param df tibble lido com ler_por_data_trf1
#' @param diretorio diretório
#'
#' @return html dos detalhes e das partes.
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_detalhes_trf1(df)
#' }
baixar_detalhes_trf1 <- function(df = NULL,diretorio = "."){

  purrr::walk2(df$url_detalhes,df$processo,purrr::possibly(~{

   arquivo <- paste0("_detalhes_processo_", .y, ".html")

 httr::RETRY("GET",url=.x,httr::write_disk(file.path(diretorio, Sys.time() %>%
  stringr::str_replace_all("\\D+", "_") %>%
  stringr::str_replace("$", arquivo))))

# k<- httr::content(a) %>%
#    xml2::xml_find_first("//script[contains(@src,'processo.js')]") %>%
#    xml2::xml_attr("src") %>%
#    xml2::url_absolute("https://processual.trf1.jus.br") %>%
#    stringr::str_replace("\\?","?_=") %>%
#    httr::GET()


  baixar_partes_trf1(processos = .y,diretorio=diretorio)
  },NULL))

}



baixar_partes_trf1<-function(processos = NULL, diretorio = "."){


  purrr::walk(processos,purrr::possibly(~{


    url <- paste0("http://processual.trf1.jus.br/consultaProcessual/arquivo/partes.php?proc=",.x,"&secao=TRF1&origem=juris")

    arquivo <- paste0("_partes_",.x,".html")

    httr::GET(url,httr::write_disk(file.path(diretorio, Sys.time() %>%
                                                                            stringr::str_replace_all("\\D+", "_") %>%
                                                                            stringr::str_replace("$", arquivo)),overwrite = TRUE))

  },NULL))

}
