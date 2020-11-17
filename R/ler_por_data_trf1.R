#' Ler arquivos baixados por baixar_por_data_trf1
#'
#' @param arquivos Se não informados, buscará diretório
#' @param diretorio Se arquivos não forem informados, buscará
#'     no diretório indicado
#'
#' @return data.frame com urls dos andamentos e dos inteiros teores
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ler_por_data_trf1()
#' }
ler_por_data_trf1 <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

 purrr::map_dfr(arquivos,purrr::possibly(~{

   pb$tick()

  html <- xml2::read_html(.x)

 url_detalhes<- html %>%
  xml2::xml_find_all("//a[@title='Acompanhamento Processual']") %>%
  xml2::xml_attr("href")

 processo<-stringr::str_extract(url_detalhes,"\\d+$") %>%
   stringr::str_pad(20,side='left',pad="0")

 url_inteiro_teor <-  html %>%
   xml2::xml_find_all("//li/a[@href]") %>%
   xml2::xml_attr("href") %>%
   xml2::url_absolute("https://arquivo.trf1.jus.br")

 data_julgamento <- stringr::str_extract(.x,"\\d{4}_\\d{2}_\\d{2}") %>%
  lubridate::ymd()

 tibble::tibble(data_julgamento,processo,url_detalhes,url_inteiro_teor)

 },NULL))
}
