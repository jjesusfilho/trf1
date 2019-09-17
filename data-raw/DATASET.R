## code to prepare `DATASET` dataset goes here

usethis::use_data("DATASET")

url<-"https://portal.trf1.jus.br/Servicos/Certidao/"

`%>%` <- purrr::`%>%`
a<-httr::GET(url) %>% httr::content()

codigo <- a %>%
  xml2::xml_find_all("//select[@name='ct_orgao']/option/@id") %>%
  xml2::xml_text()
value <- a %>%
  xml2::xml_find_all("//select[@name='ct_orgao']/option/@value") %>%
  xml2::xml_text()
unidade <- a %>%
  xml2::xml_find_all("//select[@name='ct_orgao']/option") %>%
  xml2::xml_text()

codigo_trf1<-tibble::tibble(unidade = unidade, sigla = value, codigo = codigo) %>%
  dplyr::slice(-1) %>%
  dplyr::mutate(turma_codigo = stringr::str_extract(codigo,"\\d{3}") %>%
                  stringr::str_c("9",.))

download.file(url,"data-raw/codigos.pdf")
img_file <- pdftools::pdf_convert("data-raw/codigos.pdf", format = 'tiff', pages = 1, dpi = 400)

codigos <- tesseract::ocr(url)

s<-stringr::str_extract_all(codigos,"\\D?\\d{4}\\D?")

library()


