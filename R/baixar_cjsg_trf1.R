#' Baixa decisões de segunda instância do TRF1
#'
#' @param livre busca livre
#' @param tipo Default para "ACÓRDÃOS"
#' @param data_inicial formato "dd/mm/aaaa"
#' @param data_final formato "dd/mm/aaaa"
#' @param diretorio Default para atual.
#'
#' @return Baixa os htmls nos diretório especificado
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_cjsg_trf1(
#'   livre = "", tipo = "ACORDAO",
#'   data_inicial = "10/07/2019", data_final = "31/07/2019"
#' )
#' }
#'
baixar_cjsg_trf1 <- function(livre = "", tipo = "ACORDAO", data_inicial = "", data_final = "", diretorio = ".") {
  url1 <- "https://www2.cjf.jus.br/jurisprudencia/trf1/"

  ViewState <- httr::RETRY("GET", url1, httr::timeout(30)) %>%
    httr::content() %>%
    xml2::xml_find_first("//form/input[@type='hidden'][2]") %>%
    xml2::xml_attr("value")

  url2 <- "https://www2.cjf.jus.br/jurisprudencia/trf1/index.xhtml"

  inicial <- "0"

  body1 <- list(
    formulario = "formulario",
    `formulario:textoLivre` = livre,
    `formulario:ckbAvancada_input` = "on",
    `formulario:j_idt19` = "",
    `formulario:j_idt21` = "",
    `formulario:j_idt23` = "",
    `formulario:j_idt25` = "",
    `formulario:j_idt27` = "",
    `formulario:j_idt29` = "",
    `formulario:j_idt31` = "",
    `formulario:j_idt33` = "",
    `formulario:j_idt35` = "",
    `formulario:j_idt37` = "",
    `formulario:j_idt39_input` = data_inicial,
    `formulario:j_idt41_input` = data_final,
    `formulario:combo_tipo_data_focus` = "",
    `formulario:combo_tipo_data_input` = "DTDP",
    `formulario:selectTiposDocumento` = tipo,
    `formulario:j_idt53` = "TRF1",
    `formulario:actPesquisar` = "",
    javax.faces.ViewState = ViewState
  )

  body2 <- list(
    `javax.faces.partial.ajax` = TRUE,
    `javax.faces.source` = "formulario:tabelaDocumentos",
    `javax.faces.partial.execute` = "formulario:tabelaDocumentos",
    `javax.faces.partial.render` = "formulario:tabelaDocumentos",
    `formulario:tabelaDocumentos` = "formulario:tabelaDocumentos",
    `formulario:tabelaDocumentos_pagination` = TRUE,
    `formulario` = "formulario",
    `formulario:textoLivre` = livre,
    `formulario:ckbAvancada_input` = "on",
    `formulario:tabelaDocumentos_first` = inicial,
    `formulario:tabelaDocumentos_rows` = "50",
    `formulario:j_idt19` = "",
    `formulario:j_idt21` = "",
    `formulario:j_idt23` = "",
    `formulario:j_idt25` = "",
    `formulario:j_idt27` = "",
    `formulario:j_idt29` = "",
    `formulario:j_idt31` = "",
    `formulario:j_idt33` = "",
    `formulario:j_idt35` = "",
    `formulario:j_idt37` = "",
    `formulario:j_idt39_input` = data_inicial,
    `formulario:j_idt41_input` = data_final,
    `formulario:combo_tipo_data_focus` = "",
    `formulario:combo_tipo_data_input` = "DTDP",
    `formulario:selectTiposDocumento` = tipo,
    `formulario:j_idt48_input` = "on",
    `formulario:j_idt53` = "TRF1",
    `formulario:actPesquisar` = "",
    `formulario:j_idt61_scrollState` = "0,0",
    `formulario:tabelaDocumentos_rppDD` = "50",
    `formulario:tabelaDocumentos:0:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:1:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:2:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:3:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:4:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:5:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:6:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:7:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:8:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:9:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:10:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:11:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:12:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:13:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:14:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:15:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:16:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:17:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:18:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:19:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:20:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:21:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:22:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:23:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:24:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:25:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:26:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:27:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:28:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos:29:j_idt243_activeIndex` = "0",
    `formulario:tabelaDocumentos_rppDD` = "50",
    javax.faces.ViewState = ViewState
  )
  numero <- NA_real_

  while (is.na(numero)) {
    numero <- httr::RETRY("POST", url2, body = body1, encode = "form", httr::timeout(30)) %>%
      httr::content() %>%
      xml2::xml_find_first("//span[@class='ui-paginator-current']") %>%
      xml2::xml_text(trim = T) %>%
      stringr::str_extract("\\d+(?=,)") %>%
      as.numeric()
  }

  inicial <- seq(0, numero, 50)

  purrr::walk(inicial, ~ {
    body2$`formulario:tabelaDocumentos_first` <- as.character(.x)

    arquivo <- paste0("_pagina_", .x, ".html")

    httr::RETRY("POST", url2,
      body = body2, encode = "form", httr::timeout(30),
      httr::write_disk(file.path(diretorio, Sys.time() %>%
        stringr::str_replace_all("\\D+", "_") %>%
        stringr::str_replace("$", arquivo)))
    )
  })
}
