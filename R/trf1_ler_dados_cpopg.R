#' Lê dados da consulta de processos de primeiro grau
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar se não fornecer arquivos
#'
#' @return tibble
#' @export
#'
trf1_ler_dados_cpopg <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  df <-  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    processo <- stringr::str_extract(.x,"\\d{20}")

    x <- .x %>%
      xml2::read_html()

    x %>%
      xml2::xml_find_first("//div[@id='aba-processo']/table") %>%
      rvest::html_table() %>%
      setNames(c("x1","x2")) %>%
      tidyr::pivot_wider(names_from = "x1", values_from = 'x2') %>%
      janitor::clean_names() %>%
      tibble::add_column(proc = processo,.before = 1) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("data"),~lubridate::dmy(.x)))

  },NULL)) %>%
    tidyr::separate(classe,c("codigo_classe","classe"),sep = "\\s?-\\s?") %>%
    dplyr::mutate(codigo_assunto = stringr::str_extract_all(assunto_da_peticao,"\\d{4,}"),.before = assunto_da_peticao)

  if (is.element("juiza",names(df))){

    df <- df %>%
      dplyr::mutate(juiz = dplyr::coalesce(juiz,juiza),
                    juiza = NULL)
  }

  return(df)


}
