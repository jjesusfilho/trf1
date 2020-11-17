#' Lê lodocumentos decisórios
#'
#' @param arquivos Se não informados, informar o diretório
#'     onde se encontram.
#' @param diretorio  Informar se não informou arquivos
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ler_docs_trf1()
#' }
ler_docs_trf1 <- function (arquivos = NULL, diretorio = ".")
{
  if (is.null(arquivos)) {
    arquivos <- list.files(diretorio, full.names = TRUE)
  }

  ## Muitos processos não têm o número do cnj. Por isso de 10 a 20.

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    pb$tick()

    processo <- stringr::str_extract(.x, "\\d{10,20}")

    ## O acréscimo de uma barra ao início e um subscrito ao final foi mais por segurança
    data <- stringr::str_extract(.x, "/\\d{4}_\\d{2}_\\d{2}_") %>%
      lubridate::ymd()

    documento <- stringr::str_extract(.x, "(?<=_)\\p{L}.+?(?=_\\d{10,20})") %>%
      stringi::stri_trans_general("latin-ascii")

    julgado <- textreadr::read_document(.x,combine = TRUE)

    ## Muitos documentos com extensão doc são, na verdade, docx.
    if (is.null(julgado) && stringr::str_detect(.x,"\\.doc$")){

      julgado <- textreadr::read_docx(.x,combine=TRUE)

    }

    if (is.null(julgado)) julgado <- NA_character_


    df <- tibble::tibble(processo,data, documento, julgado) %>%
      dplyr::group_by(processo) %>%
      tidyr::chop(cols = julgado)



    s <- purrr::map(df$julgado, ~stringr::str_c(.x, collapse = "\n")) %>%
      unlist()

    df %>% dplyr::mutate(julgado = !!s) %>%
      dplyr::ungroup()


  }, NULL))
}
