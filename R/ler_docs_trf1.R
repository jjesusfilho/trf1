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

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    processo <- stringr::str_extract(.x, "\\d{20}")
    documento <- stringr::str_extract(.x, "(?<=_)\\p{L}.+?(?=_\\d{20})")
    julgado <- textreadr::read_document(.x)

    ## Muitos documentos com extensão doc são, na verdade, docx.
    if (is.null(julgado) && stringr::str_detect(julgado,"\\.doc$")){

      julgado <- textreadr::read_docx(.x)

    }

    if (is.null(julgado)) julgado <- NA_character_


    tibble::tibble(processo, documento, julgado)
  }), NULL))
}
