#' Lê julgados de primeiro grau em texto
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar se vetor de arquivos for NULL
#'
#' @return tibble
#' @export
#'
trf1_ler_cpopg_docs <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, full.names = TRUE)

  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos,purrr::possibly(~{

    pb$tick()

    processo <- stringr::str_extract(.x,"\\d{20}")

    cpw <- stringr::str_extract(.x,"(?<=cpw_)\\d+")

    extensao <- stringr::str_extract(.x,"\\w+$")

    if (extensao == "doc"){

      julgado <- textreadr::read_doc(.x)

    } else{
      suppressMessages(
        julgado <- pdftools::pdf_text(.x) %>%
          stringr::str_c(collapse = "\n")
      )
    }

    if (stringr::str_detect(julgado,"^\n+$")){

      imagem <- TRUE
    } else {

      imagem <- FALSE
    }

    tibble::tibble(processo,cpw, julgado)

  },NULL))




}

