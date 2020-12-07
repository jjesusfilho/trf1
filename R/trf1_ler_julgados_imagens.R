#' Lê sentenças de primeiro grau de pdf em imagens
#'
#' @param arquivos Vetor de arquivos
#'
#' @details O datafrane criado com a trf1_ler_cpopg_docs
#'      informa se é imagem ou não.
#' @return Tibble
#' @export
#'
trf1_ler_julgados_imagens <- function(arquivos = NULL){



  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{

    processo <- stringr::str_extract(.x,"\\d{20}")

    cpw <- stringr::str_extract(.x,"(?<=cpw_)\\d+")



    tryCatch({
      dir.create(dir1 <- file.path(tempdir(), "imagem"))
    },warning = function(w){
      unlink(dir1, recursive = TRUE)
      dir.create(dir1 <- file.path(tempdir(), "imagem"))

    }
    )

    pb$tick()

    paginas <- seq_len(pdftools::pdf_info(.x)$pages)


    arq  <- file.path(dir1,paste0("pagina_",paginas,".png"))

    arquivo_img <-   pdftools::pdf_convert(.x, dpi = 600,filenames = arq)


    julgado <- purrr::map(arquivo_img,purrr::possibly(~{
      .x %>%
        magick::image_read() %>%
        magick::image_resize("1500x") %>%
        magick::image_convert(type = 'Grayscale') %>%
        #magick::image_trim(fuzz = 90) %>%
        #magick::image_write(format = 'png', density = '300x300') %>%
        tesseract::ocr(engine = tesseract::tesseract("por"))

    },NA_character_)) %>%
      unlist() %>%
      stringr::str_c(collapse = "\n\n")

    unlink(dir1, recursive = TRUE)

    tibble::tibble(processo = processo, cpw = cpw, julgado = julgado, imagem = 1L)


  },NULL))


}
