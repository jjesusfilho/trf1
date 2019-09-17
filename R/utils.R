divisor <- function(x) {x %>%
    as.character() %>%
    nchar() %>%
    exp() %>%
    floor()}
