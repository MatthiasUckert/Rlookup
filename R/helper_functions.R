#' Standardize Names
#'
#' @param .str A character String
#'
#' @return A character String
#' @export
standardize_names <- function(.str) {
  . <- NULL
  .str %>%
    stringi::stri_enc_toascii() %>%
    stringi::stri_escape_unicode() %>%
    stringi::stri_replace_all_regex(., "[[:punct:]]", " ") %>%
    stringi::stri_replace_all_regex(., "([[:blank:]]|[[:space:]])+", " ") %>%
    tolower() %>%
    trimws()
}
