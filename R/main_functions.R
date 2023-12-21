.tab <- tab <- tibble::tibble(
  country = c("Deutschland Heyho dsadas dasad dasdsa sda dsa das", "Germany", "Germny")
)

.col <- quote(country)
.col_country <- quote(test)
.col_sim <- quote(sim)
.col_code <- quote(country_code)


#' Location Lookup
#'
#' @param .str A string with location Data
#' @param .type c("country", "city", "state")
#' @param .match c("complete", "partial")
#' @param .min_sim Minimum OSA similarity for fuzzy matches (default = .75)
#' @param .max_return Maximum Number of returned matches (default = 5)
#' @param .country_code Country to search the in
#'
#' @return A Dataframe
#' @export
#'
#' @examples
#' .str        <- c("MARTINEZ, CA,")
#' .type       <- c("country", "city", "state")
#' .max_dist   <- 2
#' .match      <- "partial"
#' .min_sim    <- .75
#' .max_return <- 5
lookup_location <- function(
  .str, .type = c("country", "city", "state"), .match = c("complete", "partial"),
  .min_sim = .75, .max_return = 5, .country_code = NA)
  {
  lst_lookup <- list()
  .match <- match.arg(.match)

  name_raw <- id <- name_adj <- ngram <- name_match <- country_code <- sim <- name <- NULL

  itab0 <- tibble::tibble(id = seq_along(.str), name_raw = .str)
  itab1 <- dplyr::mutate(itab0, name_adj = standardize_names(name_raw))
  itab0 <- dplyr::select(itab0, id, name_raw)

  if (.match == "partial") {
    itab1 <- tidytext::unnest_ngrams(itab1, name_adj, name_adj, n_min = 1, n = 50)
  }
  itab1 <- dplyr::mutate(itab1, ngram = stringi::stri_count_fixed(name_adj, " ") + 1)


  helper_transform <- function(.tab) {
    .tab %>%
      dplyr::arrange(id, dplyr::desc(ngram), dplyr::desc(sim)) %>%
      dplyr::distinct(id, name_raw, country_code, .keep_all = TRUE) %>%
      dplyr::filter(sim >= .min_sim) %>%
      dplyr::group_by(id, name_raw) %>%
      dplyr::arrange(dplyr::desc(ngram), dplyr::desc(sim), .by_group = TRUE) %>%
      dplyr::slice(1:.max_return) %>%
      dplyr::select(-name_adj, -ngram, -source) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), list), .groups = "drop") %>%
      dplyr::mutate(n = lengths(sim))
  }

  lst_lookup[["orig"]] <- itab0
  for (i in .type) {

    if (i == "country") {
      tab_lu <- Rlookup::tab_country
    } else if (i == "city") {
      tab_lu <- Rlookup::tab_cities
    } else if (i == "state") {
      tab_lu <- Rlookup::tab_states
    }

    if (!is.na(.country_code)) {
      tab_lu <- dplyr::filter(tab_lu, country_code == .country_code)
    }

    itab_full <- itab1 %>%
      dplyr::filter(!is.na(name_adj)) %>%
      dplyr::inner_join(tab_lu, by = c("name_adj" = "name")) %>%
      dplyr::mutate(sim = 1) %>%
      dplyr::mutate(name_match = name_adj) %>%
      helper_transform()

    if (all(itab0$id %in% itab_full$id) | .min_sim == 1) {
      itab_fuzz <- tibble::tibble()
    } else {
      itab_fuzz <- itab1 %>%
        dplyr::filter(!id %in% itab_full$id) %>%
        dplyr::filter(!is.na(name_adj)) %>%
        fuzzyjoin::stringdist_inner_join(tab_lu, by = c("name_adj" = "name")) %>%
        dplyr::mutate(sim = round(stringdist::stringsim(name_adj, name), 4)) %>%
        dplyr::rename(name_match = name) %>%
        helper_transform()
    }

    lst_lookup[[i]] <- dplyr::bind_rows(itab_full, itab_fuzz)
    colnames(lst_lookup[[i]]) <- c("id", "name_raw", paste0(i, "_", colnames(lst_lookup[[i]])[-(1:2)]))
  }

  itab_out <- purrr::reduce(lst_lookup, dplyr::left_join, by = c("id", "name_raw"))
  return(itab_out)
}

#' Wrapper around lookup_location
#'
#' @param .str A character string with country names
#' @param .min_sim Minimum similarity
#' @param .sim Include similarity in character names
#'
#' @return A charcter with ISO 3 codes
#' @export
#'
#' @examples
#' standardize_country(c("Palestine", "Palestine", "Palestine"), .1, .sim = TRUE)
#'
standardize_country <- function(.str, .min_sim = .25, .sim = FALSE) {
  itab <- lookup_location(.str, "country", "complete", .min_sim, 1)

  chr_out <- itab[["country_country_code"]]
  is.na(chr_out) <- lengths(chr_out) == 0
  chr_out <- unlist(chr_out)

  if (.sim) {
    chr_sim <- itab[["country_sim"]]
    is.na(chr_sim) <- lengths(chr_sim) == 0
    chr_sim <- unlist(chr_sim)
    names(chr_out) <- chr_sim
  }

  return(chr_out)
}
