get_files_with_yml_header <- function(...) {
  lf <- list.files(...)

  tibble::tibble(files = lf) %>%
        dplyr::mutate(has_yaml = purrr::map_lgl(files, has_yaml_header))
}

