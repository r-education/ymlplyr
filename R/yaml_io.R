### taken from rmarkdown
read_lines_utf8 <- function(file, encoding) {
  lines <- readLines(file, warn = TRUE)
  to_utf8(lines, encoding)
}

to_utf8 <- function(x, encoding) {
  if (identical(encoding, "native.enc"))
    encoding <- ""
  if (!identical(encoding, "UTF-8"))
    iconv(x, from = encoding, to = "UTF-8")
  else mark_utf8(x)
}

mark_utf8 <- function (x) {
  if (is.character(x)) {
    Encoding(x) <- "UTF-8"
    return(x)
  }
  if (!is.list(x))
    return(x)
  attrs <- attributes(x)
  res <- lapply(x, mark_utf8)
  attributes(res) <- attrs
  names(res) <- mark_utf8(names(res))
  res
}

##' @importFrom purrr quietly
read_ymlplyr_utf8 <- function(file, encoding = getOption("encoding")) {
  quietly_read <- purrr::quietly(read_lines_utf8)
  res <- quietly_read(file, encoding)

  ## TODO: use mime type?
  ## warning for binary files
  if (any(grepl("appears to contain an embedded nul", res[["warnings"]]))) {
    warning(file, " seems to be a binary file, so we didn't look if it",
            " contained a YAML header.",
            call. = FALSE)
    return(character(0))
  }

  res$result
}

write_ymlplyr <- function(lines, output) {
  writeLines(lines, output, useBytes = TRUE)
  output
}
