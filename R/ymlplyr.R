##' Extract and Modify YAML file headers
##'
##' `yml_extract` returns the YAML header as a list. When the list of elements
##' provided by the `what` argument do not already exist in the YAML file, both
##' `yml_overwrite` and `yml_append` behave the same way and will add them to
##' the YAML header. However, if the elements already exist, `yml_overwrite`
##' will replace the values with the ones provided, while `yml_append` will add
##' them.
##'
##' Use `NULL` to remove a value from the YAML header.
##'
##' @param file the path to the file that contains the YAML header to modify.
##' @param what a list providing the elements to add or modify in the YAML
##'   header.
##' @param output the path to the file with the modified YAML header.
##' @param encoding the encoding for the input file.
##' @return the path of the file containing the modified YAML header.
##' @rdname yml_modify
##' @examples
##'
##' pet_yaml_file <- system.file("yaml_examples", "pets.yml",
##'                              package="ymlplyr")
##' cat(readLines(con = pet_yaml_file), sep="\n")
##'
##' ## Extract YAML header
##' yml_extract(pet_yaml_file)
##'
##' ## Add elements
##' example1 <- tempfile()
##' yml_append(pet_yaml_file,
##'            list(pets = list(dogs = c("Milou", "Santa Claus"),
##'                             cats = "Kitty",
##'                             rat = "Ratatouille")),
##'            output = example1)
##' cat(readLines(con = example1), sep = "\n")
##' unlink(example1)
##'
##' ## Replace elements
##' example2 <- tempfile()
##' yml_replace(pet_yaml_file,
##'            list(pets = list(dogs = "Milou", cats = "Kitty")),
##'            output = example2)
##' cat(readLines(con = example2), sep = "\n")
##' unlink(example2)
##'
##' ## Remove elements
##' example3 <- tempfile()
##' yml_replace(pet_yaml_file, list(pets = list(dogs = NULL)),
##'             output = example3)
##' cat(readLines(con = example3), sep = "\n")
##' unlink(example3)
##'
##'
##' @export
yml_replace <- function(file, what, output = file,
                        encoding = getOption("encoding")) {
  update_yml(file = file, what = what, output = output,
             method = "overwrite",
             encoding = encoding)
}

##' @export
##' @rdname yml_modify
yml_append <- function(file, what, output = file,
                       encoding = getOption("encoding")) {
  update_yml(file = file, what = what, output = output,
             method = "append",
             encoding = encoding)
}

extract_lines <- function(file, encoding = getOption("encoding")) {
  file <- normalizePath(file)

  ## Read entire file
  lines <- read_ymlplyr_utf8(file, encoding = encoding)
}


##' @export
##' @rdname yml_extract
yml_extract <- function(file, encoding = getOption("encoding")) {

  lines <- extract_lines(file)

  ## Extract the YAML header as a list
  parse_yml(lines)
}



##' @importFrom yaml as.yaml
##' @importFrom purrr list_modify list_merge
##' @importFrom rlang !!!
update_yml <- function(file, what, output = file,
                       method = c("overwrite", "append"),
                       encoding = getOption("encoding")) {

  method <- match.arg(method)

  lines <- extract_lines(file, encoding = encoding)

  existing_yml <- parse_yml(lines)

  yml_bounds <- attr(existing_yml, "yml_bounds")

  if (is.null(existing_yml)) {
    if (!identical(file, output)) {
      file.copy(file, output)
      return(output)
    } else {
      return(file)
    }
  }

  if (identical(method, "overwrite"))
    fxn <- purrr::list_modify
  else if (identical(method, "append"))
    fxn <- purrr::list_merge

  new_yml <- fxn(existing_yml, !!!what)

  new_yml <- yaml::as.yaml(new_yml)
  new_yml <- gsub("\n$", "", new_yml)
  new_yml <- enc2utf8(new_yml)

  lines <- lines[(max(yml_bounds)+1L):length(lines)]

  if (nchar(gsub("\\{\\}", "", new_yml)) >  0L) {
    lines <- c(
      "---",
      new_yml,
      "---",
      lines
    )
  }

  write_ymlplyr(lines, output)
}


get_yml_bounds <- function(input) {
  grep("^---\\s?$", input)
}

##' @importFrom yaml yaml.load
parse_yml <- function(input) {
  yml_bounds <- get_yml_bounds(input)

  if (identical(length(yml_bounds), 0L) ||
        !identical(min(yml_bounds), 1L))
    return(NULL)

  res <- yaml::yaml.load(input[yml_bounds[1]:yml_bounds[2]],
                         ## hack to work around: "root: ."
                         ## which gets recognized as REAL
                         handlers = list(
                           "float#fix" = function(x) x
                         ))
  if (is.null(res)) return(NULL)
  attr(res, "yml_bounds") <- yml_bounds
  res
}


has_yml_header <- function(f) {
  !is.null(parse_yml(read_ymlplyr_utf8(f)))
}

##' @importFrom purrr %>%
yml_variable_use <- function(path, pattern, vars, recursive = TRUE) {
  lf <- list.files(path = path, pattern = pattern,
                   full.names = TRUE, recursive = recursive)

  res <- purrr::map(lf, ~ parse_yml(read_ymlplyr_utf8(.x)))

  cbind(
    file = lf,
    purrr::map2_df(lf, res, function(.f, .r) {
      purrr::map(vars, function(.v) {
        if (rlang::has_name(.r, .v))
          .r[[.v]]
        else NA
      }) %>%
        rlang::set_names(vars) %>%
        tibble::as.tibble()
    })
  )
}
