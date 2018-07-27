##' Modify YAML file headers
##'
##' When the list of elements provided does not already exist in the YAML file,
##' both `yml_overwrite` and `yml_append` behave the same way and will add it to
##' the YAML header. However, if the element already exists, `yml_overwrite`
##' will replace the values with the one provided by the function, while
##' `yml_append` will add them.
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
  update_yml(file = file, what = what, output = output, method = "overwrite",
             encoding = encoding)
}

##' @export
##' @rdname yml_modify
yml_append <- function(file, what, output = file,
                       encoding = getOption("encoding")) {
  update_yml(file = file, what = what, output = output, method = "append",
             encoding = encoding)
}


##' @importFrom yaml as.yaml
##' @importFrom purrr list_modify list_merge
##' @importFrom rlang !!!
update_yml <- function(file, what, output = file,
                       method = c("overwrite", "append"),
                       encoding = getOption("encoding")) {

  method <- match.arg(method)
  file <- normalizePath(file)

  ## Read entire file
  lines <- read_ymlplyr_utf8(file, encoding = encoding)

  ## Extract the YML header as a list
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

  lines <- c(
    "---",
    new_yml,
    "---",
    lines[(max(yml_bounds)+1):length(lines)]

  )

  write_ymlplyr(lines, output)
}


get_yml_bounds <- function(input) {
  grep("^---\\s?$", input)
}

##' @importFrom yaml yaml.load
parse_yml <- function(input) {
  yml_bounds <- get_yml_bounds(input)

  if (length(yml_bounds) ==  0L)
    return(NULL)

  res <- yaml::yaml.load(input[yml_bounds[1]:yml_bounds[2]],
                         ## hack to work around: "root: ."
                         ## which gets recognized as REAL
                         handlers = list(
                           "float#fix" = function(x) x
                         ))
  attr(res, "yml_bounds") <- yml_bounds
  res
}


has_yml_header <- function(f) {
  !is.null(parse_yml(read_ymlplyr_utf8(f)))
}
