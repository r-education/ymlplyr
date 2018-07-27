
[![Travis-CI Build Status](https://travis-ci.org/fmichonneau/ymlplyr.svg?branch=master)](https://travis-ci.org/fmichonneau/ymlplyr)

<!-- README.md is generated from README.Rmd. Please edit that file -->
ymlplyr
=======

This package can be used to manipulate the YAML file headers.

Installation
------------

You can install ymlplyr from github with:

``` r
# install.packages("remotes")
remotes::install_github("fmichonneau/ymlplyr")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
pet_yaml_file <- system.file("yaml_examples", "pets.yml",
                             package="ymlplyr")
cat(readLines(con = pet_yaml_file), sep="\n")
#> ---
#> pets:
#>   dogs:
#>     - Snoopy
#>     - Lassie
#>   cats:
#>     - Garfield
#>     - Tigger
#>     - Maru
#> ---
#> 
#> 
#> This is a a pet example.

## Add elements
example1 <- tempfile()
yml_append(pet_yaml_file,
           list(pets = list(dogs = c("Milou", "Santa Claus"),
                            cats = "Kitty",
                            rat = "Ratatouille")),
           output = example1)
#> [1] "/tmp/RtmpZTmsRL/file361f1fa97983"
cat(readLines(con = example1), sep = "\n")
#> ---
#> pets:
#>   dogs:
#>   - Snoopy
#>   - Lassie
#>   - Milou
#>   - Santa Claus
#>   cats:
#>   - Garfield
#>   - Tigger
#>   - Maru
#>   - Kitty
#>   rat: Ratatouille
#> ---
#> 
#> 
#> This is a a pet example.
unlink(example1)

## Replace elements
example2 <- tempfile()
yml_replace(pet_yaml_file,
           list(pets = list(dogs = "Milou", cats = "Kitty")),
           output = example2)
#> [1] "/tmp/RtmpZTmsRL/file361f221127f4"
cat(readLines(con = example2), sep = "\n")
#> ---
#> pets:
#>   dogs: Milou
#>   cats: Kitty
#> ---
#> 
#> 
#> This is a a pet example.
unlink(example2)

## Remove elements
example3 <- tempfile()
yml_replace(pet_yaml_file, list(pets = list(dogs = NULL)),
            output = example3)
#> [1] "/tmp/RtmpZTmsRL/file361f46ea8100"
cat(readLines(con = example3), sep = "\n")
#> ---
#> pets:
#>   cats:
#>   - Garfield
#>   - Tigger
#>   - Maru
#> ---
#> 
#> 
#> This is a a pet example.
unlink(example3)
```
