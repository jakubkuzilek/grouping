#' Function for getting available grouping algorithms
#'
#' @description Returns vector of names of available algorithms
#'
#' @return vector
#' @export
get_available_algorithms = function(){
  lst_function <-
    ls("package:grouping") %>%
    .[grepl("_algorithm$",.)] %>%
    .[!grepl("^group",.)]

  lst_function
}
