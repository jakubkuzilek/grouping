#' Function for getting available grouping algorithms
#'
#' @description Returns vector of names of available algorithms
#'
#' @return vector
#' @export
get_available_algorithms = function(){
  lst_function <- ls("package:grouping")

  lst_function[grepl("_algorithm$",lst_function)]
}
