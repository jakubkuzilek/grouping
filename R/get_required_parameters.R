#' Function for returning required parameters of a grouping algorithm
#'
#' @description Returns required parameters list
#'
#' @param algorithm Character name of the algorithm (same name as shown in get_available_algorithms)
#'
#' @return list
#' @export
get_required_parameters = function(algorithm){
  tryCatch(get(paste0(".", algorithm, "_param_requirements")),
           error = function(e) print(e))#"Unknown algorithm")
}
