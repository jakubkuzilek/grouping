#' R6 Class GroupAlgorithm
#'
#' @description
#' GroupAlgorithm is basic (maternal) R6 class for creating student groupings.
#'
#' @details
#' GroupAlgorithm is basic (maternal) R6 class for creating student groupings. It
#' does not implement any grouping algorithm, but it implements helper
#' functionalities and proper behavior. This class should be used as a template
#' for creating new Algorithm class inheriting from this one.
#'
#' All methods constructors should be ending with "_algorithm".
#'
#' @importFrom R6 R6Class
GroupAlgorithm <- R6::R6Class(classname = "GroupAlgorithm",

                              # private variables
                              private = list(
                                .name = "Grouping Algorithm",
                                .features = NULL,
                                .preferences = NULL,
                                .parameters = NULL,
                                .param_requirements = NULL,
                                .upper_bound = NULL,
                                .lower_bound = NULL,
                                .result = NULL,
                                # check the algorithm parameters
                                .check_params = function(parameters,
                                                         requirements) {
                                  if((length(parameters) != length(requirements)) &
                                     length(parameters) == 0){
                                    stop("None of the required parameters is specified.")
                                  }

                                  if(!(
                                    all(names(parameters) %in% names(requirements)) &
                                    all(names(requirements) %in% names(parameters))
                                    )) {
                                      indx <- which(! names(parameters) %in% names(requirements))
                                      missing_params <- names(parameters)[indx]

                                      indx <- which(! names(requirements) %in% names(parameters))
                                      missing_params <- c(missing_params, names(requirements)[indx])

                                      stop(paste("Parameter(s) missing/residing:",
                                                 paste(missing_params,
                                                       collapse = ", ")
                                                 )
                                           )
                                  }

                                  invisible(TRUE)
                                }
                              ),

                              # getters and setters
                              active = list(

                                #' @field features data.frame containing the grouping data
                                features = function(value) {
                                  if(missing(value)) {
                                    private$.features
                                  } else {
                                    stopifnot(is.data.frame(value),
                                              ncol(value) > 1,
                                              nrow(value) > 1)
                                    private$.features <- value
                                    self
                                  }
                                },

                                #' @field preferences num matrix containing the preferences data
                                preferences = function(value) {
                                  if(missing(value)) {
                                    private$.preferences
                                  } else {
                                    stopifnot(is.matrix(value) | is.null(value),
                                              ncol(value) == nrow(private$.features),
                                              nrow(value) == nrow(private$.features))
                                    private$.preferences <- value
                                    self
                                  }
                                },

                                #' @field parameters list containing algorithm parameters
                                parameters = function(value) {
                                  if(missing(value)) {
                                    private$.parameters
                                  } else {
                                    stopifnot(is.null(value) | is.list(value))
                                    private$.check_params(value,
                                                          private$.param_requirements$params)
                                    private$.parameters <- value
                                    self
                                  }
                                },

                                #' @field bounds num scalar or vector of 2 elements with bounds
                                bounds = function(value) {
                                  if(missing(value)) {
                                    c(private$.lower_bound, private$.upper_bound)
                                  } else {
                                    stopifnot(is.numeric(value),
                                              length(value) < 3,
                                              length(value) > 0)
                                    if(length(value) == 1) {
                                      private$.lower_bound <- private$.upper_bound <- value
                                    } else {
                                      private$.lower_bound <- value[1]
                                      private$.upper_bound <- value[2]
                                    }
                                    self
                                  }
                                },

                                #' @field result vector containing assignment of each student to group
                                result = function(value) {
                                  if(missing(value)) {
                                    private$.result
                                  } else {
                                    warning("Result cannot be manually assigned. Ignoring input.")
                                  }
                                }
                              ),

                              # public functions
                              public = list(

                                #' @description
                                #' Create new object
                                #' @param features data.frame containing grouping data
                                #' @param preferences matrix of student preferences between 0 and 1
                                #' @param parameters list containing algorithm configuration
                                #' @param param_requirements list containing all required parameters and possible values
                                #' @param bounds num scalar or vector of 2 elements with group bounds
                                #'
                                #' @return A new object
                                initialize = function(features,
                                                      preferences,
                                                      parameters = NULL,
                                                      param_requirements,
                                                      bounds){

                                  private$.param_requirements <- param_requirements

                                  stopifnot(is.data.frame(features),
                                            ncol(features) > 1,
                                            nrow(features) > 1)
                                  private$.features <- features

                                  stopifnot(is.matrix(preferences) | is.null(preferences),
                                            ncol(preferences) == nrow(private$.features),
                                            nrow(preferences) == nrow(private$.features))
                                  private$.preferences <- preferences

                                  stopifnot(is.null(parameters) | is.list(parameters))
                                  private$.check_params(parameters,
                                                        private$.param_requirements$params)
                                  private$.parameters <- parameters

                                  stopifnot(is.numeric(bounds),
                                            length(bounds) < 3,
                                            length(bounds) > 0)
                                  if(length(bounds) == 1) {
                                    private$.lower_bound <-
                                      private$.upper_bound <-
                                      bounds
                                  } else {
                                    private$.lower_bound <- bounds[1]
                                    private$.upper_bound <- bounds[2]
                                  }

                                },

                                #' @description
                                #' Print the information about the algorithm.
                                #' @param ... dots for compatibility reasons
                                #' @return invisible self
                                print = function(...) {
                                  cat("Grouping algoritm: ", private$.name,"\n", sep = "")
                                  cat("==================================================\n")
                                  cat("Features:\n")
                                  cat("---------\n")
                                  str(private$.features)
                                  cat("\n")
                                  cat("Parameters:\n")
                                  cat("-----------\n")
                                  str(private$.parameters)
                                  cat("\n")
                                  cat("Bounds:\n")
                                  cat("-------\n")
                                  cat("lower: ",private$.lower_bound,"\n", sep= "")
                                  cat("upper: ",private$.upper_bound,"\n", sep= "")
                                  cat("\n")
                                  cat("Result:\n")
                                  cat("-------\n")
                                  if(is.null(private$.result)) {
                                    cat("Result unknown: to trigger computation use group(<this object>).")
                                  } else {
                                    cat(as.character(private$.result))
                                  }

                                  invisible(self)
                                },

                                #' @description
                                #' Prints out required parameters and their specification
                                #' @return invisible self
                                get_required_parameters = function(){
                                  private$.param_requirements
                                },

                                #' @description
                                #' Perform grouping of students.
                                #' @return invisible self
                                group = function() {
                                  stop("You cannot call group on GroupAlgorithm object!")
                                }
                              )
)


.group_algorithm_param_requirements <-
  list()
# EXAMPLE
  # list(
  #   param1 = list(val = c(0,1), desc = "This is param1")),
  #   param2 = list(val = c("A","B","C"), desc = "This is param2")),
  #   param3 = list(val = c(TRUE,FALSE), desc = "This is param3"))
  # )

#' Constructor function for GroupAlgorithm R6 class.
#'
#' @description Creates GroupAlgorithm object.
#' @param features data.frame containing the grouping data
#' @param preferencds matrix of preferences [NOT USED]
#' @param parameters list containing algorithm parameters
#' @param bounds num scalar or vector of 2 elements with bounds
#'
#' @return GroupAlgorithm object.
#'
#' @export
group_algorithm <- function(features, preferences = NULL, parameters = NULL, bounds) {
  GroupAlgorithm$new(features,
                     preferences,
                     parameters,
                     .group_algorithm_param_requirements,
                     bounds)
}
