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
#' @export
GroupAlgorithm <- R6::R6Class(classname = "GroupAlgorithm",

                              # private variables
                              private = list(
                                .name = "Grouping Algorithm",
                                .features = NULL,
                                .parameters = NULL,
                                .upper_bound = NULL,
                                .lower_bound = NULL,
                                .result = NULL
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
                                    private$.attributes <- value
                                    self
                                  }
                                },

                                #' @field parameters list containing algorithm parameters
                                parameters = function(value) {
                                  if(missing(value)) {
                                    private$.parameters
                                  } else {
                                    stopifnot(is.list(value))
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
                                #' Create new GroupAlgorithm object
                                #' @param features data.frame containing grouping data
                                #' @param parameters list containing algorithm configuration
                                #' @param bounds num scalar or vector of 2 elements with group bounds
                                #'
                                #' @return A new `GroupAlgorithm` object
                                initialize = function(features,
                                                      parameters,
                                                      bounds){

                                  stopifnot(is.data.frame(features),
                                            ncol(features) > 1,
                                            nrow(features) > 1)
                                  private$.features <- features

                                  stopifnot(is.list(parameters))
                                  private$.parameters <- parameters

                                  stopifnot(is.numeric(bounds),
                                            length(bounds) < 3,
                                            length(bounds) > 0)
                                  if(length(bounds) == 1) {
                                    private$.lower_bound <- private$.upper_bound <- bounds
                                  } else {
                                    private$.lower_bound <- bounds[1]
                                    private$.upper_bound <- bounds[2]
                                  }

                                },

                                #' @description
                                #' Print the information about the model.
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
                                    cat(as.character(private$.features))
                                  }

                                  invisible(self)
                                },

                                #' @description
                                #' Perform grouping of students.
                                #' @return invisible self
                                group = function() {
                                  stop("You cannot call group on GroupAlgorithm object!")
                                }
                              )
)

#' Constructor function for GroupAlgorithm R6 class.
#'
#' @description Creates GroupAlgorithm object.
#' @param features data.frame containing the grouping data
#' @param parameters list containing algorithm parameters
#' @param bounds num scalar or vector of 2 elements with bounds
#'
#' @return GroupAlgorithm object.
#'
#' @export
group_algorithm <- function(features, parameters, bounds) {
  GroupAlgorithm$new(features, parameters, bounds)
}
