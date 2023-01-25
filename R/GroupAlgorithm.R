#' R6 Class GroupAlgorithm
#'
#' @description
#' GroupAlgorithm is basic (maternal) R6 class for creating student groupings.
#'
#' @details
#' GroupAlgorithm is basic (maternal) R6 class for creating student groupings. It
#' does not implement any grouping algorithm, but it implements helper
#' functionalities and proper behavior.
#'
#' @export
GroupAlgorithm <- R6::R6Class(classname = "GroupAlgorithm",

                              private = list(
                                .features = NULL,
                                .parameters = NULL,
                                .upper_bound = NULL,
                                .lower_bound = NULL
                              ),

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
                                }
                              ),

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

                                #TODO: rework print function
                                #' @description
                                #' Print the information about the model.
                                #' @param ... dots for compatibility reasons
                                #' @return invisible self
                                print = function(...) {
                                  cat("Grouping algoritm: ", class(self)[1],"\n", sep = "")
                                  cat("--------------------------------------------------\n")
                                  object_names <- ls(private,all.names = TRUE)
                                  values <- vapply(object_names, function(name) {
                                    obj <- .subset2(private, name)
                                    if (is.function(obj)) "NA"
                                    else if (is.null(obj)) "NA"
                                    else if (is.atomic(obj)) {
                                      txt <- as.character(utils::head(obj, 60))
                                      txt <- paste(txt, collapse = ", ")
                                      trimws(txt)
                                    }
                                    else paste(class(obj), collapse = "")
                                  }
                                  , FUN.VALUE = character(1))

                                  object_names <- gsub("\\.","",object_names)

                                  cat(
                                    paste0(object_names[values != "NA"],": ",
                                           values[values != "NA"],"\n"),
                                    sep="")

                                  invisible(self)
                                },

                                #' @description
                                #' Perform grouping of students.
                                #' @return invisible self
                                group = function() {
                                  stop("You cannot call group on GroupAlgorithm object!")
                                }
                              ))
