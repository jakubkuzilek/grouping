#' R6 Class RandomAlgorithm
#'
#' @description
#' RandomAlgorithm is R6 class for creating student random groupings.
#'
#' @details
#' RandomAlgorithm is very simple (example) algorithm for creating the groups.
#'
#' @importFrom R6 R6Class
RandomAlgorithm <- R6::R6Class(classname = "RandomAlgorithm",
                               inherit = GroupAlgorithm,

                              # private variables
                              private = list(
                                .name = "Random Groups Algorithm"
                              ),

                              # public functions
                              public = list(

                                #' @description
                                #' Perform grouping of students.
                                #' @return invisible self
                                group = function() {

                                  number_of_students <- nrow(private$.features)

                                  number_of_groups <- floor(number_of_students/private$.upper_bound)

                                  residual_students <- number_of_students - number_of_groups*private$.upper_bound

                                  if(abs(residual_students) < private$.lower_bound) {
                                    stop("Cannot fit the students to the bounds. Algorithm uses upper bound and then
                                         fits the rest to the additional one group.")
                                  }

                                  # assign random groups
                                  res <- rep(1:number_of_groups, private$.upper_bound)[1:number_of_students]
                                  res[is.na(res)] <- number_of_groups+1

                                  # set result
                                  private$.result <- res

                                  invisible(self)
                                }
                              )
)

.random_algorithm_param_requirements <-
  list()

#' Constructor function for RandomAlgorithm R6 class.
#'
#' @description Creates RandomAlgorithm object.
#' @param features data.frame containing the grouping data
#' @param parameters list containing algorithm parameters
#' @param bounds num scalar or vector of 2 elements with bounds
#'
#' @return RandomAlgorithm object.
#'
#' @export
random_algorithm <- function(features, parameters, bounds) {
  RandomAlgorithm$new(features,
                      parameters,
                      .random_algorithm_param_requirements,
                      bounds)
}
