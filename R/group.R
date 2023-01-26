#' Perform grouping.
#'
#' Function that executes the grouping over the provided algorithm object.
#'
#' @param model one model from group.formation.algorithms package
#' @param copy TRUE/FALSE make a copy of the model and perform the grouping on copy
#'
#' @return model with the groups created
#'
#' @examples
#' \dontrun{
#' g <- GroupAlgorithm$new(
#'        data.frame(a = c(1,2), b = c(2,2)),
#'        list(c = 1, d = "a"),
#'        c(3,4))
#' g2 <- group(g)
#' }
#'
#' @export
group <- function(model, copy = FALSE) {
  if(!any(class(model) == "GroupAlgorithm")) {
    stop("This fuction works with the grouping package algorithms only.")
  }

  if(copy) {
    model$clone()$group()
  } else {
    model$group()
  }
}
