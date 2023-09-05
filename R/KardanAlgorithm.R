#' R6 Class KardanAlgorithm
#'
#' @description
#' KardanAlgorithm is R6 class for creating student random groupings.
#'
#' @details
#' KardanAlgorithm is the grouping model developed by Kardan et. al. based on homogeneous
#' and heterogeneous attributes, student preferences. The model computes compatibility
#' measure and then performs the mathematical modeling (long-step simplex optimization).
#' Fairness is guaranteed by initial step, which checks the convergence to solution.
#' The algorithm is not suitable for large groups of students (> 100). The algorithm
#' is implemented without fairness step.
#'
#' Ahmad A. Kardan & Hamid Sadeghi (2016).
#' \emph{An efficacious dynamic mathematical modelling approach for creation
#'  of best collaborative groups}. Mathematical and Computer Modelling
#'  of Dynamical Systems, 22:1, 39-53, DOI: \url{https://doi.org/10.1080/13873954.2015.1086382}
#'
#' @importFrom R6 R6Class
#' @import ROI.plugin.glpk
KardanAlgorithm <- R6::R6Class(classname = "KardanAlgorithm",
                               inherit = GroupAlgorithm,

                               # private variables
                               private = list(
                                 .name = "Kardan Algorithm",
                                 .compatibility = NULL,

                                 # check algorithm params
                                 .check_params = function(parameters,
                                                          requirements) {
                                   # general check use GroupAlgorithm
                                   super$.check_params(parameters,
                                                       requirements)

                                   if((length(parameters$homogeneous) > ncol(private$.features))){
                                     stop("Too many homogeneous parameters.")
                                   }

                                   if((length(parameters$heterogeneous) > ncol(private$.features))){
                                     stop("Too many heterogeneous parameters.")
                                   }

                                   if((length(parameters$homogeneous) + length(parameters$heterogeneous) != ncol(private$.features))){
                                     stop("Too many homogeneous and heterogeneous parameters.")
                                   }

                                   if(any(! parameters$homogeneous %in% names(private$.features))) {
                                     stop("Non-exitent feature(s) defined in homogeneous.")
                                   }

                                   if(any(! parameters$heterogeneous %in% names(private$.features))) {
                                     stop("Non-exitent feature(s) defined in heterogeneous.")
                                   }

                                   if(any(! names(private$.features) %in% c(parameters$heterogeneous, parameters$homogeneous))){
                                     stop("Undefinde feature in homogenous and heterogeneous lists.")
                                   }

                                   if(length(parameters$w_homogeneous) != length(parameters$homogeneous)) {
                                     stop("Non-matching length of weights and homogeneous parameters.")
                                   }

                                   if(length(parameters$w_heterogeneous) != length(parameters$heterogeneous)) {
                                     stop("Non-matching length of weights and homogeneous parameters.")
                                   }

                                   if(any(parameters$w_homogeneous > 1 | parameters$w_homogeneous < 0)) {
                                     stop("Weights of homogeneous features are out of bounds <0,1>.")
                                   }

                                   if(any(parameters$w_heterogeneous > 1 | parameters$w_heterogeneous < 0)) {
                                     stop("Weights of heterogeneous features are out of bounds <0,1>.")
                                   }

                                   if(sum(parameters$w_homogeneous) != 1){
                                     stop("Weights of homogeneous features needs to sum to 1.")
                                   }

                                   if(sum(parameters$w_heterogeneous) != 1){
                                     stop("Weights of heterogeneous features needs to sum to 1.")
                                   }

                                   if(any(parameters$weight_hom > 1 | parameters$weight_hom < 0)) {
                                     stop("Weight of homogeneous features is out of bounds <0,1>.")
                                   }

                                   if(any(parameters$weight_het > 1 | parameters$weight_het < 0)) {
                                     stop("Weight of heterogenous features is out of bounds <0,1>.")
                                   }

                                   if(parameters$weight_hom + parameters$weight_het != 1){
                                     stop("Sum of weight of homogeneous features and weight of heterogeneous features needs to sum to 1.")
                                   }

                                   if(is.null(private$.preferences)) {
                                     parameters$weight_features <- 1
                                     parameters$weight_preferences <- 0
                                   }

                                   if(any(parameters$weight_features > 1 | parameters$weight_features < 0)) {
                                     stop("Weight of homogeneous features is out of bounds <0,1>.")
                                   }

                                   if(any(parameters$weight_preferences > 1 | parameters$weight_preferences < 0)) {
                                     stop("Weight of heterogenous features is out of bounds <0,1>.")
                                   }

                                   if(parameters$weight_features + parameters$weight_preferences != 1){
                                     stop("Sum of weight of homogeneous features and weight of heterogeneous features needs to sum to 1.")
                                   }

                                   invisible(TRUE)
                                 },

                                 # feature normalization
                                 .normalize = function(x) {
                                   x %>%
                                     apply(2, function(col) {
                                       (col - min(col))/(max(col) - min(col))
                                     })
                                 },

                                 # computes heterogeneous compatibility between students
                                 .compute_het_compatibility = function(df, w) {

                                   # for each row compute the compatibility
                                   lambda <- function(r, m, w) {
                                     # create matrix with the same row multiple times
                                     matrix(r, ncol = length(r), nrow = dim(m)[1], byrow=T) %>%
                                       # subtract all other person features
                                       magrittr::subtract(m) %>%
                                       # absolute value
                                       abs() %>%
                                       # multiply by weights of features
                                       magrittr::multiply_by(matrix(w, ncol = length(r), nrow = dim(m)[1], byrow = T)) %>%
                                       # summarise over all features
                                       apply(1, sum)
                                   }

                                   # transform to matrix and normalize
                                   df2 <-
                                     df %>%
                                     as.matrix() %>%
                                     private$.normalize()

                                   # compute the compatibility
                                   df2 %>%
                                     apply(1, lambda, df2, w)
                                 },

                                 .compute_hom_compatibility = function(df, w) {

                                   # for each row compute the compatibility
                                   lambda <- function(r, m, w) {
                                     #create matrix with the same row multiple times
                                     matrix(r, ncol = length(r), nrow = dim(m)[1], byrow=T) %>%
                                       # subtract all other person features
                                       magrittr::subtract(m) %>%
                                       # absolute values
                                       abs() %>%
                                       # multiply by weights of features
                                       magrittr::multiply_by(matrix(w, ncol = length(r), nrow = dim(m)[1], byrow = T)) %>%
                                       # summarise over all features
                                       apply(1, sum) %>%
                                       # subtract the value from 1
                                       magrittr::subtract(1, .)
                                   }

                                   # transform to matrix and normalize
                                   df2 <-
                                     df %>%
                                     as.matrix() %>%
                                     private$.normalize()

                                   # compute compatibility
                                   df2 %>%
                                     apply(1, lambda, df2, w)
                                 },

                                 # compute student preferences matrix
                                 .compute_preferences = function(m) {
                                   # one part of preferences student1 -> student2
                                   L <- m
                                   L[lower.tri(L)] <- 0

                                   # second part of preferences student1 <- student2
                                   U <- m
                                   U[upper.tri(U)] <- 0

                                   # average preference between student1 and student2
                                   Z <- (L + t(U))/2
                                   Z[lower.tri(Z)] <- Z[upper.tri(Z)]

                                   Z
                                 },

                                 # computes compatibility
                                 .compute_compatibility = function(attributes_hom,
                                                                   attributes_het,
                                                                   preferences,
                                                                   attr_weight_hom,
                                                                   attr_weight_het,
                                                                   weight_hom,
                                                                   weight_het,
                                                                   weight_attributes,
                                                                   weight_preferences) {

                                   # partial compatibilities
                                   c_het <- private$.compute_het_compatibility(attributes_het, attr_weight_het)
                                   c_hom <- private$.compute_hom_compatibility(attributes_hom, attr_weight_hom)

                                   # combination of partial compatibilities
                                   c <- weight_hom*c_hom + weight_het*c_het

                                   # preferences
                                   if(!is.null(preferences)) {
                                    c_pref <- private$.compute_preferences(preferences)

                                    # compatibility and preferences combined
                                    c_tot <- weight_attributes*c + weight_preferences*c_pref
                                   } else {
                                     # compatibility is equal only to the partial compatibilities combination
                                     c_tot <- c
                                   }

                                   # set diagonal to 1 (student needs to be with himself in the group)
                                   diag(c_tot) <- 1

                                   c_tot
                                 }
                               ),

                               # public functions
                               public = list(
                                 #' @description
                                 #' Perform grouping of students.
                                 #' @return invisible self
                                 group = function() {

                                   # get homogeneous features
                                   attributes_hom <-
                                     private$.features[private$.parameters$homogeneous]

                                   # get heterogeneous features
                                   attributes_het <-
                                     private$.features[private$.parameters$heterogeneous]

                                   # compute compatibility
                                   private$.compatibility <-
                                     private$.compute_compatibility(
                                       attributes_hom,
                                       attributes_het,
                                       private$.preferences,
                                       private$.parameters$w_homogeneous,
                                       private$.parameters$w_heterogeneous,
                                       private$.parameters$weight_hom,
                                       private$.parameters$weight_het,
                                       private$.parameters$weight_features,
                                       private$.parameters$weight_preferences
                                     )

                                   # set bounds
                                   lower_bound <- private$.lower_bound
                                   upper_bound <- private$.upper_bound

                                   # number of persons
                                   n <- nrow(private$.features)

                                   # optimizing for ompr MILP model
                                   int_c <- function(i,j) {
                                     private$.compatibility[i,j]
                                   }

                                   # define MILP model
                                   model <-
                                     ompr::MILPModel() %>%
                                     ompr::add_variable(x[i,j],i = 1:n, j = 1:n, type = "binary") %>%
                                     ompr::set_objective(sum_expr(sum_expr(int_c(i,j) * x[i,j], j = 1:n, i = 1:n))) %>%
                                     ompr::add_constraint( x[i,j] + x[i,k] - x[j,k] <= 1, i = 1:n, j = 1:n, k = 1:n, i <= j, i <= k, j <= k) %>%
                                     ompr::add_constraint( x[i,j] - x[i,k] + x[j,k] <= 1, i = 1:n, j = 1:n, k = 1:n, i <= j, i <= k, j <= k) %>%
                                     ompr::add_constraint(-x[i,j] + x[i,k] + x[j,k] <= 1, i = 1:n, j = 1:n, k = 1:n, i <= j, i <= k, j <= k) %>%
                                     ompr::add_constraint(sum_expr(x[a,i], a = 1:n)  >= lower_bound, i = 1:n) %>%
                                     ompr::add_constraint(sum_expr(x[a,i], a = 1:n)  <= upper_bound, i = 1:n) %>%
                                     ompr::add_constraint(x[i,j] == x[j,i], i = 1:n, j = 1:n)

                                   # solve
                                   result <- ompr::solve_model(model, ompr.roi::with_ROI(solver = "glpk", verbose = FALSE))

                                   # extract matchings
                                   matching <- result %>%
                                     ompr::get_solution(x[i,j]) %>%
                                     dplyr::filter(value > .9) %>%
                                     dplyr::select(i, j, value) %>%
                                     tidyr::pivot_wider(names_from = j, values_from = value, values_fill = 0) %>%
                                     dplyr::arrange(i) %>%
                                     tibble::column_to_rownames("i") %>%
                                     as.matrix()

                                   # convert to the vector of groups for each person
                                   private$.result <-
                                     matching %>%
                                     dplyr::as_tibble() %>%
                                     dplyr::mutate(id = dplyr::row_number()) %>%
                                     dplyr::select(id, dplyr::everything()) %>%
                                     tidyr::pivot_longer(.,2:ncol(.), names_to = "group_member", values_to = "val") %>%
                                     dplyr::filter(val == 1) %>%
                                     dplyr::arrange(id, group_member) %>%
                                     dplyr::group_by(id) %>%
                                     dplyr::summarise(group_members = stringr::str_c(group_member, collapse = ",")) %>%
                                     dplyr::ungroup() %>%
                                     dplyr::select(group_members) %>%
                                     dplyr::distinct() %>%
                                     dplyr::mutate(group_id = dplyr::row_number()) %>%
                                     tidyr::separate_longer_delim(group_members,delim=",") %>%
                                     dplyr::select(group_id, group_members) %>%
                                     dplyr::mutate(group_members = as.numeric(group_members)) %>%
                                     dplyr::arrange(group_members) %>%
                                     magrittr::extract2("group_id")

                                   invisible(self)
                                 }
                               )
)

.kardan_algorithm_param_requirements <-
  list(desc = "KardanAlgorithm is the grouping model developed by Kardan et. al. based on homogeneous
  and heterogeneous attributes, student preferences. The model computes compatibility
  measure and then performs the mathematical modeling (long-step simplex optimization).
  Fairness is guaranteed by initial step, which checks the convergence to solution.
  The algorithm is not suitable for large groups of students (> 100). The algorithm
  is implemented without fairness step.",
       data = list(features = "Data.frame (table) containing the grouping data features (numeric values only). nrows is number of students",
                   preferences = "Matrix containing student preferences. Value in <0,1>. dim(preferences) = nrows(features)"),
       params = list(homogeneous = list(val = character(0), desc = "String vector of homogeneous feature names. length(homogeneous) + length(heterogenous) = ncol(features)"),
                     heterogeneous = list(val = character(0), desc = "String vector of heterogeneous feature names. length(homogeneous) + length(heterogenous) = ncol(features)"),
                     w_homogeneous = list(val = c(0,1), desc = "Numeric vector of homogeneous feature weights towards grouping. Values in <0,1>. length(w_homogeneous) = length(homogeneous)"),
                     w_heterogeneous = list(val = c(0,1), desc = "Numeric vector of heterogeneous feature weights towards grouping. Values in <0,1>. length(w_heterogeneous) = length(heterogeneous)"),
                     weight_hom = list(val = c(0,1), desc = "Numeric value of weight of sum of all homogeneous features. Value in <0,1>"),
                     weight_het = list(val = c(0,1), desc = "Numeric value of weight of sum of all heterogeneous features. Value in <0,1>"),
                     weight_features = list(val = c(0,1), desc = "Numeric value of weight of features in final compatibility. Value in <0,1>"),
                     weight_preferences = list(val = c(0,1), desc = "Numeric value of weight of preferences in final compatibility. Value in <0,1>")
       ))

#' Constructor function for KardanAlgorithm R6 class.
#'
#' @description Creates KardanAlgorithm object.
#' @param features data.frame containing the grouping data
#' @param preferences matrix containing student preferences
#' @param parameters list containing algorithm parameters
#' @param bounds num scalar or vector of 2 elements with bounds
#'
#' @return KardanAlgorithm object.
#'
#' @export
kardan_algorithm <- function(features, preferences = NULL, parameters, bounds) {
  KardanAlgorithm$new(features,
                      preferences,
                      parameters,
                      .kardan_algorithm_param_requirements,
                      bounds)
}
