#' Get Bayes information criterion from a mexhaz model object
#'
#' @param mod a mexhaz model object
#'
#' @return A dataframe with the BIC value
#' @export
#'
#' @examples
#' # We will use the pbc dataset from the `survival` package
#'
#' pbc <- survival::pbc %>% dplyr::mutate(time_years = time / 365.241) %>%
#'  dplyr::select(time_years, status, sex, age, albumin)
#'
#' kts <- quantile(pbc$time_years, probs=c(1/3,2/3)) # Knot positions for the baseline hazard
#' outcome_mod <- mexhaz::mexhaz(Surv(time_years, status == 1) ~ sex + age + albumin,
#'                              data = pbc,
#'                              base = "exp.bs",
#'                              degree = 3,
#'                              knots = kts,
#'                              verbose = 0,
#'                              print.level = 0)
#'
#' get_bic(outcome_mod)
get_bic <- function(mod){
  data.frame(BIC = -2 * mod$loglik + log(mod$n.obs) * mod$n.par)
}
