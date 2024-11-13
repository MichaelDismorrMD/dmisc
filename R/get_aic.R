#' Obtain Akaike's information criterion from a mexhaz model object
#'
#' @param mod A mexhaz model object
#'
#' @return A dataframe with the AIC value
#' @export
#'
#' @examples
#' # We will use the pbc dataset from the `survival` package
#'
#' pbc <- survival::pbc %>% dplyr::mutate(time_years = time / 365.241) %>%
#'  dplyr::select(time_years, status, sex, age, albumin)
#'
#' kts <- quantile(pbc$time_years, probs=c(1/3,2/3)) # Knot positions for the baseline hazard
#'
#' outcome_mod <- mexhaz::mexhaz(Surv(time_years, status == 1) ~ sex + age + albumin,
#'                              data = pbc,
#'                              base = "exp.bs",
#'                              degree = 3,
#'                              knots = kts,
#'                              verbose = 0,
#'                              print.level = 0)
#'
#' get_aic(outcome_mod)
get_aic <- function(mod){
  data.frame(AIC = -2*mod$loglik + 2*mod$n.par)
}
