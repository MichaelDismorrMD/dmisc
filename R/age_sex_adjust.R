#' Calculate age- and sex-adjusted incidence rates
#'
#' This function uses a poisson model to adjust for age and sex.
#' It returns a dataframe with estimated incidence rate per 100 person years.
#' Uses marginaleffects::avg_predictions to obtain marginal population standardized estimates
#'
#' @param data A dataframe containting the variables used. Dataframe.
#' @param exposure_var Column in data that contains the exposure variable. Numeric or categorical vector.
#' @param age_var Column in data that contains the age variable in years. Numerical vector.
#' @param sex_var Column in data that contains the sex variable. Binary numerical or categorical vector.
#' @param event_var Column in data that contains the event indicator. Binary numerical vector, 0 = no event, 1 = event.
#' @param time_var Column in data that contains the follow-up time indicator in days. Numerical vector.
#'
#' @return A dataframe with the following columns
#' * `Treatment`:  Treatment groups
#' * `IR`:  Age- and sex-adjusted incidence rate per 100 person year
#' * `Lower_CI`:  Lower 95% confidence interval
#' * `Upper_CI`:  Upper 95% confidence interval
#' @export
#'
#' @examples
#' colon_death <- colon[colon$etype == 2, ] # Select rows with event indicator and follow-up time for death
#' age_sex_adjust(colon_death, rx, age, sex, status, time)
#' age_sex_adjust(colon[colon$etype == 2, ], rx, age, sex, status, time)
#'
age_sex_adjust <- function(data,
                            exposure_var,
                            age_var,
                            sex_var,
                            event_var,
                            time_var){

  exposure_var <- deparse(substitute(exposure_var))
  age_var <- deparse(substitute(age_var))
  sex_var <- deparse(substitute(sex_var))
  event_var <- deparse(substitute(event_var))
  time_var <- deparse(substitute(time_var))
  group_levels <- levels({{ data }}[[{{ exposure_var }}]]) # not currently needed
  formula <- stats::formula(paste0(event_var,
                            " ~ ",
                            exposure_var,
                            " * ",
                            "I(",
                            age_var,
                            "^2)",
                            " + ",
                            sex_var,
                            " + offset(log(as.numeric(",
                            time_var,
                            ")))"))

  poisson_fit <- stats::glm(formula = formula,
                     data = data,
                     family = "poisson")

  newdat <- data

  newdat[time_var] <- 36525 # To get IR since offset is specified as days/365.25/100

  result <- marginaleffects::avg_predictions(poisson_fit, variables = exposure_var, newdata = newdat, type = "response")

  data.frame(Treatment = result[[1]],
  IR = result[["estimate"]],
  Lower_CI = result[["conf.low"]],
  Upper_CI = result[["conf.high"]])


}
