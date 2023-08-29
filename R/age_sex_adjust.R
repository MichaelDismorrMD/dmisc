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

  marginaleffects::avg_predictions(poisson_fit, variables = exposure_var, newdata = newdat, type = "response")


}
