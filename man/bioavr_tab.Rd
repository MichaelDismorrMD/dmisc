% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flextab_functions.R
\name{bioavr_tab}
\alias{bioavr_tab}
\title{Make a styled flextable.
Suitable for saving to a word table.}
\usage{
bioavr_tab(df, header, footer, default = bioavr_flextab_defaults())
}
\arguments{
\item{df}{A dataframe containing the data for the table. Dataframe}

\item{header}{Header text. String}

\item{footer}{Footer text. String}

\item{default}{function from flextable::set_flextable_defaults()}
}
\value{
A flextable object.
}
\description{
Make a styled flextable.
Suitable for saving to a word table.
}
\examples{
colon_death <- colon[colon$etype == 2, ]

colon_death \%>\% dplyr::group_by(rx) \%>\%
dplyr::summarise("No." = dplyr::n(),
         "Mean age" = round(mean(age)),
         "Percent women" = paste0(round(sum(sex == "0")*100/`No.`), "\%")) \%>\%
 bioavr_tab("Header", "Footer")

}
