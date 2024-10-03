

#' Set default values for flextable objects
#'
#' This function sets the font family to Calibri, size to 10pts, and border color to black.
#'
#' @return A function.
#' @export
#'
#' @examples
#' bioavr_flextab_defaults()
#'
bioavr_flextab_defaults <- function(){
  flextable::set_flextable_defaults(font.family = "Calibri",
                        font.size = 10,
                        border.color = "black")
}


#' Make a styled flextable.
#' Suitable for saving to a word table.
#' @param df A dataframe containing the data for the table. Dataframe
#' @param header Header text. String
#' @param footer Footer text. String
#' @param default function from flextable::set_flextable_defaults()
#'
#' @return A flextable object.
#' @export
#'
#' @examples
#' colon_death <- colon[colon$etype == 2, ]
#'
#' colon_death %>% dplyr::group_by(rx) %>%
#' dplyr::summarise("No." = dplyr::n(),
#'          "Mean age" = round(mean(age)),
#'          "Percent women" = paste0(round(sum(sex == "0")*100/`No.`), "%")) %>%
#'  bioavr_tab("Header", "Footer")
#'
bioavr_tab <- function(df, header, footer, default = bioavr_flextab_defaults()){
  default
  flextable::flextable(df) %>%
    flextable::add_header_lines(header) %>%
    flextable::add_footer_lines(footer) %>%
    flextable::bold(i = 1, part = "header") %>%
    flextable::hline_top(part = "header",
              border = officer::fp_border(color = "red",
                                 width = 3,
                                 style = "solid")) %>%
    flextable::hline(i = 1,
          part = "header",
          border = officer::fp_border(color = "black",
                             width = 0.25,
                             style = "solid")) %>%
    flextable::hline_top(part = "body",
              border = officer::fp_border(color = "black",
                                 width = 0.25,
                                 style = "solid")) %>%
    flextable::hline_bottom(part = "body",
                 border = officer::fp_border(color = "black",
                                    width = 0.25,
                                    style = "solid")) %>%
    flextable::hline_bottom(part = "footer",
                 border = officer::fp_border(color = "black",
                                    width = 0.25,
                                    style = "solid")) %>%
    flextable::border_inner_h(part = "body",
                   border = officer::fp_border(color = "black",
                                      width = 0.25,
                                      style = "dotted")) %>%
    flextable::autofit(part = "body") %>%
    flextable::bg(part = "body", bg = "#f5f5f5") %>%
    flextable::align(part = "all", align = "center") %>%
    flextable::align(j = 1, part = "all", align = "left")
}


#' Create a grouped flextable object
#'
#' Usually used to create crude and age-, and sex-adjusted incidence rate tables.
#' Useful for exporting to word using the `flextable::save_as_docx()` function.
#'
#' @param flextab data. Dataframe.
#' @param header header text. String.
#' @param footer footer text. String.
#' @param default Set flextable defaults, using flextable::set_flextable_defaults(). Function
#'
#' @return A flextable object.
#' @export
#'
#' @examples
#' colon_death <- colon[colon$etype == 2, ]
#' dplyr::bind_rows("Crude" = incidrate_crude(colon_death, rx, status, time),
#' "Age- and sex-adjusted" = age_sex_adjust(colon_death, rx, age, sex, status, time), .id = "id") %>%
#' bioavr_group_tab(header = "Table 2.
#' Crude and age- and sex-adjusted incidence rates per 100 person-years",
#' footer = "Estimated using a Poisson model")
#'
#'
bioavr_group_tab <- function(flextab, header, footer, default = bioavr_flextab_defaults()){
  default
  flextab %>%
    flextable::as_grouped_data(groups = "id") %>%
    flextable::as_flextable(hide_grouplabel = TRUE) %>%
    flextable::add_header_lines(header) %>%
    flextable::add_footer_lines(footer) %>%
    flextable::bold(i = 1, part = "header") %>%
    flextable::hline_top(part = "header",
              border = officer::fp_border(color = "red",
                                 width = 3,
                                 style = "solid")) %>%
    flextable::hline(i = 1,
          part = "header",
          border = officer::fp_border(color = "black",
                             width = 0.25,
                             style = "solid")) %>%
    flextable::hline_top(part = "body",
              border = officer::fp_border(color = "black",
                                 width = 0.25,
                                 style = "solid")) %>%
    flextable::hline_bottom(part = "body",
                 border = officer::fp_border(color = "black",
                                    width = 0.25,
                                    style = "solid")) %>%
    flextable::hline_bottom(part = "footer",
                 border = officer::fp_border(color = "black",
                                    width = 0.25,
                                    style = "solid")) %>%
    flextable::border_inner_h(part = "body",
                   border = officer::fp_border(color = "black",
                                      width = 0.25,
                                      style = "dotted")) %>%
    flextable::autofit(part = "body") %>%
    flextable::bg(part = "body", bg = "#f5f5f5") %>%
    flextable::align(part = "all", align = "center") %>%
    flextable::align(j = 1, part = "all", align = "left") %>%
    flextable::bg(i = ~ !is.na(id), bg = "transparent", part = "body") %>%
    flextable::border(i = ~ !is.na(id),
           part = "body",
           border.top = officer::fp_border(color = "black",
                                  width = 0.25,
                                  style = "solid"))
}
