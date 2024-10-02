

bioavr_flextab_defaults <- function()
{flextable::set_flextable_defaults(font.family = "Calibri",
                        font.size = 10,
                        border.color = "black")
}


bioavr_tab <- function(df, header, footer){
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
#'
#' @return A flextable object.
#' @export
#'
#' @examples
#' dplyr::bind_rows(list("Crude" = rates_table,
#' "Age adjusted" = rates_table_adjust), .id = "id") %>%
#'  bioavr_group_tab(header = header, footer = footer)
#'
#'
bioavr_group_tab <- function(flextab, header, footer){
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
