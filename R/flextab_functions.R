

bioavr_flextab_defaults <- function()
{set_flextable_defaults(font.family = "Calibri",
                        font.size = 10,
                        border.color = "black")
}


bioavr_tab <- function(df, header, footer){
  flextable(df) %>%
    add_header_lines(header) %>%
    add_footer_lines(footer) %>%
    bold(i = 1, part = "header") %>%
    hline_top(part = "header",
              border = fp_border(color = "red",
                                 width = 3,
                                 style = "solid")) %>%
    hline(i = 1,
          part = "header",
          border = fp_border(color = "black",
                             width = 0.25,
                             style = "solid")) %>%
    hline_top(part = "body",
              border = fp_border(color = "black",
                                 width = 0.25,
                                 style = "solid")) %>%
    hline_bottom(part = "body",
                 border = fp_border(color = "black",
                                    width = 0.25,
                                    style = "solid")) %>%
    hline_bottom(part = "footer",
                 border = fp_border(color = "black",
                                    width = 0.25,
                                    style = "solid")) %>%
    border_inner_h(part = "body",
                   border = fp_border(color = "black",
                                      width = 0.25,
                                      style = "dotted")) %>%
    autofit(part = "body") %>%
    bg(part = "body", bg = "#f5f5f5") %>%
    align(part = "all", align = "center") %>%
    align(j = 1, part = "all", align = "left")
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
#' bind_rows(list("Crude" = rates_table,
#' "Age adjusted" = rates_table_adjust), .id = "id") %>%
#'  as_grouped_data(groups = c("id")) %>%
#'  as_flextable(hide_grouplabel = T) %>%
#'  bioavr_group_tab(header = header, footer = footer)
#'
#'
bioavr_group_tab <- function(flextab, header, footer){
  flextab %>%
    add_header_lines(header) %>%
    add_footer_lines(footer) %>%
    bold(i = 1, part = "header") %>%
    hline_top(part = "header",
              border = fp_border(color = "red",
                                 width = 3,
                                 style = "solid")) %>%
    hline(i = 1,
          part = "header",
          border = fp_border(color = "black",
                             width = 0.25,
                             style = "solid")) %>%
    hline_top(part = "body",
              border = fp_border(color = "black",
                                 width = 0.25,
                                 style = "solid")) %>%
    hline_bottom(part = "body",
                 border = fp_border(color = "black",
                                    width = 0.25,
                                    style = "solid")) %>%
    hline_bottom(part = "footer",
                 border = fp_border(color = "black",
                                    width = 0.25,
                                    style = "solid")) %>%
    border_inner_h(part = "body",
                   border = fp_border(color = "black",
                                      width = 0.25,
                                      style = "dotted")) %>%
    autofit(part = "body") %>%
    bg(part = "body", bg = "#f5f5f5") %>%
    align(part = "all", align = "center") %>%
    align(j = 1, part = "all", align = "left") %>%
    bg(i = ~ !is.na(id), bg = "transparent", part = "body") %>%
    border(i = ~ !is.na(id),
           part = "body",
           border.top = fp_border(color = "black",
                                  width = 0.25,
                                  style = "solid"))
}
