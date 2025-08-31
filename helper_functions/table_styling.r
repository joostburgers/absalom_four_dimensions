library(dplyr)
library(kableExtra)
library(htmltools)

kable_trunc <- function(data,
                        caption = "Table",
                        n = 5,
                        span = 10,                       # 10em default truncation span
                        cols = tidyselect::everything(),
                        striped = TRUE,
                        hover = TRUE,
                        container_width = "100%",        # fill parent container
                        allow_v_scroll = FALSE,
                        max_height = NULL) {
  
  span_css <- if (is.numeric(span)) paste0(span, "em") else as.character(span)
  
  df <- if (n > 0) dplyr::slice_sample(data, n = n) else data
  
  df_trunc <- dplyr::mutate(
    df,
    dplyr::across(
      {{ cols }},
      ~ kableExtra::cell_spec(
        as.character(.x),
        escape = TRUE,
        extra_css = paste0(
          "display:inline-block;",
          "max-width:", span_css, ";",
          "white-space:nowrap;",
          "overflow:hidden;",
          "text-overflow:ellipsis;",
          "vertical-align:top;"
        )
      )
    )
  )
  
  tbl <- kableExtra::kbl(
    df_trunc,
    caption = caption,
    format  = "html",
    escape  = FALSE,
    table.attr = 'style="table-layout:fixed; width:auto;"'
  ) |>
    kableExtra::kable_styling(
      bootstrap_options = c(if (striped) "striped", if (hover) "hover"),
      full_width = FALSE,
      position = "center"
    )
  
  # Only show scrollbars when needed
  x_css <- "overflow-x:auto;"
  y_css <- if (allow_v_scroll) "overflow-y:auto;" else "overflow-y:hidden;"
  h_css <- if (!is.null(max_height) && allow_v_scroll) paste0("max-height:", max_height, ";") else ""
  
  htmltools::div(
    style = paste0(
      "max-width:", container_width, "; ",
      x_css, " ", y_css, " ", h_css,
      " -webkit-overflow-scrolling:touch;"
    ),
    # IMPORTANT: treat table HTML as HTML, not text
    htmltools::HTML(as.character(tbl))
  )
}

