#' The ggplot2 Theme of a Size Preset
#'
#' @param size Character string. See `.FigSize()`.
#' @returns Returns a ggplot2 theme.
#' @noRd
.FigTheme <- function(size = "page") {
  preset <- .FigSize(size = size)
  ggplot2::theme_bw(
    base_size = preset$base_size,
    base_family = preset$family
  ) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}
