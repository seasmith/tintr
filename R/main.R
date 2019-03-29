#' Tint and shade your colors.
#'
#' @param color (character) Hex or R color.
#' @param steps (double) [0, 1] How far from your color [0] to black [1] or white [1].
#'
#' @rdname tint_shade
#' @export
shade <- function (color, steps = 0.33) {

  # if (is_hex()) do this else do that
  rgb_color <- grDevices::col2rgb(color)
  distance  <- 255 - rgb_color

  shaded <- ((distance * steps) + rgb_color) / 255

  grDevices::rgb(red   = shaded["red",   ],
                 green = shaded["green", ],
                 blue  = shaded["blue",  ])
}

#' @rdname tint_shade
#' @export
tint <- function (color, steps = 0.33) {

  rgb_color <- grDevices::col2rgb(color)

  tinted <- ( rgb_color - (rgb_color * steps)) / 255

  grDevices::rgb(red   = tinted["red",   ],
                 green = tinted["green", ],
                 blue  = tinted["blue",  ])
}
