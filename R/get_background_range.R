example_settings <- list(
  min_exp = 5, # минимальное значение, по которому идентифицируется экспоненциальный участок шумов
  min_range = 5, # минимальная длина участка шумов
  goodness = 0.90, # минимальное значение R^2
  algorithm = "smart"
)

get_background_range <- function(curve, exp_start, st = example_settings) {

  start <- exp_start
  if (is.null(start)) return(NULL)

  else {
    r.squared_x <- start
    r.squared_value <- 0.0
    coef <- list()

    for (j in (st$min_exp):(start-1)) {

      fitt <- fit_linear_model(curve, c(start:(start-j)))
      r.squared <- fitt$r.squared

      if ((r.squared > r.squared_value) ) {
        r.squared_value <- r.squared
        r.squared_x <- j
        coef <- fitt$coefficients
      }

    }

    c(start-r.squared_x, start)
  }
}



