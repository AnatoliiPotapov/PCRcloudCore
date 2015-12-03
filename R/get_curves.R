#' get_derivatives function
#'
#' Функция считает первую и вторую производную от кривой флуорисценции
#' @param data - вектор со значениями флуорисценции
#' @keywords derivative estimation
#' @export
#' @examples
#' get_derivatives(curve)


example_settings <- lapply(c(1:48), function(i) { list(channel = "FAM_2", curve = i)})

get_curves <- function(data, settings = example_settings) {
  lapply(seq_along(settings), function(i) {
    get_curve(data,settings[[i]]$channel, settings[[i]]$curve)
  })
}
