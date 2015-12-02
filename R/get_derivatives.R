#' get_derivatives function
#'
#' Функция считает первую и вторую производную от кривой флуорисценции
#' @param data - вектор со значениями флуорисценции
#' @keywords derivative estimation
#' @export
#' @examples
#' get_derivatives(curve)

get_derivatives <- function(data) {
  lapply(data, function(curve) { inder(c(1:length(curve)), curve)})
}
