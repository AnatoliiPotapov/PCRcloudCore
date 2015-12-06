#' get_derivatives function
#'
#' Функция считает первую и вторую производную от кривой флуорисценции
#' @param data - вектор со значениями флуорисценции
#' @keywords derivative estimation
#' @export
#' @examples
#' get_derivatives(curve)

get_curves <- function(data, settings = NULL) {
  
  optic_channels <- 
    list("FAM_1", "FAM_2",
         "HEX_1", "HEX_2",
         "ROX_1", "ROX_2",
         "Cy5_1", "Cy5_2",
         "Cy5.5_1", "Cy5.5_2")
  
  default_settings <- lapply(c(1:(data$samples_count*10)),
    function(i) { 
      list(channel = optic_channels[[(i - 1) %/%  data$samples_count + 1]], curve = (i-1) %% data$samples_count + 1)
  })

  if (is.null(settings)) {settings <- default_settings}
  
  lapply(seq_along(settings), function(i) {
    get_curve(data,settings[[i]]$channel, settings[[i]]$curve)
  })
}
