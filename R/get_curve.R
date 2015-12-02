get_curve <- function(data, channel, curve) {
  return(data$optic[[channel]][,curve])
}
