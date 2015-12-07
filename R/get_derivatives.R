
get_derivatives <- function(data) {
  lapply(data, function(curve) { inder(c(1:length(curve)), curve)})
}
