get_strange_curves <- function(curve, mod = 10000) {
  min <- curve[1]
  max <- curve[1]
  lapply(seq_along(curve), function(i) {
    if (curve[i] > max) max <<- curve[i]
    if (curve[i] < min) min <<- curve[i]
  })
  if (abs(max) > mod) return(TRUE)
  if (abs(max - min) > mod ) return(TRUE) else return(FALSE)
}
