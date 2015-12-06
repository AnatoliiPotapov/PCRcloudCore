get_start_of_exponential_growth <- function(curve, threshold = 5) {

  len <- length(curve)
  last_der <- curve[len] - curve[len-1]
  accumulate <- 0.0
  der_curve <- c()

  for (i in (len-2):1) {
    der <- curve[i+1] - curve[i]
    if (der < last_der) {
      accumulate <- accumulate + 1
      last_der <- der
    } else {
      accumulate <- 0
      last_der <- der
    }
    der_curve[i] <- accumulate
  }

  der_curve <- c(der_curve,0,0)

  for (i in 1:len) {
    if (der_curve[i] > threshold) {
      return(i)
      break
    }
  }

  return(NULL)
}
