preprocess_curve <- function(curve) {
  CPP(c(1:length(curve)), curve, amptest = TRUE, trans = TRUE)$y.norm
}
