# Фитирует линейную модель на часть точек графика
fit_linear_model <- function(curve, range) {
  x <- c(1:length(curve))
  subset <- asub(curve, range)
  linear_model <- lm(subset ~ range)
  coef <- linear_model$coefficients
  fit <- as.numeric(lapply(x, function(i) { coef[2] * i + coef[1]}))
  summary(linear_model)
}
