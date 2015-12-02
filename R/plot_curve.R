# Строит одну кривую
plot_curve <- function(data) {
  df = data.frame(x = c(1:length(data)), y = data)
  ggplot(df, aes(x = x, y = y)) +
    geom_point() +
    labs(list(title = "PCR curve", x = "Номер цикла", y = "Флуорисценция"))
}
