# Строит все кривые на одном графике
plot_curves <- function(data, graph_name = "PCR curves") {

  df <- as.data.frame(abind(
    lapply(seq_along(data), function(i) {
      data.frame(
        x = c(1:length(data[[i]])),
        y = data[[i]],
        type = i
      )
    }),
    along = 1
  ))

  ggplot(df, aes(x = x, y = y, group = type, color = type)) +
    geom_point() +
    geom_line() +
    labs(list(title = graph_name, x = "Номер цикла", y = "Флуорисценция"))
}
