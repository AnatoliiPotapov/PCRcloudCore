plot_derivatives <- function(data) {

  curve <- data.frame(x = data[,1], y = data[,2], type = "curve")
  der1 <- data.frame(x = data[,1], y = data[,3], type = "1 derivative")
  der2 <- data.frame(x = data[,1], y = data[,4], type = "2 derivative")

  pre_df <- abind(list(curve,der1,der2), along = 1)
  df <- data.frame(
    x = as.numeric(pre_df[,1]),
    y = as.numeric(pre_df[,2]),
    type = pre_df[,3]
  )

  ggplot(df, aes(x = x, y = y, color = type)) +
    geom_point() +
    geom_line() +
    labs(list(title = "lol", x = "Номер цикла", y = "Флуорисценция"))
}
