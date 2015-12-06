visualise_linearity <- function(curve, st = example_settings) {

  square.df <- data.frame()

  for (i in st$pstart:st$pstop) {

    r.squared_x <- i
    r.squared_value <- 0.0
    coefficients <- list()

    for (j in st$min_range:st$max_range) {

      fitt <- fit(curve, c(i:(i+j)))
      r.squared <- fitt$r.squared

      if ((r.squared > r.squared_value) ) {
        r.squared_value <- r.squared
        r.squared_x <- j
        coefficients <- fitt$coefficients
      }

    }
    square.df <- rbind(square.df, c(r.squared_value,i,i + r.squared_x,coefficients[1],coefficients[2]))
  }

  square.df <- cbind(c(1:dim(square.df)[1]),square.df)
  names(square.df) <- c("id","r","x_start","x_end","intersept","slope")
  square.df <- square.df[order(square.df$r, decreasing = TRUE),]
  dl <- gather(square.df, cond, value, x_start:x_end)

  ggplot(data = dl, aes(x = value, y = r, color = cond, group = id)) + geom_point() + geom_line(color = "black")
  #ggplot(data = dl, aes(x = intersept, y = slope)) + geom_point()
  #square.df
}
