

preprocess_curve <- function(curve) {

  output <- list(
    strange = "NA",
    positive = "NA",
    preprocessed_curve = "NA"
  )

  # 1) проверяем, не странная ли кривая
  strange <- get_strange_curves(curve)
  if (strange == TRUE) {
    output$strange <- strange
    output$preprocessed_curve <- rep(0.0, length(curve))
    return(output)
  } else 
  if (strange == FALSE) {
    output$strange <- strange
  
    # 2) прогоняем алгоритм сглаживания
    #
    curve <- smoother(c(1:length(curve)),curve)
  
    # 3) проверяем, есть ли амплификация
    #    и находим начало амплификации
    exp_start <- get_start_of_exponential_growth(curve)
    #print(exp_start)
    if (is.null(exp_start)) {
      output$positive <- FALSE
      output$preprocessed_curve <- rep(0.0, length(curve))
    } 
    if (!is.null(exp_start)) {
      output$positive <- TRUE
  
      background_range <- get_background_range(curve, exp_start)
  
      # 4) вычитаем фоновую флуорисценцию
      #
      output$preprocessed_curve <- CPP(c(1:length(curve)),curve, trans = TRUE,bg.range = background_range)$y.norm
      
      # 5) обнуляем ту часть графика, где считаем что еще сигнала нет
      #
      output$preprocessed_curve <- c(rep(0.0, exp_start), output$preprocessed_curve[(exp_start+1):(length(curve))])
    }
  
  }

  return(output)
}