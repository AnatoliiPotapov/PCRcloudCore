Experiment <- R6Class("Experiment",
                      public = list(
                        name = NA,
                        id = NA,

                        initialize = function(name, id, file_path) {
                          self$name <- name
                          self$id <- id
                          private$parsed_file <- read_dt(file_path)
                          private$raw_curves <- get_curves(private$parsed_file)
                          self$preprocess()
                        },

                        preprocess = function( parameters = DEFAULT_PREPROCESSING ) {
                          curves <- private$raw_curves
                            lapply(seq_along(private$raw_curves), function(i) {
                              curve <- curves[[i]]
                              prep <- preprocess_curve(curve)
                              private$cpp_curves[[i]] <- prep$preprocessed_curve
                              private$settings[[i]] <- list(strange = prep$strange, positive = prep$positive)
                            })
                        },

                        fit = function() {
                          private$pcrfit <- lapply(seq_along(private$cpp_curves), function(i) {
                            if (private$settings[[i]]$positive == TRUE) {
                              curve <- private$cpp_curves[[i]]
                              fluo <- data.frame(
                                x = c(1:length(curve)),
                                y = curve
                              )
                              pcrfit(fluo, "x","y", model = l5)
                            } else {
                              NULL
                            }
                          })
                        },

                        plot = function(mode = "RAW", indexes = NULL, name = "PCR curves") {
                          if (is.null(indexes)) { indexes = c(1:480)}

                          if (mode == "RAW") {
                            plot_curves(asub(private$raw_curves, indexes))
                          } else
                          if (mode == "CPP") {
                            plot_curves(asub(private$cpp_curves, indexes))
                          }
                          if (mode == "MATH") {
                            curves <- lapply(indexes, function(i) {
                              curve <- c()
                              if (private$settings[[i]]$positive == TRUE) {
                                curve <- predict(private$pcrfit[[i]])$Prediction
                              } else {
                                curve <- private$cpp_curves[[i]]
                              }
                            })
                            plot_curves(curves)
                          }
                        },

                        get_file = function() {
                          private$parsed_file
                        },

                        get_raw_curves = function() {
                          private$raw_curves
                        },

                        get_cpp_curves = function() {
                          private$cpp_curves
                        },

                        get_settings = function() {
                          private$settings
                        },

                        get_pcrfit = function() {
                          private$pcrfit
                        }
                      ),

                      private = list(
                        parsed_file = list(),    # content of parsed file
                        raw_curves = list(),     # raw curves
                        cpp_curves = list(),     # preprocessed curves
                        settings = list(),       # parameters of each curve
                        pcrfit = list()          # math representation
                      )
)
