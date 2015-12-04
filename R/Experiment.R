Experiment <- R6Class("Experiment",
                      public = list(
                        name = NA,
                        id = NA,

                        initialize = function(name, id, file_path) {
                          self$name <- name
                          self$id <- id
                          private$parsed_file <- read_dt(file_path)
                          private$raw_curves <- get_curves(private$parsed_file)
                        },

                        preprocess = function( parameters = DEFAULT_PREPROCESSING ) {
                          pre_cpp <- lapply(private$raw_curves, function(curve) {
                            amptester(curve)
                          })
                          private$cpp_curves <- lapply(pre_cpp, function(curve) {
                            preprocess_curve(curve)
                          })
                        },

                        plot = function(mode = "RAW", indexes = NULL, name = "PCR curves") {
                          if (is.null(indexes)) { indexes = c(1:48)}

                          if (mode == "RAW") {
                            plot_curves(asub(private$raw_curves, indexes))
                          } else
                          if (mode == "CPP") {
                            plot_curves(asub(private$cpp_curves, indexes))
                          }
                        }

                      ),
                      private = list(
                        parsed_file = list(),
                        raw_curves = list(),
                        cpp_curves = list()
                      )
)
