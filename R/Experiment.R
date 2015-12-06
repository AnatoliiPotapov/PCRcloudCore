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
                          private$cpp_curves <- lapply(private$raw_curves, function(curve) { preprocess_curve(curve)$preprocessed_curve })
                        },



                        plot = function(mode = "RAW", indexes = NULL, name = "PCR curves") {
                          if (is.null(indexes)) { indexes = c(1:480)}

                          if (mode == "RAW") {
                            plot_curves(asub(private$raw_curves, indexes))
                          } else
                          if (mode == "CPP") {
                            plot_curves(asub(private$cpp_curves, indexes))
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
                        }
                      ),

                      private = list(
                        parsed_file = list(),
                        raw_curves = list(),
                        cpp_curves = list()

                      )
)
