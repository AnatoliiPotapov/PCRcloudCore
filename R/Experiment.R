experiment <- R6Class("Experiment",
  public = list(
    name = NA,
    id = NA,
    initialize = function(name, id, file_path) {
      self$name <- name
      self$id <- id
      self$parsed_file <- read_dt(file_path)
    },
    get_curve = function(channel, curve) {

    }
  ),
  private = list(
    parsed_file,
    raw_curves,
    cpp_curves
  )
)
