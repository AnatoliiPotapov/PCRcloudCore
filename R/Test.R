Test <- R6Class("Experiment",
                      public = list(
                        name = NA,
                        id = NA,

                        initialize = function() {

                        },


                        change_settings = function() {

                        }
                      ),

                      private = list(
                        curves = list(),
                        models = list()
                      )
)
