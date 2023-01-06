#' an example function
#'
#' @export
a <- function(x) {
  if (x <= 1) {
    1
  } else {
    2
  }
}

#' @export
TestR6 <- R6::R6Class("TestR6", # nolint
  public = list(
    show = function(x) {
      1 + 3
    },
    print2 = function(x) {
      1 + 2
    }
  )
)

.InternalTestR6 <- R6::R6Class("InternalTestR6", # nolint
    public = list(
        some_method = function(x){
            1 + 2
        }
    )
)

#' @export
TestR6Child <- R6::R6Class("TestR6", # nolint
  inherit = TestR6,
  public = list(
    show = function(x) {
      3 + 5
    }
  )
)

#' an example function only called within R6 "static" method
#'
#' @export
class_switch <- function(x) {
  if (x <= 1) {
    "parent"
  } else {
    "child"
  }
}


#' an example 'static' R6 function
TestR6$factory <- function(x) {
  wclass <- class_switch(x)
  constructor <- switch(wclass,
    "parent" = TestR6$new,
    "child" = TestR6Child$new
  )
  t <- constructor()
  t
}
