check_initial_values <- function(x, type) {
  if (type == "cd") {
    x_names <- names(x)
    if (is.null(x_names)) {
      names(x) <- c("b0", "bl", "bk")
      message(
        "Named initial values are recommended. Otherwise their order is assumed to be (1) b0, (2) bl, (3) bk."
      )
      return(x)
    } else {
      if (all(c("b0", "bl", "bk") %in% x_names)) {
        return(x)
      } else {
        stop("Please specify initial values as b0, bl, and bk.")
      }
    }
  }
}
