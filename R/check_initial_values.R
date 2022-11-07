check_initial_values <- function(x, type, version = NULL){

  if (type == "cd") {coefs <- c("b0", "bl", "bk")}
  if (type == "tl") {coefs <- c("b0", "bl", "bk", "bll", "bkk", "blk")}
  if (type == "cdgo") {coefs <- c("b0", "bl", "bk", "bm")}
  if (type == "tlgo") {coefs <- c("b0", "bl", "bk", "bm", "bll", "bkk", "bmm", "blk", "blm", "bkm", "blkm")}

  if (!is.null(version)) { # no b0 estimate in DLW
    coefs <- coefs[-c(coefs == "b0")]
  }

  x_names <- names(x)

  # case 1: no named vector, correct number of values
  if (is.null(x_names) & length(x) == length(coefs)) {
    names(x) <- coefs

    order_message <- paste(paste("(", seq_along(coefs), ") ", coefs, sep = ""), collapse = ", ")

    message(
      paste0("Named initial values are recommended. Otherwise their order is assumed to be ",
             order_message, ".")
    )
    return(x)
  } else {
    # case 2: correctly named vector
    if (all(coefs %in% x_names)) {
      return(x)
    } else {
      # case 3: incorrect number of values in vector
      stop_message <- paste(paste("(", seq_along(coefs), ") ", coefs, sep = ""), collapse = ", ")
      stop(
        paste0("Please specify initial values for ",
               stop_message, ".")
      )
    }
  }
}

