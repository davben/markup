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

  if (type == "tl") {
    x_names <- names(x)
    if (is.null(x_names)) {
      names(x) <- c("b0", "bl", "bk", "bll", "bkk", "blk")
      message(
        "Named initial values are recommended. Otherwise their order is assumed to be (1) b0, (2) bl, (3) bk, (4) bll, (5) bkk, and (6) blk."
      )
      return(x)
    } else {
      if (all(c("b0", "bl", "bk", "bll", "bkk", "blk") %in% x_names)) {
        return(x)
      } else {
        stop("Please specify initial values as b0, bl, bk, bll, bkk, and blk.")
      }
    }
  }

  if (type == "cdgo") {
    x_names <- names(x)
    if (is.null(x_names)) {
      names(x) <- c("b0", "bl", "bk", "bm")
      message(
        "Named initial values are recommended. Otherwise their order is assumed to be (1) b0, (2) bl, (3) bk, (4) bm."
      )
      return(x)
    } else {
      if (all(c("b0", "bl", "bk", "bm") %in% x_names)) {
        return(x)
      } else {
        stop("Please specify initial values as b0, bl, bk, and bm.")
      }
    }
  }

  if (type == "tlgo") {
    x_names <- names(x)
    if (is.null(x_names)) {
      names(x) <- c("b0", "bl", "bk", "bm", "bll", "bkk", "bmm", "blk", "blm", "bkm")
      message(
        "Named initial values are recommended. Otherwise their order is assumed to be (1) b0, (2) bl, (3) bk, (4) bm, (5) bll, (6) bkk, (7) bmm, (8) blk, (9) blm and (10) bkm."
      )
      return(x)
    } else {
      if (all(c("b0", "bl", "bk", "bm", "bll", "bkk", "bmm", "blk", "blm", "bkm") %in% x_names)) {
        return(x)
      } else {
        stop("Please specify initial values as b0, bl, bk, bm, bll, bkk, bmm, blk, blm, and bkm.")
      }
    }
  }
}
