check_initial_values <- function(x, type) {
  if (type == "cd") {
    cd_coefs <- c("b0", "bl", "bk")
    x_names <- names(x)
    if (is.null(x_names)) {
      names(x) <- cd_coefs
      message(
        "Named initial values are recommended. Otherwise their order is assumed to be (1) b0, (2) bl, (3) bk."
      )
      return(x)
    } else {
      if (all(cd_coefs %in% x_names)) {
        return(x)
      } else {
        stop("Please specify initial values as b0, bl, and bk.")
      }
    }
  }

  if (type == "tl") {
    tl_coefs <- c("b0", "bl", "bk", "bll", "bkk", "blk")
    x_names <- names(x)
    if (is.null(x_names)) {
      names(x) <- tl_coefs
      message(
        "Named initial values are recommended. Otherwise their order is assumed to be (1) b0, (2) bl, (3) bk, (4) bll, (5) bkk, and (6) blk."
      )
      return(x)
    } else {
      if (all(tl_coefs %in% x_names)) {
        return(x)
      } else {
        stop("Please specify initial values as b0, bl, bk, bll, bkk, and blk.")
      }
    }
  }

  if (type == "cdgo") {
    cdgo_coefs <- c("b0", "bl", "bk", "bm")
    x_names <- names(x)
    if (is.null(x_names)) {
      names(x) <- cdgo_coefs
      message(
        "Named initial values are recommended. Otherwise their order is assumed to be (1) b0, (2) bl, (3) bk, (4) bm."
      )
      return(x)
    } else {
      if (all(cdgo_coefs %in% x_names)) {
        return(x)
      } else {
        stop("Please specify initial values as b0, bl, bk, and bm.")
      }
    }
  }

  if (type == "tlgo") {
    tlgo_coefs <- c("b0", "bl", "bk", "bm", "bll", "bkk", "bmm", "blk", "blm", "bkm", "blkm")
    x_names <- names(x)
    if (is.null(x_names)) {
      names(x) <- tlgo_coefs
      message(
        "Named initial values are recommended. Otherwise their order is assumed to be (1) b0, (2) bl, (3) bk, (4) bm, (5) bll, (6) bkk, (7) bmm, (8) blk, (9) blm, (10) bkm, and (11) blkm."
      )
      return(x)
    } else {
      if (all(tlgo_coefs %in% x_names)) {
        return(x)
      } else {
        stop("Please specify initial values as b0, bl, bk, bm, bll, bkk, bmm, blk, blm, bkm, and blkm.")
      }
    }
  }
}
