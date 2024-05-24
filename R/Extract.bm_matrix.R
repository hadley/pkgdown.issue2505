#' Extract or replace parts of a bitmap/pixmap matrix
#'
#' `[.bm_matrix()` is defined so that it returns a matrix object
#' (if the value is a matrix).
#' `[<-.bm_bitmap()` casts any replacement values as integers while
#' `[<-.bm_pixmap()` casts any replacement values as standardized color strings.
#' @param x Matrix object
#' @param i,j indices specifying elements to extract or replace.
#'            See [base::`[()`] for more information.
#' @param ... Passed to [base::`[()`].
#' @param drop If `TRUE` the result is coerced to a integer vector.
#' @param value Replacement value
#' @rdname Extract.bm_matrix
#' @return `[.bm_matrix()` returns a matrix
#'         value is a matrix and/or `drop` is `FALSE`
#'         otherwise it returns a vector of integers or color strings.
#' @aliases [.bm_bitmap [.bm_pixmap
#' @export
`[.bm_matrix` <- function(x, i, j, ..., drop = TRUE) {
    v <- NextMethod()
    if (is.matrix(v))
        class(v) <- class(x)
    v
}

#' @rdname Extract.bm_matrix
#' @export
`[<-.bm_bitmap` <- function(x, i, j, ..., value) { # nolint
    value <- as.integer(value)
    x <- NextMethod()
    x
}

#' @rdname Extract.bm_matrix
#' @export
`[<-.bm_pixmap` <- function(x, i, j, ..., value) { # nolint
    value <- as.character(value)
    x <- NextMethod()
    x
}
