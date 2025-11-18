#' @importFrom methods new setClass setValidity setGeneric setMethod setAs
#' @importMethodsFrom methods coerce show
#' @importFrom graphics barplot
NULL
#' @title sparse_numeric S4 class
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector of positions (1-based) corresponding to each stored value.
#' @slot length Integer scalar giving the total length of the full vector.
#' @name sparse_numeric-class
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

#' @rdname sparse_numeric-class
#' @name sparse_numeric-class-validity
#' @keywords internal
setValidity(
  Class = "sparse_numeric",
  method = function(object){
    check <- anyNA(object@value) || anyNA(object@pos) || anyNA(object@length)
    if (check){
      return("Values are NA")
    }
    check <- length(object@length) != 1L || object@length <= 0L
    if (check){
      return("length needs to be a single positive integer")
    }
    check <- length(object@value) != length(object@pos)
    if (check){
      return("Position vector and value vector are not the same length")
    }
    check <- any(object@pos < 1L) || any(object@pos > object@length)
    if (check){
      return("Position not in range")
    }
    check <- any(duplicated(object@pos))
    if (check){
      return("Duplicate positions")
    }
    check <- any(object@value == 0)
    if (check){
      return("Zero in value vector")
    }
  }
)
#' Coercion methods for sparse_numeric
#'
#' These methods convert between `numeric` vectors and `sparse_numeric`
#' objects using the S4 `coerce` generic.
#'
#' @docType methods
#' @aliases coerce,numeric,sparse_numeric-method
#' @aliases coerce,sparse_numeric,numeric-method
#'
#' @name sparse_numeric-coercion
NULL

#' Coercion: numeric → sparse_numeric
#'
#' Converts a standard numeric vector into a sparse_numeric object.
#'
#' @param from A numeric vector.
#' @return A sparse_numeric object.
#'
#' @name numeric_to_sparse_numeric
#' @rdname numeric_to_sparse_numeric
#' @exportMethod coerce
setAs("numeric","sparse_numeric",
      function(from){
        new("sparse_numeric",
            value = from[which(from != 0)],
            pos = as.integer(which(from != 0)),
            length = as.integer(length(from)))
      })
#' Coercion: sparse_numeric → numeric
#'
#' Converts a sparse_numeric object back into a full numeric vector.
#'
#' @param from A sparse_numeric object.
#' @return A numeric vector.
#'
#' @name sparse_numeric_to_numeric
#' @rdname sparse_numeric_to_numeric
#' @exportMethod coerce
setAs("sparse_numeric","numeric",
      function(from){
        num <- numeric(length = from@length)
        num[from@pos] <- from@value
        num
      })
#' Add two sparse_numeric vectors
#'
#' Adds two \code{sparse_numeric} objects elementwise, returning a
#' new \code{sparse_numeric} object. Resulting zero-valued elements
#' are omitted to preserve sparsity.
#'
#' @param x,y Objects of class \code{sparse_numeric}.
#' @param ... Additional arguments (unused).
#'
#' @return A \code{sparse_numeric} object representing the elementwise sum.
#' @export
setGeneric("sparse_add", function(x, y, ...){
  standardGeneric("sparse_add")
})
#' @rdname sparse_add
#' @export
setMethod("sparse_add", "sparse_numeric",
          function(x, y, ...){
            if (x@length != y@length) {
              stop("Lengths do not match match")
            }
            poses <- sort(unique(c(x@pos, y@pos)))
            values <- numeric(length(poses))
            for (i in seq_along(poses)){
              pos <- poses[i]
              x_v <- 0
              y_v <- 0
              if (pos %in% x@pos){
                x_v <- x@value[which(x@pos == pos)]
              }
              if (pos %in% y@pos){
                y_v <- y@value[which(y@pos == pos)]
              }
              values[i] <- x_v + y_v
            }

            keep <- values != 0
            poses <- poses[keep]
            values <- values[keep]
            new("sparse_numeric",
                value = values,
                pos = as.integer(poses),
                length = x@length)
          }
)
#' @param e1 A sparse_numeric object (left operand)
#' @param e2 A sparse_numeric object (right operand)
#' @rdname sparse_add
#' @export
setMethod(
  "+",
  c("sparse_numeric", "sparse_numeric"),
  function(e1, e2) {
    sparse_add(e1, e2)
  }
)
#' Subtract two sparse_numeric vectors
#'
#' @inheritParams sparse_add
#' @return A sparse_numeric object representing x - y.
#'
#' @export
setGeneric("sparse_sub", function(x, y, ...){
  standardGeneric("sparse_sub")
})
#' @rdname sparse_sub
#' @export
setMethod("sparse_sub", "sparse_numeric",
          function(x, y, ...){
            if (x@length != y@length) {
              stop("Lengths do not match match")
            }
            poses <- sort(unique(c(x@pos, y@pos)))
            values <- numeric(length(poses))
            for (i in seq_along(poses)){
              pos <- poses[i]
              x_v <- 0
              y_v <- 0
              if (pos %in% x@pos){
                x_v <- x@value[which(x@pos == pos)]
              }
              if (pos %in% y@pos){
                y_v <- y@value[which(y@pos == pos)]
              }
              values[i] <- x_v - y_v
            }
            keep <- values != 0
            poses <- poses[keep]
            values <- values[keep]
            new("sparse_numeric",
                value = values,
                pos = as.integer(poses),
                length = x@length)
          }
)
#' @param e1 A sparse_numeric object (left operand)
#' @param e2 A sparse_numeric object (right operand)
#' @rdname sparse_sub
#' @export
setMethod(
  f = "-", c("sparse_numeric", "sparse_numeric"),
  function(e1, e2) {
    sparse_sub(e1, e2)
  }
)
#' Elementwise multiply two sparse_numeric vectors
#'
#' @inheritParams sparse_add
#' @return A sparse_numeric object representing x * y.
#'
#' @export
setGeneric("sparse_mult", function(x, y, ...){
  standardGeneric("sparse_mult")
})
#' @rdname sparse_mult
#' @export
setMethod("sparse_mult", "sparse_numeric",
          function(x, y, ...){
            if (x@length != y@length) {
              stop("Lengths do not match match")
            }
            poses <- sort(unique(c(x@pos, y@pos)))
            values <- numeric(length(poses))
            for (i in seq_along(poses)){
              pos <- poses[i]
              x_v <- 0
              y_v <- 0
              if (pos %in% x@pos){
                x_v <- x@value[which(x@pos == pos)]
              }
              if (pos %in% y@pos){
                y_v <- y@value[which(y@pos == pos)]
              }
              values[i] <- x_v * y_v
            }
            keep <- values != 0
            poses <- poses[keep]
            values <- values[keep]
            new("sparse_numeric",
                value = values,
                pos = as.integer(poses),
                length = x@length)
          }
)
#' @param e1 A sparse_numeric object (left operand)
#' @param e2 A sparse_numeric object (right operand)
#' @rdname sparse_mult
#' @export
setMethod(
  f = "*", c("sparse_numeric", "sparse_numeric"),
  function(e1, e2) {
    sparse_mult(e1, e2)
  }
)
#' Computes \eqn{\sum_i x_i y_i}.
#'
#' @inheritParams sparse_add
#' @return A numeric scalar.
#'
#' @export
setGeneric("sparse_crossprod", function(x, y, ...){
  standardGeneric("sparse_crossprod")
})
#' @rdname sparse_crossprod
#' @export
setMethod("sparse_crossprod", "sparse_numeric",
          function(x, y, ...){
            if (x@length != y@length) {
              stop("Lengths do not match match")
            }
            poses <- sort(unique(c(x@pos, y@pos)))
            values <- numeric(length(poses))
            for (i in seq_along(poses)){
              pos <- poses[i]
              x_v <- 0
              y_v <- 0
              if (pos %in% x@pos){
                x_v <- x@value[which(x@pos == pos)]
              }
              if (pos %in% y@pos){
                y_v <- y@value[which(y@pos == pos)]
              }
              values[i] <- x_v * y_v
            }
            as.numeric(sum(values, na.rm = FALSE))

          }
)
#' Display sparse_numeric object
#'
#' Provides a compact printed representation showing the length
#' and the stored non-zero elements with their positions.
#'
#' @param object A \code{sparse_numeric} object.
#'
#' @export
setMethod("show", "sparse_numeric",
          function(object){
            cat("Length:", object@length, "\n")
            cat("with elements",object@value, "at positions", object@pos)
          })
#' Plot two sparse_numeric vectors
#'
#' Produces a barplot comparing non-zero values at positions where
#' both sparse vectors have non-zero elements. If there are no
#' overlapping positions, nothing is plotted.
#'
#' @param x,y Two \code{sparse_numeric} objects.
#' @param ... Additional graphical parameters passed to \code{barplot()}.
#'
#' @return A barplot (or a message if no overlapping positions exist).
#' @export
setMethod("plot",signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length) {
              stop("Vectors must have the same overall length to compare")
            }
            overlap <- intersect(x@pos, y@pos)
            if (length(overlap) == 0L) {
              message("No overlapping nonzero positions to plot.")
              return(invisible(NULL))
            }
            x_vals <- sapply(overlap, function(p) x@value[which(x@pos == p)])
            y_vals <- sapply(overlap, function(p) y@value[which(y@pos == p)])
            data_mat <- rbind(x_vals, y_vals)
            colnames(data_mat) <- paste0("Pos", overlap)
            rownames(data_mat) <- c("x", "y")

            barplot(
              data_mat,
              beside = TRUE,
              col = c("steelblue", "tomato"),
              main = "Overlapping Non-Zero Elements",
              xlab = "Overlapping position",
              ylab = "Value",
              legend.text = rownames(data_mat),
              args.legend = list(x = "topright", bty = "n", inset = 0.02)
            )
          })
#' Mean of a sparse_numeric vector
#'
#' Computes the mean of the underlying dense numeric vector, treating
#' unspecified entries as zero.
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Unused.
#'
#' @return A numeric scalar.
#' @export
setMethod(
  "mean",
  signature(x = "sparse_numeric"),
  function(x, ...) {
    total_sum <- sum(x@value)
    total_len <- x@length
    total_sum / total_len
  }
)
#' Euclidean norm of a sparse_numeric vector
#'
#' Computes the Euclidean (L2) norm of the underlying full vector:
#' \deqn{ \|x\|_2 = \sqrt{\sum_i x_i^2} }
#' using only stored non-zero values.
#'
#' @param x A \code{sparse_numeric} object.
#' @param ... Unused.
#'
#' @return A numeric scalar.
#' @export
setGeneric(
  "norm",
  function(x, ...) standardGeneric("norm")
)
#' @rdname norm
#' @export
setMethod(
  "norm",
  "sparse_numeric",
  function(x, ...) {
    sq <- x@value^2
    result <- sqrt(sum(sq))
    return(result)
  }
)
#' Standardize a sparse_numeric object
#'
#' Standardizes the underlying full vector by subtracting the mean
#' and dividing by the standard deviation. Only non-zero standardized
#' values are stored.
#'
#' @param x A sparse_numeric object.
#' @param ... Unused.
#'
#' @return A standardized sparse_numeric vector.
#'
#' @export
setGeneric(
  "standardize",
  function(x, ...) standardGeneric("standardize")
)
#' @rdname standardize
#' @export
setMethod(
  "standardize",
  "sparse_numeric",
  function(x, ...) {

    mu <- mean(x)

    # Full dense vector for correct standardization
    dense <- numeric(x@length)
    dense[x@pos] <- x@value

    sd <- stats::sd(dense)

    # special case: all values identical
    if (sd == 0) {
      return(new("sparse_numeric",
                 value = numeric(0),
                 pos   = integer(0),
                 length = x@length))
    }

    dense_std <- (dense - mu) / sd

    # Now convert standardized dense vector to sparse
    nz <- which(dense_std != 0)

    new("sparse_numeric",
        value  = dense_std[nz],
        pos    = as.integer(nz),
        length = x@length)
  }
)
