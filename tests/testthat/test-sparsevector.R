context("sparse_numeric full test coverage")

test_that("validity method exists", {
  validity_method <- getValidity(getClassDef("sparse_numeric"))
  expect_false(is.null(validity_method))
})

test_that("valid sparse_numeric object passes validity", {
  x <- new("sparse_numeric",
           value = c(1, 2, 1, 1),
           pos = c(1L, 2L, 3L, 5L),
           length = 5L)
  expect_true(validObject(x))
})

test_that("invalid: length mismatch triggers error", {
  x <- new("sparse_numeric",
           value = c(1, 2, 1, 1),
           pos = c(1L, 2L, 3L, 5L),
           length = 5L)
  x@length <- 2L
  expect_error(validObject(x))
})

test_that("invalid: NA in value", {
  expect_error(new("sparse_numeric",
                   value = c(1, NA),
                   pos = c(1L, 2L),
                   length = 5L))
})

test_that("invalid: NA in pos", {
  expect_error(new("sparse_numeric",
                   value = c(1, 2),
                   pos = c(1L, NA),
                   length = 5L))
})

test_that("invalid: NA in length", {
  expect_error(new("sparse_numeric",
                   value = c(1),
                   pos = c(1L),
                   length = NA))
})

test_that("invalid: pos and value lengths differ", {
  expect_error(new("sparse_numeric",
                   value = c(1, 2),
                   pos = c(1L),
                   length = 5L))
})

test_that("invalid: pos out of range", {
  expect_error(new("sparse_numeric",
                   value = c(1),
                   pos = c(10L),
                   length = 5L))
})

test_that("invalid: duplicate positions", {
  expect_error(new("sparse_numeric",
                   value = c(1, 2),
                   pos = c(3L, 3L),
                   length = 5L))
})

test_that("invalid: zero not allowed in value", {
  expect_error(new("sparse_numeric",
                   value = c(0),
                   pos = c(1L),
                   length = 1L))
})


# ------------------------------------------------------------
# 2. COERCION TESTS
# ------------------------------------------------------------

test_that("coercion return class is sparse_numeric", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  expect_s4_class(x, "sparse_numeric")
})

test_that("coercion all-zero numeric returns empty sparse", {
  s <- as(c(0, 0, 0), "sparse_numeric")
  expect_equal(s@value, numeric(0))
  expect_equal(s@pos, integer(0))
  expect_equal(s@length, 3L)
})

test_that("round-trip numeric -> sparse -> numeric is identical", {
  v <- c(0, 5, 0, -3, 0, 7)
  s <- as(v, "sparse_numeric")
  expect_equal(as(s, "numeric"), v)
})


# ------------------------------------------------------------
# 3. METHOD EXISTENCE
# ------------------------------------------------------------

test_that("show method exists", {
  expect_no_error(getMethod("show", "sparse_numeric"))
})

test_that("plot method exists", {
  expect_no_error(getMethod("plot", c("sparse_numeric", "sparse_numeric")))
})

test_that("+ method exists", {
  expect_no_error(getMethod("+", c("sparse_numeric", "sparse_numeric")))
})

test_that("- method exists", {
  expect_no_error(getMethod("-", c("sparse_numeric", "sparse_numeric")))
})

test_that("* method exists", {
  expect_no_error(getMethod("*", c("sparse_numeric", "sparse_numeric")))
})

test_that("all generics exist", {
  expect_true(isGeneric("sparse_add"))
  expect_true(isGeneric("sparse_sub"))
  expect_true(isGeneric("sparse_mult"))
  expect_true(isGeneric("sparse_crossprod"))
  expect_true(isGeneric("norm"))
  expect_true(isGeneric("standardize"))
})


# ------------------------------------------------------------
# 4. ARITHMETIC & OPERATOR TESTS
# ------------------------------------------------------------

test_that("sparse_add returns sparse_numeric", {
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
  expect_s4_class(sparse_add(x, y), "sparse_numeric")
})

test_that("sparse_add matches expected result", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
  expect_equal(sparse_add(x, y), result)
})

test_that("sparse_add matches dense addition", {
  x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
  y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
  dense <- as(x, "numeric") + as(y, "numeric")
  expect_equal(as(sparse_add(x, y), "numeric"), dense)
})

test_that("length mismatch in add throws error", {
  x <- as(rep(0, 10), "sparse_numeric")
  y <- as(rep(0, 9), "sparse_numeric")
  expect_error(sparse_add(x, y))
})

test_that("operator + matches sparse_add", {
  v1 <- as(c(1, 0, 3), "sparse_numeric")
  v2 <- as(c(0, 4, 3), "sparse_numeric")
  expect_equal(v1 + v2, sparse_add(v1, v2))
})

test_that("operator - matches sparse_sub", {
  v1 <- as(c(1, 0, 3), "sparse_numeric")
  v2 <- as(c(0, 4, 3), "sparse_numeric")
  expect_equal(v1 - v2, sparse_sub(v1, v2))
})

test_that("operator * matches sparse_mult", {
  v1 <- as(c(1, 2, 3), "sparse_numeric")
  v2 <- as(c(0, 4, 3), "sparse_numeric")
  expect_equal(v1 * v2, sparse_mult(v1, v2))
})


# ------------------------------------------------------------
# 5. CROSSPROD
# ------------------------------------------------------------

test_that("sparse_crossprod matches dense dot product", {
  x <- as(c(0, 5, 0, -2, 0), "sparse_numeric")
  y <- as(c(1, 0, 3, 4, 0), "sparse_numeric")
  expect_equal(
    sparse_crossprod(x, y),
    sum(as(x, "numeric") * as(y, "numeric"))
  )
})

test_that("sparse_crossprod no overlapping nonzeros = 0", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 3), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), 0)
})


# ------------------------------------------------------------
# 6. PLOTTING
# ------------------------------------------------------------

test_that("plot() works with overlapping nonzeros", {
  x <- as(c(1, 0, 2), "sparse_numeric")
  y <- as(c(3, 0, 4), "sparse_numeric")
  expect_no_error(plot(x, y))
})

test_that("plot() returns invisible when no overlap", {
  x <- as(c(1,0,0), "sparse_numeric")
  y <- as(c(0,2,0), "sparse_numeric")
  expect_invisible(plot(x,y))
})


# ------------------------------------------------------------
# 7. MEAN & NORM
# ------------------------------------------------------------

test_that("mean matches dense mean", {
  x <- as(c(0, 5, 0, -2, 0), "sparse_numeric")
  expect_equal(mean(x), mean(as(x, "numeric")))
})

test_that("mean of all-zero vector is zero", {
  x <- as(rep(0, 5), "sparse_numeric")
  expect_equal(mean(x), 0)
})

test_that("norm matches dense L2 norm", {
  x <- as(c(0, 5, 0, -2, 0), "sparse_numeric")
  expect_equal(norm(x), sqrt(sum(as(x, "numeric")^2)))
})

test_that("norm of all-zero vector is zero", {
  x <- as(rep(0, 10), "sparse_numeric")
  expect_equal(norm(x), 0)
})


# ------------------------------------------------------------
# 8. STANDARDIZE
# ------------------------------------------------------------

test_that("standardize yields zero mean & unit sd", {
  x <- as(c(0, 5, 0, -2, 0), "sparse_numeric")
  s <- standardize(x)
  dense <- as(s, "numeric")

  expect_equal(mean(dense), 0, tolerance = 1e-8)
  expect_equal(sd(dense), 1, tolerance = 1e-8)
})

test_that("standardize many zeros still works", {
  x <- as(c(0, 10, 0, 0, -5, 0), "sparse_numeric")
  s <- standardize(x)
  dense <- as(s, "numeric")
  expect_equal(mean(dense), 0, tolerance = 1e-8)
  expect_equal(sd(dense), 1, tolerance = 1e-8)
})

test_that("standardize single nonzero element", {
  x <- as(c(0, 5, 0), "sparse_numeric")
  s <- standardize(x)
  dense <- as(s, "numeric")
  expect_equal(mean(dense), 0, tolerance = 1e-8)
  expect_equal(sd(dense), 1, tolerance = 1e-8)
})

test_that("standardize constant vector produces empty sparse", {
  x <- as(c(1, 1, 1, 1), "sparse_numeric")
  s <- standardize(x)
  expect_equal(s@value, numeric(0))
  expect_equal(s@pos, integer(0))
  expect_equal(s@length, x@length)
})
