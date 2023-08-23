f <- function(x, y) x * y
a <- 1:5
b <- 3

test_that("papply", {
  for (n in c(1, length(a) - 1, length(a), length(a) + 1)) {
    expect_equal(
      papply(
        numThreads = n,
        x = a,
        fun = f,
        b
      ),
      as.list(f(a, b))
    )
  }
})
test_that("papply preserves options", {
  options("MyPreciousOption" = "Awesome value")
  expect_equal(
    papply(
      numThreads = 3,
      x = a,
      fun = function(x) {
        unname(options("MyPreciousOption"))
      }
    ),
    as.list(
      rep(
        x = "Awesome value",
        times = length(a)
      )
    )
  )
})
