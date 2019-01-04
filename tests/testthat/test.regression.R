# test SimpleGP regression

# 2019-01-04 - Shaun L. Cloherty <s.cloherty@ieee.org>

context("SimpleGP regression")

library("simpleGP")

data <- function() {
  data.frame(
    x = c(-1.5, -1.0, -0.75, -0.4, -0.25, 0.0),
    y = c(-1.65, -1.10, -0.33, 0.21, 0.55, 0.87),
    check.rows = TRUE
  )
}

test_that("training works", {
  # get test data
  tst <- data()

  k <- matrix(c(1.70, 1.42, 1.21, 0.87, 0.72, 0.51,
                1.42, 1.70, 1.56, 1.34, 1.21, 0.97,
                1.21, 1.56, 1.70, 1.51, 1.42, 1.21,
                0.87, 1.34, 1.51, 1.70, 1.59, 1.48,
                0.72, 1.21, 1.42, 1.59, 1.70, 1.56,
                0.51, 0.97, 1.21, 1.48, 1.56, 1.70),
              nrow = length(tst$x), ncol = length(tst$x), byrow = TRUE)

  gp <- SimpleGP$new(l = 1.0, sigmaf = 1.27, sigman = 0.3)

  gp <- gp$train(tst$x,tst$y)

  expect_equal(gp$k, k, tolerance = 2e-2)
})

test_that("regression works", {
  # get test data
  tst <- data()

  gp <- SimpleGP$new(l = 1.0, sigmaf = 1.27, sigman = 0.3)

  gp <- gp$train(tst$x,tst$y);

  # get prediction
  xstar = 0.2;
  ystar = gp$predict(xstar) # mean and variance

  expect_equal(ystar$mean, 0.972, tolerance = 1e-3)
  expect_equal(ystar$var, 0.206, tolerance = 1e-3)
})
