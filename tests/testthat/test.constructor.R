# test SimpleGP constructor

# 2019-01-03 - Shaun L. Cloherty <s.cloherty@ieee.org>

context("SimpleGP constructor")

library("simpleGP")

test_that("constructor handles all invocations", {
  # no arguments (default parameters)
  gp <- SimpleGP$new()
  expect_is(gp, "SimpleGP")

  # named arguments
  gp <- SimpleGP$new(l = 0.5, sigmaf = 5.0, sigman = 0.25)
  expect_is(gp, "SimpleGP")

  expect_equal(gp$l,0.5)
  expect_equal(gp$sigmaf,5.0)
  expect_equal(gp$sigman,0.25)

  # positional arguments
  gp <- SimpleGP$new(0.5, 5.0, 0.25)
  expect_is(gp, "SimpleGP")

  expect_equal(gp$l,0.5)
  expect_equal(gp$sigmaf,5.0)
  expect_equal(gp$sigman,0.25)
})
