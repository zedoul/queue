context("queue")

test_that("queue", {
  .q <- queue(name = "test")
  expect_true(inherits(.q, "queue"))
  expect_true(.q$name == "test")
  expect_true(length(.q) == 0)
  push(.q, 1)
  expect_true(length(.q) == 1)
  v <- pop(.q)
  expect_true(v == 1)
  expect_true(length(.q) == 0)
})

