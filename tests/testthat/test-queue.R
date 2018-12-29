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

test_that("iter", {
  .q <- queue(name = "test")
  push(.q, "A")
  push(.q, "B")
  push(.q, "C")
  .items <- items(.q)
  expect_equal(LETTERS[1:3], unlist(.items))
})

