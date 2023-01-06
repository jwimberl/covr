test_that("regular function `a` works as expected", {
  expect_equal(a(1), 1)
  expect_equal(a(2), 2)
  expect_equal(a(3), 2)
  expect_equal(a(4), 2)
  expect_equal(a(0), 1)
})

test_that("TestR6 class can be instantiated", {
  t1 <- TestR6$new() # nolint
})

test_that("TestR6 Methods can be evaluated", {
  t1 <- TestR6$new() # nolint

  t1$show()
  print(t1$print2())
})

test_that("TestR6 static methods can be evaluated", {
  t1 <- TestR6$factory(1) #nolint
  
  t1$show()
  print(t1$print2())

  t2 <- TestR6$factory(2) #nolint
  
  t2$show()
  print(t2$print2())
  
})
