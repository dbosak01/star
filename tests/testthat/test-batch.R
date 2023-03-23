base_path <- "c:/packages/star/tests/testthat/"

base_path <- "."

dev <- FALSE




test_that("batch1: program_parms() works as expected.", {



  p <- program_parms("myprog", "v1.3", list(x = 1, y = "fork", z = c(1, 2, 3)))


  expect_equal(p$program, "myprog")
  expect_equal(p$version, "v1.3")
  expect_equal(p$parms$x, 1)
  expect_equal(p$parms$y, "fork")
  expect_equal(p$parms$z, c(1, 2, 3))

})

test_that("batch2: run_program() works as expected.", {


  pth <- file.path(base_path, "output/test6.R")

  res <- run_program(pth, a = 1, b = 2)


  expect_equal(res$a, 1)
  expect_equal(res$b, 2)

})


test_that("batch3: get_parm() works as expected.", {

  expect_equal(get_parm("fork", 1, "integer"), "fork=1")
  expect_equal(get_parm("fork", 1, "character"), "fork='1'")
  expect_equal(get_parm("fork", c(1, 2, 3), "integer"), "fork=c(1, 2, 3)")
  expect_equal(get_parm("fork", c(1, 2, 3), "character"), "fork=c('1', '2', '3')")

})

test_that("batch4: create_batch() works as expected.", {

  lst <- list(program_parms("test6", "v0.0", list(x = 1, y = "fork", z = c(1, 2, 3))),
              program_parms("test7", "v0.0", list(x = 1, y = "bork", z = c(3, 2, 1))),
              program_parms("test9", "v0.0", list(z = 1, x = "bork", y = c(3, 2, 1))))


  loc <- file.path(base_path, "output")
  rmt <- file.path(base_path, "remote")

  res <- create_batch(lst, loc, rmt)



  expect_equal(file.exists(file.path(loc, "test6.R")), TRUE)
  expect_equal(file.exists(file.path(loc, "test7.R")), TRUE)
  expect_equal(file.exists(file.path(loc, "test9.R")), TRUE)
  expect_equal(file.exists(file.path(loc, "run_all.R")), TRUE)



})
