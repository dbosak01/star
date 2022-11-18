base_path <- "c:/packages/star/tests/testthat/"

base_path <- "."



dev <- FALSE


test_that("mod1: module() works as expected", {



  mod <- module("Test1", "Here is a description",
                1, 2, FALSE, "test", c("hello", "goodbye"),
                c("fmtr", "reporter"))


  expect_equal(mod$name, "Test1")
  expect_equal(mod$description, "Here is a description")
  expect_equal(mod$major_version, 1)
  expect_equal(mod$minor_version, 2)
  expect_equal(mod$active, FALSE)
  expect_equal(mod$level, "test")
  expect_equal(mod$keywords, c("hello", "goodbye"))
  expect_equal(mod$dependancies, c("fmtr", "reporter"))

})


test_that("mod2: write_module() works as expected.", {



  mod <- module("Test1", "Here is a description",
                1, 2, FALSE, "test", c("hello", "goodbye"),
                c("fmtr", "reporter"))


  fp <- file.path(base_path, "output")
  tpth <- file.path(fp, "module.yml")


  if (file.exists(tpth))
    file.remove(tpth)

  write_module(mod, fp)



  res <- file.exists(tpth)

  expect_equal(res, TRUE)


})

test_that("mod3: write_module() works as expected.", {


  fp <- file.path(base_path, "output")

  mod <- read_module(fp)


  expect_equal(mod$name, "Test1")
  expect_equal(mod$description, "Here is a description")
  expect_equal(mod$major_version, 1)
  expect_equal(mod$minor_version, 2)
  expect_equal(mod$active, FALSE)
  expect_equal(mod$level, "test")
  expect_equal(mod$keywords, c("hello", "goodbye"))
  expect_equal(mod$dependancies, c("fmtr", "reporter"))


})


test_that("mod4: add_parameter() works as expected.", {


  mod <- module("Test1", "Here is a description",
                1, 2, FALSE, "test", c("hello", "goodbye"),
                c("fmtr", "reporter"))


  mod <- add_parameter(mod, "spork", 1L, label = "Spork", data_type = "integer",
                       input_type = "text")


  expect_equal(mod$parameters$spork$name, "spork")
  expect_equal(mod$parameters$spork$value, 1L)
  expect_equal(mod$parameters$spork$data_type, "integer")
  expect_equal(mod$parameters$spork$input_type, "text")


})


test_that("mod4: add_function() works as expected.", {



  mod <- module("Test1", "Here is a description",
                1, 2, FALSE, "test", c("hello", "goodbye"),
                c("fmtr", "reporter"))

  mod <- add_method(mod, "hellomtd", function(){print("hello")})


  expect_equal(mod$hellomtd(), "hello")


})


test_that("mod4: add_function() works as expected.", {



  mod <- module("Test1", "Here is a description",
                1, 2, FALSE, "test", c("hello", "goodbye"),
                c("fmtr", "reporter"))

  #print(mod)


  expect_equal(1, 1)

})

