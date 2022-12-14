base_path <- "c:/packages/star/tests/testthat/"

base_path <- "."



dev <- FALSE


test_that("test1: setup_module() works as expected", {


  pth <- file.path(base_path, "modules/test1")


  res <- setup_module("test1", pth)

  dex <- dir.exists(res)
  f1ex <- file.exists(file.path(pth, "module.yml"))
  f2ex <- file.exists(file.path(pth, "test-module.R"))

  expect_equal(dex, TRUE)
  expect_equal(f1ex, TRUE)
  expect_equal(f2ex, TRUE)

  if (!dev) {

    if (dex)
      unlink(res, recursive = TRUE, force = TRUE)

  }

})



test_that("test2: setup_module() works as expected", {


  pth <- file.path(base_path, "modules/test2")


  res <- setup_module("test2", pth)

  dex <- dir.exists(res)
  f2ex <- file.exists(file.path(pth, "module.yml"))

  expect_equal(dex, TRUE)
  expect_equal(f2ex, TRUE)

  res2 <- push_module(res)

  dex2 <- dir.exists(res2)

  cpth <- rappdirs::user_cache_dir("star")
  expect_equal(dex2, TRUE)
  expect_equal(file.exists(file.path(res2, "module.yml")), TRUE)
  expect_equal(file.exists(file.path(res2, "export.R")), TRUE)
  expect_equal(file.exists(file.path(res2, "build.R")), TRUE)
  expect_equal(file.exists(file.path(res2, "create.R")), TRUE)
  expect_equal(file.exists(file.path(res2, "test-module.R")), TRUE)

  if (!dev) {

    if (dex)
      unlink(res, recursive = TRUE, force = TRUE)

    if (dex2)
      unlink(res2, recursive = TRUE, force = TRUE)

  }

})

