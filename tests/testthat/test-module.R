base_path <- "c:/packages/star/tests/testthat/"

base_path <- "."



dev <- FALSE


test_that("mod1: module() works as expected", {



  mod <- module("Test1", description = "Here is a description",
                major_version = 1, minor_version = 2, active = FALSE,
                template = FALSE,
                level =  "test", keywords = c("hello", "goodbye"),
                dependancies = c("fmtr", "reporter"))


  expect_equal(mod$name, "Test1")
  expect_equal(mod$description, "Here is a description")
  expect_equal(mod$major_version, 1)
  expect_equal(mod$minor_version, 2)
  expect_equal(mod$active, FALSE)
  expect_equal(mod$level, "test")
  expect_equal(mod$keywords, c("hello", "goodbye"))
  expect_equal(mod$dependancies, c("fmtr", "reporter"))

})


test_that("mod2: write_module() and read_module() work as expected.", {



  mod <- module("Test1", "Here is a description",
                1, 2, FALSE, FALSE, "test", c("hello", "goodbye"),
                c("fmtr", "reporter"))


  fp <- file.path(base_path, "output")
  tpth <- file.path(fp, "module.yml")


  if (file.exists(tpth))
    file.remove(tpth)

  write_module(mod, fp)



  res <- file.exists(tpth)

  expect_equal(res, TRUE)


  mod2 <- read_module(fp)


  expect_equal(mod$name, mod2$name)
  expect_equal(mod$description, mod2$description)
  expect_equal(mod$version, mod2$version)
  expect_equal(mod$active, mod2$active)
  expect_equal(mod$template, mod2$template)
  expect_equal(mod$level, mod2$level)
  expect_equal(mod$keywords, mod2$keywords)
  expect_equal(mod$dependancies, mod2$dependancies)
  expect_equal(mod$created_by, mod$created_by)
  expect_equal(mod$create_date, mod$create_date)

})



test_that("mod4: add_parameter() works as expected.", {


  mod <- module("Test1", "Here is a description",
                1, 2, FALSE, FALSE, "test", c("hello", "goodbye"),
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
                1, 2, FALSE, FALSE, "test", c("hello", "goodbye"),
                c("fmtr", "reporter"))

  mod <- add_method(mod, "hellomtd", function(){print("hello")})


  expect_equal(mod$hellomtd(), "hello")


})


test_that("mod4: print.module() works as expected.", {



  mod <- module("Test1", "Here is a description",
                1, 2, FALSE, FALSE, "test", c("hello", "goodbye"),
                c("fmtr", "reporter"))

  #print(mod)


  expect_equal(1, 1)

})




test_that("mod5: create_module() works as expected", {


  pth <- file.path(base_path, "modules/test5")


  res <- create_module("test1", pth)


  dex <- dir.exists(res$local_path)
  f1ex <- file.exists(file.path(pth, "module.yml"))
  f2ex <- file.exists(file.path(pth, "module.R"))
  f2ex <- file.exists(file.path(pth, "test-module.R"))

  expect_equal(dex, TRUE)
  expect_equal(f1ex, TRUE)
 # expect_equal(f2ex, TRUE)

  if (!dev) {

    if (dex)
      unlink(res, recursive = TRUE, force = TRUE)

  }

})



test_that("mod6: push_module() works as expected", {


  pth <- file.path(base_path, "modules/test6")


  res <- create_module("test6", pth)

  res$description <- "here is a description for module 6."


  write_module(res)

  res2 <- push_module(res)

    dex <- dir.exists(res2$remote_path)
    f1ex <- file.exists(file.path(res2$remote_path, "module.yml"))
    f2ex <- file.exists(file.path(res2$remote_path, "module.R"))
    f3ex <- file.exists(file.path(res2$remote_path, "test-module.R"))

    expect_equal(dex, TRUE)
    expect_equal(f1ex, TRUE)
    expect_equal(f2ex, TRUE)
    expect_equal(f3ex, TRUE)

    if (!dev) {

      if (dex)
        unlink(res$remote_path, recursive = TRUE, force = TRUE)

    }

})





test_that("mod7: push_module() works as expected again", {


  lpth <- file.path(base_path, "modules/test7")

  res <- create_module("test7", lpth, "Test mod7")


  dex <- dir.exists(res$local_path)
  f2ex <- file.exists(file.path(lpth, "module.yml"))

  expect_equal(dex, TRUE)
  expect_equal(f2ex, TRUE)

  res2 <- push_module(res)

  dex2 <- dir.exists(res2$remote_path)

  expect_equal(dex2, TRUE)
  expect_equal(file.exists(file.path(res2$remote_path, "module.yml")), TRUE)
  expect_equal(file.exists(file.path(res2$remote_path, "module.R")), TRUE)
  expect_equal(file.exists(file.path(res2$remote_path, "test-module.R")), TRUE)

  expect_equal("test7" %in% names(find_modules()), TRUE)
#
#   if (!dev) {
#
#     if (dex)
#       unlink(res$local_path, recursive = TRUE, force = TRUE)
#
#     if (dex2)
#       unlink(res2$remote_path, recursive = TRUE, force = TRUE)
#
#   }

})



test_that("mod8: pull_module() works as expected", {

  tpth <- file.path(base_path, "modules")


  mod <- find_modules(name = "test7")


  if (dir.exists(tpth)) {

    unlink(tpth, recursive = TRUE, force = TRUE)
  }


  res2 <- pull_module(mod, tpth)


  dex <- dir.exists(res2$local_path)
  f2ex <- file.exists(file.path(tpth, "test7/module.yml"))

  expect_equal(dex, TRUE)
  expect_equal(f2ex, TRUE)
  expect_equal(file.exists(file.path(res2$local_path, "module.R")), TRUE)
  expect_equal(file.exists(file.path(res2$local_path, "test-module.R")), TRUE)

  expect_equal("test7" %in% names(find_modules()), TRUE)

  if (!dev) {

    if (dex)
      unlink(res2$local_path, recursive = TRUE, force = TRUE)

    if (dex)
      unlink(res2$remote_path, recursive = TRUE, force = TRUE)

  }

})


test_that("mod9: run_module() works as expected.", {


  tpth <- file.path(base_path, "modules")


  mod <- find_modules(name = "test7")



})




# test_that("mod9: copy_module() works as expected", {
#
#   tpth <- file.path(base_path, "modules")
#
#
#   mod <- find_modules(name = "test7")
#
#
#   if (dir.exists(spth)) {
#
#     unlink(spth, recursive = TRUE, force = TRUE)
#   }
#
#
#   res2 <- copy_module(mod, "test8", tpth)
#
#
#   dex <- dir.exists(res$local_path)
#   f2ex <- file.exists(file.path(tpth, "test7/module.yml"))
#
#   expect_equal(dex, TRUE)
#   expect_equal(f2ex, TRUE)
#   expect_equal(file.exists(file.path(res2$local_path, "module.R")), TRUE)
#   expect_equal(file.exists(file.path(res2$local_path, "test-module.R")), TRUE)
#
#   expect_equal("test7" %in% names(find_modules()), TRUE)
#
#   if (!dev) {
#
#     if (dex)
#       unlink(res$local_path, recursive = TRUE, force = TRUE)
#
#     if (dex2)
#       unlink(res2$remote_path, recursive = TRUE, force = TRUE)
#
#   }
#
# })


