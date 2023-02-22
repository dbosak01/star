base_path <- "c:/packages/star/tests/testthat/"

base_path <- "."

dev <- FALSE


test_that("cache1: refresh_modules() works as expected", {

  if (dev) {

    pth <- file.path(base_path, "remote")


    lst <- refresh_modules(pth)

    lst

    nms1 <- names(lst)

    nms1

    expect_equal(length(nms1) > 0, TRUE)

    lst2 <- refresh_modules()

    lst2[1, ]$Version


    nms2 <- names(lst2)

    nms2


    expect_equal(TRUE, TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }

})


test_that("cache2: add_module() works as expected", {


  lpth <- file.path(base_path, "modules/test8")


  res <- create_module("test8", lpth, "Test mod8", overwrite = TRUE)

  lst <- find_modules(version = "all")


  lst2 <- add_module(res)

  expect_equal(nrow(lst) + 1, nrow(lst2))


})


test_that("cache3: find_modules() works with wildcard in name.", {




  lst <- find_modules(name = "tbl*")

  lst


  expect_equal(nrow(lst), 1)
  expect_equal(lst[1, "Version"], "v0.3")

  lst <- find_modules(name = "tbl*", version = "all")

  expect_equal(nrow(lst), 3)


  lst <- find_modules(name = "tbl*", version = "v0.2")

  expect_equal(nrow(lst), 1)
  expect_equal(lst[1, "Version"], "v0.2")


})


test_that("cache3: find_modules() works with wildcard in name.", {




  lst <- find_modules(name = "lst*")

  lst

  mod <- pull_module(lst$Name)

  expect_equal(is.null(mod), FALSE)


})



test_that("cache4: push_module() works as expected", {


  lpth <- file.path(base_path, "modules/test9")


  mod <- create_module("test9", lpth, "Test mod9", overwrite = TRUE)

  fl <- file.create(file.path(lpth, "v0.0/output", "myfile.txt"))


  res <- push_module(mod)


  expect_equal(nrow(lst) + 1, nrow(lst2))

})

#
#
# test_that("mod6: create_module() works as expected", {
#
#
#   pth <- file.path(base_path, "modules/test2")
#
#
#   res <- create_module("test2", pth)
#
#   dex <- dir.exists(res$local_path)
#   f2ex <- file.exists(file.path(pth, "module.yml"))
#
#   expect_equal(dex, TRUE)
#   expect_equal(f2ex, TRUE)
#
#   res2 <- push_module(res)
#
#   dex2 <- dir.exists(res2)
#
#   cpth <- rappdirs::user_cache_dir("star")
#   expect_equal(dex2, TRUE)
#   expect_equal(file.exists(file.path(res2, "module.yml")), TRUE)
#   expect_equal(file.exists(file.path(res2, "test-module.R")), TRUE)
#
#   if (!dev) {
#
#     if (dex)
#       unlink(res, recursive = TRUE, force = TRUE)
#
#     if (dex2)
#       unlink(res2, recursive = TRUE, force = TRUE)
#
#   }
#
# })
#
#
# test_that("mod6: copy_module() works as expected", {
#
#
#   pth <- file.path(base_path, "modules/test2")
#
#
#   res <- create_module("test2", pth)
#
#   dex <- dir.exists(res$local_path)
#   f2ex <- file.exists(file.path(pth, "module.yml"))
#
#   expect_equal(dex, TRUE)
#   expect_equal(f2ex, TRUE)
#
#   res2 <- push_module(res)
#
#   dex2 <- dir.exists(res2)
#
#   cpth <- rappdirs::user_cache_dir("star")
#   expect_equal(dex2, TRUE)
#   expect_equal(file.exists(file.path(res2, "module.yml")), TRUE)
#   expect_equal(file.exists(file.path(res2, "export.R")), TRUE)
#   expect_equal(file.exists(file.path(res2, "build.R")), TRUE)
#   expect_equal(file.exists(file.path(res2, "create.R")), TRUE)
#   expect_equal(file.exists(file.path(res2, "test-module.R")), TRUE)
#
#   if (!dev) {
#
#     if (dex)
#       unlink(res, recursive = TRUE, force = TRUE)
#
#     if (dex2)
#       unlink(res2, recursive = TRUE, force = TRUE)
#
#   }
#
# })


test_that("mod7: get_cache_directory() works as expected", {


  cfgdir <- rappdirs::user_config_dir("star", roaming = FALSE)

  cfgpth <-  file.path(cfgdir, "Config/config.yml")

  if (file.exists(cfgpth))
    file.remove(cfgpth)

  res <- get_cache_directory()

  cd <- rappdirs::user_cache_dir("star")

  expect_equal(res, cd)


  cfg <- list("cache_directory" = file.path(base_path, "remote"))


  yaml::write_yaml(cfg, cfgpth)

  res2 <- get_cache_directory()

  res2

  expect_equal(res2, cfg$cache_directory)



})


# test_that("mod8: find_modules() works as expected.", {
#
#
#   refresh_modules()
#
#
#   res <- find_modules()
#
#   res
#
# })

