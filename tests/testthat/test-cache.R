base_path <- "c:/packages/star/tests/testthat/"

base_path <- "."

dev <- FALSE


test_that("cache1: refresh_modules() works as expected", {

  if (dev) {

    pth <- file.path(base_path, "modules")


    lst <- refresh_modules(pth)

    lst

    nms1 <- names(lst)

    nms1

    expect_equal(length(nms1) > 0, TRUE)

    lst2 <- refresh_modules()

    lst2$module2$major_version


    nms2 <- names(lst2)

    nms2


    expect_equal(length(nms2), 0)

  } else {

    expect_equal(TRUE, TRUE)
  }

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

