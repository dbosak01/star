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
