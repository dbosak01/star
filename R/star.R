
#' @title Function to promote a module
#' @param pth The path to the module.
#' @export
#' @import rappdirs
star_push <- function(func, status = "d") {


  pth <- rappdirs::user_cache_dir("star")

  if (!dir.exists(pth)) {
    dir.create(pth, recursive = TRUE)

  }

  nm <- deparse(substitute(func, env = environment()))


  fl <-  file.path(pth, paste0(nm, ".rds"))

  if (file.exists(fl)) {

    file.remove(fl)
  }

  save(list = nm, file = fl, envir = environment())

}


#' @title Generic function to run module
#' @export
runmod <- function (x) {
  UseMethod("runmod", x)
}


# https://docs.tibco.com/pub/enterprise-runtime-for-R/6.0.0/doc/html/Language_Reference/tools/R_user_dir.html
#tools::R_user_dir()


# RStudio package to do same
# https://cran.r-project.org/web/packages/rappdirs/index.html




# Function that runs when package loads
#' @import rappdirs
.onAttach <- function(...) {

  pth <- rappdirs::user_cache_dir("star")

  lst <- list.files(pth, full.names = TRUE)

  if (length(lst)) {


    for (fl in lst) {

      print(fl)

      if (file.exists(fl)) {


        load(fl, envir = globalenv())

      }

    }
  }

}


# myfunc <- function() {
#
#  print("Hello")
# }
#
# star_push(myfunc)

#
#
# save(myfunc, file = "myfunc.rds")
#
#
# load("myfunc.rds")
# myfunc()
