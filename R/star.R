

# RStudio package to manage application directories
# https://cran.r-project.org/web/packages/rappdirs/index.html


#' @title Function to promote a module
#' @param location The location of the development directory of the module to push.
#' @param level The status of the function.  Valid values are d, t, and p.
#' @export
#' @import rappdirs
#' @import common
push <- function(location, level = "d") {

  #browser()

  pth <- rappdirs::user_cache_dir("star")

  if (!dir.exists(pth)) {
    dir.create(pth, recursive = TRUE)

  }

  if (!dir.exists(location)) {

    stop("Module directory location does not exist")
  }

  lst <- file.find(location, "*.yml", up = 0, down = 0)

  nm <- ""

  if (length(lst) > 0) {

     nm <- gsub(".yml", "", basename(lst[[1]]), fixed = TRUE)

  }


  dr <-  file.path(pth, nm)

  if (!dir.exists(dr)) {

    dir.create(dr)
  }

  lst <- file.find(location, up = 0, down = 0)

  for (src in lst) {


    fl <- file.path(dr, basename(src))

    if (file.exists(fl)) {

      file.remove(fl)
    }

    file.copy(src, fl, overwrite = TRUE)
  }


  return(dr)

}

# Version where you pass a loaded function
# push_back <- function(location, level = "d") {
#
#
#   pth <- rappdirs::user_cache_dir("star")
#
#   if (!dir.exists(pth)) {
#     dir.create(pth, recursive = TRUE)
#
#   }
#
#   nm <- deparse(substitute(func, env = environment()))
#
#
#   dr <-  file.path(pth, nm)
#
#   if (!dir.exists(dr)) {
#
#     dir.create(dr)
#   }
#
#   if (file.exists(fl)) {
#
#     file.remove(fl)
#   }
#
#   save(list = nm, file = fl, envir = environment())
#
# }



#' @title Generic function to run module
#' @param x An object to run.
#' @export
run <- function (x) {
  UseMethod("star_run", x)
}



#' @title Initialize a new module
#' @description Function will create folder and template files to
#' begin development of a new \strong{star} module.
#' @param name The name of the new module.
#' @param location The development directory for the new module. If the
#' directory does not exist, it will be created.
#' @export
setup_module <- function(name, location) {


  ret <- NULL

  # Get external data directory
  extd <- system.file("extdata", package = "star")


  if (!dir.exists(location)) {
    dir.create(location)

  }

  #browser()

  ypth <- file.path(location, paste0(name, ".yml"))
  rpth <- file.path(location, paste0(name, ".R"))

  file.copy(file.path(extd, "mod.yml"), ypth, overwrite = TRUE)
  file.copy(file.path(extd, "template.R"), rpth, overwrite = TRUE)

  ylns <- readLines(ypth, encoding = "UTF-8")

  ylns <- gsub("<modname>", name, ylns, fixed = TRUE)

  writeLines(ylns, ypth)


  rlns <- readLines(rpth, encoding = "UTF-8")

  rlns <- gsub("modname", name, rlns, fixed = TRUE)

  writeLines(rlns, rpth)


  return(location)

}




# Function that runs when package loads
#' @import rappdirs
#' @import common
.onAttach <- function(...) {

  pth <- rappdirs::user_cache_dir("star")

  lst <- file.find(pth, pattern = "*.r", up = -1, down = 1)

  # print(lst)

  if (length(lst) > 0) {


    for (fl in lst) {


      # print(fl)

      if (file.exists(fl)) {

        source(fl, local = globalenv())

      }
    }
  }

}


