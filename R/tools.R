

# RStudio package to manage application directories
# https://cran.r-project.org/web/packages/rappdirs/index.html


#' @title Function to promote a module
#' @param location The location of the development directory of the module to push.
#' @param level The status of the function.  Valid values are d, t, and p.
#' @export
#' @import rappdirs
#' @import common
push_module <- function(location, level = "d") {

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



#' @title Initialize a new module
#' @description Function will create folder and template files to
#' begin development of a new \strong{star} module.
#' @param name The name of the new module.
#' @param location The development directory for the new module. If the
#' directory does not exist, it will be created.
#' @import common
#' @export
setup_module <- function(name, location) {


  ret <- NULL

  # Get external data directory
  extd <- file.path(system.file("extdata", package = "star"), "templates/v0.0")


  if (!dir.exists(location)) {
    dir.create(location)

  }

  #browser()

  lst <- file.find(extd, up = 0, down = 0)

  for (fl in lst) {

    tpth <- file.path(location, basename(fl))

    file.copy(fl, tpth, overwrite = TRUE)
  }

  mod <- module(name)
  write_module(mod, location)


  return(location)

}


#' @title Test a module
#' @param mod The program module to test.
#' @export
test_module <- function(mod = NULL) {

  pth <- mod
  if ("module" %in% class(mod)) {


  }



}
