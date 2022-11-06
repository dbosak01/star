

# RStudio package to manage application directories
# https://cran.r-project.org/web/packages/rappdirs/index.html




#' @title Function to promote a module
#' @param func The module to push.
#' @param level The status of the function.  Valid values are d, t, and p.
#' @export
#' @import rappdirs
star_push <- function(func, level = "d") {


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
#' @param x An object to run.
#' @export
star_run <- function (x) {
  UseMethod("star_run", x)
}





# Function that runs when package loads
#' @import rappdirs
.onAttach <- function(...) {

  pth <- rappdirs::user_cache_dir("star")

  lst <- list.files(pth, full.names = TRUE)

  if (length(lst)) {


    for (fl in lst) {

     # print(fl)

      if (file.exists(fl)) {


        load(fl, envir = globalenv())

      }

    }
  }

}


