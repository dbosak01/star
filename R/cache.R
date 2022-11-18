

#' @title A list of available report modules.
#' @export
modules <- list()


# Function that runs when package loads

.onAttach <- function(...) {

  refresh_modules()

}


#' @title Refresh the module cache
#' @param pth An optional path to the cache.  If NULL, it will use the
#' path associated with the package.
#' @import rappdirs
#' @import common
#' @export
refresh_modules <- function(pth = NULL) {

  if (is.null(pth))
    pth <- rappdirs::user_cache_dir("star")

  lst <- file.find(pth, pattern = "module.yml", up = -1, down = 1)

  # print(lst)

  modules <- list()

  if (length(lst) > 0) {


    for (fl in lst) {

      # print(fl)

      if (file.exists(fl)) {

        mod <- read_module(dirname(fl))

        if (!is.null(mod$name)) {
          if (!mod$name %in% names(modules)) {
            modules[[mod$name]] <- mod
          }
        }
      }
    }
  }

  return(modules)

}

