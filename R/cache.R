e <- new.env()

e$modules <- list()



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
    pth <- get_cache_directory()

  lst <- file.find(pth, pattern = "module.yml", up = -1, down = 1)

  # print(lst)

  e$modules <- list()

  if (length(lst) > 0) {


    for (fl in lst) {

      # print(fl)

      if (file.exists(fl)) {

        mod <- read_module(dirname(fl))

        if (!is.null(mod$name)) {
          if (!mod$name %in% names(e$modules)) {
            e$modules[[mod$name]] <- mod
          }
        }
      }
    }
  }

  return(e$modules)

}




# RStudio package to manage application directories
# https://cran.r-project.org/web/packages/rappdirs/index.html


#' @title Function to promote a module
#' @param module The module to push or the location of the
#' development directory of the module to push.
#' @param level The status of the function.  Valid values are d, t, and p.
#' @export
#' @import common
push_module <- function(module, level = "d") {

  # Get location of local development directory
  location <- module
  if ("module" %in% class(module))
    location <- file.path(module$local_path, module$version)

  pth <- get_cache_directory()

  if (!dir.exists(pth)) {
    dir.create(pth, recursive = TRUE)

  }

  if (!dir.exists(location)) {

    stop("Module directory location does not exist.")
  }

  # Get module
  fl <- file.path(location, "module.yml")

  if (!file.exists(fl)) {
    stop("Module file does not exist.")
  }


  mod <- yaml::read_yaml(fl)

  # Get module name
  nm <- mod$name

  dr <-  file.path(pth, nm)

  # Create module folder in cache if needed
  if (!dir.exists(dr)) {

    dir.create(dr)

  }

  vdr <- file.path(dr, mod$version)
  if (!dir.exists(vdr)) {

    dir.create(vdr)
  }

  mod$remote_path <- dr

  # Get list of other files in module
  drs <- dir.find(location, up = -1, down = 5)

  # Copy everything to cache
  for (dr in drs) {

   d <- file.path(vdr, basename(dr))

    if (dir.exists(d)) {

      unlink(d, recursive = TRUE, force = TRUE)
    }

    dir.create(d)
  }



  # Get list of other files in module
  lst <- file.find(location, up = 0, down = 5)

  # Copy everything to cache
  for (src in lst) {


    fl <- file.path(vdr, basename(src))

    if (file.exists(fl)) {

      file.remove(fl)
    }

    file.copy(src, fl, overwrite = TRUE)
  }

  if (!is.null(mod$name)) {
    e$modules[[mod$name]] <- mod
  }


  return(mod)

}

#' @title Pull a module down from the cache
#' @param mod The module to pull.
#' @param location  The directory in which to pull the specified module.
#' @export
pull_module <- function(mod, location = NULL) {

  if (!dir.exists(location)) {

    dir.create(location, recursive = TRUE)
  }

  lpth <- file.path(location, mod$name)

  if (!dir.exists(lpth)) {

    dir.create(lpth, recursive = TRUE)
  }

  spth <- mod$remote_path

  # Get list of other files in module
  lst <- file.find(spth, up = 0, down = 0)

  # Copy everything to location
  for (src in lst) {


    fl <- file.path(lpth, basename(src))

    if (file.exists(fl)) {

      file.remove(fl)
    }

    file.copy(src, fl, overwrite = TRUE)
  }


  mod$local_path <- lpth


  return(mod)

}


#' @import rappdirs
#' @import yaml
get_cache_directory <- function() {


  pth <- user_cache_dir("star")

  cpth <- file.path(user_config_dir("star", roaming = FALSE), "Config/config.yml")

  if (file.exists(cpth)) {

    yml <- yaml::read_yaml(cpth)

    pth <- yml$cache_directory
  }

  return(pth)
}

#' @title Find Modules
#' @export
find_modules <- function(name = NULL, keywords = NULL, templates = NULL, active = NULL,
                         level = NULL) {


  ret <- e$modules

  if (!is.null(name)) {

     ret <- ret[[name]]
  }

  # More filters here


  return(ret)

}
