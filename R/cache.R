

# Globals -----------------------------------------------------------------



e <- new.env()

e$modules <- NULL


# Events ------------------------------------------------------------------



# Function that runs when package loads
.onAttach <- function(...) {

  refresh_modules()

}



# Functions ---------------------------------------------------------------



#' @title Refresh the module cache
#' @param pth An optional path to the cache.  If NULL, it will use the
#' path associated with the package.
#' @import rappdirs
#' @import common
#' @family cache
#' @export
refresh_modules <- function(pth = NULL) {

  if (is.null(pth))
    pth <- get_cache_directory()

  lst <- c()

  # Get module list
  if (dir.exists(pth)) {

    lst <- file.find(pth, pattern = "module.yml", up = -1, down = 2)
  }


  # Initalize variables
  nm <- c()
  ver <- c()
  desc <- c()
  ta <- c()
  kwrd <- c()
  rpth <- c()
  lver <- c()

  # Populate module data frame
  if (length(lst) > 0) {

    cnt <- 0

    for (fl in lst) {


      cnt <- cnt + 1
      if (file.exists(fl)) {

        mod <- read_module(dirname(fl))

        if (!is.null(mod$name)) {
          nm[cnt] <- mod$name
          if (is.null(mod$version))
            ver[cnt] <- ""
          else
            ver[cnt] <- mod$version

          if (is.null(mod$description))
            desc[cnt] <- ""
          else
            desc[cnt] <- mod$description

          if (is.null(mod$TA))
            ta[cnt] <- ""
          else
            ta[cnt] <- mod$TA

          if (is.null(mod$keywords))
            kwrd[cnt] <- ""
          else
            kwrd[cnt] <- paste(mod$keywords, collapse = " ")

          if (is.null(mod$remote_path))
            rpth[cnt] <- ""
          else
            rpth[cnt] <- mod$remote_path

          lver[cnt] <- FALSE

        }
      }
    }
  }

  # Create data frame from vectors
  mdf <- data.frame(Name = nm, Version = ver, Description = desc,
                    TA = ta,
                    Keywords = kwrd, RemotePath = rpth,
                    LastVersion = lver)

  if (length(lst) > 0) {

  # Set last version
  mdf$LastVersion <- as.logical(ave(mdf$Version, mdf$Name,
                                        FUN = function(x) x == max(x)))

  } else {

    mdf$LastVersion <- c()
  }

  # Assign to modules global variable
  e$modules <-mdf

  return(mdf)

}


#' @title Find Modules
#' @description
#' @param name The name of the module to search for. Parameter will accept wildcards.
#' @param keywords  A vector of keywords to search for.
#' @param version A parameter that identifies the version or version to search for.
#' Valid values are "all", "latest", or a vector of versions in the form "vX.Y".
#' @family cache
#' @import utils
#' @export
find_modules <- function(name = NULL, keywords = NULL, version = "latest") {


  # Refresh the module list
  refresh_modules()

  # Store module list temporarily
  ret <- e$modules

  # Filter by version
  if (!is.null(version)) {
    if (version == "latest") {
      ret <- ret[ret$LastVersion == TRUE, ]

    } else if (version != "all") {

      ret <- ret[ret$Version %in% version, ]
    }

  }

  # Filter by name
  if (!is.null(name)) {

    pos <- grep(glob2rx(name), ret$Name, ignore.case = TRUE)

    ret <- ret[pos, ]
  }

  # Filter by keywords
  if (!is.null(keywords)) {

    pos <- c()
    for (kw in keywords) {
      kws <- paste0("*", kw, "*")

      tmp <- grep(glob2rx(kws), ret$Keywords, ignore.case = TRUE)
      pos <- append(pos, tmp)

    }

    pos <- sort(unique(pos))

    ret <- ret[pos, ]
  }

  return(ret)

}



#' @title Function to promote a module
#' @description The \code{push_module} function promotes a module to the
#' remote module cache. The function accepts a module object and a promotion
#' level.
#' @param module The module to push or the location of the
#' development directory of the module to push.
#' @param level The status of the function.  Valid values are d, t, and p.
#' @return The function will return the pushed module.
#' @family cache
#' @export
#' @import common
push_module <- function(module, level = "d") {

  # Get location of local development directory
  location <- module
  if ("module" %in% class(module)) {
    location <- file.path(module$local_path, module$version)
    mod <- module

  } else if ("character" %in% class(module)) {

    if (!dir.exists(location)) {

      stop("Module directory location does not exist.")
    }

    mod <- read_module(location)

  } else {

    stop("Module parameter must be a module object or a local module directory.")
  }

  pth <- get_cache_directory()

  if (!dir.exists(pth)) {
    dir.create(pth, recursive = TRUE)

  }

  # Get module name
  nm <- mod$name

  rpth <-  file.path(pth, nm)

  # Create module folder in cache if needed
  if (!dir.exists(rpth)) {

    dir.create(rpth)

  }

  vdr <- file.path(rpth, mod$version)
  if (!dir.exists(vdr)) {

    dir.create(vdr)
  }



  # Get list of other folders in module
  drs <- dir.find(location, up = 0, down = 5)

  # Copy everything to cache
  for (dr in drs) {

    if (basename(vdr) != basename(dr)) {

      d <- file.path(vdr, basename(dr))

      if (dir.exists(d)) {

        unlink(d, recursive = TRUE, force = TRUE)
      }

      dir.create(d)

    } else {

      d <- vdr
    }

    # Get list of other files in module
    lst <- file.find(dr, up = 0, down = 0)

    # Copy everything to cache
    for (src in lst) {


      fl <- file.path(d, basename(src))

      if (file.exists(fl)) {

        file.remove(fl)
      }

      file.copy(src, fl, overwrite = TRUE)
    }

  }

  # Update remote path
  mod$remote_path <- rpth

  # Save locally
  write_module(mod)

  # Save remotely
  write_module(mod, vdr)

  # Add to module list
  if (!is.null(mod$name)) {
    e$modules <- add_module(mod)
  }

  return(mod)

}

#' @title Pull a module down from the cache
#' @description The \code{pull_module} function pulls a module and associated
#' files down from the module cache and places them in a local directory.  This
#' function is used to edit a module or examine the code.  You may specify a
#' specific version of the module.  If  a version is not specified, the function
#' will return the most recent version.
#' @param name The module name to pull.
#' @param version The module version to pull.  By default, the function
#' will pull the latest version.
#' @param location  The directory in which to pull the specified module.  By
#' default, the function will pull the module into a directory with the same
#' name as the module in the current working directory.
#' @return The function will return the pulled module.  Any file associated
#' with the module will be copied to the local path.
#' @family cache
#' @export
pull_module <- function(name, version = NULL, location = NULL) {

  #browser()

  if (is.null(location)) {

   location <- "."
  }

  lpth <- file.path(location, name)
  rpth <- file.path(get_cache_directory(), name)

  if (is.null(version)) {
    lst <- find_modules(name = name, version = "latest")

    if (nrow(lst) == 1) {
      version <- lst[1, "Version"]
    }
  }


  vpth <- file.path(lpth, version)

  if (!dir.exists(lpth)) {

    dir.create(lpth, recursive = TRUE)

  }

  if (!dir.exists(vpth)) {
    dir.create(vpth, recursive = TRUE)
  }


  spth <- file.path(rpth, version)

  # Get list of other files in module
  drs <- dir.find(spth, up = 0, down = 5)

  # Copy everything to local
  for (dr in drs) {

    if (basename(vpth) != basename(dr)) {

      d <- file.path(vpth, basename(dr))

      if (dir.exists(d)) {

        unlink(d, recursive = TRUE, force = TRUE)
      }

      dir.create(d)

    } else {

      d <- vpth
    }


    # Get list of other files in module
    lst <- file.find(dr, up = 0, down = 0)

    # Copy everything to location
    for (src in lst) {


      fl <- file.path(d, basename(src))

      if (file.exists(fl)) {

        file.remove(fl)
      }

      file.copy(src, fl, overwrite = TRUE)
    }

  }


  mod <- read_module(vpth)

  mod$local_path <- lpth

  write_module(mod, )

  return(mod)

}



# Utilities ---------------------------------------------------------------



# RStudio package to manage application directories
# https://cran.r-project.org/web/packages/rappdirs/index.html

# Internal function to return the location of the module cache
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



# Internal function to append a module to the module list
add_module <- function(module) {

  ret <- e$modules

  cnt <- nrow(ret) + 1

  if (!is.null(module$name)) {
    ret[cnt, "Name"] <- module$name
    if (is.null(module$version))
      ret[cnt, "Version"] <- ""
    else
      ret[cnt, "Version"] <- module$version

    if (is.null(module$description))
      ret[cnt, "Description"] <- ""
    else
      ret[cnt, "Description"] <- module$description

    if (is.null(module$TA))
      ret[cnt, "TA"] <- ""
    else
      ret[cnt, "TA"] <- module$TA

    if (is.null(module$keywords))
      ret[cnt, "Keywords"] <- ""
    else
      ret[cnt, "Keywords"] <- paste(module$keywords, collapse = " ")

    if (is.null(module$remote_path))
      ret[cnt, "RemotePath"] <- ""
    else
      ret[cnt, "RemotePath"] <- module$remote_path

    ret[cnt, "LastVersion"] <- FALSE

  }

  return(ret)

}

