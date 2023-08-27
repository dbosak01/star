

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
#' @description Read the available modules from the module cache and creates
#' a data frame with information about the modules.
#' @param pth An optional path to the cache.  If NULL, it will use the
#' path associated with the package.
#' @return A dataframe of available modules.
#' @import rappdirs
#' @import common
#' @import stats
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
  typ <- c()
  sta <- c()
  ta <- c()
  ind <- c()
  dom <- c()
  adm <- c()
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

          if (is.null(mod$type))
            typ[cnt] <- ""
          else
            typ[cnt] <- mod$type

          if (is.null(mod$status))
            sta[cnt] <- ""
          else
            sta[cnt] <- mod$status

          if (is.null(mod$TA))
            ta[cnt] <- ""
          else
            ta[cnt] <- mod$TA

          if (is.null(mod$indication))
            ind[cnt] <- ""
          else
            ind[cnt] <- mod$indication

          if (is.null(mod$domain))
            dom[cnt] <- ""
          else
            dom[cnt] <- mod$domain

          if (is.null(mod$ADaMIG))
            adm[cnt] <- ""
          else
            adm[cnt] <- mod$ADaMIG

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
                    Type = typ, Status = sta,
                    TA = ta, Indication = ind, Domain = dom,
                    ADaMIG = adm,
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
#' @description Allows the user to search for modules in the module cache.  The
#' function will return a data frame of modules that meet the specified criteria.
#' Available search criteria are the name, keywords, and module version.
#' @param name The name of the module to search for. Parameter will accept wildcards.
#' @param type The type of module to search for.  Valid values are "Table",
#' "Listing", or "Figure".
#' @param TA The Therapeutic Area to search for.
#' @param domain The domain to search for.
#' @param indication The indication to search for.
#' @param keywords  A vector of keywords to search for.
#' @param status The status of the module to search for.  Valid values are
#' "Development" and "Release".
#' @param version A parameter that identifies the version or version to search for.
#' Valid values are "all", "latest", or a vector of versions in the form "vX.Y".
#' @return A data frame of modules that meet the search criteria.
#' @family cache
#' @import utils
#' @export
find_modules <- function(name = NULL, type = NULL, TA = NULL, domain = NULL,
                         indication = NULL,
                         keywords = NULL, status = "Release", version = "latest") {


  # Refresh the module list
  refresh_modules()

  # Store module list temporarily
  ret <- e$modules

  # Filter by status
  if (!is.null(status)) {
    if (status != "all")
      ret <- ret[ret$Status == status, ]
  }

  # Filter by version
  if (!is.null(version)) {
    if (version == "latest") {
      ret <- ret[ret$LastVersion == TRUE, ]

    } else if (version != "all") {

      ret <- ret[ret$Version %in% version, ]
    }

  }

  # Filter by type
  if (!is.null(type)) {
    ret <- ret[ret$Type == type, ]
  }

  # Filter by TA
  if (!is.null(TA)) {

    pos <- grep(glob2rx(TA), ret$TA, ignore.case = TRUE)

    ret <- ret[pos, ]
  }

  # Filter by domain
  if (!is.null(domain)) {

    pos <- grep(glob2rx(domain), ret$Domain, ignore.case = TRUE)

    ret <- ret[pos, ]
  }

  # Filter by indication
  if (!is.null(indication)) {

    pos <- grep(glob2rx(indication), ret$Indication, ignore.case = TRUE)

    ret <- ret[pos, ]
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
    lst <- find_modules(name = name, version = "latest", status = "all")

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


#' @title Gets and sets the configuration file on the local machine
#' @description The \code{set_config} function opens the configuration
#' file on the local machine and sets it with any passed parameters.  If
#' no parameters are passed, the function simply returns the existing config.
#' If no config file exists, it will be created.
#' @param cache_directory The path of the global module cache.
#' @param local_directory The path of the local module development area.
#' @return The updated configuration.  Configuration is returned as a list.
#' @family cache
#' @export
#' @import rappdirs
#' @import yaml
set_config <- function(cache_directory = NULL, local_directory = NULL) {

  cnfg <- list()


  cpth <- file.path(user_config_dir("star", roaming = FALSE), "Config/config.yml")

  if (file.exists(cpth)) {

    yml <- yaml::read_yaml(cpth)

    cnfg[["cache_directory"]] <- yml$cache_directory
    cnfg[["local_directory"]] <- yml$local_directory
  }

  if (!is.null(cache_directory)) {
    cnfg[["cache_directory"]] <- cache_directory
  }

  if (!is.null(local_directory)) {
    cnfg[["local_directory"]] <- local_directory
  }

  yaml::write_yaml(cnfg, cpth, fileEncoding = "UTF-8")

  return(cnfg)
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

    if (is.null(module$status))
      ret[cnt, "Status"] <- ""
    else
      ret[cnt, "Status"] <- module$status

    if (is.null(module$type))
      ret[cnt, "Type"] <- ""
    else
      ret[cnt, "Type"] <- module$type

    if (is.null(module$TA))
      ret[cnt, "TA"] <- ""
    else
      ret[cnt, "TA"] <- module$TA

    if (is.null(module$domain))
      ret[cnt, "Domain"] <- ""
    else
      ret[cnt, "Domain"] <- module$domain

    if (is.null(module$indication))
      ret[cnt, "Indication"] <- ""
    else
      ret[cnt, "Indication"] <- module$indication

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

