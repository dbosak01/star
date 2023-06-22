
# Create Module -----------------------------------------------------------


#' @title Defines a new program module
#' @description A function to instantiate a new module object.  The object
#' only requires the "name" parameter.  All other parameters are optional.
#' @param name The name of the new module.
#' @param description A description of the module.
#' @param major_version An integer that identifies the major version number. If
#' no value is supplied, the major version will default to zero.
#' @param minor_version An integer that identifies the minor version number. If
#' no value is supplied, the minor version will default to zero.
#' @param type The type of module.  Valid values are "Table", "Listing", or
#' "Figure".
#' @param status The status of the module.  Valid values are "Development" or
#' "Release".
#' @param TA The therapeutic area to assign this module to.
#' @param indication The indication that the module will be used for.  Indication,
#' if supplied, is a text string.
#' @param domain The ADaM domains to be used for the module. The domain should
#' be supplied as a vector of ADaM domain names.
#' @param ADaMIG The version of ADaMIG used for this version of the module.
#' @param keywords A vector of keywords to use for module search functions.
#' @param dependancies A vector of packages on which this module is dependent.
#' @family module
#' @return The new module object.
#' @export
module <- function(name, description = "",
                   major_version = 0L,
                   minor_version = 0L, type = NULL,
                   status = "Development",
                   TA = NULL,
                   indication = NULL,
                   domain = NULL,
                   ADaMIG = NULL, keywords = c(),
                   dependancies = c()) {


  # Create new structure of class "module"
  ret <- structure(list(), class = c("module", "list"))

  ret$name <- name
  ret$description <- description
  ret$major_version <- major_version
  ret$minor_version <- minor_version
  ret$type <- type
  ret$status <- status
  ret$TA <- TA
  ret$indication <- indication
  ret$domain <- domain
  ret$ADaMIG <- ADaMIG
  ret$keywords <- keywords
  ret$dependancies <- dependancies
  ret$parameters <- list()
  ret$version <- paste0("v", major_version, ".", minor_version)
  ret$created_by <- Sys.info()[["user"]]
  ret$create_date <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%S")
  ret$local_path <- ""
  ret$remote_path <- ""

  return(ret)

}


#' @title Initialize a new module
#' @description Function will create folder and template files to
#' begin development of a new \strong{star} module.
#' @param name The name of the new module.
#' @param local_path The development directory for the new module. If the
#' directory does not exist, it will be created.
#' @param description A description of the module.
#' @param major_version An integer that identifies the major version number.
#' @param minor_version An integer that identifies the minor version number.
#' @param type The type of module.  Valid values are "Table", "Listing", or
#' "Figure".
#' @param status The status of the module.  Valid values are "Development" or
#' "Release".
#' @param TA The therapeutic area to assign this module to.
#' @param indication The indication that the module will be used for.  Indication,
#' if supplied, is a text string.
#' @param domain The ADaM domains to be used for the module. The domain should
#' be supplied as a vector of ADaM domain names.
#' @param ADaMIG The version of ADaMIG used for this version of the module.
#' @param keywords A vector of keywords to use for module search functions.
#' @param dependancies A vector of packages on which this module is dependent.
#' @param overwrite If the module directory already exists, the create_module()
#' function will generate an error by default.  Set force = TRUE to overwrite
#' the existing directory.
#' @return The newly created module.  By default, the parameter list will be
#' empty.  To add a parameter to the module, use the \code{\link{add_parameter}}
#' function.
#' @family module
#' @import common
#' @export
create_module <- function(name, local_path, description = "",
                          major_version = 0L,
                          minor_version = 0L, type = NULL,
                          status = "Development",
                          TA = NULL,
                          indication = NULL,
                          domain = NULL,
                          ADaMIG = NULL, keywords = c(),
                          dependancies = c(), overwrite = FALSE) {


  res <- TRUE

  # Get external data directory
  ret <- module(name, description = description, major_version = major_version,
                minor_version = minor_version, type = type, status = status,
                TA = TA, indication = indication, domain = domain, ADaMIG = ADaMIG,
                keywords = keywords, dependancies = dependancies)

  ret$local_path <- local_path
  lp <- file.path(local_path, ret$version)

  if (dir.exists(lp) & overwrite == FALSE) {

    stop("Module path '" %p% lp %p% "' already exists.  " %p%
    "If desired, set overwrite = TRUE.")
  }

  if (!dir.exists(local_path))
    res <- dir.create(local_path, recursive = TRUE)

  if (!res)
    stop("Location cannot be created.")

  if (!dir.exists(lp))
    dir.create(lp, recursive = TRUE)

  # Get package directory
  pkg <- system.file("extdata", package = "star")

  # Get template directory
  tmplt <- file.path(pkg, "templates/blank")

  # Retrieve file list from template directory
  lst <- file.find(tmplt, pattern = NULL, up = 0, down = 0)

  # Copy files to new location
  for (fl in lst) {

    file.copy(fl, file.path(lp, basename(fl)),
              overwrite = TRUE)
  }

  # Get folders in template directory
  drs <- dir.find(tmplt, pattern = NULL, up = -1, down = 1)

  # Create folders in new location
  for (dr in drs) {

    dir.create(file.path(lp, basename(dr)),
               showWarnings = FALSE, recursive = TRUE)
  }

  # Write the module yaml to the local path
  write_module(ret, lp)

  return(ret)

}



#' @title Run a module
#' @description The \code{run_module} function executes a module.  The function
#' will pass parameters via an environment, and execute the module code
#' in that environment.  The environment will then be returned from the function
#' so it can be examined by the calling program.
#' @param module  The module to run.
#' @param ... Parameters for the module.
#' @family module
#' @return The function will return the environment that the code runs in.
#' @export
run_module <- function(module, ...) {

  # Create new environment for module execution
  ret <- new.env()

  # Retrieve parameter definitions
  defs <- module$parameters

  # Assign default parameter values
  for (d in names(defs)) {
    ret[[d]] <- defs[[d]]$default
  }

  # Retrieve parameters
  parms <- list(...)

  # Assign parameter values to environment
  for (p in names(parms)) {
    ret[[p]] <- parms[[p]]
  }

  # Get module source file
  pth <- module$local_path

  if (is.null(pth)) {
    stop("Module local path not found.")
  }

  fl <- file.path(pth, module$version, "module.R")


  if (file.exists(fl)) {

  source(fl, local = ret)

  } else {

    stop("Source file '" %p% fl %p% "' not found.")
  }


  return(ret)

}


# In Progress -------------------------------------------------------------



#' @title Copy an existing module
#' @description Function will copy the passed module and return the copy.
#' The copy will be located in the specified path, or copy it to the
#' local path if not specified.
#' @param module The module to copy.  You can pass either a module object
#' or a module name.
#' @param name The name of the new module.
#' @param local_path The development directory for the new module. If the
#' directory does not exist, it will be created.
#' @family module
#' @import common
#' @return Returns the copied module.
#' @export
copy_module <- function(module, name, local_path = NULL) {


  ret <- NULL

  # Determine if actual module or name of module
  # Then take appropriate action.
  # return module object.

  ret <- module

  if (!dir.exists(local_path)) {
    dir.create(local_path, recursive = TRUE)

  }

  ret$name <- name
  ret$local_path <- local_path


  write_module(ret, local_path)


  return(ret)

}



#' @title Test a module
#' @param mod The program module to test.
#' @family module
#' @export
test_module <- function(mod = NULL) {

  pth <- mod
  if ("module" %in% class(mod)) {


  }



}

# Read and Write ----------------------------------------------------------


#' @title Reads a program module from the file system
#' @description To read a module from disk, use the \code{read_module} function.
#' The function will read the module into a object and return it.
#' @param location The location to read the module from.
#' @param version The version of the module to read.
#' @return A module located at the path provided.
#' @family module
#' @import yaml
#' @return Returns the module at the specified path.
#' @export
read_module <- function(location, version = NULL) {

  if (!dir.exists(location))
    stop(paste0("Location directory does not exist: ", location))

  # Look locally
  if (!is.null(version))
    pth <- file.path(location, version, "module.yml")
  else {

    pth <- file.path(location, "module.yml")

    # If path doesn't exist, look in version folder
    if (!file.exists(pth)) {

      dirs <- dir.find(location, pattern = "v*", up = 0, down = 1)

      if (length(dirs) > 0)
        pth <- file.path(dirs[[length(dirs)]], "module.yml")

    }
  }

  # Look remotely
  if (!file.exists(pth)) {
    if (!is.null(version))
      pth <- file.path(get_cache_directory(), location, version, "module.yml")
    else {

      pth <- file.path(get_cache_directory(), location, "module.yml")

      # If path doesn't exist, look in version folder
      if (!file.exists(pth)) {

        dirs <- dir.find(file.path(get_cache_directory(), location),
                         pattern = "v*", up = 0, down = 1)

        if (length(dirs) > 0)
          pth <- file.path(dirs[[length(dirs)]], "module.yml")

      }
    }
  }

  # Give up
  if (!file.exists(pth)) {

    stop("Module file not found in this location.")
  }

  # Read in yaml
  ret <- read_yaml(pth, fileEncoding = "UTF-8")

  # Assign class of module
  class(ret) <- c("module", "list")

  # Assign class of parameters
  if (!is.null(ret$parameters)) {
    if (length(ret$parameters) > 0) {

      for (i in seq_len(length(ret$parameters))) {
        class(ret$parameters[[i]]) <- c("parameter", "list")

      }
    }
  }


  return(ret)

}


#' @title Writes a program module to the file system
#' @description The \code{write_module} function saves a module to disk.
#' The function accepts a module object and a location to write it to.  The module
#' will be saved in a file called "module.R".  If the location parameter is not
#' passed, the module will be saved in the local path.
#' @param mod The module to write.
#' @param location The file system location to write the module to.  By default,
#' the module will be saved in the local path, in a version sub-folder.
#' @family module
#' @import yaml
#' @return The path of the written module.
#' @export
write_module <- function(mod, location = NULL) {

  # If location is not specified, write to local path
  if (is.null(location)) {

    location <- file.path(mod$local_path, mod$version)

  }

  if (!dir.exists(location))
    stop(paste0("Location directory '", location, "' does not exist."))

  # Get yaml path
  pth <- file.path(location, "module.yml")

  # Write module yaml
  write_yaml(mod, pth, fileEncoding = "UTF-8")


  return(pth)
}



# Add Functions -----------------------------------------------------------



#' @title Adds a parameter to a program module
#' @description The \code{add_parameter} function adds a parameter to a module.
#' Each module is initalized with a parameter list.  This function adds a
#' parameter to the list.  The arguments of the function define the new
#' parameter.
#' @param module The module to add a parameter to.
#' @param name The name to associate with the module.  This name should
#' be unique in the module cache.
#' @param default The default value to use for the parameter.
#' @param data_type The data type of the parameter value.  Should be specified
#' as an R data type class name.
#' @param input_type The type of input control to use for this parameter.
#' @param label The label for this parameter.
#' @param description A brief description for this parameter.
#' @param options A vector of options to use for parameters that accept multiple
#' options values.
#' @return The input module, with new parameter added.
#' @family module
#' @return The modified module with new parameter added to the module
#' parameter list.
#' @export
add_parameter <- function(module, name, default = NULL, data_type = NULL,
                          input_type = NULL, label = NULL, description = NULL,
                          options = NULL) {

  # Create new structure of class "parameter"
  ret <- structure(list(), class = c("parameter", "list"))

  # Assign parameters
  ret$name <- name
  ret$default <- default
  ret$data_type <- data_type
  ret$input_type <- input_type
  ret$label <- label
  ret$description <- description
  ret$options <- options

  # Append parameter to the module parameter list
  module$parameters[[name]] <- ret

  return(module)

}



# Print -------------------------------------------------------------------

#' @title Prints information about module
#' @description A function to print a module.  The print out
#' will include most attributes of the module, organized in a readable fashion.
#' The module parameters will also be included in the print out.
#' @param x The object to print.
#' @param ... Follow on parameters to the print function.
#' @param verbose Whether to print the module in verbose mode.  Valid values
#' are TRUE and FALSE.
#' @return The module, invisibly.
#' @family module
#' @import crayon
#' @export
print.module <- function(x, ..., verbose = FALSE) {


  if (!any(class(x) == "module"))
    stop("Class must be of type 'module'.")

  if (verbose == TRUE) {
    print(unclass(x))
  } else {

    grey60 <- make_style(grey60 = "#999999")
    cat(grey60("# A program module\n"))

    if (!is.null(x$name))
      cat("- Name:", x$name, "\n")


    if (!is.null(x$description))
      cat("- Description:", x$description, "\n")


    if (!is.null(x$major_version) & !is.null(x$minor_version))
      cat("- Version:",  x$version, "\n")

    if (!is.null(x$type))
      cat("- Type:", x$type, "\n")

    if (!is.null(x$status))
      cat("- Status:", x$status, "\n")

    if (!is.null(x$TA))
      cat("- Theraputic Area:", x$TA, "\n")

    if (!is.null(x$indication))
      cat("- Indication:", x$indication, "\n")

    if (!is.null(x$domain))
      cat("- Domain:", x$domain, "\n")

    if (!is.null(x$ADaMIG))
      cat("- ADaMIG:", x$ADaMIG, "\n")

    if (!is.null(x$keywords))
      cat("- Keywords:", paste(x$keywords, collapse = ", "), "\n")

    if (!is.null(x$dependancies))
      cat("- Dependancies:", paste(x$dependancies, collapse = ", "), "\n")

    if (!is.null(x$local_path))
      cat("- Local Path:", x$local_path, "\n")

    if (!is.null(x$remote_path))
      cat("- Remote Path:", x$remote_path, "\n")

    if (!is.null(x$created_by))
      cat("- Created:", x$created_by, x$create_date, "\n")

    if (!is.null(x$parameters)) {
      if (length(x$parameters) > 0) {
        cat("- Parameters:", length(x$parameters), "\n")
        for (prm in x$parameters) {

          print(prm)

        }
      }
    }
  }

  invisible(x)

}



#' @title Prints information about parameter
#' @description A function to print a module parameter.  The print out
#' will include most attributes of the parameter, organized in a readable fashion.
#' @param x The parameter to print.
#' @param ... Follow on parameters to the print function.
#' @param verbose Whether to print the module in verbose mode.  Valid values
#' are TRUE and FALSE.
#' @return The parameter, invisibly.
#' @import crayon
#' @family module
#' @export
print.parameter <- function(x, ..., verbose = FALSE) {

  if (!any(class(x) == "parameter"))
    stop("Class must be of type 'parameter'.")

  if (verbose == TRUE) {
    print(unclass(x))
  } else {

    grey60 <- make_style(grey60 = "#999999")
    cat(grey60("# A module parameter\n"))

    if (!is.null(x$name))
      cat("- Name:", x$name, "\n")


    if (!is.null(x$description))
      cat("- Description:", x$description, "\n")

    if (!is.null(x$default))
      cat("- Default Value:", x$default, "\n")

    if (!is.null(x$data_type))
      cat("- Data Type:", x$data_type, "\n")

    if (!is.null(x$input_type))
      cat("- Input Type:", x$input_type, "\n")

    if (!is.null(x$label))
      cat("- Label:", x$label, "\n")
  }

  invisible(x)
}
