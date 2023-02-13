
# Create Module -----------------------------------------------------------


#' @title Defines a new program module
#' @param name The name of the new module.
#' @param description A description of the module.
#' @param major_version An integer that identifies the major version number.
#' @param minor_version An integer that identifies the minor version number.
#' @param active Whether the module should be activated or not.
#' @param template Whether the module should be marked as a template.
#' @param level The development level for this module.  Valid values are
#' "dev", "test", and "prod".
#' @param keywords A vector of keywords to use for module search functions.
#' @param dependancies A vector of packages on which this module is dependent.
#' @export
module <- function(name, description = "",
                   major_version = 0L,
                   minor_version = 0L,
                   active = TRUE, template = FALSE, level = "dev", keywords = c(),
                   dependancies = c()) {


  # Create new structure of class "module"
  ret <- structure(list(), class = c("module", "list"))

  ret$name <- name
  ret$description <- description
  ret$major_version <- major_version
  ret$minor_version <- minor_version
  ret$active <- active
  ret$template <- template
  ret$level <- level
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
#' @param active Whether the module should be activated or not.
#' @param template Whether the module should be marked as a template.
#' @param level The development level for this module.  Valid values are
#' "dev", "test", and "prod".
#' @param keywords A vector of keywords to use for module search functions.
#' @param dependancies A vector of packages on which this module is dependent.
#' @param overwrite If the module directory already exists, the create_module()
#' function will generate an error by default.  Set force = TRUE to overwrite
#' the existing directory.
#' @import common
#' @export
create_module <- function(name, local_path, description = "",
                          major_version = 0L,
                          minor_version = 0L,
                          active = TRUE, template = FALSE, level = "dev", keywords = c(),
                          dependancies = c(), overwrite = FALSE) {


  res <- TRUE

  # Get external data directory
  ret <- module(name, description = description, major_version = major_version,
                minor_version = minor_version, active = active, template = template,
                level = level, keywords = keywords, dependancies = dependancies)

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

  pkg <- system.file("extdata", package = "star")

  tmplt <- file.path(pkg, "templates/blank")

  lst <- file.find(tmplt, pattern = NULL, up = 0, down = 0)

  for (fl in lst) {

    file.copy(fl, file.path(lp, basename(fl)),
              overwrite = TRUE)
  }
  #browser()
  drs <- dir.find(tmplt, pattern = NULL, up = -1, down = 1)
  # print(tmplt)
  # print(drs)

  for (dr in drs) {

    dir.create(file.path(lp, basename(dr)),
               showWarnings = FALSE, recursive = TRUE)
  }


  write_module(ret, lp)

  return(ret)

}



#' @title Copy an existing module
#' @description Function will create folder and template files to
#' begin development of a new \strong{star} module.
#' @param module The module to copy.  You can pass either a module object
#' or a module name.
#' @param name The name of the new module.
#' @param local_path The development directory for the new module. If the
#' directory does not exist, it will be created.
#' @import common
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
#' @export
test_module <- function(mod = NULL) {

  pth <- mod
  if ("module" %in% class(mod)) {


  }



}

#' @title Run a module
#' @param module  The module to run.
#' @param ... Parameters for the module.
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


# Read and Write ----------------------------------------------------------


#' @title Reads a program module from the file system
#' @param location The location to read the module from.
#' @return A module located at the path provided.
#' @import yaml
#' @export
read_module <- function(location) {

  if (!dir.exists(location))
    stop("Location directory does not exist")

  pth <- file.path(location, "module.yml")

  # If path doesn't exist, look in version folder
  if (!file.exists(pth)) {

    dirs <- dir.find(location, pattern = "v*", up = 0, down = 1)

    if (length(dirs) > 0)
      pth <- file.path(dirs[[length(dirs)]], "module.yml")
  }

  if (!file.exists(pth)) {

    stop("Module file not found in this location.")
  }

  ret <- read_yaml(pth, fileEncoding = "UTF-8")

  class(ret) <- c("module", "list")


  return(ret)

}


#' @title Writes a program module to the file system
#' @param mod The module to write.
#' @param location The file system location to write the module to.
#' @import yaml
#' @export
write_module <- function(mod, location = NULL) {

  if (is.null(location)) {

    location <- file.path(mod$local_path, mod$version)

  }


  if (!dir.exists(location))
    stop(paste0("Location directory '", location, "' does not exist."))

  pth <- file.path(location, "module.yml")

  write_yaml(mod, pth, fileEncoding = "UTF-8")


  return(pth)
}



# Add Functions -----------------------------------------------------------



#' @title Adds a parameter to a program module
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
#' @return The modified module.
#' @export
add_parameter <- function(module, name, default = NULL, data_type = NULL,
                          input_type = NULL, label = NULL, description = NULL,
                          options = NULL) {

  # Create new structure of class "parameter"
  ret <- structure(list(), class = c("parameter", "list"))


  ret$name <- name
  ret$default <- default
  ret$data_type <- data_type
  ret$input_type <- input_type
  ret$label <- label
  ret$description <- description
  ret$options <- options


  module$parameters[[name]] <- ret

  return(module)

}



# Print -------------------------------------------------------------------

#' @title Prints information about module
#' @param x The object to print.
#' @param ... Follow on parameters to the print function.
#' @param verbose Whether to print the module in verbose mode.  Valid values
#' are TRUE and FALSE.
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

    if (!is.null(x$active))
      cat("- Active:", x$active, "\n")

    if (!is.null(x$template))
      cat("- Template:", x$template, "\n")

    if (!is.null(x$level))
      cat("- Level:", x$level, "\n")

    if (!is.null(x$keywords))
      cat("- Keywords:", paste(x$keywords, collapse = ", "), "\n")

    if (!is.null(x$dependancies))
      cat("- Dependancies:", paste(x$dependancies, collapse = ", "), "\n")

    if (!is.null(x$created_by))
      cat("- Created:", x$created_by, x$create_date, "\n")

  }

  invisible(x)

}
