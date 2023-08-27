


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


#' @title Gets and sets global information
#' @description A function to instantiate a new global information object.  The
#' object contains global information shared among programs.
#' The function creates, edits, or returns the global information file.
#' @param path The name of the new globals file.  Should have a ".globals" extension.
#' Default is NULL, meaning the function will search for the nearest globals
#' file and return the object.
#' @param ... Other parameters dynamically defined for this information file.
#' @family module
#' @return The global information object.
#' @export
#' @import common
set_globals <- function(path = NULL, ...) {


  ret <- list(...)

  if (!is.null(path)) {
    gpth <- path
  } else {

    glst <- file.find(getwd(), pattern = "*.globals", up = 10, down = 1)

    if (length(glst) > 0)
      gpth <- glst[1]
    else
      gpth <- file.path(getwd(), "module.globals")

  }

  if (file.exists(gpth)) {

    rds <- readRDS(gpth)

    for (nm in names(ret)) {

      rds[[nm]] <- ret[[nm]]
    }

    ret <- rds

  }

  saveRDS(ret, gpth)

  return(ret)
}
