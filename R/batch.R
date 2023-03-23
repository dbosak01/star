
#' @title Create program parameters
#' @description The \code{program_parms} function defines
#' a program and set of parameters for that program.  This information
#' is returned as an object, which can then be sent to the
#' \code{\link{create_batch}} function.  The \code{\link{create_batch}}
#' function is used to download programs into a study and generate
#' a "run_all" batch program.
#' @param program The name of the program you wish to run.
#' @param version The version of the program you wish to run.
#' @param parms A list of parameters to send to the program.
#' @family batch
#' @return The new program parameter object.
#' @export
program_parms <- function(program, version, parms) {


  ret <- structure(list(), class = c("program_parms", "list"))


  ret$program <- program
  ret$version <- version
  ret$parms <- parms


  return(ret)
}


#' @title Create program batch
#' @description This function will copy a list of programs to a location and
#' create a batch file to run them.  The function accepts a list of program
#' parameter objects and a directory to copy the programs.
#' @param lst A list of programs to create the batch for.  The parameter value
#' should be a list of program_parms objects.
#' @param location The location is a full or relative
#' path to a directory to copy the indicated programs.  In most cases, this
#' location will be the study program directory.
#' @param remote The location of the remote module cache.
#' @return The path to the "run_all" file.
#' @family batch
#' @export
create_batch <- function(lst, location, remote = NULL) {


  if (!dir.exists(location)) {
    stop("Location does not exist.")

  }

  if (length(lst) == 0) {

   stop("Length of program list cannot be zero.")
  }

  if (is.null(remote))
    rpth <- get_cache_directory()
  else
    rpth <- remote

  fp <- file.path(location, "run_all.R")

  if (file.exists(fp))
    file.remove(fp)

  # Open run_all program
  f <- file(fp, open="a", encoding = "native.enc")

  # Start with blank line
  writeLines("", con = f, useBytes = TRUE)

  cnt <- 0

  for (pp in lst) {

    cnt <- cnt + 1

    mod <- read_module(file.path(rpth, pp$program, pp$version))
    if (!is.null(mod)) {

      spth <- file.path(rpth, pp$program, pp$version, "module.R")
      tpth <- file.path(location, paste0(mod$name, ".R"))
      file.copy(spth, tpth)


      if (length(mod$parameters) > 0)
        txt <- paste0("res", cnt, ' <- run_program("', tpth, '", ')
      else
        txt <- paste0("res", cnt, ' <- run_program("', tpth, '"')

      writeLines(txt, con = f, useBytes = TRUE)


      # Write parms
      for (i in seq_len(length(mod$parameters))) {

        prm <- mod$parameter[[i]]

        if (!is.null(pp$parms[[prm$name]])) {
          ptxt <- paste0("  ", get_parm(prm$name, pp$parms[[prm$name]], prm$data_type))

        } else {
          ptxt <- paste0("  ", get_parm(prm$name,  prm$default, prm$data_type))

        }

        if (i == length(mod$parameters))
          writeLines(ptxt, con = f, useBytes = TRUE)
        else
          writeLines(paste0(ptxt, ","), con = f, useBytes = TRUE)

      }


      writeLines(")\n", con = f, useBytes = TRUE)

    }

  }

  close(f)


  return(fp)

}

#' @noRd
get_parm <- function(name, value, type) {


  ret <- ""

  if (type == "character") {

    if (length(value) > 1) {
      ret <- paste0(name, "=c(", paste("'", value, "'", sep = "", collapse = ", "), ")")

    } else {

      ret <- paste0(name, "='", value, "'")
    }


  } else {

    if (length(value) > 1) {

      ret <- paste0(name, "=c(", paste(value, sep = "", collapse = ", "), ")")
    } else {

      ret <- paste0(name, "=", value)
    }

  }

  return(ret)

}



#' @title Run a program
#' @description The \code{run_program} function executes a program.  The function
#' will pass parameters via an environment, and execute the program code
#' in that environment.  The environment will then be returned from the function
#' so it can be examined by the calling program.
#' @param program  The full or relative path of the program to run.
#' @param ... Parameters for the module.
#' @family batch
#' @return The function will return the environment that the code runs in.
#' @export
run_program <- function(program, ...) {

  # Create new environment for module execution
  ret <- new.env()

  # # Retrieve parameter definitions
  # defs <- module$parameters
  #
  # # Assign default parameter values
  # for (d in names(defs)) {
  #   ret[[d]] <- defs[[d]]$default
  # }

  # Retrieve parameters
  parms <- list(...)

  # Assign parameter values to environment
  for (p in names(parms)) {
    ret[[p]] <- parms[[p]]
  }


  fl <- program


  if (file.exists(fl)) {

    source(fl, local = ret)

  } else {

    stop("Source file '" %p% fl %p% "' not found.")
  }


  return(ret)

}
