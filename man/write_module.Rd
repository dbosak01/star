% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module.R
\name{write_module}
\alias{write_module}
\title{Writes a program module to the file system}
\usage{
write_module(mod, location = NULL)
}
\arguments{
\item{mod}{The module to write.}

\item{location}{The file system location to write the module to.  By default,
the module will be saved in the local path, in a version sub-folder.}
}
\value{
The path of the written module.
}
\description{
The \code{write_module} function saves a module to disk.
The function accepts a module object and a location to write it to.  The module
will be saved in a file called "module.R".  If the location parameter is not
passed, the module will be saved in the local path.
}
\seealso{
Other module: 
\code{\link{add_parameter}()},
\code{\link{copy_module}()},
\code{\link{create_module}()},
\code{\link{module}()},
\code{\link{print.module}()},
\code{\link{print.parameter}()},
\code{\link{read_module}()},
\code{\link{run_module}()},
\code{\link{set_globals}()},
\code{\link{test_module}()}
}
\concept{module}
