% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch.R
\name{program_parms}
\alias{program_parms}
\title{Create program parameters}
\usage{
program_parms(program, version, parms)
}
\arguments{
\item{program}{The name of the program you wish to run.}

\item{version}{The version of the program you wish to run.}

\item{parms}{A list of parameters to send to the program.}
}
\value{
The new program parameter object.
}
\description{
The \code{program_parms} function defines
a program and set of parameters for that program.  This information
is returned as an object, which can then be sent to the
\code{\link{create_batch}} function.  The \code{\link{create_batch}}
function is used to download programs into a study and generate
a "run_all" batch program.
}
\seealso{
Other batch: 
\code{\link{create_batch}()},
\code{\link{run_program}()}
}
\concept{batch}
