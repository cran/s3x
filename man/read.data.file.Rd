\name{read.data.file}
\alias{read.data.file}
\alias{read.package.data}
\title{map file data to data.frame}
\description{Convenience functions for reading data in an installed package (doesn't have to be loaded) or a file and creating (returning) a data.frame. By default, the data is assumed to be in csv format. Note, contrary to the data.frame and read.table functions, by default, this function doesn't enforce syntactically valid column names.}
\usage{
read.data.file (path,
    header=TRUE, check.names=FALSE, sep=",", \dots)
read.package.data (package, file,
    header=TRUE, check.names=FALSE, sep=",", \dots)
}
\arguments{
\item{path}{The file path.}
\item{package}{The package name.}
\item{file}{The file name, with extension.}
\item{header}{Assume a header line exists.}
\item{check.names}{Enforce valid column names.}
\item{sep}{Separator.}
\item{\dots}{Further arguments for read.table.}
}

