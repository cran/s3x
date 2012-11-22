\name{io}
\alias{read_file_data}
\alias{read_package_data}
\alias{write_file_data}
\title{map file data to data.frame}
\description{Convenience functions for reading data in an installed package (doesn't have to be loaded) or a file and creating (returning) a data.frame. By default, the data is assumed to be in csv format. Note, contrary to the data.frame and read.table functions, by default, this function doesn't enforce syntactically valid column names.}
\usage{
read_file_data (path,
    sep=",", header=TRUE, check.names=FALSE, \dots)
read_package_data (package, file,
    sep=",", header=TRUE, check.names=FALSE, \dots)
write_file_data (path, table, \dots,
	sep=",", header=TRUE, quote=FALSE)
}
\arguments{
\item{path}{The file path.}
\item{package}{The package name.}
\item{file}{The file name, with extension.}
\item{header}{Assume a header line exists.}
\item{check.names}{Enforce valid column names.}
\item{sep}{Separator.}
\item{table}{.}
\item{quote}{.}
\item{\dots}{Further arguments for read.table.}
}



