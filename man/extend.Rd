\name{extend}
\alias{extend}
\title{extend s3 class attributes, convenience function}
\description{The extend function, extends an object. For objects with an explicit class attribute (e.g. data.frame(s)), extend concatenates the class attribute with the subclass. For objects without an explicit class attribute (e.g. numeric(s)), extend sets the class attribute to the subclass. Note that this function doesn't modify the original object, rather it returns a new object. Note that the subclass argument maybe vectorised, however, the order's reversed.}
\usage{
extend (object, subclass)
}
\arguments{
\item{object}{An object to extend.}
\item{subclass}{A character giving the subclass name.}
}





