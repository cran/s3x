\name{objref}
\alias{objref}
\alias{is.objref}
\alias{length.objref}
\alias{s3x_print.objref}
\alias{s3x_as.data.frame.objref}
\alias{$.objref}
\alias{$<-.objref}
\alias{[.objref}
\alias{[<-.objref}
\alias{[[.objref}
\alias{[[<-.objref}
\alias{deref}
\title{object references}
\description{Environment-based objects with reference-like syntax. Note that standard environments should be used, if performance is important.}
\usage{
objref (object)
is.objref (ref)
\method{s3x_print}{objref}(ref, \dots)
\method{s3x_as.data.frame}{objref}(ref, \dots)
\method{length}{objref}(x)
\method{$}{objref}(ref, name)
\method{$}{objref}(ref, name) <- value
\method{[}{objref}(ref, \dots)
\method{[}{objref}(ref, \dots) <- value
\method{[[}{objref}(ref, \dots)
\method{[[}{objref}(ref, \dots) <- value
deref (ref)
}
\arguments{
\item{ref}{.}
\item{x}{.}
\item{name}{.}
\item{object}{.}
\item{value}{.}
\item{\dots}{.}
}


