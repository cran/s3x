\name{objref}
\alias{objref}
\alias{deref}
\alias{print.objref}
\alias{length.objref}
\alias{$.objref}
\alias{$<-.objref}
\alias{[.objref}
\alias{[<-.objref}
\alias{[[.objref}
\alias{[[<-.objref}
\title{object references}
\description{Syntactic sugar, designed for prototyping and for writing elegant top-level algorithms, at the cost of performance. Use an environment (or ENVIRONMENT), for better performance. See package vignette for more information.}
\usage{
objref (obj)
deref (ref)
\method{print}{objref}(ref, \dots)
\method{length}{objref}(x)
\method{$}{objref}(ref, name)
\method{$}{objref}(ref, name) <- value
\method{[}{objref}(ref, \dots)
\method{[}{objref}(ref, \dots) <- value
\method{[[}{objref}(ref, \dots)
\method{[[}{objref}(ref, \dots) <- value
}
\arguments{
\item{ref}{.}
\item{x}{.}
\item{name}{.}
\item{obj}{.}
\item{value}{.}
\item{\dots}{.}
}

