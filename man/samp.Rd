\name{samp}
\alias{samp}
\alias{samp.matrix}
\alias{samp.data.frame}
\alias{samp.objref}
\alias{samp.default}
\title{nonrandom sampling}
\description{Nonrandom sampling, intended to show the first n and last m, elements (of a vector) or rows (of a table-based object). If the total number of elements or rows, exceeds n + m, then the entire object is returned.}
\usage{
samp (x, n=3, m=n)
\method{samp}{matrix}(x, n=3, m=n)
\method{samp}{data.frame}(x, n=3, m=n)
\method{samp}{objref}(x, n=3, m=n)
\method{samp}{default}(x, n=3, m=n)
}
\arguments{
\item{x}{An object.}
\item{n}{The first n elements or rows.}
\item{m}{The last m elements or rows.}
}




