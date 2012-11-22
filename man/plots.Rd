\name{plots}
\alias{plot.FUNCTION}
\alias{lines.FUNCTION}
\alias{points.FUNCTION}
\title{plot functions}
\description{Plot function objects. Note that that the plot (no f version) merely calls the plotf (with f version) for FUNCTION objects.}
\usage{
\method{plot}{FUNCTION}(f, \dots,
	xlim=f$xlim, ylim=f$ylim, xlab="x", ylab="f(x)", n=200)
\method{lines}{FUNCTION}(f, \dots,
	xlim=f$xlim, n=200)
\method{points}{FUNCTION}(f, \dots,
	xlim=f$xlim, n=30)
}
\arguments{
\item{f}{.}
\item{xlim}{.}
\item{ylim}{.}
\item{xlab}{.}
\item{ylab}{.}
\item{n}{.}
\item{\dots}{.}
}








