\name{s3x_bridges}
\alias{print.FUNCTION}
\alias{format.FUNCTION}
\alias{plot.FUNCTION}
\alias{lines.FUNCTION}
\alias{points.FUNCTION}
\alias{print.LIST}
\alias{format.LIST}
\alias{plot.LIST}
\alias{lines.LIST}
\alias{points.LIST}
\alias{print.VECTOR}
\alias{as.data.frame.VECTOR}
\alias{print.objref}
\alias{as.data.frame.objref}
\title{s3x bridge functions}
\description{Bridge functions are intended to allow a standard s3 generic to call an s3x generic, for certain classes. The advantage is that s3x generics don't constrain argument names in an inappropriate way. The disadvantage is that method despatch is done twice.}
\usage{
\method{print}{FUNCTION}(x, \dots)
\method{format}{FUNCTION}(x, \dots)
\method{plot}{FUNCTION}(x, \dots)
\method{lines}{FUNCTION}(x, \dots)
\method{points}{FUNCTION}(x, \dots)
\method{print}{LIST}(x, \dots)
\method{format}{LIST}(x, \dots)
\method{plot}{LIST}(x, \dots)
\method{lines}{LIST}(x, \dots)
\method{points}{LIST}(x, \dots)
\method{print}{VECTOR}(x, \dots)
\method{as.data.frame}{VECTOR}(x, \dots)
\method{print}{objref}(x, \dots)
\method{as.data.frame}{objref}(x, \dots)
}
\arguments{
\item{x}{.}
\item{\dots}{.}
}


