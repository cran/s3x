\name{clone}
\alias{clone}
\alias{clone.list}
\alias{clone.ENVIRONMENT}
\alias{clone.environment}
\alias{clone.default}
\title{clone objects}
\description{These functions clone objects, copying environments. When cloning nested environments (environments within environments), duplicate and circular references are accounted for. However, when cloning lists that contain environments, or functions with environments, such references may not be accounted for. These functions require further testing, and should be regarded as unreliable.}
\usage{
clone (\dots)
\method{clone}{list}(object, \dots)
\method{clone}{ENVIRONMENT}(e, \dots)
\method{clone}{environment}(e, \dots)
\method{clone}{default}(x, \dots)
}
\arguments{
\item{x}{.}
\item{object}{.}
\item{e}{.}
\item{\dots}{.}
}





