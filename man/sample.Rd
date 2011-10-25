\name{sample}
\alias{sample}
\alias{sample.default}
\alias{sample_deterministic}
\alias{sample_random}
\alias{samp}
\title{generic sampling}
\description{Sample is redefined as a generic. In general, the sample_deterministic function returns the first n and last m rows (or elements, if dim (x) is null). If the total number of rows (or elements) is less than n + m, then the entire object is returned. Both the sample.default and sample_random call the standard sample function. The samp function is shorthand for calling sample_deterministic.}
\usage{
sample (\dots)
\method{sample}{default}(\dots)
sample_deterministic (x, n=3, m=n)
sample_random (\dots)
samp (\dots)
}
\arguments{
\item{x}{An object.}
\item{n}{The first n elements or rows.}
\item{m}{The last m elements or rows.}
\item{\dots}{.}
}








