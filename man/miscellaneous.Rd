\name{miscellaneous}
\alias{evalt}
\alias{ifo}
\alias{enumv}
\title{miscellaneous functions}
\description{Convenience functions, including evalt (evaluate text), ifo (if object) and enumv (enumeration vector). The evalt function evaluates text. The ifo function, tests for a non-null value. The enumv function is for iterating 0 or n times. The function produces an integer vector. If n<m, then the vector is empty. If n>=m, then the vector is 1:n.}
\usage{
evalt (text, e=parent.frame () )
ifo (object)
enumv (n, m=1)
}
\arguments{
\item{text}{A character vector}
\item{e}{The environment to evaluate the text in. By default, the environment where evalt was called.}
\item{object}{An object or null.}
\item{n}{An integer, giving the intended number of iterations.}
\item{m}{An integer, giving the minimum number of iterations.}
}


