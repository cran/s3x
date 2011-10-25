\name{miscellaneous}
\alias{is_obj}
\alias{is_object}
\alias{is_s3x_object}
\alias{evalt}
\alias{enumv}
\alias{ifo}
\title{miscellaneous functions}
\description{The functions is_obj and is_object, both test for non-null values. The function is_s3x_object, tests for enhanced primitives. The function evalt, evaluates a character vector. The function enumv, returns an integer vector. If n>=1, the vector is 1:n, otherwise the vector is empty. The function ifo, is the same as is_obj, however is deprecated.}
\usage{
is_obj (object)
is_object (object)
is_s3x_object (object)
evalt (text, e=parent.frame () )
enumv (n)
ifo (object)
}
\arguments{
\item{object}{An object or null.}
\item{text}{A character vector}
\item{e}{The environment to evaluate the text in. By default, the environment where evalt was called.}
\item{n}{An integer, giving the intended number of iterations.}
}


