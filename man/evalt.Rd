\name{evalt}
\alias{evalt}
\title{evaluate text}
\description{A convenience function for evaluating text. Note that this function behaves differently, depending on the value of return. If return is true (the default), the text argument must be scalar and by default a new environment is used. If return is false, the text argument can be vectorised, by default the environment where evalt is called is used (hence evalt can modify it's environment), and each line is evaluated sequentially.}
\usage{
evalt (text, return=TRUE, e)
}
\arguments{
\item{text}{A string to evaluate.}
\item{return}{Whether or not to return the evaluated text.}
\item{e}{Environment to evaluate the text in.}
}




