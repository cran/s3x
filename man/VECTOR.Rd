\name{VECTOR}
\alias{INTEGER}
\alias{LOGICAL}
\alias{TEXT}
\alias{REAL}
\alias{COMPLEX}
\alias{INTEGERv}
\alias{LOGICALv}
\alias{TEXTv}
\alias{REALv}
\alias{COMPLEXv}
\alias{is.VECTOR}
\alias{is.TEXT}
\alias{is.REAL}
\alias{is.COMPLEX}
\alias{is.INTEGER}
\alias{is.LOGICAL}
\alias{print.VECTOR}
\alias{$.VECTOR}
\alias{$<-.VECTOR}
\alias{[.VECTOR}
\title{enhanced vectors}
\description{Enhanced vectors, are vectors with simplified constructors (object creation and attribute assignment can be performed in a single call) and an alternative treatment of attributes.}
\usage{
INTEGER (n, \dots, call=sys.call () [-2])
LOGICAL (n, \dots, call=sys.call () [-2])
TEXT (n, \dots, call=sys.call () [-2])
REAL (n, \dots, call=sys.call () [-2])
COMPLEX (n, \dots, call=sys.call () [-2])
INTEGERv (x, \dots, call=sys.call () [-2])
LOGICALv (x, \dots, call=sys.call () [-2])
TEXTv (x, \dots, call=sys.call () [-2])
REALv (x, \dots, call=sys.call () [-2])
COMPLEXv (x, \dots, call=sys.call () [-2])
is.VECTOR (x)
is.INTEGER (x)
is.LOGICAL (x)
is.TEXT (x)
is.REAL (x)
is.COMPLEX (x)
\method{print}{VECTOR}(x, \dots)
\method{$}{VECTOR}(x, name)
\method{$}{VECTOR}(x, name) <- value
\method{[}{VECTOR}(x, \dots)
}
\arguments{
\item{n}{Vector length.}
\item{x}{A vector, for constructors, should match. e.g. The TEXT constructor requires a character.}
\item{\dots}{For constructors, the attributes to assign to the vector.}
\item{call}{Call that contains the new attributes (along with their names). If using the default call setting, then the call to the constructor can't contain "...".}
\item{name}{Name of the attribute.}
\item{value}{Value of the attribute.}
}






