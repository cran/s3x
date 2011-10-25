\name{lps}
\alias{lps}
\alias{dlps}
\alias{lps_series}
\alias{lps_interpolate}
\title{local polynomial based smoothing}
\description{Prototype functions for producing a smooth series (with coordinate vectors u an v) from a noisy series (with coordinate vectors x and y). Smooth series estimates are based on local polynomials, fitted using locally weighted linear least squares. The lps and dlps functions are the more general form, differing in that lps returns a vector of estimates and that dlps returns a two column matrix, with the first column giving the estimated values and the second column giving the estimated derivatives. Both these function require the u values to be specified first. The lps_series and lps_interpolate functions are more user friendly versions, which don't require u and have an optional n argument for the length of the smooth series. The lps_series function, returns a two column matrix with u and v. The lps_interpolate function, returns a function. By default quadratics are used.}
\usage{
lps (x, y, u, degree=2, smoothness=1, ..., m, wc, wf, bw, z)
dlps (x, y, u, degree=2, smoothness=1, ..., m, wc, wf, bw, z)
lps_series (x, y, degree=2, smoothness=1, n=30)
lps_interpolate (x, y, degree=2, smoothness=1, n=30)
}
\arguments{
\item{x}{Vector of x values.}
\item{y}{Vector of y values.}
\item{u}{Vector of new x values, for the smooth series.}
\item{degree}{Local polynomial degree.}
\item{smoothness}{Smoothing parameter.}
\item{m}{Matrix used for least squares, with length (x) rows and degree + 1 columns.}
\item{wc}{String (coerced to an expression) used to compute a vector of weights for each smooth series estimate. Defaults to "wf (bw, x - u [i])". Possible tokens include wf, bw, u, x, z and i, where i is the ith point.}
\item{wf}{.}
\item{bw}{Bandwidth, defaults to smoothness * diff (range (x) ).}
\item{z}{By default, ignored.}
\item{n}{Length of the smooth series.}
\item{\dots}{Ignored.}
}



