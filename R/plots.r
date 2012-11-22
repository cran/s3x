plot.FUNCTION = function (f, ..., xlim=f$xlim, ylim=f$ylim, xlab="x", ylab="f(x)", n=200)
{	if (is.null (xlim) )
		stop ("plotf.FUNCTION requires xlim")
	x = seq (f, xlim=xlim, n=n)
	y = f (x)
	if (is.null (ylim) )
		ylim = range (y, na.rm = TRUE)
	plot (x, y, type="l", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, ...)
}

lines.FUNCTION = function (f, ..., xlim=f$xlim, n=200)
{	if (is.null (xlim) )
		stop ("linesf.FUNCTION requires xlim")
	x = seq (f, xlim=xlim, n=n)
	y = f (x)
	lines (x, y, ...)
}

points.FUNCTION = function (f, ..., xlim=f$xlim, n=30)
{	if (is.null (xlim) )
		stop ("pointsf.FUNCTION requires xlim")
	x = seq (f, xlim=xlim, n=n)
	y = f (x)
	points (x, y, ...)
}





