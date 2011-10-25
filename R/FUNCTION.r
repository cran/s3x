FUNCTION = function (f, ..., xlim=NULL, ylim=NULL, zlim=NULL)
{	if (!is.function (f) )
		stop ("FUNCTION requires function")
	f = .modifyf (f)
	attributes (f) = NULL
	class (f) = "FUNCTION"
	attr (f, ".") = list ()
	f$xlim = xlim
	f$ylim = ylim
	f$zlim = zlim
	f
}

is_FUNCTION = function (object)
	inherits (object, "FUNCTION")

print.FUNCTION = function (f, ...)
{	cat (format (f), "\n")
	k = as.character (body (f) )
	cat ("{ ", k [3], "\n")
	n = length (k)
	if (n > 3)
		for (i in 4:n)
			cat ("  ", k [i], "\n")
	cat ("}\n")
	.printattr (f)
}

plot.FUNCTION = function (f, ..., xlim=f$xlim, ylim=f$ylim, xlab="x", ylab="f(x)", n=200)
{	if (is.null (xlim) )
		stop ("plot.FUNCTION requires xlim")
	x = seq (f, n=n, xlim=xlim)
	y = f (x)
	if (is.null (ylim) )
		ylim = range (y, na.rm = TRUE)
	plot (x, y, type="l", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, ...)
}

lines.FUNCTION = function (f, ..., xlim=f$xlim, n=200)
{	if (is.null (xlim) )
		stop ("lines.FUNCTION requires xlim")
	x = seq (f, n=n, xlim=xlim)
	y = f (x)
	lines (x, y, ...)
}

format.FUNCTION = function (f, ...)
{	k1 = "FUNCTION"
	k2 = substring (format (args (f) ) [1], 9)
	paste (k1, k2, sep="")
}

seq.FUNCTION = function (f, ..., xlim=f$xlim, n=30)
	seq (xlim [1], xlim [2], length=n)

.modifyf = function (f)
{	g = list ()
	g [[1]] = as.name ("{")
	cmd = '. = attr (sys.function (), ".")'
	g [[2]] = parse (text=cmd) [[1]]
	b = body (f)
	if (class (b) == "{")
	{	b = as.list (b) [-1]
		for (i in 1:length (b) ) g [[i + 2]] = b [[i]]
	}
	else g [[3]] = b
	body (f) = as.call (g)
	f
}

