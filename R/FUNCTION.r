FUNCTION = function (f, ..., xlim, ylim, zlim)
{	if (!is.function (f) )
		stop ("as.FUNCTION requires function")
	f = .modifyf (f)
	class (f) = "FUNCTION"
	attr (f, ".") = list ()
	pa = list ()
	if (!missing (xlim) ) pa$xlim = xlim
	if (!missing (ylim) ) pa$ylim = ylim
	if (!missing (zlim) ) pa$zlim = zlim
	attr (f, ".plotattr") = pa
	f
}

is.FUNCTION = function (f) inherits (f, "FUNCTION")

s3x_print.FUNCTION = function (f, ...)
{	cat (format (f), "\n")
	k = as.character (body (f) )
	cat ("{ ", k [3], "\n")
	n = length (k)
	if (n > 3)
		for (i in 4:n)
			cat ("  ", k [i], "\n")
	cat ("}\n")
	.printatt (f)
}

s3x_format.FUNCTION = function (f, ...)
{	k1 = "FUNCTION"
	k2 = substring (format (args (f) ) [1], 9)
	paste (k1, k2, sep="")
}

"$.FUNCTION" = function (f, name) attr (f, ".") [[name]]

"$<-.FUNCTION" = function (f, name, value)
{	attr (f, ".") [[name]] = value
	f
}

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

