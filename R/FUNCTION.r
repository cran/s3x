FUNCTION = function (f, ..., xlim=NULL, ylim=NULL)
{	if (! is.function (f) )
		stop ("FUNCTION requires function")
	f = .modifyf (f)
	attributes (f) = NULL
	class (f) = "FUNCTION"
	attr (f, ".") = list ()
	f$xlim = xlim
	f$ylim = ylim
	f
}

is_FUNCTION = function (object)
	inherits (object, "FUNCTION")

#error
print.FUNCTION = function (f, ...)
{	cat (format (f), "\n")
	k = as.character (body (as.function (f) ) )
	cat ("{ ", k [3], "\n")
	n = length (k)
	if (n > 3)
		for (i in 4:n)
			cat ("  ", k [i], "\n")
	cat ("}\n")
	.printattr (f)
}

format.FUNCTION = function (f, ...)
{	k1 = "FUNCTION"
	k2 = substring (format (args (f) ) [1], 9)
	paste (k1, k2, sep="")
}

seq.FUNCTION = function (f, ..., xlim=f$xlim, n=30)
	seq (xlim [1], xlim [2], length=n)

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

.printattr = function (object)
{	k = names (attr (object, ".") )
	if (length (k) > 0)
	{	cat ("object attributes:\n")
		cat (paste (k, collapse=", "), "\n")
	}
}





