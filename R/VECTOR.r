#standard constructors
INTEGER = function (n=0, ..., call=sys.call () [-2]) INTEGERv (integer (n), ..., call=call)
LOGICAL = function (n=0, ..., call=sys.call () [-2]) LOGICALv (logical (n), ..., call=call)
REAL = function (n=0, ..., call=sys.call () [-2]) REALv (numeric (n), ..., call=call)
TEXT = function (n=0, ..., call=sys.call () [-2]) TEXTv (character (n), ..., call=call)
COMPLEX = function (n=0, ..., call=sys.call () [-2]) COMPLEXv (complex (n), ..., call=call)

#constructors, using seed vector
INTEGERv = function (x, ..., call=sys.call () [-2])
{	class (x) = .vc ("integer", "INTEGER", x)
	attr (x, ".") = list ()
	implant (x, attributes=LIST (..., call=call) )
}

LOGICALv = function (x, ..., call=sys.call () [-2])
{	class (x) = .vc ("logical", "LOGICAL", x)
	attr (x, ".") = list ()
	implant (x, attributes=LIST (..., call=call) )
}

REALv = function (x, ..., call=sys.call () [-2])
{	class (x) = .vc ("numeric", "REAL", x)
	attr (x, ".") = list ()
	implant (x, attributes=LIST (..., call=call) )
}

TEXTv = function (x, ..., call=sys.call () [-2])
{	class (x) = .vc ("character", "TEXT", x)
	attr (x, ".") = list ()
	implant (x, attributes=LIST (..., call=call) )
}

COMPLEXv = function (x, ..., call=sys.call () [-2])
{	class (x) = .vc ("complex", "COMPLEX", x)
	attr (x, ".") = list ()
	implant (x, attributes=LIST (..., call=call) )
}

is.VECTOR = function (x) inherits (x, "VECTOR")
is.INTEGER = function (x) inherits (x, "INTEGER")
is.LOGICAL = function (x) inherits (x, "LOGICAL")
is.REAL = function (x) inherits (x, "REAL")
is.TEXT = function (x) inherits (x, "TEXT")
is.COMPLEX = function (x) inherits (x, "COMPLEX")

print.VECTOR = function (x, ...)
{	print (as.vector (x) )
	.printv (x, ...)
}

"$.VECTOR" = function (x, name) attr (x, ".") [[name]]
"$<-.VECTOR" = function (x, name, value)
{	attr (x, ".") [[name]] = value
	x
}

"[.VECTOR" = function (x, ...)
{	y = NextMethod ()
	class (y) = class (x)
	attr (y, ".") = attr (x, ".")
	y
	
}

.vc = function (cn1, cn2, x)
{	if (!inherits (x, cn1) )
		stop ("seed vector wrong class")
	c (cn2, "VECTOR", cn1)
}

.printv = function (x)
{	cat (class (x) [1], "\n")
	k = names (attr (x, ".") )
	if (ife (k) ) k = "(none)"
	cat ("object_attributes:", k, "\n" )
}


