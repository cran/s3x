VECTOR = function (v)
{	v = as.vector (v)
	class (v) = "VECTOR"
	attr (v, ".") = list ()
	v
}

is.VECTOR = function (v) inherits (v, "VECTOR")

s3x_as.data.frame.VECTOR = function (v, ...)
	structure (list (v), class="data.frame", row.names=1:length (v) )

s3x_print.VECTOR = function (v, ...)
{	cat (class (v) [1], "\n")
	print (as.vector (v) )
	.printatt (v, ...)
}

"$.VECTOR" = function (v, name) attr (v, ".") [[name]]

"$<-.VECTOR" = function (v, name, value)
{	attr (v, ".") [[name]] = value
	v
}

"[.VECTOR" = function (v, ...)
{	u = NextMethod ()
	class (u) = class (v)
	attr (u, ".") = attr (v, ".")
	u
}


