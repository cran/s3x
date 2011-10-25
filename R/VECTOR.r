VECTOR = function (v)
{	v = as.vector (v)
	class (v) = "VECTOR"
	attr (v, ".") = list ()
	v
}

is_VECTOR = function (object)
	inherits (object, "VECTOR")

print.VECTOR = function (v, ...)
{	cat (class (v) [1], "\n")
	print (as.vector (v), ...)
	.printattr (v)
}

"[.VECTOR" = function (v, ...)
{	u = NextMethod ()
	attributes (u) = attributes (v)
	u
}

"[[.VECTOR" = function (v, ...)
{	attributes (v) = NULL
	`[` (v, ...)
}

as.data.frame.VECTOR = function (v, ...)
	structure (list (v), class="data.frame", row.names=1:length (v) )

