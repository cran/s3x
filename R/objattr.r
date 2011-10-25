is_s3x_object = function (object)
	is_TABLE (object) || is_FUNCTION (object) || is_VECTOR (object)

objattr = function (object)
{	if (is_s3x_object (object) )
		attr (object, ".")
	else if (is.list (object) )
	{	k = object
		attributes (k) = NULL
		names (k) = names (object)
		k
	}
	else
		stop ("objattr requires list or s3x object")
}

"$.TABLE" = function (table, name) attr (table, ".") [[name]]
"$.FUNCTION" = function (f, name) attr (f, ".") [[name]]
"$.VECTOR" = function (v, name) attr (v, ".") [[name]]

"$<-.TABLE" = function (table, name, value)
{	attr (table, ".") [[name]] = value
	table
}

"$<-.FUNCTION" = function (f, name, value)
{	attr (f, ".") [[name]] = value
	f
}

"$<-.VECTOR" = function (v, name, value)
{	attr (v, ".") [[name]] = value
	v
}

.printattr = function (object)
{	k = names (attr (object, ".") )
	if (length (k) > 0)
	{	cat ("object attributes:\n")
		cat (paste (k, collapse=", "), "\n")
	}
}

