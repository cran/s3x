LIST = function (...) extend (list (...), "LIST")

s3x_print.LIST = function (object, ...)
{	cat ("LIST\n")
	print.default (unclass (object), ...)
}

s3x_format.LIST = function (object, ...) format.default (object, ...)

"[.LIST" = function (object, ...) extend (NextMethod (), "LIST")

