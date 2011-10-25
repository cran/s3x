.LIST = function (objs, call)
{	n = length (objs)
	names = names (objs)
	if (is.null (names) ) names = rep ("", n)
	args = call [-1]
	for (i in enumv (n) )
	{	if (names [i] == "" && is.name (args [[i]]) )
			names [i] = as.character (args [[i]])
	}
	names (objs) = names
	objs
}

extend = function (object, subclass, ..., attributes)
{	superclass = attr (object, "class")
	if (inherits (object, subclass) ) stop ("can't re-extend class")
	class (object) = c (rev (subclass), superclass)
	if (missing (attributes) )
	{	attributes = list (...)
		if (length (attributes) == 0)
			object
		else
		{	if (.withoutdots () )
			{	call = .callr (c ("object", "subclass", "attributes") )
				attributes = .LIST (attributes, call)
				implant (object, attributes=attributes)
			}
			else
				stop ("extend attribute list can't include dots")
		}
	}
	else
		implant (object, attributes=attributes)
}

implant = function (object, ..., attributes)
{	if (missing (attributes) )
	{	attributes = list (...)
		if (length (attributes) > 0)
		{	if (.withoutdots () )
			{	call = .callr (c ("object", "attributes") )
				attributes = .LIST (attributes, call)
			}
			else
				stop ("implant attribute list can't include dots")
		}
	}
	if (length (attributes) > 0)
	{	names = names (attributes)
		for (i in 1:length (attributes) )
			evalt (paste ("object$", names [i], "=attributes[[i]]") )
	}
	object
}

.withoutdots = function (call=sys.call (-1) )
{	clean = TRUE
	n = length (call)
	if (n > 1)
		for (i in 1:n)
			if (as.character (call [i]) == "...")
				clean = FALSE
	clean
}

.callr = function (k)
{	call = match.call (sys.function (-1), sys.call (-1) )
	if (missing (k) ) call
	else
	{	i = match (k, names (call), 0)
		call [-i]
	}
}


