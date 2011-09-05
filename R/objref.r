objref = function (object)
{	e = new.env ()
	e$object = object
	class (e) = "objref"
	e
}

is.objref = function (ref) inherits (ref, "objref")
length.objref = function (x) length (deref (x) )

s3x_print.objref = function (ref, ...)
	cat ("objref ->", class (deref (ref) ) [[1]], "\n")

s3x_as.data.frame.objref = function (ref, ...)
	as.data.frame (deref (ref) )

"$.objref" = function (ref, name)
	evalt (paste ("object$", name), ref)

"$<-.objref" = function (ref, name, value)
{	k = class (ref)
	class (ref) = NULL
	e = try (evalt (paste ("ref$object$", name, "=value") ), silent=TRUE)
	class (ref) = k
	if (.iserr (e) ) stop (e)
	ref
}

"[.objref" = function (ref, ...) deref (ref) [...]
"[[.objref" = function (ref, ...) deref (ref) [[...]]

"[<-.objref" = function (ref, ..., value)
{	k = class (ref)
	class (ref) = NULL
	e = try (ref$object [...] <- value, silent=TRUE)
	class (ref) = k
	if (.iserr (e) ) stop (e)
	ref
}

"[[<-.objref" = function (ref, ..., value)
{	k = class (ref)
	class (ref) = NULL
	e = try (ref$object [[...]] <- value, silent=TRUE)
	class (ref) = k
	if (.iserr (e) ) stop (e)
	ref
}

deref = function (ref) get ("object", ref)

.iserr = function (e) class (e) == "try-error"

