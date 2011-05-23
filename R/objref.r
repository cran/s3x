#this implementation is very inefficient
objref = function (obj)
{	e = extend (environment (), "objref")
	#note, can't use `$<-` (or extend/implant) for assignment
	assign ("obj", obj, e)
	e
}

deref = function (ref) get ("obj", ref)

print.objref = function (ref, ...)
	cat ("objref -> ", class (deref (ref) ) [[1]], "\n")

length.objref = function (x) length (deref (x) )

"$.objref" = function (ref, name)
	evalt (paste ("obj$", name), e=ref)

"$<-.objref" = function (ref, name, value)
{	cn = class (ref)
	class (ref) = NULL
	evalt (paste ("ref$obj$", name, "=value"), FALSE)
	class (ref) = cn
	ref
}

"[.objref" = function (ref, ...) deref (ref) [...]
"[[.objref" = function (ref, ...) deref (ref) [[...]]

"[<-.objref" = function (ref, ..., value)
{	cn = class (ref)
	class (ref) = NULL
	ref$obj [...] = value
	class (ref) = cn
	ref
}

"[[<-.objref" = function (ref, ..., value)
{	cn = class (ref)
	class (ref) = NULL
	ref$obj [[...]] = value
	class (ref) = cn
	ref
}

