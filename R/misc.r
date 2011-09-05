evalt = function (text, e=parent.frame () )
	eval (parse (text=text), e)

objattr = function (object) attr (object, ".")
plotattr = function (f)  attr (f, ".plotattr")

.printatt = function (object)
{	k = names (attr (object, ".") )
	if (length (k) > 0)
	{	cat ("object attributes:\n")
		cat (paste (k, collapse=", "), "\n")
	}
}

ifo = function (object)
	!is.null (object)

enumv = function (n, m=1)
	if (n >= m) 1:n else integer ()

