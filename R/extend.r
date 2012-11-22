extend = function (object, subclass)
{	superclass = attr (object, "class")
	if (inherits (object, subclass) ) stop ("can't extend object with duplicated class")
	class (object) = c (rev (subclass), superclass)
	object
}


