where = function (...)
	UseMethod ("where")

where.TABLE = function (table, variable, value, ...)
{	i = rep (FALSE, nrow (table) )
	v = table [[variable]]
	for (z in value)
		i = i | v == z
	table [i,]
}

sort.TABLE = function (table, by=1, increasing=TRUE, ...)
{	args = paste ("table [,", by, "]", sep="", collapse=", ")
	i = evalt (paste ("order (", args, ", decreasing=!increasing)", sep="") )
	table [i,]
}

sample.TABLE = function (table, ...)
	sample_deterministic (table, ...)


