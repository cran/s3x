TABLE = function (object)
{	object.names = names (object)
	attributes (object) = NULL
	class (object) = "TABLE"
	names (object) = object.names
	attr (object, ".") = list ()
	object
}

is_TABLE = function (object)
	inherits (object, "TABLE")

print.TABLE = function (table, ndp, ...)
{	k = as.data.frame (table, optional=TRUE, check.names=FALSE)
	if (!missing (ndp) )
		for (j in 1:length (k) )
		{	if (is.integer (k [[j]]) || is.numeric (k [[j]]) )
				k [[j]] = round (k [[j]], ndp)
		}
	print (k, ...)
}

summary.TABLE = function (table, ...)
{	cat ("TABLE\n")
	k = names (table)
	q = inv = numeric (length (table) )
	sm = smin = smax = character (length (table) )
	for (j in 1:length (table) )
	{	obj = table [[j]]
		q [j] = class (obj)
		inv [j] = sum (is.na (obj) )
		if (is.numeric (obj) || is.integer (obj) )
		{	sm [j] = mean (obj, na.rm=TRUE)
			smin [j] = min (obj, na.rm=TRUE)
			smax [j] = max (obj, na.rm=TRUE)
		}
	}
	data.frame (variable=k, class=q, "NAs"=inv, "mean"=sm, "min"=smin, "max"=smax,
		check.names=FALSE)
}

plot.TABLE = function (table, ...)
	plot (as.data.frame (table), ...)

"[.TABLE" = function (table, i, j, ...)
{	n = length (sys.call () ) - 2
	case = .table.case (n, i, j)
	if (case == "[1]")
		stop ("[.TABLE single index isn't allowed")
	if (!missing (i) && i [1] < 0)
		i = (1:nrow (table) )[i]
	if (!missing (j) )
	{	if (is.character (j) ) j = .getcolumnindex (table, j)
		if (j [1] < 0) j = (1:ncol (table) )[j]
	}
	if (case == "00")
		table
	else if (case == "01")
	{	k = unclass (table) [j]
		class (k) = class (table)
		k
	}
	else if (case == "10")
	{	k = unclass (table)
		for (j_ in 1:length (k) ) k [[j_]] = k [[j_]][i]
		class (k) = class (table)
		k
	}
	else
	{	k = unclass (table) [j]
		for (j_ in 1:length (k) ) k [[j_]] = k [[j_]][i]
		class (k) = class (table)
		k
	}
}

"[[.TABLE" = function (table, i, j, ...)
{	n = length (sys.call () ) - 2
	case = .table.case (n, i, j)
	if (case == "[1]")
	{	case = "01"
		j = i
	}
	if ( (case == "11" || case == "01") && length (j) > 1)
		stop ("[[.TABLE column index must be scalar")
	if (!missing (j) && is.character (j) )
		j = .getcolumnindex (table, j)
	if (case == "00") stop ("[[.TABLE requires index")
	else if (case == "01") unclass (table) [[j]]
	else if (case == "10") stop ("[[.TABLE requires column index")
	else unclass (table) [[j]][i]
}

"[[<-.TABLE" = function (table, i, j, ..., value)
{	n = length (sys.call () ) - 3
	case = .table.case (n, i, j)
	if (case == "[1]")
	{	case = "01"
		j = i
	}
	if ( (case == "11" || case == "01") && length (j) > 1)
		stop ("[[<-.TABLE column index must be scalar")
	if (!missing (j) && is.character (j) )
		j = .getcolumnindex (table, j)
	if (case == "00")
		stop ("[[.TABLE requires index")
	else if (case == "01")
	{	if (nrow (table) != length (value) )
			stop ("[[<-TABLE inappropriate length")
		kc = class (table)
		class (table) = NULL
		table [[j]] = value
		class (table) = kc
		table
	}
	else if (case == "10")
		stop ("[[.TABLE requires column index")
	else
	{	kc = class (table)
		class (table) = NULL
		table [[j]][i] = value
		class (table) = kc
		table
	}
}

dim.TABLE = function (x, ...) c (length (x [[1]]), length (x) )

as.list.TABLE = function (table, ...)
{	k = table
	attributes (k) = NULL
	names (k) = names (table)
	k
}

as.data.frame.TABLE = function (table, ...)
	as.data.frame (as.list (table), 1:nrow (table), ...)

.table.case = function (n, i, j)
{	if (n == 0) "00"
	else if (n == 1)
	{	if (missing (i) ) "00"
		else "[1]"
	}
	else if (n == 2)
	{	mi = missing (i)
		mj = missing (j)
		if (mi && mj) "00"
		else if (mi && !mj) "01"
		else if (!mi && mj) "10"
		else "11"
	}
	else stop ("too many indices")
}

.getcolumnindex = function (v, j)
	match (j, names (v) )



