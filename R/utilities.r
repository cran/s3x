read_file_data = function (path, sep=",", header=TRUE, check.names=FALSE, ...)
{	if (!file.exists (path) ) stop ("file doesn't exist")
	read.table (path, header=header, check.names=check.names, sep=sep, ...)
}

read_package_data = function (package, file, sep=",", header=TRUE, check.names=FALSE, ...)
{	path = paste (.find.package (package), "/data/", file, sep="")
	read_file_data (path, sep, header, check.names, ...)
}

write_file_data = function (path, table, ..., sep=",", header=TRUE, quote=FALSE)
{	write.table (table, path, quote=quote, sep=sep,
		row.names=FALSE, col.names=header, ...)
}

deterministic_sample = function (x, n=3, m=n)
{	z = dim (x)
	if (length (z) == 2)
	{	t =  z [1]
		if (t > n + m) x [c (1:n, (t - m + 1):t),]
		else x
	}
	else
	{	t = length (x)
		if (t > n + m) x [c (1:n, (t - m + 1):t)]
		else x
	}
}

detsam = function (...)
	deterministic_sample (...)




