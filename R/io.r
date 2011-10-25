read_file_data = function (path, TABLE=TRUE,
	header=TRUE, check.names=FALSE, sep=",", ...)
{	if (!file.exists (path) ) stop ("file doesn't exist")
	table = read.table (path, header=header, check.names=check.names, sep=sep, ...)
	if (TABLE) TABLE (table)
	else table
}

read_package_data = function (package, file, TABLE=TRUE,
		header=TRUE, check.names=FALSE, sep=",", ...)
{	path = paste (.find.package (package), "/data/", file, sep="")
	read_file_data (path, TABLE, header, check.names, sep, ...)
}

write_file_data = function (path, table, ..., header=TRUE, quote=FALSE, sep=",")
{	write.table (table, path, quote=quote, sep=sep,
		row.names=FALSE, col.names=header, ...)
}

