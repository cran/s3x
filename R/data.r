read.data.file = function (path, header=TRUE, check.names=FALSE, sep=",", ...)
{	if (!file.exists (path) ) stop ("file doesn't exist")
	read.table (path, header=header, check.names=check.names, sep=sep, ...)
}

read.package.data = function (package, file, header=TRUE, check.names=FALSE, sep=",", ...)
{	path = paste (.find.package (package), "/data/", file, sep="")
	read.data.file (path, header, check.names, sep, ...)
}

samp = function (x, n=3, m=n) UseMethod ("samp")

samp.objref = function (x, n=3, m=n) samp (deref (x), n, m)
samp.matrix = function (x, n=3, m=n) .samp.table (x, n, m)
samp.data.frame = function (x, n=3, m=n) .samp.table (x, n, m)
samp.default = function (x, n=3, m=n) .samp.vector (x, n, m)

.samp.vector = function (x, n, m)
{	t = length (x)
	if (t > n + m) x [c (1:n, (t - m + 1):t)]
	else x
}

.samp.table = function (x, n, m)
{	t = nrow (x)
	if (t > n + m) x [c (1:n, (t - m + 1):t),]
	else x
}

