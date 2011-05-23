samp = function (x, n=3, m=n) UseMethod ("samp")

samp.data.frame = function (x, n=3, m=n) .samp.table (x, n, m)
samp.matrix = function (x, n=3, m=n) .samp.table (x, n, m)
samp.objref = function (x, n=3, m=n) samp (deref (x), n, m)
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


