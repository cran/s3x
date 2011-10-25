"sample" = function (...)
	UseMethod ("sample")

sample.default = function (...)
	base::"sample" (...)

sample_deterministic = function (x, n=3, m=n)
{	z = dim (x)
	if (ifo (z) && length (z) == 2)
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

sample_random = function (...)
	base::"sample" (...)

samp = function (...)
	sample_deterministic (...)


