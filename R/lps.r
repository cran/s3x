lps = function (x, y, u, degree=2, smoothness=1, ..., m, wc, wf, bw, z)
{	if (missing (wc) ) wc = parse (text="wf (bw, x - u [i])")
	if (missing (wf) ) wf = .wf_default
	if (missing (m) ) m = .lps.matrix (degree, x)
	if (missing (bw) ) bw = smoothness * diff (range (x) )
	pows = 0:degree
	v = numeric (length (u) )
	for (i in 1:length (u) )
	{	w = eval (wc)
		k = lm.wfit (m, y, w)$coefficients
		v [i] = sum (k * u [i]^pows)
	}
	v
}

dlps = function (x, y, u, degree=2, smoothness=1, ..., m, wc, wf, bw, z)
{	if (missing (wc) ) wc = parse (text="wf (bw, x - u [i])")
	if (missing (wf) ) wf = .wf_default
	if (missing (m) ) m = .lps.matrix (degree, x)
	if (missing (bw) ) bw = smoothness * diff (range (x) )
	pows = 0:degree
	dpows = c (0, 0:(degree - 1) )
	v = dvdu = rep (0, length (u) )
	for (i in 1:length (u) )
	{	w = eval (wc)
		k = lm.wfit (m, y, w)$coefficients
		if (degree > 0)
		{	v [i] = sum (k * u [i]^pows)
			dvdu [i] = sum (pows * k * u [i] ^ dpows)
		}
		else
			v [i] = k
	}
	cbind (v, dvdu)
}

lps_series = function (x, y, degree=2, smoothness=1, n=30)
{	xrng = range (x)
	u = seq (xrng [1], xrng [2], length=n)
	v = lps (x, y, u, degree, smoothness)
	cbind (u, v)
}

lps_interpolate = function (x, y, degree=2, smoothness=1, n=30)
{	m = lps_series (x, y, degree, smoothness, n)
	interpolate (m [,1], m [,2])
}

.lps.matrix = function (degree, x)
{	m = cbind (1, x)
	if (degree > 1)
		for (j in 2:degree)
			m = cbind (m, x^j)
	m
}

.wf_default = function (bw, x)
{	y = 2 * x / bw
	y = 1 - y * y
	y [y < 0] = 0
	y

}




