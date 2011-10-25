interpolate = function (x, y)
{	f = FUNCTION (.interpolate_main, xlim=range (x) )
	f = extend (f, "interpolate", x, y)
}

.interpolate_main = function (u)
{	v = numeric (length (u) )
	for (i in 1:length (u) )
	{	k = .smooth.interval (.$x, u [i])
		v [i] = interpolate_points (.$x [k], .$y [k],
			.$x [k + 1], .$y [k + 1], u [i])
	}
	v
}

plot.interpolate = function (f, points=FALSE, ...)
{	plot.FUNCTION (f, ...)
	if (points)
		points (f$x, f$y, pch=16, cex=1.25)
}

interpolate_points = function (x1, y1, x2, y2, u)
	y1 + (u - x1) * (y2 - y1) / (x2 - x1)

.smooth.interval = function (x, u)
{	k = sum (x <= u)
	n = length (x)
	if (k >= n)
		k = n - 1L
	k
}

