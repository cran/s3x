s3x_print = function (...) UseMethod ("s3x_print")
s3x_format = function (...) UseMethod ("s3x_format")
s3x_as.data.frame = function (...) UseMethod ("s3x_as.data.frame")
s3x_plot = function (...) UseMethod ("s3x_plot")
s3x_lines = function (...) UseMethod ("s3x_lines")
s3x_points = function (...) UseMethod ("s3x_points")

.s3x_generic = function (f, x, ...)
{	call = sys.call (-1)
	if (length (call) > 2)
	{	msg = paste (call [1], "can only have one argument")
		stop (msg)
	}
	f (x)
}

