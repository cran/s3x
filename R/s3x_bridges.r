print.LIST = function (x, ...) .s3x_generic (s3x_print, x, ...)
format.LIST = function (x, ...) .s3x_generic (s3x_format, x, ...)
plot.LIST = function (x, ...) .s3x_generic (s3x_plot, x, ...)
lines.LIST = function (x, ...) .s3x_generic (s3x_lines, x, ...)
points.LIST = function (x, ...) .s3x_generic (s3x_points, x, ...)

print.FUNCTION = function (x, ...) .s3x_generic (s3x_print, x, ...)
format.FUNCTION = function (x, ...) .s3x_generic (s3x_format, x, ...)
plot.FUNCTION = function (x, ...) .s3x_generic (s3x_plot, x, ...)
lines.FUNCTION = function (x, ...) .s3x_generic (s3x_lines, x, ...)
points.FUNCTION = function (x, ...) .s3x_generic (s3x_points, x, ...)

print.VECTOR = function (x, ...) .s3x_generic (s3x_print, x, ...)
as.data.frame.VECTOR = function (x, ...)
	.s3x_generic (s3x_as.data.frame, x, ...)

print.objref = function (x, ...) .s3x_generic (s3x_print, x, ...)
as.data.frame.objref = function (x, ...)
	.s3x_generic (s3x_as.data.frame, x, ...)

