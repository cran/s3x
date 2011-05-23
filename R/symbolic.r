#conditionals:
ifo = function (object) (!is.null (object) )
ife = function (object) (length (object) == 0)
ifn = function (object) (length (object) > 0)

#sequence-vectors (for iteration)
seqv = function (n) if (n >= 1) 1:n else NULL
seqv.ol = function (object) seqv (length (object) )

evalt = function (text, return=TRUE, e)
{	if (return)
	{	if (missing (e) ) e = sys.frame ()
		eval (parse (text=text), e)
	}
	else
	{	if (missing (e) ) e = sys.frame (-1)
		for (k in text) eval (parse (text=k), e)
	}
}






