\name{constructors}
\alias{extend}
\alias{implant}
\title{construction utilities}
\description{The extend function, extends an object. For objects with an explicit class attribute (e.g. data.frame(s)), extend concatenates the class attribute with the subclass. For objects without an explicit class attribute (e.g. numeric(s)), extend sets the class attribute to the subclass. Both the extend and implant functions can take a list or an enhanced primitive along with a list of object attributes and "implant" the attributes inside the object. If the list of attributes forms part of the argument list, then each argument can be in name=value or name (only) form. Where name (only) form is used, the functions try to find the name in the parent environment(s) along with it's value. Calling the functions with \dots as part of their argument list will produce an error. Note that the functions don't modify the original object, rather they return a new object. Also note that the subclass argument may be vectorised, however superclasses preceed subclasses.}
\usage{
extend (object, subclass, \dots, attributes)
implant (object, \dots, attributes)
}
\arguments{
\item{object}{An object to extend.}
\item{subclass}{A character giving the subclass name.}
\item{\dots}{A list of object attributes, ignored if attributes is provided.}
\item{attributes}{A list of object attributes.}
}



