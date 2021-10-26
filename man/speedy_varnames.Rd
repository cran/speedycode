\name{speedy_varnames}
\alias{speedy_varnames}

\title{
Quickly generate a vector of new variable names
}
\description{
speedy_varnames generates a vector of new, generic variable names beginning with a given character value through the number of new names desired. This is espeically useful for converting ASCII files when a large number of new variable names must be created.
}
\usage{
speedy_varnames(prefix = "Q", first_number = 1, last_number = 25)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prefix}{
  A character value to precede all of the new variable names
}
  \item{first_number}{
  The beginning number of the new variable name vector
  }
  \item{last_number}{
  The final number of the new variable name vector
  }
}

\value{
A vector of new variable names
}

\author{
Jacob Harris
}
\note{
If using this function for ASCII conversions, you can paste the new vector of names into the col_names argument of the 'read_rpr' function.
}

\examples{
speedy_varnames("Q", 1, 25)
}

