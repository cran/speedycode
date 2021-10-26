\name{speedy_rename}
\alias{speedy_rename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Automate code for renaming variables
}
\description{
'speedy_rename' automates the code for renaming variables. With 'speedy_rename', all the code is automatically written with dplyr syntax. The user only needs to provide the new variable names.
}
\usage{
speedy_rename(data, path = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  name of dataset
}
  \item{path}{
  If saving code to a separate file, specify the file path here. Leave blank if not saving code.
  }
}
\details{
'speedy_rename' automatically writes all the code necessary to rename a large number of variables at once with the exception of the new variable names. The code may be copied and pasted from the console or saved out to a separate R script. The dplyr package is not required to run the package, but the automated code uses 'dplyr' syntax so you will need to load it to run the code.
}
\value{
Formatted code written with 'dplyr' syntax for renaming all the variables in a dataset which may be copied and pasted from the console or saved out to a separate R script.
}
\author{
Jacob Harris
\email{jh2689@cornell.edu}
}

\examples{
speedy_rename(data = iris)

\dontrun{speedy_rename(data = iris, path = "~/INPUT-FILE-PATH")}

}

