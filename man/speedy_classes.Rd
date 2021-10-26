\name{speedy_classes}
\alias{speedy_classes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Automate code for changing variable classes
}

\description{
'speedy_classes' automates the code for changing variable classes for many variables at time. The default is for each variable to be saved back to the its original class so the new classes are the only required input. Variables that do not need to be changed may be kept or removed from the code.
}

\usage{
speedy_classes(data, path = "")
}

\arguments{
  \item{data}{
  name of dataset
}
  \item{path}{
  If saving code to a separate file, specify the file path here. Leave blank if not saving code.
  }
}

\details{
'speedy_classes' automates the code for changing the classes of a large number of variables at once. The code may be copied and pasted from the console or saved out to a separate R script. The 'dplyr' package is not required to run the package, but the automated code uses 'dplyr' syntax so you will need to load it to run the code.
}

\value{
Formatted code written with 'dplyr' syntax for changing the classes of all variables in a dataset. The automated code maintains the original class for each variable so the only required input is a different class for the variables that need it. You can run the entire code chuck created by 'speedy_classes' without changing anything in the generated code.
}

\author{
Jacob Harris
}

\examples{
speedy_classes(data = iris)

\dontrun{speedy_classes(data = iris, path = "~/INPUT-FILE-PATH")}

}

