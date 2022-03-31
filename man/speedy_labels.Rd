\name{speedy_labels}
\alias{speedy_labels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Automate code for labelling variables and values with the 'labelled' package
}
\description{
'speedy_labels' automates the code for labelling variables and values. With 'speedy_labels', all the code is automatically written other than the new labels. It is primarily designed for working with survey data but can be used for any data that requires labels.
}
\usage{
speedy_labels(data, nrows = 5, path = "")
}

\arguments{
  \item{data}{
  name of dataset
}
  \item{nrows}{
  Number of rows for value labels of each variable. The minimum number of rows allowed is 2 and the maximum is 10. Extra rows without values are set to NA.
  }
  \item{path}{
    If saving code to a new R script file, specify the file path here. Leave blank if not saving code.
  }
}
\details{
'speedy_labels' automatically writes all the code necessary to add labels to a dataset. The code may be copied and pasted from the console or saved out to a separate R script. The 'dplyr' and 'labelled' packages are not required to run the package, but the automated code uses syntax from these packages so you will need to load them to run the code.
}
\value{
Formatted code written with 'dplyr' syntax for labelling variables with the 'labelled' package. The automated code maintains the original class for each variable so the only required input is a different class for the variables that need it. Replace the final comma with a parenthese and press "Cmd + I" (or Ctrl + I for PC users) to format the code indentations.
}

\author{
Jacob Harris
}
\note{
For ease, the iris dataset is used as an example. However, the usage of 'speedy_labels' is more intuitive with actual survey data with categorical responses since it usually doesn't make sense to label continuous values.
}

\seealso{
This function is particularly useful for adding labels to data files that were converted from ASCII format. Click \href{https://jacob-harris.com/}{here} to learn more about ASCII converions in R.
}
\examples{
speedy_labels(iris, nrows = 5)

\dontrun{speedy_labels(data = iris, nrows = 5, path = "~/INPUT-FILE-PATH")}

}
