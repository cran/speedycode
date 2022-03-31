\name{speedy_mutate}
\alias{speedy_mutate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Automate code for recoding variables with the 'mutate' and 'case_when' functions
}
\description{
`speedy_mutate` automates the code for quickly recoding variables with a large number of unique levels with `dplyr` syntax. The user only needs to supply the variable to recode and whether or not those variables should be quoted or not.
}
\usage{
speedy_mutate(data, var, var_classes = "sn", path = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  Name of dataset
}

  \item{var}{
  String of the name of the variable being recoded
  }

  \item{var_classes}{
  Specifies whether or not the current variable and the new variable being created should have quotes around them. There are four possible inputs ("ss", "sn", "nn", "ns"). "ss" means the current and new variable with both have quotes. "sn" means the first will have quotes and the second will not and so forth.
  }

  \item{path}{
    If saving code to a new R script file, specify the file path here. Leave blank if not saving code.
  }
}
\details{
`speedy_mutate` generates a formatted chunk of the code for creating a new variable using the `mutate` and `case_when` functions. The code may be copied and pasted from the console or saved out to a separate R script. This is useful when a new variable needs to be created with many different levels based on the values in another variable.
}
\value{
Formatted code written with 'dplyr' syntax for recoding variables with `mutate` and `case_when`.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
\dontrun{A simple applications is to add geographical FIPS codes to U.S. states}

states <- as.data.frame(state.abb)
speedy_mutate(data = states, var = "state.abb")
}
