\name{debug_ascii}
\alias{debug_ascii}

\title{
Debug errors when converting ASCII files with the readroper package
}
\description{
A common issue when converting ASCII files is getting the column positions, variable widths, and new variable names to align. A simple way to debug errors is to compare the lengths of each which must be equivalent to convert ASCII files with the readroper package. debug_ascii calculates the lengths of each of these inputs so you can quickly diagnose errors. Each argument can be copied directly from the 'read_rpr' function within the readroper package.
}
\usage{
debug_ascii(col_positions_input, widths_input, col_names_input)
}

\arguments{
  \item{col_positions_input}{
The col_positions argument in the read_rpr function
}
  \item{widths_input}{
  The widths argument in the read_rpr function
}
  \item{col_names_input}{
  The col_names argument in the read_rpr function
  }
}
\value{
A dataframe containing the lengths of the col_positions, widths, and col_names arguments
}

\author{
Jacob Harris
}

\examples{
\dontrun{For ease of replicability, the examples here
come from generated data rather than data from
an ASCII file.}

col_positions <- c(1, 2, 3, 5, 8)
widths <- c(1, 1, 2, 3, 1)
col_names <- c("Q1", "Q2", "Q3", "Q4", "Q5")

debug_ascii(col_positions, widths, col_names)

\dontrun{Now, if the lengths differ from an error
(see the missing "Q4" variable), the function
will throw a warning.}

col_positions <- c(1, 2, 3, 5, 8)
widths <- c(1, 1, 2, 3, 1)
col_names <- c("Q1", "Q2", "Q3", "Q5")

debug_ascii(col_positions, widths, col_names)
}

