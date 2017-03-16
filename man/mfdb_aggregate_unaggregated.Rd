\name{mfdb_aggregate_unaggregated}
\alias{mfdb_unaggregated}

\title{MareFrame DB unaggregated data}
\description{
    Tell mfdb functions not to aggregate this column, just return all values.
}

\usage{
mfdb_unaggregated(omitNA = FALSE, like = c(), not_like = c())
}

\arguments{
    \item{omitNA}{Skip over rows where column is NA}
    \item{like}{Vector of SQL like expressions to check column against}
    \item{not_like}{Vector of SQL like expressions to check column against}
}

\details{
SQL like expessions can use the wildcards "_" to match any character and
"%" to match any number of characters.
}

\examples{
# All vessels with a name ending with 'e' or 'd'
mfdb_unaggregated(like = c("\%e", "\%d"))
}
