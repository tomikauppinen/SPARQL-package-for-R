\name{SPARQL-package}
\alias{SPARQL-package}
\docType{package}
\title{
SPARQL client
}
\description{
Load SPARQL SELECT query result tables from an end-point as data frame.
}
\details{
\tabular{ll}{
Package: \tab SPARQL\cr
Type: \tab Package\cr
Version: \tab 1.4\cr
Date: \tab 2011-07-08\cr
License: \tab GPL-3\cr
Depends: \tab XML\cr
LazyLoad: \tab yes\cr
}
}
\author{
Willem Robert van Hage <W.R.van.Hage@vu.nl> and Tomi Kauppinen <tomi.kauppinen@uni-muenster.de>, with contributions from Christopher Davis <C.B.Davis@tudelft.nl>.
Maintainer: Willem Robert van Hage <W.R.van.Hage@vu.nl> and Tomi Kauppinen <tomi.kauppinen@uni-muenster.de>.
}
\references{
SPARQL specification, \url{http://www.w3.org/TR/rdf-sparql-query/}.\cr
Examples of SPARQL end-points, \url{http://www.w3.org/wiki/SparqlEndpoints}.
}
\keyword{ package }
\examples{
d <- SPARQL(url="http://services.data.gov.uk/reference/sparql",
            query="SELECT * WHERE { ?s ?p ?o . } LIMIT 10",
            ns=c('time','<http://www.w3.org/2006/time#>'))

is.data.frame(d)
}
