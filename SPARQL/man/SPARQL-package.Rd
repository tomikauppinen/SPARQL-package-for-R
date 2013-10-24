\name{SPARQL-package}
\alias{SPARQL-package}
\docType{package}
\title{
SPARQL client
}
\description{
Load SPARQL SELECT query result tables as a data frame, or UPDATE the triple store by connecting to an end-point over HTTP.

The development of this library has been developed in part within the COMBINE project supported by the ONR Global NICOP grant N62909-11-1-7060.
}
\details{
\tabular{ll}{
Package: \tab SPARQL\cr
Type: \tab Package\cr
Version: \tab 1.15\cr
Date: \tab 2013-10-23\cr
License: \tab GPL-3\cr
Depends: \tab XML\cr
LazyLoad: \tab yes\cr
}
}
\author{
Willem Robert van Hage <willem.van.hage@synerscope.com>, with contributions from: Tomi Kauppinen, Benedikt Graeler, Christopher Davis, Jesper Hoeksema, Alan Ruttenberg, and Daniel Bahls.
Maintainer: Willem Robert van Hage <willem.van.hage@synerscope.com>
}
\references{
SPARQL specification, \url{http://www.w3.org/TR/rdf-sparql-query/}.\cr
Examples of SPARQL end-points, \url{http://www.w3.org/wiki/SparqlEndpoints}.
}
\keyword{ package }
\examples{
\dontrun{
d <- SPARQL(url="http://services.data.gov.uk/reference/sparql",
            query="SELECT * WHERE { ?s ?p ?o . } LIMIT 10",
            ns=c('time','<http://www.w3.org/2006/time#>'))

is.data.frame(d$results)

# draw a pie chart from data from the Linked Open Piracy data set
endpoint <- "http://semanticweb.cs.vu.nl/lop/sparql/"
q <-
  "SELECT *
   WHERE {
     ?event sem:hasPlace ?place .
     ?place eez:inPiracyRegion ?region .
   } LIMIT 20"
prefix <- c("lop","http://semanticweb.cs.vu.nl/poseidon/ns/instances/",
            "eez","http://semanticweb.cs.vu.nl/poseidon/ns/eez/")
res <- SPARQL(endpoint,q,prefix)$results
pie(sort(table(res$region)),col=rainbow(12))
}
}
