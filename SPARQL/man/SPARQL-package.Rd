\name{SPARQL-package}
\alias{SPARQL-package}
\docType{package}
\title{
SPARQL client
}
\description{
Load SPARQL SELECT query result tables as a data frame, or UPDATE the triple store by connecting to an end-point over HTTP.
}
\details{
\tabular{ll}{
Package: \tab SPARQL\cr
Type: \tab Package\cr
Version: \tab 1.10\cr
Date: \tab 2012-06-25\cr
License: \tab GPL-3\cr
Depends: \tab XML\cr
LazyLoad: \tab yes\cr
}
}
\author{
Willem Robert van Hage <W.R.van.Hage@vu.nl> and Tomi Kauppinen <tomi.kauppinen@uni-muenster.de>, with contributions from Christopher Davis <C.B.Davis@tudelft.nl> and Jesper Hoeksema <J.E.Hoeksema@vu.nl>.
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

is.data.frame(d$results)

# draw a pie chart from data from the Linked Open Piracy data set
endpoint <- "http://semanticweb.cs.vu.nl/lop/sparql/"
q <-
  "SELECT *
   WHERE {
     ?event sem:hasPlace ?place .
     ?place eez:inPiracyRegion ?region .
   }"
prefix <- c("lop","http://semanticweb.cs.vu.nl/poseidon/ns/instances/",
            "eez","http://semanticweb.cs.vu.nl/poseidon/ns/eez/")
res <- SPARQL(endpoint,q,prefix)$results
pie(sort(table(res$region)),col=rainbow(12))

# draw a stacked bar chart from data from the Linked Open Piracy data set
q <-
  "SELECT *
   WHERE {
     ?event sem:eventType ?event_type .
     ?event sem:hasPlace ?place .
     ?place eez:inPiracyRegion ?region .
   }"
res <- SPARQL(endpoint,q,ns=prefix)$results
restable <- table(res$event_type,res$region)
par(mar=c(4,10,1,1))
barplot(restable,col=rainbow(10),horiz=TRUE,las=1,cex.names=0.8)
legend("topright",rownames(restable),
       cex=0.8,bty="n",fill=rainbow(10))
}
