\name{SPARQL}
\alias{SPARQL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SPARQL client
}
\description{
This function connects to a SPARQL end-point over HTTP or HTTPs, poses a SELECT query or an update query (LOAD, INSERT, DELETE). If given a SELECT query it returns the results as a data frame with a named column for each variable from the SELECT query, a list of prefixes and namespaces that were shortened to qnames is also returned. If given an update query nothing is returned.
If the parameter "query" is given, it is assumed the given query is a SELECT query and a GET request will be done to get the results from the URL of the end point.
Otherwise, if the parameter "update" is given, it is assumed the given query is an update query and a POST request will be done to send the request to the URL of the end point.
}
\usage{
SPARQL(url = "http://localhost/", query = "", update="", ns = NULL, param = "",
       extra = NULL, format="xml", curl_args=NULL, parser_args=NULL, encoding="UTF-8")
}
%%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{
%%     ~~Describe \code{url} here~~
  The URL of the SPARQL end-point.
}
  \item{query}{
%%     ~~Describe \code{query} here~~
  A SPARQL SELECT query to fire at the end-point.
}
  \item{update}{
%%     ~~Describe \code{query} here~~
  A SPARQL update query (LOAD, INSERT, DELETE)) to fire at the end-point.
}
  \item{ns}{
%%     ~~Describe \code{ns} here~~
  Prefixes to shorten IRIs returned by the SPARQL end-point.
  For example, \preformatted{ns=c('dc','<http://purl.org/dc/elements/1.1/>',
     'rdfs','<http://www.w3.org/2000/01/rdf-schema#>')} will shorten the IRIs \code{'http://purl.org/dc/elements/1.1/title'} to \code{'dc:title'} and \code{'http://www.w3.org/2000/01/rdf-schema#label'} to \code{'rdfs:label'}.
}
  \item{param}{
%%     ~~Describe \code{param} here~~
  By default a SPARQL end-point accepts queries in the \code{"query"} HTTP parameter and updates in the \code{"update"} parameter. If the end-point uses a different parameter you can specify this here.
}
  \item{extra}{
%%     ~~Describe \code{extra} here~~
  Extra parameters and their values that will be added to the HTTP request. Some SPARQL end-points require extra parameters to work. These can be supplied, in URL encoded form, as a character vector with this parameter. This field can be used to specify the various ways in which different end-points can be told to return a certain format. For example, extra=list(resultFormat="xml") or extra=list(output="xml",queryLn="SPARQL")
}
  \item{format}{
 Can be used to explicitly state what kind of format is returned by the output. This version supports "xml", "csv" and "tsv".
}
  \item{curl_args}{
A list of arguments that will be passed to RCurl when fetching the SPARQL results over HTTP. This can be used, for example, to pass authentication arguments, or to change the mime type of a post request from multipart/form-data to application/x-www-form-urlencoded, by passing curl_args=list(style="post").
}
  \item{parser_args}{
A list of arguments that will be passed to the XML, CSV, TSV, etc. parser that processed the returned SPARQL result table.
}
  \item{encoding}{
Encoding parameter passed to RCurl::getURL, e.g. "UTF-8" or "ISO-8859-1".
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
The returned data frame contains a column for each variable in the SELECT query. For example, the query \code{"SELECT * WHERE { ?s ?p ?o . } LIMIT 10"} will yield three columns named "s", "p", and "o". The query \code{"SELECT ?s WHERE { ?s ?p ?o . } LIMIT 10"} will yield only one column named "s".
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
SPARQL specification, \url{http://www.w3.org/TR/rdf-sparql-query/}.\cr
SPARQL Update specification, \url{http://www.w3.org/TR/sparql11-update/}.\cr
Examples of SPARQL end-points, \url{http://www.w3.org/wiki/SparqlEndpoints}.
%% ~put references to the literature/web site here ~
}
\author{
Willem Robert van Hage and Tomi Kauppinen
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
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
}
\keyword{ SPARQL }
