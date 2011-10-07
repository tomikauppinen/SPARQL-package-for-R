\name{SPARQL}
\alias{SPARQL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SPARQL client
}
\description{
This function connects to a SPARQL end-point over HTTP, poses a SELECT query, and return the results as a data frame with a named column for each variable from the SELECT query.
}
\usage{
SPARQL(url = "http://localhost/", query = "", ns = NULL, param = "query", extra = "", format="xml", lossy=TRUE)
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
  \item{ns}{
%%     ~~Describe \code{ns} here~~
  Prefixes to shorten IRIs returned by the SPARQL end-point.
  For example, \preformatted{ns=c('dc','<http://purl.org/dc/elements/1.1/>',
     'rdfs','<http://www.w3.org/2000/01/rdf-schema#>')} will shorten the IRIs \code{'http://purl.org/dc/elements/1.1/title'} to \code{'dc:title'} and \code{'http://www.w3.org/2000/01/rdf-schema#label'} to \code{'rdfs:label'}. 
}
  \item{param}{
%%     ~~Describe \code{param} here~~
  By default a SPARQL end-point accepts queries in the \code{"query"} HTTP parameter. If the end-point uses a different parameter you can specify this here.
}
  \item{extra}{
%%     ~~Describe \code{extra} here~~
  Extra characters appended to the HTTP request URL. Some SPARQL end-points require extra parameters to work. These can be supplied, in URL encoded form, as a character vector with this parameter. This field can be used to specify the various ways in which different end-points can be told to return a certain format. For example, extra="&resultFormat=xml" or extra="&output=xml"
}
  \item{format}{
 Can be used to explicitly state what kind of format is returned by the output. This version supports "xml", "csv" and "tsv".
}
  \item{lossy}{
  The SPARQL client contains two parsers for SPARQL XML results, a fast lossy parser and a slow non-lossy parser. The lossy parser ignores literal datatypes and languages and does not wrap URIs with pointy braces. The lossy parameter can be set to FALSE to select the slow non-lossy XML parser when the format parameter is set to "xml" (which is the case by default). NB, Most SPARQL end-points that support CSV or TSV output are also lossy.
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
d <- SPARQL(url="http://services.data.gov.uk/reference/sparql",
            query="SELECT * WHERE { ?s ?p ?o . } LIMIT 10",
            ns=c('time','<http://www.w3.org/2006/time#>'))

is.data.frame(d)
}
\keyword{ SPARQL }
