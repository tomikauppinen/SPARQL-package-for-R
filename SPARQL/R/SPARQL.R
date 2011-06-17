library(XML)
library(RCurl)

#
# Read SPARQL results from end-point
#

SPARQL <- function(url="http://localhost/",query="",ns=NULL,param="query",extra="") {
	tf <- tempfile()
        tf <- getURL(paste(url,'?',param,'=',URLencode(query),extra,sep=""), httpheader = c(Accept="application/sparql-results+xml"))
	DOM <- xmlParse(tf)
	attrs <- unlist(xpathApply(DOM,
		paste('//s:head/s:variable',sep=""),
		namespaces=c('s'='http://www.w3.org/2005/sparql-results#'),
		quote(xmlGetAttr(x,"name"))))
	df <- data.frame(sapply(
		attrs,
		function(attr) {
			sapply(
				getNodeSet(DOM,
					paste('//s:result/s:binding[@name="',attr,'"]/s:uri/text() ',
						'| //s:result/s:binding[@name="',attr,'"]/s:bnode',
						'| //s:result/s:binding[@name="',attr,'"]/s:literal/text()',
						sep=""),
					namespaces=c('s'='http://www.w3.org/2005/sparql-results#')),
				function(x) {
					qnames(xmlValue(x),ns)
				})
			}))
	names(df) <- attrs
	rm(DOM)
	df
}


substNS <- function(str0, ns) {
	gsub(ns[2],paste(ns[1],":",sep=""),str0)
}

qnames <- function(str0, ns_list) {
	if(!length(ns_list))
		str0
	else
		substNS(qnames(str0,ns_list[-1:-2]),ns_list[1:2])
}
