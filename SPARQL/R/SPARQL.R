library(XML)
library(RCurl)

sparqlns <- c('s'='http://www.w3.org/2005/sparql-results#')
commonns <- c('xsd','<http://www.w3.org/2001/XMLSchema#>','rdf','<http://www.w3.org/1999/02/22-rdf-syntax-ns#>','rdfs','<http://www.w3.org/2000/01/rdf-schema#>','owl','<http://www.w3.org/2002/07/owl#>','skos','<http://www.w3.org/2004/02/skos/core#>','dc','<http://purl.org/dc/elements/1.1/>','foaf','<http://xmlns.com/foaf/0.1/>','wgs84','<http://www.w3.org/2003/01/geo/wgs84_pos#>')

sparqltest <- function(...) {
  SPARQL('http://semanticweb.cs.vu.nl/lop/sparql/','SELECT ?et ?r ?at ?t WHERE { ?e sem:eventType ?et . ?e sem:hasActor ?a . ?a sem:actorType ?at . ?e sem:hasPlace ?p . ?p eez:inPiracyRegion ?r . ?e sem:hasTimeStamp ?t . }',c('lop','<http://semanticweb.cs.vu.nl/poseidon/ns/instances/>','eez','<http://semanticweb.cs.vu.nl/poseidon/ns/eez/>'), ...)
}

#
# Read SPARQL results from end-point
#
SPARQL <- function(url="http://localhost/", query="", update="", ns=NULL, param="", extra=NULL, format="xml", lossy=TRUE) {
  if (!is.null(extra)) {
	  extrastr <- paste(sapply(seq(1,length(extra)),
			    function (i) { paste(names(extra)[i],'=',URLencode(extra[[i]]), sep="") }),
		            collapse="&")
  } else {
	extrastr <- ""
  }
  tf <- tempfile()
  if (query != "") {
          if (param == "") {
		param <- "query"
	  }
	  if(format == 'xml') {
	    tf <- getURL(paste(url, '?', param, '=', URLencode(query), extrastr, sep=""),
	                 httpheader = c(Accept="application/sparql-results+xml"))
	    DOM <- xmlParse(tf)
	    if(length(getNodeSet(DOM, '//s:result[1]', namespaces=sparqlns)) == 0) {
	      rm(DOM)
	      data.frame(c())
	    } else {
	      attrs <- unlist(xpathApply(DOM,
	                                 paste('//s:head/s:variable', sep=""),
	                                 namespaces=sparqlns,
	                                 quote(xmlGetAttr(x, "name"))))			
	      ns2 <- noBrackets(ns)
	      res <- sapply(attrs, get_attr(attr, DOM, ns2, lossy), simplify=FALSE)
	      df <- data.frame(res) # FIXME: fails when there are NULL values in the result table, like with incompatible UNIONS
	      names(df) <- attrs
	      rm(res)
	      rm(DOM)
	    }
	  } else if (format == 'csv') {
	    tf <- getURL(paste(url, '?', param, '=', URLencode(query), extrastr, sep=""))
	    df <- readCSVstring(tf, blank.lines.skip=TRUE, strip.white=TRUE)
	  } else if (format == 'tsv') {
	    tf <- getURL(paste(url, '?', param, '=', URLencode(query), extrastr, sep=""))
	    df <- readTSVstring(tf, blank.lines.skip=TRUE, strip.white=TRUE)
	  }
	  list(results=df, namespaces=ns)
  } else if (update != "") {
	if (param == "") {
		param <- "update"
	}
        extra[[param]] <- update
          postForm(url, .params=extra)
  }
}

readTSVstring <- function(text, ...)
{
   dfr <- read.delim(tc <- textConnection(text), ...)
   close(tc)
   dfr
}


readCSVstring <- function(text, ...)
{
   dfr <- read.csv(tc <- textConnection(text), ...)
   close(tc)
   dfr
}

get_attr <- function(attr, DOM, ns, lossy) {
  if (lossy) {
    rv <- function(attr) {
      unlist(sapply(getNodeSet(DOM,
                               paste('//s:result/s:binding[@name="', attr, '"]/s:uri/text() ',
                                     '| //s:result/s:binding[@name="', attr, '"]/s:bnode',
                                     '| //s:result/s:binding[@name="', attr, '"]/s:literal/text()',
                                     sep=""),
                               namespaces=c('s'='http://www.w3.org/2005/sparql-results#')),
                    function(x) {
                      qnames(xmlValue(x), ns)
                    },
                    simplify=FALSE))
    }
  } else {
    rv <- function(attr) {
      as.factor(unlist(sapply(getNodeSet(DOM,
                                         paste('//s:result/s:binding[@name="', attr, '"]/*[1]', sep=""),
                                         namespaces=sparqlns),
                              function(x) { # FIXME: very slow...
                                node = xmlDoc(x)
                                uri = xpathSApply(node, '/s:uri', xmlValue, namespaces=sparqlns)
                                if(length(uri) == 0) {
                                  literal = xpathSApply(node, '/s:literal', xmlValue, namespaces=sparqlns)
                                  if(length(literal) == 0) {
                                    bnode = xpathSApply(node, '/s:bnode', xmlValue, namespaces=sparqlns)
                                    if (length(bnode) == 0) { # error
                                      
                                    } else { # found bnode
                                      paste('_:genid', bnode, sep='')
                                    }
                                  } else { # found literal
                                    lang = xpathApply(node, '/s:literal', xmlGetAttr, "xml:lang", namespaces=sparqlns)
                                    if(is.null(lang[[1]])) {
                                      type = xpathApply(node, '/s:literal', xmlGetAttr, "datatype", namespaces=sparqlns)
                                      if(is.null(type[[1]])) {
                                        paste('"', literal, '"', sep="")
                                      } else {
					qname = qnames(type, ns)
					if(unlist(qname) == unlist(type))
	                                        paste('"', literal, '"^^<', qname, '>', sep="")
					else
						paste('"', literal, '"^^', qname, sep="")
                                      }
                                    } else {
                                      paste('"', literal, '"@', lang, sep='')
                                    }
                                  }
                                } else { # found URI
                                  qname = qnames(uri, ns)
                                  if(qname == uri)
				    paste('<', uri, '>', sep="")
				  else
				    qname
                                }
                              },
                              simplify=FALSE)))
    }
  }
  rv
}

noBrackets <- function(ns) {
  sapply(ns,function(br_ns) {
	if(substr(br_ns,1,1)=='<')
		substr(br_ns,2,nchar(br_ns)-1)
	else
		br_ns
	})
}

substNS <- function(str0, ns) {
  regex <- paste('^', ns[2], sep="")
  gsub(regex, paste(ns[1], ":", sep=""), str0)
}

qnames <- function(str0, ns_list) {
  if(!length(ns_list))
    str0
  else
    substNS(qnames(str0, ns_list[-1:-2]), ns_list[1:2])
}

