library(SPARQL)
library(XML)

large.query.handler<-function(endpoint, query.list, query.builder, limit=5000){
  
  results<-NULL
  i<-1
  
  while (i<length(query.list)){
    
    begin<-i
    size<-0
    while (size + nchar(query.list[i]) < limit && i<length(query.list)) {
      i<-i+1
      size<-size+nchar(query.list[i])
    }
    
    #print(i)
    
    query<-paste(query.list[begin:i], collapse="")
    query<-query.builder(query)
    results<-rbind(results, SPARQL(endpoint,query)$results)
    
  }
  
  return(results)
}

load.classification.links<-function(endpoint, file.xml){
  
  classification.links.xml<-xmlTreeParse(file.xml, useInternalNodes = TRUE)
  top<-xmlRoot(classification.links.xml)
  
  classification.source.id<-xmlGetAttr(top[["LinkSet"]][["ClassificationLink"]][["LinkSource"]], "id")
  classification.target.id<-xmlGetAttr(top[["LinkSet"]][["ClassificationLink"]][["LinkTarget"]], "id")

  link.list<-xpathApply(top, "//LinkSet/ItemLink", function (x) xmlSApply(x,xmlGetAttr, name="id"))
  
  query.list <- lapply(link.list, function (x) paste("<", classification.source.id, "_", x[1], ">", " skos:related ", "<", classification.target.id, "_", x[2], "> .", sep=""))
  print(length(query.list))
  insert.output<-large.query.handler(endpoint, query.list, function(x) paste("INSERT DATA {", x, "}"))

}

endpoint<-"https://dydra.com/luca-gramaglia/ramon_classifications/sparql"

folder<-"/home/luca/Desktop/R-SPARQL/Links"

files.xml <- list.files(folder, pattern = "\\.xml$", full.names=TRUE, recursive=FALSE)

for (i in files.xml){
  print(i)
  load.classification.links(endpoint, i)
}