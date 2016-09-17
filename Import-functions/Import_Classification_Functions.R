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
    
    query<-paste(query.list[begin:i], collapse="")
    query<-query.builder(query)
    results<-rbind(results, SPARQL(endpoint,query)$results)
    
  }
  
  return(results)
}

form.concept.query<-function(file.row, classification.id){
  
  code.id<-paste("<", classification.id, "_", file.row["Code"], ">", sep="")
  parent.id<-paste("<", classification.id, "_", file.row["Parent"], ">", sep="")
  classification.id<-paste("<", classification.id, ">", sep="")
  description<-paste("\"", gsub("\"", "", file.row["Description"]), "\"", sep="")
  
  query<-paste(code.id, "rdf:type skos:Concept .")
  query<-paste(query, code.id, "skos:prefLabel", description, ".")
  query<-paste(query, code.id, "skos:inScheme", classification.id, ".")
  
  if(!(is.na(file.row["Parent"]) || file.row["Parent"]=="")) query<-paste(query, code.id, "skos:narrower", parent.id, ".")
  
  if("Code.1" %in% colnames(file.row) ){
    if(is.null(file.row["Code.1"])) query<-paste(query, code.id, "skos:notation", paste("\"", file.row["Code.1"], "\"", sep=""), ".")
  }
  else query<-paste(query, code.id, "skos:notation", paste("\"", file.row["Code"], "\"", sep=""), ".")
  
  if(file.row["Level"]=="1") query<-paste(query, classification.id, "skos:hasTopConcept", code.id, ".")
  
  return(query)
}

form.classification.query<-function(classification.id, classification.label){
  
  classification.id<-paste("<", classification.id, ">", sep="")
  
  query<-"INSERT DATA {"
  query<-paste(query, classification.id, "rdf:type skos:ConceptScheme .")
  query<-paste(query, classification.id, "skos:prefLabel", paste("\"", classification.label, "\"", sep=""), ".")
  query<-paste(query, "}")
  
  return(query)
}

load.classification<-function(endpoint, file.csv, file.xml){
  
  classification.xml<-xmlTreeParse(file.xml, useInternalNodes = TRUE)
  top<-xmlRoot(classification.xml)
  classification.id<-xmlGetAttr(top[["Classification"]], "id")
  classification.label<-xmlValue(top[["Classification"]][["Label"]][["LabelText"]])
  
  classification.csv<-read.csv(file.csv, sep=";", colClasses = "character")
  
  classification.query<-form.classification.query(classification.id, classification.label)
  SPARQL(endpoint, classification.query)
  
  concept.query.list<-apply(classification.csv, 1, form.concept.query, classification.id=classification.id)
  
  insert.output<-large.query.handler(endpoint, concept.query.list, function(x) paste("INSERT DATA {", x, "}"))
}

load.classification.links<-function(endpoint, file.xml){
  
  classification.links.xml<-xmlTreeParse(file.xml, useInternalNodes = TRUE)
  top<-xmlRoot(classification.links.xml)
  
  classification.source.id<-xmlGetAttr(top[["LinkSet"]][["ClassificationLink"]][["LinkSource"]], "id")
  classification.target.id<-xmlGetAttr(top[["LinkSet"]][["ClassificationLink"]][["LinkTarget"]], "id")
  
  link.list<-xpathApply(top, "//LinkSet/ItemLink", function (x) xmlSApply(x,xmlGetAttr, name="id"))
  
  query.list <- lapply(link.list, function (x) paste("<", classification.source.id, "_", x[1], ">", " skos:related ", "<", classification.target.id, "_", x[2], "> .", sep=""))
  
  insert.output<-large.query.handler(endpoint, query.list, function(x) paste("INSERT DATA {", x, "}"))
  
}
