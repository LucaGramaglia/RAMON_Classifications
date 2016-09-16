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
  print(length(concept.query.list))
  insert.output<-large.query.handler(endpoint, concept.query.list, function(x) paste("INSERT DATA {", x, "}"))
}

endpoint<-"https://dydra.com/luca-gramaglia/ramon_classifications/sparql"

folder<-"/home/luca/Desktop/R-SPARQL/Classifications"
classification.list<-dir(folder, full.names=TRUE, recursive = FALSE)

for (i in classification.list) {
  
  files.csv <- list.files(i, pattern = "\\.csv$", full.names=TRUE, recursive=FALSE)
  files.xml <- list.files(i, pattern = "\\.xml$", full.names=TRUE, recursive=FALSE)
  
  if (length(files.csv)==1 && length(files.xml)==1){
  
    file.csv<-files.csv[[1]]
    file.xml<-files.xml[[1]]
    
    print(i)

    load.classification(endpoint, file.csv, file.xml)
  
  }
  
}