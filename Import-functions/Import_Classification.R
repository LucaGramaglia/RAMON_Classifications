library(XML)
source("Import_Classification_Functions.R", chdir=T)

endpoint<-"https://dydra.com/luca-gramaglia/ramon_classifications/sparql"

folder<-"Classifications"
classification.list<-dir(folder, full.names=TRUE, recursive = FALSE)

for (i in classification.list) {
  
  print(i)
  
  files.csv <- list.files(i, pattern = "\\.csv$", full.names=TRUE, recursive=FALSE)
  files.xml <- list.files(i, pattern = "\\.xml$", full.names=TRUE, recursive=FALSE)
  
  if (length(files.csv)==1 && length(files.xml)==1){
    
    file.csv<-files.csv[[1]]
    file.xml<-files.xml[[1]]
    load.classification(endpoint, file.csv, file.xml)
    
  }
  
}