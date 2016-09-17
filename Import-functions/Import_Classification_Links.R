library(XML)
source("Import_Classification_Functions.R", chdir=T)

endpoint<-"https://dydra.com/luca-gramaglia/ramon_classifications/sparql"

folder<-"Links"

files.xml <- list.files(folder, pattern = "\\.xml$", full.names=TRUE, recursive=FALSE)

for (i in files.xml) {
  print(i)
  load.classification.links(endpoint, i)
}