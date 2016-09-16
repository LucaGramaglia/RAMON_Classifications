library(shiny) 
library(SPARQL)
library(igraph)

endpoint<-"https://dydra.com/luca-gramaglia/ramon_classifications/sparql"

concept.scheme.nodes.query<-"SELECT ?class ?description WHERE { ?class rdf:type skos:ConceptScheme . ?class skos:prefLabel ?description }"
concept.scheme.nodes<-SPARQL(endpoint, concept.scheme.nodes.query)$results
concept.scheme.nodes<-cbind(concept.scheme.nodes, sub('<','', sub('>','',concept.scheme.nodes[,1])))
colnames(concept.scheme.nodes)[3]<-"label"

concept.scheme.edges.query<-"SELECT DISTINCT ?class1 ?class2 WHERE{ ?x skos:inScheme ?class1 . ?y skos:inScheme ?class2 . ?x skos:related ?y }"
concept.scheme.edges<-SPARQL(endpoint, concept.scheme.edges.query)$results

class_net <- graph_from_data_frame(d=concept.scheme.edges, vertices=concept.scheme.nodes)

ui <- fluidPage(
  
  plotOutput(outputId = "Class_Network"),
  
  selectInput(inputId ="ConceptScheme", "Choose classification", as.character(concept.scheme.nodes[,3])),
  
  selectInput(inputId ="Item", "Choose item", choices=NULL),
  
  plotOutput(outputId = "Network")
  
) 

server <- function(input, output, session){
  
    observeEvent(input$ConceptScheme, {
    
    concept.scheme<-paste("<", input$ConceptScheme, ">", sep="")
    code.query<-paste("SELECT ?x WHERE {", concept.scheme, "skos:hasTopConcept ?y. ?y skos:notation ?x }")
    code.list<-as.character(SPARQL(endpoint, code.query)$results[1,])
    
    updateSelectInput(session, "Item", choices = code.list)
    
  })

  output$Class_Network<-renderPlot({ plot(class_net, edge.arrow.size=.4)})
   
  output$Network<-renderPlot({
    
    edges.query<- paste("SELECT ?x ?y WHERE { ?u skos:notation \"", input$Item, "\". ?u skos:inScheme <", isolate(input$ConceptScheme), "> . ?u (skos:related|^skos:related)+ ?y . ?x (skos:related|^skos:related) ?y }", sep="")
    edges<-SPARQL(endpoint, edges.query)$results
    
    if (nrow(edges)!=0){
      nodes.list<-unique(edges[,1])
      nodes.list<-lapply(nodes.list, function (x) { paste("(", x, ")", sep="")} )
      nodes.list<-paste(nodes.list, collapse="")
    
      nodes.query<-paste("SELECT ?x ?y ?z WHERE{ VALUES (?x) {", nodes.list, "} ?x skos:notation ?y. ?x skos:inScheme ?z }")
      nodes<-SPARQL(endpoint, nodes.query)$results
    
      net <- graph_from_data_frame(d=edges, vertices=nodes)
    
      plot(net, edge.arrow.size=.4)
    }
  })
  
}

shinyApp(ui = ui, server = server)
