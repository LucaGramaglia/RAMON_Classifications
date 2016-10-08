library(shiny) 
library(SPARQL)
library(igraph)
library(networkD3)


endpoint<-"https://dydra.com/luca-gramaglia/ramon_classifications/sparql"

concept.scheme.nodes.query<-"SELECT ?class ?description WHERE { ?class rdf:type skos:ConceptScheme . ?class skos:prefLabel ?description }"
concept.scheme.nodes<-SPARQL(endpoint, concept.scheme.nodes.query)$results
concept.scheme.nodes<-cbind(concept.scheme.nodes, sub('<','', sub('>','',concept.scheme.nodes[,1])))
colnames(concept.scheme.nodes)[3]<-"label"

concept.scheme.edges.query<-"SELECT DISTINCT ?class1 ?class2 WHERE{ ?x skos:inScheme ?class1 . ?y skos:inScheme ?class2 . ?x skos:related ?y }"
concept.scheme.edges<-SPARQL(endpoint, concept.scheme.edges.query)$results

class_net <- graph_from_data_frame(d=concept.scheme.edges, vertices=concept.scheme.nodes)
group1<-rep("Classification on activities", 4)
group2<-rep("Classification on products",5)
group3<-rep("Classification on activities",1)
groupp<-c(group1,group2,group3)

ui <- fluidPage(
  titlePanel("Open Linked Metadata"),
  mainPanel(
    tabsetPanel(
      tabPanel("Classification network", forceNetworkOutput(outputId = "Class_Network")),
      
  
  tabPanel("Correspondence network",
  selectInput(inputId ="ConceptScheme", "Choose classification", as.character(concept.scheme.nodes[,3])),
  
  selectInput(inputId ="Item", "Choose item", choices=NULL),
  
  forceNetworkOutput(outputId = "Network")
  ))  
) )

server <- function(input, output, session){
  
  observeEvent(input$ConceptScheme, {
    
    concept.scheme<-paste("<", input$ConceptScheme, ">", sep="")
    code.query<-paste("SELECT ?x WHERE {", concept.scheme, "skos:hasTopConcept ?y. ?y skos:notation ?x }")
    code.list<-as.character(SPARQL(endpoint, code.query)$results[1,])
    
    updateSelectInput(session, "Item", choices = code.list)
    
  })


    
  output$Class_Network<-renderForceNetwork({ 
    class_netd3 <- igraph_to_networkD3(class_net, group=groupp)
    forceNetwork(Links = class_netd3$links, Nodes = class_netd3$nodes, 
                 Source = 'source', Target = 'target', 
                 NodeID = 'name', Group = 'group', fontSize = 22, opacity = 0.8,
                 legend = TRUE)
  
    })
  output$Network<-renderForceNetwork({
    
    edges.query<- paste("SELECT ?x ?y WHERE { ?u skos:notation \"", input$Item, "\". ?u skos:inScheme <", isolate(input$ConceptScheme), "> . ?u (skos:related|^skos:related)+ ?y . ?x (skos:related|^skos:related) ?y }", sep="")
    edges<-SPARQL(endpoint, edges.query)$results
    
    if (nrow(edges)!=0){
      nodes.list<-unique(edges[,1])
      nodes.list<-lapply(nodes.list, function (x) { paste("(", x, ")", sep="")} )
      nodes.list<-paste(nodes.list, collapse="")
      
      nodes.query<-paste("SELECT ?x ?y ?z WHERE{ VALUES (?x) {", nodes.list, "} ?x skos:notation ?y. ?x skos:inScheme ?z }")
      nodes<-SPARQL(endpoint, nodes.query)$results
      
      net <- graph_from_data_frame(d=edges, vertices=nodes)
      groupe<-c(1:vcount(net))                                   #here should be the name of classification to which node of the code belongs
      netd3 <- igraph_to_networkD3(net, group=groupe)

      forceNetwork(Links = netd3$links, Nodes = netd3$nodes, 
                   Source = 'source', Target = 'target', 
                   NodeID = 'name', Group = 'group', fontSize = 22, opacity = 0.8,
                   legend = TRUE)      

    }
  })
  
}

shinyApp(ui = ui, server = server)
