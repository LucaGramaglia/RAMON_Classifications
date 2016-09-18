library("igraph") 
library("tnet")
library("bipartite")
library(networkD3)
library(shiny)


data<-read.table("D:\\MethNet.txt",header=T,quote=NULL, comment="",sep="\t",na.strings=c("","0"))

n<-nrow(data)
m<-ncol(data)

data<-sapply(data, as.character)
data[is.na(data)]<-0
for(j in 1:m) {
for(i in 1:n) {

if(data[i,j]=="During my studies") {data[i,j]<-1} 
if(data[i,j]=="Working in Eurostat") {data[i,j]<-1}
if(data[i,j]=="Working elsewhere") {data[i,j]<-1}
if(data[i,j]=="Writing, editoring or reviewing articles") {data[i,j]<-1}
if(data[i,j]=="Personal interest") {data[i,j]<-1}


if(data[i,j]=="\"During my studies;Working in Eurostat\"") {data[i,j]<-2}
if(data[i,j]=="\"During my studies;Working elsewhere\"") {data[i,j]<-2}
if(data[i,j]=="\"During my studies;Writing, editoring or reviewing articles\"") {data[i,j]<-2}
if(data[i,j]=="\"During my studies;Personal interest\"") {data[i,j]<-2}

if(data[i,j]=="\"Working in Eurostat;Working elsewhere\"") {data[i,j]<-2}
if(data[i,j]=="\"Working in Eurostat;Writing, editoring or reviewing articles\"") {data[i,j]<-2}

if(data[i,j]=="\"Working elsewhere;Writing, editoring or reviewing articles\"") {data[i,j]<-2}

if(data[i,j]=="\"Personal interest;Working in Eurostat\"") {data[i,j]<-2}
if(data[i,j]=="\"Personal interest;Working elsewhere\"") {data[i,j]<-2}


if(data[i,j]=="\"During my studies;Working in Eurostat;Working elsewhere\"") {data[i,j]<-3}
if(data[i,j]=="\"During my studies;Working in Eurostat;Writing, editoring or reviewing articles\"") {data[i,j]<-3}

if(data[i,j]=="\"During my studies;Personal interest;Working in Eurostat\"") {data[i,j]<-3}
if(data[i,j]=="\"During my studies;Personal interest;Working elsewhere\"") {data[i,j]<-3}

if(data[i,j]=="\"During my studies;Working elsewhere;Writing, editoring or reviewing articles\"") {data[i,j]<-3}

if(data[i,j]=="\"Working in Eurostat;Working elsewhere;Writing, editoring or reviewing articles\"") {data[i,j]<-3}

if(data[i,j]=="\"Personal interest;Working in Eurostat;Working elsewhere\"") {data[i,j]<-3}


if(data[i,j]=="\"During my studies;Personal interest;Working in Eurostat;Working elsewhere\"") {data[i,j]<-4}

if(data[i,j]=="\"During my studies;Working in Eurostat;Working elsewhere;Writing, editoring or reviewing articles\"") {data[i,j]<-4}
}
}


MA <- as.matrix(data[,3:30])
MAsatsr<- as.matrix(data[,c(12,23,29,45)])


for(i in 1:n){
if(data[i,58]=="No") {data[i,58]<-rgb(1,0.2,0,1)}
if(data[i,58]=="Yes, but only as a informed/consulted member") {data[i,58]<-rgb(0,0.5,1,1)}
if(data[i,58]=="Yes, I want to take up an active role") {data[i,58]<-rgb(0,.7,.1,1)}
}

SD <- as.matrix(data[,32:43])
ST <- as.matrix(data[,45:54])
MA_SD_ST<-cbind(MA,SD,ST)
m_network<-MA_SD_ST

n<-nrow(MA)
m<-ncol(MA)

colnames(MA)<-c("Big Data Analytics, Data Mining and Data Science", "Data Analysis", "Data Dissemination", 
  "Data Integration", "Data Processing", "Data Validation", "Data Warehousing", "Descriptive Statistics", 
  "Design and optimisation of statistical processes", "Econometrics", "Enterprise Architecture", "Indices", 
  "Indicators, composite and synthetic", "Inferential Statistics", "Information models and standards",
  "Linear Algebra", "Metadata models and standards", "Micro-data access", "Model-based Estimation", "Quality",
  "Seasonal adjustment", "Statistical confidentiality", "Statistical Software", "Survey Methodology", "Sampling",
  "Administrative Data", "Time Series", "Other")
#row.names(ST)<-row.names(SD)<-row.names(MA) <- row.names(MAsatsr) <-data[,1]
row.names(ST)<-row.names(SD)<-row.names(MA) <- row.names(MAsatsr) <- c(1:n)
colnames(SD)<-c("Agricultural and Fisheries Statistics", "Business Statistics","Demography", "Environmental Statistics", "International trade", "National Accounts", "Prices",
                 "Science and technology", "Social Statistics", "Transport", "Energy Statistics", "Other")
colnames(ST)<-c("R", "SAS", "Eviews", "Stata", "Matlab", "Hadoop", "Python", "SPSS", "Java", "Other") 


im_network<-graph.incidence(m_network, mode=c("all"), weighted=TRUE )
iMA <- graph.incidence(MA, mode=c("all"), weighted=TRUE )
iSD <- graph.incidence(SD, mode=c("all"), weighted=TRUE )
iST <- graph.incidence(ST, mode=c("all"), weighted=TRUE )


iMAsatsr <- graph.incidence(MAsatsr, mode=c("all"), weighted=TRUE)
MA<-as_incidence_matrix(iMA)
V(iMA)$color[1:n] <- data[,58]
V(iMA)$color[(n+1):(n+1+m)] <- rgb(1,1,0,0.5)
V(iMA)$label <- V(iMA)$name
V(iMA)$label.color <- rgb(0,0,0,1)
V(iMA)$label.cex <- degree(iMA)*0.1

V(iMA)$label.cex[degree(iMA)<=5] <- 0.4
V(iMA)$label.cex[degree(iMA)>5] <- 0.6
V(iMA)$label.cex[degree(iMA)>10] <- 0.8
V(iMA)$label.cex[degree(iMA)>15] <- 1
V(iMA)$label.cex[degree(iMA)>20] <- 1.2
V(iMA)$label.cex <- degree(iMA)/( max(degree(iMA))/2)+0.3
V(iMA)$size <- (degree(iMA)/2)+2
V(iMA)$frame.color <- rgb(0.5,0.5,0.5,0.5)
E(iMA)$color <- rgb(.5,.5,.5,.5)


V(iMAsatsr)$color[1:n] <- data[,58]
V(iMAsatsr)$color[(n+1):(n+4)] <- rgb(1,1,0,.5)
V(iMAsatsr)$label <- V(iMAsatsr)$name
V(iMAsatsr)$label.color <- rgb(0,0,0,1)
V(iMAsatsr)$label.cex <- degree(iMAsatsr)/( max(degree(iMAsatsr)))+0.2
V(iMAsatsr)$size <- degree(iMAsatsr)+2

V(iMAsatsr)$frame.color <- rgb(0.5,0.5,0.5,0.5)
a<-0.5
E(iMAsatsr)$color <- rgb(.5,.5,.5,a)

group1<-rep('Staff',67)
group3<-rep('Methodological area',28)
groupMA<-c(group1,group3)
group1<-rep('Staff',67)
group3<-rep('Statistical domain',12)
groupSD<-c(group1,group3)
group1<-rep('Staff',67)
group3<-rep('Statistical tool',10)
groupST<-c(group1,group3)

iMA_d33 <- igraph_to_networkD3(iMA, group = groupMA)
iSD_d3 <- igraph_to_networkD3(iSD, group = groupSD)
iST_d3 <- igraph_to_networkD3(iST, group = groupST)

#linksMA<-iMA_d3$links
#nodesMA<-iMA_d3$nodes
#deg<-degree(iMA)
#nodesMA<-cbind(nodesMA,deg)
#linksSD<-iSD_d3$links
#nodesSD<-iSD_d3$nodes
#deg<-degree(iSD)
#nodesSD<-cbind(nodesSD,deg)
#linksST<-iST_d3$links
#nodesST<-iST_d3$nodes
#deg<-degree(iST)
#nodesST<-cbind(nodesST,deg)
#m<-nodesMA

forceNetwork(Links = iMA_d33$links, Nodes = iMA_d33$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 22, opacity = 0.8,
             legend = TRUE)

forceNetwork(Links = iSD_d3$links, Nodes = iSD_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 22, opacity = 0.8,
             legend = TRUE)
forceNetwork(Links = iST_d3$links, Nodes = iST_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 22, opacity = 0.8,
             legend = TRUE)

iMA_d3<-iMA_d33
server <- function(input, output, session) {
 

   output$MethArea <- renderForceNetwork({
    
     if (input$name != "") {
       iMA_d3<-iMA_d33
       iMA_d3$nodes<- iMA_d3$nodes[which(iMA_d3$nodes$name == input$name),]
       iMA_d3$links<- iMA_d3$links[which(iMA_d3$links$target == rownames(iMA_d3$nodes)),]
     }
     

    forceNetwork(Links = iMA_d3$links, Nodes = iMA_d3$nodes, 
                 Source = 'source', Target = 'target', 
                 NodeID = 'name', Group = 'group', fontSize = 22, opacity = 0.8,
                 legend = TRUE)
    })
    
  output$StatDom <- renderForceNetwork({
    forceNetwork(Links = iSD_d3$links, Nodes = iSD_d3$nodes, 
                 Source = 'source', Target = 'target', 
                 NodeID = 'name', Group = 'group', fontSize = 22, opacity = 0.8,
                 legend = TRUE)
  })

  output$StatTool <- renderForceNetwork({
    forceNetwork(Links = iST_d3$links, Nodes = iST_d3$nodes, 
                 Source = 'source', Target = 'target', 
                 NodeID = 'name', Group = 'group', fontSize = 22, opacity = 0.8,
                 legend = TRUE)
  })
}




ui <- shinyUI(fluidPage(
  
  titlePanel("ESTAT Methodological Network"),
  
#  sidebarLayout(
#    sidebarPanel(
 #     sliderInput("opacity", "Opacity", 0.6, min = 0.1,
 #                 max = 1, step = .1)
  #  ),
    mainPanel(
      tabsetPanel(
        tabPanel("Methodological Areas", forceNetworkOutput("MethArea"),
                   selectInput("name", "Select methodological area", 
                               c("",as.character(iMA_d33$nodes$name[68:90]))
                   )),
        


        tabPanel("Statistical Domains", forceNetworkOutput("StatDom"),
                 selectInput(inputId = "name", "Select statistical domain", 
                             c("",as.character(iSD_d3$nodes$name[68:79])) 
                 )),
        tabPanel("Statistical Tools", forceNetworkOutput("StatTool"), 
                 selectInput(inputId = "name", "Select statistical tool", 
                             choices = c("",as.character(iST_d3$nodes$name[68:77])) 
                 ))
        
        
        )
      )
    )
)


#### Run ####
shinyApp(ui = ui, server = server)


pdf("iMA.pdf")
plot(iMA, layout=layout.fruchterman.reingold, main="Two-mode network of methodological areas by competent ESTAT staff")
plot(iMA, layout=layout.kamada.kawai, main="Two-mode network of methodological areas by competent ESTAT staff")
plot(iMA, layout=layout.auto, main="Two-mode network of methodological areas by competent ESTAT staff")
plot(iMA, layout=layout.lgl, main="Two-mode network of methodological areas by competent ESTAT staff")
plot(iMA, layout=layout.random, main="Two-mode network of methodological areas by competent ESTAT staff")
plot(iMA, layout=layout.circle, main="Two-mode network of methodological areas by competent ESTAT staff")
plot(iMA, layout=layout.sphere, main="Two-mode network of methodological areas by competent ESTAT staff")
plot(iMA, layout=layout.graphopt)
dev.off()

data<-cbind(data,c(1:n))
data_ncontribution<-data_consultantcontribution<-data_activecontribution<-numeric()
for(i in 1:n){
#if(data[i,58]==rgb(1,0.2,0,1)) {data_ncontribution<-rbind(data_ncontribution,data[i,])}
if(data[i,58]==rgb(0,0.5,1,1)) {data_consultantcontribution<-rbind(data_consultantcontribution,data[i,])}
if(data[i,58]==rgb(0,.7,.1,1)) {data_activecontribution<-rbind(data_activecontribution,data[i,])}
}
#MA_ncontribution <- as.matrix(data_ncontribution[,3:30])
MA_consultantcontribution <- as.matrix(data_consultantcontribution[,3:30])
MA_activecontribution <- as.matrix(data_activecontribution[,3:30])
ncc<-nrow(MA_consultantcontribution)
nac<-nrow(MA_activecontribution)
mMA<-ncol(MA)
#row.names(MA_ncontribution) <- data_ncontribution[,60]
row.names(MA_consultantcontribution) <-data_consultantcontribution[,60]
row.names(MA_activecontribution) <-data_activecontribution[,60]


iMA_consultantcontribution <- graph.incidence(MA_consultantcontribution, mode=c("all"), weighted=TRUE )
iMA_activecontribution <- graph.incidence(MA_activecontribution, mode=c("all"), weighted=TRUE  )

V(iMA_consultantcontribution)$color[1:ncc] <- rgb(0,0.5,1,1)
V(iMA_consultantcontribution)$color[(ncc+1):(ncc+1+mMA)] <- rgb(1,1,0,.5)
V(iMA_consultantcontribution)$label <- V(iMA_consultantcontribution)$name
V(iMA_consultantcontribution)$label.color <- rgb(0,0,0,1)
V(iMA_consultantcontribution)$label.cex <- degree(iMA_consultantcontribution)/( max(degree(iMA_consultantcontribution))/2)+0.1
V(iMA_consultantcontribution)$size <- (degree(iMA_consultantcontribution)/2)+1
V(iMA_consultantcontribution)$frame.color <- rgb(0.5,0.5,0.5,0.5)
E(iMA_consultantcontribution)$color <- rgb(.5,.5,.5,.5)

V(iMA_activecontribution)$color[1:nac] <- rgb(0,.7,.1,1)
V(iMA_activecontribution)$color[(nac+1):(nac+1+mMA)] <- rgb(1,1,0,.5)
V(iMA_activecontribution)$label <- V(iMA_activecontribution)$name
V(iMA_activecontribution)$label.color <- rgb(0,0,0,1)
V(iMA_activecontribution)$label.cex <- degree(iMA_activecontribution)/( max(degree(iMA_activecontribution)))+0.3
V(iMA_activecontribution)$size <- (degree(iMA_activecontribution)/2)+2
V(iMA_activecontribution)$frame.color <- rgb(0.5,0.5,0.5,0.5)
E(iMA_activecontribution)$color <- rgb(.5,.5,.5,.5)

pdf("iMA_contribution.pdf")
plot(iMA_activecontribution, layout=layout.fruchterman.reingold)
plot(iMA_activecontribution, layout=layout.fruchterman.reingold)

plot(iMA_activecontribution, layout=layout.fruchterman.reingold)
plot(iMA_activecontribution, layout=layout.fruchterman.reingold)

plot(iMA_activecontribution, layout=layout.auto)
plot(iMA_activecontribution, layout=layout.lgl)
plot(iMA_activecontribution, layout=layout.random)
plot(iMA_activecontribution, layout=layout.circle)
plot(iMA_activecontribution, layout=layout.sphere)
plot(iMA_activecontribution, layout=layout.kamada.kawai)
plot(iMA_activecontribution, layout=layout.reingold.tilford)
plot(iMA_activecontribution, layout=layout.graphopt)
dev.off()


#FN_ncontribution <- cbind(data_ncontribution[,3:30],data_ncontribution[,45:54])
FN_consultantcontribution <- cbind(data_consultantcontribution[,3:30],data_consultantcontribution[,45:54])
FN_activecontribution <- cbind(data_activecontribution[,3:30],data_activecontribution[,45:54])
colnames(FN_consultantcontribution)<-colnames(FN_activecontribution)<-c("Big Data Analytics, Data Mining and Data Science", "Data Analysis", "Data Dissemination", 
			"Data Integration", "Data Processing", "Data Validation", "Data Warehousing", "Descriptive Statistics", "Design and optimisation of statistical processes",
			"Econometrics", "Enterprise Architecture", "Indices", "Indicators, composite and synthetic", "Inferential Statistics", "Information models and standards",
			"Linear Algebra", "Metadata models and standards", "Micro-data access", "Model-based Estimation", "Quality", "Seasonal adjustment", "Statistical confidentiality",
			"Statistical Software", "Survey Methodology", "Sampling", "Administrative Data", "Time Series", "Other MA", 
			"R", "SAS", "Eviews", "Stata", "Matlab", "Hadoop", "Python", "SPSS", "Java", "Other ST")


ncc<-nrow(FN_consultantcontribution)
nac<-nrow(FN_activecontribution)
mFN<-sum(ncol(MA),ncol(ST))
#row.names(FN_ncontribution) <- data_ncontribution[,60]
row.names(FN_consultantcontribution) <-data_consultantcontribution[,60]
row.names(FN_activecontribution) <-data_activecontribution[,60]


iFN_consultantcontribution <- graph.incidence(FN_consultantcontribution, mode=c("all"), weighted=TRUE )
iFN_activecontribution <- graph.incidence(FN_activecontribution, mode=c("all"), weighted=TRUE  )

V(iFN_consultantcontribution)$color[1:ncc] <- rgb(0,0.5,1,1)
V(iFN_consultantcontribution)$color[(ncc+1):(ncc+1+mFN)] <- rgb(1,1,0,.5)
V(iFN_consultantcontribution)$label <- V(iFN_consultantcontribution)$name
V(iFN_consultantcontribution)$label.color <- rgb(0,0,0,1)
V(iFN_consultantcontribution)$label.cex <- degree(iFN_consultantcontribution)/( max(degree(iFN_consultantcontribution))/2)+0.1
V(iFN_consultantcontribution)$size <- (degree(iFN_consultantcontribution)/2)+1
V(iFN_consultantcontribution)$frame.color <- rgb(0.5,0.5,0.5,0.5)
E(iFN_consultantcontribution)$color <- rgb(.5,.5,.5,.5)

V(iFN_activecontribution)$color[1:nac] <- rgb(0,.7,.1,1)
V(iFN_activecontribution)$color[(nac+1):(nac+1+ncol(MA))] <- rgb(1,1,0,1)
V(iFN_activecontribution)$color[(nac+1+ncol(MA)):(nac+1+ncol(MA)+ncol(ST))] <- rgb(1,1,0,.3)
V(iFN_activecontribution)$label <- V(iFN_activecontribution)$name
V(iFN_activecontribution)$label.color <- rgb(0,0,0,1)
V(iFN_activecontribution)$label.cex <- degree(iFN_activecontribution)/( max(degree(iFN_activecontribution))+10)+0.4
V(iFN_activecontribution)$size <- (degree(iFN_activecontribution)/2)+2
V(iFN_activecontribution)$frame.color <- rgb(0.5,0.5,0.5,0.5)
E(iFN_activecontribution)$color <- rgb(.5,.5,.5,.5)

pdf("iFN_contribution_fullnetwork.pdf")
plot(iFN_activecontribution, layout=layout.fruchterman.reingold)
plot(iFN_activecontribution, layout=layout.fruchterman.reingold)

plot(iFN_activecontribution, layout=layout.fruchterman.reingold)
plot(iFN_activecontribution, layout=layout.fruchterman.reingold)

plot(iFN_activecontribution, layout=layout.auto)
plot(iFN_activecontribution, layout=layout.lgl)
plot(iFN_activecontribution, layout=layout.random)
plot(iFN_activecontribution, layout=layout.circle)
plot(iFN_activecontribution, layout=layout.sphere)
plot(iFN_activecontribution, layout=layout.kamada.kawai)
plot(iFN_activecontribution, layout=layout.reingold.tilford)
plot(iFN_activecontribution, layout=layout.graphopt)

iFN_activecontribution <- delete.vertices(iFN_activecontribution, V(iFN_activecontribution)[ degree(iFN_activecontribution)==0 ])
plot(iFN_activecontribution, layout=layout.fruchterman.reingold)
plot(iFN_activecontribution, layout=layout.fruchterman.reingold)

plot(iFN_activecontribution, layout=layout.fruchterman.reingold)
plot(iFN_activecontribution, layout=layout.fruchterman.reingold)

plot(iFN_activecontribution, layout=layout.auto)
plot(iFN_activecontribution, layout=layout.lgl)
plot(iFN_activecontribution, layout=layout.random)
plot(iFN_activecontribution, layout=layout.circle)
plot(iFN_activecontribution, layout=layout.sphere)
plot(iFN_activecontribution, layout=layout.kamada.kawai)
plot(iFN_activecontribution, layout=layout.reingold.tilford)
plot(iFN_activecontribution, layout=layout.graphopt)
dev.off()

MASDR <- cbind(MA,SD,ST[,1])
MASDR1<-cbind(MASDR,c(1:nrow(MASDR)))
MASDR1 <- as.matrix(MASDR1[MASDR1[,41]>0,])
MASDR <- as.matrix(MASDR[MASDR[,41]>0,])


colnames(MASDR)<-c("Big Data Analytics, Data Mining and Data Science", "Data Analysis", "Data Dissemination", 
			"Data Integration", "Data Processing", "Data Validation", "Data Warehousing", "Descriptive Statistics", "Design and optimisation of statistical processes",
			"Econometrics", "Enterprise Architecture", "Indices", "Indicators, composite and synthetic", "Inferential Statistics", "Information models and standards",
			"Linear Algebra", "Metadata models and standards", "Micro-data access", "Model-based Estimation", "Quality", "Seasonal adjustment", "Statistical confidentiality",
			"Statistical Software", "Survey Methodology", "Sampling", "Administrative Data", "Time Series", "Other MA", "Agricultural and Fisheries Statistics", "Business Statistics",
			"Demography", "Environmental Statistics", "International trade", "National Accounts", "Prices", "Science and technology", "Social Statistics", "Transport", 
			"Energy Statistic", "Other SD", "R")

n<-nrow(MASDR)
m<-ncol(MASDR)

#row.names(MASDR)<-data[,1]
row.names(MASDR) <- MASDR1[,42]
iMASDR<-graph.incidence(MASDR, mode=c("all"), weighted=TRUE )

MASDR<-as_incidence_matrix(iMASDR)
MASDR <- t(MASDR) %*% MASDR
 
iMASDR <- graph.adjacency(MASDR,, mode = 'undirected')
E(iMASDR)$weight <- count.multiple(iMASDR)
iMASDR <- simplify(iMASDR)

V(iMASDR)$color <- rgb(1,1,0,0.5)
#V(iMASDR)$color[(n+1):(n+1+m)] <- rgb(1,1,0,0.5)
V(iMASDR)$label <- V(iMASDR)$name
V(iMASDR)$label.color <- rgb(0,0,0,1)
V(iMASDR)$label.cex <- degree(iMASDR)*0.1

V(iMASDR)$label.cex[degree(iMASDR)<=5] <- 0.4
V(iMASDR)$label.cex[degree(iMASDR)>5] <- 0.6
V(iMASDR)$label.cex[degree(iMASDR)>10] <- 0.8
V(iMASDR)$label.cex[degree(iMASDR)>15] <- 1
V(iMASDR)$label.cex[degree(iMASDR)>20] <- 1.2
V(iMASDR)$label.cex <- degree(iMASDR)/( max(degree(iMASDR)))+0.5
V(iMASDR)$size <- (degree(iMASDR)/2)+2
V(iMASDR)$frame.color <- rgb(0.5,0.5,0.5,0.5)

pdf("iMASDR.pdf")
plot(iMASDR, layout=layout.fruchterman.reingold, main="Network of methodological areas and statistical domains")
plot(iMASDR, layout=layout.kamada.kawai, main="Network of methodological areas and statistical domains")
plot(iMASDR, layout=layout.auto, main="Network of methodological areas and statistical domains")
plot(iMASDR, layout=layout.lgl, main="Network of methodological areas and statistical domains")
plot(iMASDR, layout=layout.random, main="Network of methodological areas and statistical domains")
plot(iMASDR, layout=layout.circle, main="Network of methodological areas and statistical domains")
plot(iMASDR, layout=layout.sphere, main="Network of methodological areas and statistical domains")
plot(iMASDR, layout=layout.reingold.tilford, main="Network of methodological areas and statistical domains")
plot(iMASDR, layout=layout.graphopt, main="Network of methodological areas and statistical domains")

dev.off()

NETWact <- FN_activecontribution

n<-nrow(NETWact)
m<-ncol(NETWact)

#row.names(NETWact)<-data[,1]
#row.names(NETWact) <- c(1:n)

iNETWact<-graph.incidence(NETWact, mode=c("all"), weighted=TRUE )

NETWact<-as_incidence_matrix(iNETWact)
compNETWact <- t(NETWact) %*% NETWact
membNETWact <- NETWact %*% t(NETWact)
 
icompNETWact <- graph.adjacency(compNETWact,, mode = 'undirected')
E(icompNETWact)$weight <- count.multiple(icompNETWact)
icompNETWact <- simplify(icompNETWact)

V(icompNETWact)$color <- rgb(1,1,0,0.5)
V(icompNETWact)$label <- V(icompNETWact)$name
V(icompNETWact)$label.color <- rgb(0,0,0,1)
V(icompNETWact)$label.cex <- degree(icompNETWact)*0.1
a<-(degree(icompNETWact)/max(degree(icompNETWact)))
E(icompNETWact)$color <- rgb(0.5,0.5,0.5,a)
V(icompNETWact)$label.cex <- degree(icompNETWact)/20
V(icompNETWact)$label.cex <- degree(icompNETWact)/( max(degree(icompNETWact)))+0.1
V(icompNETWact)$size <- (degree(icompNETWact)/2)
V(icompNETWact)$frame.color <- rgb(0.5,0.5,0.5,0.5)

pdf("icompNETWact.pdf")
plot(icompNETWact, layout=layout.fruchterman.reingold)
plot(icompNETWact, layout=layout.kamada.kawai)
plot(icompNETWact, layout=layout.auto)
plot(icompNETWact, layout=layout.lgl)
plot(icompNETWact, layout=layout.random)
plot(icompNETWact, layout=layout.circle)
plot(icompNETWact, layout=layout.sphere)
plot(icompNETWact, layout=layout.reingold.tilford)
plot(icompNETWact, layout=layout.graphopt)
icompNETWact <- delete.vertices(icompNETWact, V(icompNETWact)[ degree(icompNETWact)==0 ])
plot(icompNETWact, layout=layout.fruchterman.reingold)
plot(icompNETWact, layout=layout.kamada.kawai)
plot(icompNETWact, layout=layout.auto)
plot(icompNETWact, layout=layout.lgl)
plot(icompNETWact, layout=layout.random)
plot(icompNETWact, layout=layout.circle)
plot(icompNETWact, layout=layout.sphere)
plot(icompNETWact, layout=layout.reingold.tilford)
plot(icompNETWact, layout=layout.graphopt)

dev.off()

imembNETWact <- graph.adjacency(membNETWact,, mode = 'undirected')
E(imembNETWact)$weight <- count.multiple(imembNETWact)
imembNETWact <- simplify(imembNETWact)

V(imembNETWact)$color <- rgb(0,.7,.1,1)

V(imembNETWact)$label <- V(imembNETWact)$name
V(imembNETWact)$label.color <- rgb(0,0,0,1)
V(imembNETWact)$label.cex <- degree(imembNETWact)*0.1

V(imembNETWact)$label.cex <- degree(imembNETWact)/20
V(imembNETWact)$label.cex <- degree(imembNETWact)/( max(degree(imembNETWact)))+0.1
V(imembNETWact)$size <- (degree(imembNETWact)/2)
V(imembNETWact)$frame.color <- rgb(0.5,0.5,0.5,0.5)

pdf("imembNETWact.pdf")
plot(imembNETWact, layout=layout.fruchterman.reingold)
plot(imembNETWact, layout=layout.kamada.kawai)
plot(imembNETWact, layout=layout.auto)
plot(imembNETWact, layout=layout.lgl)
plot(imembNETWact, layout=layout.random)
plot(imembNETWact, layout=layout.circle)
plot(imembNETWact, layout=layout.sphere)
plot(imembNETWact, layout=layout.reingold.tilford)
plot(imembNETWact, layout=layout.graphopt)
imembNETWact <- delete.vertices(imembNETWact, V(imembNETWact)[ degree(imembNETWact)==0 ])
plot(imembNETWact, layout=layout.fruchterman.reingold)
plot(imembNETWact, layout=layout.kamada.kawai)
plot(imembNETWact, layout=layout.auto)
plot(imembNETWact, layout=layout.lgl)
plot(imembNETWact, layout=layout.random)
plot(imembNETWact, layout=layout.circle)
plot(imembNETWact, layout=layout.sphere)
plot(imembNETWact, layout=layout.reingold.tilford)
plot(imembNETWact, layout=layout.graphopt)

dev.off()

MAR <- cbind(MA,ST[,1])
MAR1<-cbind(MAR,c(1:nrow(MAR)))
MAR1 <- as.matrix(MAR1[MAR1[,29]>0,])
MAR <- as.matrix(MAR[MAR[,29]>0,])


colnames(MAR)<-c("Big Data, Data Mining and Science", "Data Analysis", "Data Dissemination", "Data Integration", "Data Processing", "Data Validation", "Data Warehousing", 
			"Descriptive Statistics", "Design and optimisation of statistical processes", "Econometrics", "Enterprise Architecture", "Indices", "Indicators, composite and synthetic",
			"Inferential Statistics", "Information models and standards", "Linear Algebra", "Metadata models and standards", "Micro-data access", "Model-based Estimation", "Quality",
			"Seasonal adjustment", "Statistical confidentiality", "Statistical Software", "Survey Methodology", "Sampling", "Administrative Data", "Time Series", "Other MA", "R")

n<-nrow(MAR)
m<-ncol(MAR)

#row.names(MAR)<-data[,1]
row.names(MAR) <- MAR1[,30]

iMAR <- graph.incidence(MAR[,1:28], mode=c("all"))

V(iMAR)$color[1:n] <- data[,58]
V(iMAR)$color[(n+1):(n+1+m)] <- rgb(1,1,0,0.5)
V(iMAR)$label <- V(iMAR)$name
V(iMAR)$label.color <- rgb(0,0,0,1)
V(iMAR)$label.cex <- degree(iMAR)*0.1
V(iMAR)$label.cex <- degree(iMAR)/( max(degree(iMAR)))+0.3
V(iMAR)$size <- (degree(iMAR)/2)+2
V(iMAR)$frame.color <- rgb(0.5,0.5,0.5,0.5)

pdf("iMAR.pdf")
plot(iMAR, layout=layout.fruchterman.reingold, main="Methodological areas by ESTAT staff competent in R")
plot(iMAR, layout=layout.fruchterman.reingold)
plot(iMAR, layout=layout.fruchterman.reingold)
plot(iMAR, layout=layout.fruchterman.reingold)
plot(iMAR, layout=layout.kamada.kawai, main="Methodological areas by ESTAT staff competent in R")
plot(iMAR, layout=layout.kamada.kawai)
plot(iMAR, layout=layout.kamada.kawai)
plot(iMAR, layout=layout.kamada.kawai)
plot(iMAR, layout=layout.kamada.kawai)
plot(iMAR, layout=layout.auto, main="Methodological areas by ESTAT staff competent in R")
plot(iMAR, layout=layout.auto)
plot(iMAR, layout=layout.auto)
plot(iMAR, layout=layout.auto)
plot(iMAR, layout=layout.auto)
plot(iMAR, layout=layout.random, main="Methodological areas by ESTAT staff competent in R")
plot(iMAR, layout=layout.circle, main="Methodological areas by ESTAT staff competent in R")
plot(iMAR, layout=layout.sphere, main="Methodological areas by ESTAT staff competent in R")
plot(iMAR, layout=layout.reingold.tilford, main="Methodological areas by ESTAT staff competent in R")
dev.off()



SDR <- cbind(SD,ST[,1])
SDR1<-cbind(SDR,c(1:nrow(SDR)))
SDR1 <- as.matrix(SDR1[SDR1[,13]>0,])
SDR <- as.matrix(SDR[SDR[,13]>0,])


colnames(SDR)<-c("Agricultural and Fisheries Statistics", "Business Statistics","Demography", "Environmental Statistics", "International trade", "National Accounts", "Prices",
			"Science and technology", "Social Statistics", "Transport", "Energy Statistic", "Other SD", "R")

n<-nrow(SDR)
m<-ncol(SDR)

#row.names(SDR)<-data[,1]
row.names(SDR) <- SDR1[,14]

iSDR <- graph.incidence(SDR[,1:12], mode=c("all"))

V(iSDR)$color[1:n] <- data[,58]
V(iSDR)$color[(n+1):(n+1+m)] <- rgb(1,1,0,0.5)
V(iSDR)$label <- V(iSDR)$name
V(iSDR)$label.color <- rgb(0,0,0,1)
V(iSDR)$label.cex <- degree(iSDR)*0.1


V(iSDR)$label.cex <- degree(iSDR)/( max(degree(iSDR)))+0.3
V(iSDR)$size <- (degree(iSDR)/2)+2
V(iSDR)$frame.color <- rgb(0.5,0.5,0.5,0.5)

pdf("iSDR.pdf")
plot(iSDR, layout=layout.fruchterman.reingold, main="Statistical domains by ESTAT staff competent in R")
plot(iSDR, layout=layout.kamada.kawai, main="Statistical domains by ESTAT staff competent in R")
plot(iSDR, layout=layout.auto, main="Statistical domains by ESTAT staff competent in R")
plot(iSDR, layout=layout.random, main="Statistical domains by ESTAT staff competent in R")
plot(iSDR, layout=layout.circle, main="Statistical domains by ESTAT staff competent in R")
plot(iSDR, layout=layout.sphere, main="Statistical domains by ESTAT staff competent in R")
plot(iSDR, layout=layout.reingold.tilford, main="Statistical domains by ESTAT staff competent in R")

dev.off()


pdf("iMAsatsr.pdf")
plot(iMAsatsr, layout=layout.fruchterman.reingold, main="Two-mode network of methodological areas by ESTAT staff")
E(iMAsatsr)$width <- E(iMAsatsr)$weight

iMAsatsr <- delete.vertices(iMAsatsr, V(iMAsatsr)[ degree(iMAsatsr)==0 ])
plot(iMAsatsr, layout=layout.fruchterman.reingold, main="Two-mode network of methodological areas by competent ESTAT staff")

V(iMAsatsr)$label[degree(iMAsatsr)<6]<- NA
iMAsatsr <- delete.vertices(iMAsatsr, V(iMAsatsr)[ degree(iMAsatsr)==0 ])
plot(iMAsatsr, layout=layout.fruchterman.reingold, main="Two-mode network of methodological areas by competent ESTAT staff")
dev.off()

n<-nrow(SD)
m<-ncol(SD)

iSD <- graph.incidence(SD, mode=c("all") )
V(iSD)$color[1:n] <- data[,58]
V(iSD)$shape[1:n] <- "square"
V(iSD)$color[(n+1):(n+1+m)] <- rgb(1,1,0,.5)
V(iSD)$label <- V(iSD)$name
V(iSD)$label.color <- rgb(0,0,0,1)
V(iSD)$label.cex <- degree(iSD)/( max(degree(iSD))/2)+0.2
V(iSD)$size <- (degree(iSD)/2)+1
V(iSD)$frame.color <- rgb(0.5,0.5,0.5,0.5)

E(iSD)$color <- rgb(.5,.5,.5,.5)
pdf("iSD.pdf")
plot(iSD, layout=layout.fruchterman.reingold, main="Two-mode network of statistical domains by ESTAT staff")
plot(iSD, layout=layout.kamada.kawai, main="Two-mode network of statistical domains by ESTAT staff")
#iSD <- delete.vertices(iSD, V(iSD)[ degree(iSD)==0 ])
plot(iSD, layout=layout.fruchterman.reingold, main="Two-mode network of statistical domains by competent ESTAT staff")
plot(iSD, layout=layout.kamada.kawai, main="Two-mode network of statistical domains by competent ESTAT staff")
dev.off()

n<-nrow(ST)
m<-ncol(ST)
colnames(ST)<-c("R", "SAS", "Eviews", "Stata", "Matlab", "Hadoop", "Python", "SPSS", "Java", "Other") 
iST <- graph.incidence(ST, mode=c("all") )
V(iST)$color[1:n] <- data[,58]
V(iST)$shape[1:n] <- "circle"
V(iST)$color[(n+1):(n+1+m)] <- rgb(1,1,0,.5)
V(iST)$shape[(n+1):(n+1+m)] <- "circle"
V(iST)$label <- V(iST)$name
V(iST)$label.color <- rgb(0,0,0,1)
V(iST)$label.cex <- degree(iST)/( max(degree(iST))/2)+0.2
V(iST)$size <- (degree(iST)/2)+1
V(iST)$frame.color <- rgb(0.5,0.5,0.5,0.5)
E(iST)$color <- rgb(.5,.5,.5,.5)

pdf("iST.pdf")
plot(iST, layout=layout.fruchterman.reingold, main="Two-mode network of statistical tools by ESTAT staff")
plot(iST, layout=layout.kamada.kawai, main="Two-mode network of statistical tools by ESTAT staff")
#iST <- delete.vertices(iST, V(iST)[ degree(iST)==0 ])
plot(iST, layout=layout.fruchterman.reingold)
plot(iST, layout=layout.kamada.kawai)
dev.off()

#statistics of network
NETWORK<-cbind(MA,SD,ST)
colnames(NETWORK)<-c("Big Data Analytics, Data Mining and Data Science", "Data Analysis", "Data Dissemination", "Data Integration", "Data Processing", "Data Validation", 
			"Data Warehousing", "Descriptive Statistics", "Design and optimisation of statistical processes", "Econometrics", "Enterprise Architecture", "Indices", 
			"Indicators, composite and synthetic", "Inferential Statistics", "Information models and standards", "Linear Algebra", "Metadata models and standards",
			"Micro-data access", "Model-based Estimation", "Quality", "Seasonal adjustment", "Statistical confidentiality", "Statistical Software", "Survey Methodology",
			"Sampling", "Administrative Data", "Time Series", "Other MA", "Agricultural and Fisheries Statistics", "Business Statistics","Demography", "Environmental Statistics",
			"International trade", "National Accounts", "Prices", "Science and technology", "Social Statistics", "Transport", "Energy Statistic", "Other SD","R", "SAS", "Eviews",
			"Stata", "Matlab", "Hadoop", "Python", "SPSS", "Java", "Other ST")
n<-nrow(NETWORK)
m<-ncol(NETWORK)
 
iNETWORK <- graph.incidence(NETWORK, mode=c("all") )
#iNETWORK <- bipartite.projection(iNETWORK)

V(iNETWORK)$color[1:m] <- data[,58]
V(iNETWORK)$shape[1:m] <- "circle"
V(iNETWORK)$color[(m+1):(m+1+m)] <- rgb(1,1,0,.5)
V(iNETWORK)$shape[(m+1):(m+1+m)] <- "circle"
V(iNETWORK)$label <- V(iNETWORK)$name
V(iNETWORK)$label.color <- rgb(0,0,0,1)
V(iNETWORK)$label.cex <- degree(iNETWORK)/( max(degree(iNETWORK))/2)+0.2
V(iNETWORK)$size <- (degree(iNETWORK)/2)+1
V(iNETWORK)$frame.color <- rgb(0.5,0.5,0.5,0.5)
E(iNETWORK)$color <- rgb(.5,.5,.5,.5)

pdf("iNETWORK.pdf")
plot(iNETWORK, layout=layout.fruchterman.reingold)
plot(iNETWORK, layout=layout.kamada.kawai)
#iST <- delete.vertices(iST, V(iST)[ degree(iST)==0 ])
plot(iNETWORK, layout=layout.fruchterman.reingold)
plot(iNETWORK, layout=layout.kamada.kawai)
dev.off()
vcount(iNETWORK)
ecount(iNETWORK)
degree(iNETWORK, v = V(iNETWORK), mode = c("all", "out", "in", "total"), loops = FALSE, normalized = FALSE)
summary(clusters(iNETWORK))
degree_distribution(iNETWORK, cumulative = FALSE)
adjacent_vertices(iNETWORK, 2, mode = c("out", "in", "all", "total"))  #adjacent vertices of multiply vertices
adjacent_vertices(iNETWORK, c(1,108))
alpha_centrality(iNETWORK, nodes = V(iNETWORK), alpha = 1, loops = FALSE, exo = 1, weights = NULL, tol = 1e-07, sparse = TRUE)
assortativity_degree(iNETWORK, directed=FALSE)
NETWORK<-as_incidence_matrix(iNETWORK)
net<-web2edges(NETWORK, return=TRUE)
clustering_tm(net)

summary(centr_degree(iNETWORK)$res)

net<-NETWORK
net <- as.tnet(net, type="binary two-mode tnet")
a<-clustering_local_tm(net)
c<-colSums(a)/67
neto<-cbind(net[,2],net[,1])
b<-clustering_local_tm(neto)
b<-b[-18,]
colSums(b)/49

net<-MA
net <- as.tnet(net, type="binary two-mode tnet")
a<-clustering_local_tm(net)
a<-a[-5,]
a<-a[-41,]
colSums(a)/67
neto<-cbind(net[,2],net[,1])
b<-clustering_local_tm(neto)
b<-b[-18,]
colSums(b)/27

net<-SD
net <- as.tnet(net, type="binary two-mode tnet")
a<-clustering_local_tm(net)
a<-a[-5,]
a<-a[-10,]
a<-a[-16,]
a<-a[-17,]
a<-a[-20,]
a<-a[-25,]
a<-a[-25,]
a<-a[-26,]
a<-a[-26,]
a<-a[-27,]
a<-a[-31,]
a<-a[-32,]
a<-a[-32,]
a<-a[-32,]
a<-a[-34,]
a<-a[-34,]
a<-a[-41,]
a<-a[-45,]
colSums(a)/49
neto<-cbind(net[,2],net[,1])
b<-clustering_local_tm(neto)
colSums(b)/12

net<-ST
net <- as.tnet(net, type="binary two-mode tnet")
a<-clustering_local_tm(net)
a<-a[-5,]
a<-a[-8,]
a<-a[-12,]
a<-a[-19,]
a<-a[-20,]
a<-a[-25,]
a<-a[-25,]
a<-a[-26,]
a<-a[-26,]
a<-a[-29,]
a<-a[-29,]
a<-a[-29,]
a<-a[-30,]
a<-a[-34,]
a<-a[-39,]
colSums(a)/52
neto<-cbind(net[,2],net[,1])
b<-clustering_local_tm(neto)
colSums(b)/12

a<-similarity(iNETWORK, vids = V(iNETWORK), mode = c("all", "out", "in", "total"), loops = FALSE, method = c("jaccard", "dice", "invlogweighted"))

a<-similarity(iSD, vids = V(iSD), mode = c("all", "out", "in", "total"), loops = FALSE, method = c("jaccard", "dice", "invlogweighted"))
b<-a[1:67,1:67]
(sum(b)-67)/4489
b<-a[68:79,68:79]
(sum(b)-12)/144

nMA<-nrow(MA)
mMA<-ncol(MA)
data_vlow<-data_low<-data_high<-data_vhigh<-matrix(0,nMA,mMA)
freq_vlow<-freq_low<-freq_high<-freq_vhigh<-matrix(0,1,mMA)
for(i in 1:nMA){
for(j in 1:mMA){
if(MA[i,j]==1) {data_vlow[i,j]<-1}
if(MA[i,j]==2) {data_low[i,j]<-1}
if(MA[i,j]==3) {data_high[i,j]<-1}
if(MA[i,j]==4) {data_vhigh[i,j]<-1}
}
freq_vlow<-colSums(data_vlow)
freq_low<-colSums(data_low)
freq_high<-colSums(data_high)
freq_vhigh<-colSums(data_vhigh)
}
freq_vlow<-t(freq_vlow)
freq_low<-t(freq_low)
freq_high<-t(freq_high)
freq_vhigh<-t(freq_vhigh)

colnames(freq_vlow)<-c("Big Data Analytics, Data\nMining and Data Science", "Data Analysis", "Data Dissemination", "Data Integration",
			"Data Processing", "Data Validation", "Data Warehousing", "Descriptive Statistics", "Design and optimisation\nof statistical processes", 
			"Econometrics", "Enterprise Architecture", "Indices", "Indicators, composite\nand synthetic", "Inferential Statistics", 
			"Information models\nand standards", "Linear Algebra", "Metadata models\nand standards", "Micro-data access", "Model-based Estimation",
			"Quality", "Seasonal adjustment", "Statistical confidentiality", "Statistical Software", "Survey Methodology", "Sampling",
			"Administrative Data", "Time Series", "Other")


par(mar=c(11,2,2,0)+0.1,mgp=c(5,1,0))
PERnrMAlevel_barplot<-barplot(rbind(freq_vlow,freq_low,freq_high,freq_vhigh), border = 9, main = "Competency deepness", ylab = "number of people", axes = TRUE, axisnames = TRUE, axis.lty=0,  width=0.15, xaxs = "i", 
		ylim=c(0,max(rbind(freq_vlow,freq_low,freq_high,freq_vhigh))+4), las=2, beside=TRUE, col=c("grey","pink","orange","light green"))
text(PERnrMAlevel_barplot, rbind(freq_vlow,freq_low,freq_high,freq_vhigh), format(rbind(freq_vlow,freq_low,freq_high,freq_vhigh)), xpd = TRUE, pos=3, col = 1, cex = .5)
legend("top",c("1 out of 4", "2 out of 4", "3 out of 4","4 out of 4"),col=c("grey","pink","orange","light green"), lwd=2, legend=, cex=0.85, bty="n", xjust=1)

################# Clustering 1 ###################
n<-nrow(MA)
m<-ncol(MA)
row.names(MA) <- data[,1]
CMA<-matrix(0,n,5)
for(i in 1:n){
if(MA[i,1]!=0) {CMA[i,2]<-1}
if(MA[i,2]!=0) {CMA[i,2]<-1}
if(MA[i,3]!=0) {CMA[i,4]<-1}
if(MA[i,4]!=0) {CMA[i,5]<-1}
if(MA[i,5]!=0) {CMA[i,4]<-1}
if(MA[i,6]!=0) {CMA[i,4]<-1}
if(MA[i,7]!=0) {CMA[i,4]<-1}
if(MA[i,8]!=0) {CMA[i,2]<-1}
if(MA[i,9]!=0) {CMA[i,5]<-1}
if(MA[i,10]!=0) {CMA[i,1]<-1}
if(MA[i,11]!=0) {CMA[i,5]<-1}
if(MA[i,12]!=0) {CMA[i,2]<-1}
if(MA[i,13]!=0) {CMA[i,2]<-1}
if(MA[i,14]!=0) {CMA[i,1]<-1}
if(MA[i,15]!=0) {CMA[i,5]<-1}
if(MA[i,16]!=0) {CMA[i,1]<-1}
if(MA[i,17]!=0) {CMA[i,5]<-1}
if(MA[i,18]!=0) {CMA[i,4]<-1}
if(MA[i,19]!=0) {CMA[i,1]<-1}
if(MA[i,20]!=0) {CMA[i,4]<-1}
if(MA[i,21]!=0) {CMA[i,1]<-1}
if(MA[i,22]!=0) {CMA[i,4]<-1}
if(MA[i,23]!=0) {CMA[i,4]<-1}
if(MA[i,24]!=0) {CMA[i,3]<-1}
if(MA[i,25]!=0) {CMA[i,1]<-1}
if(MA[i,26]!=0) {CMA[i,3]<-1}
if(MA[i,27]!=0) {CMA[i,1]<-1}}
row.names(CMA) <- data[,1]
row.names(CMA) <- c(1:n)

colnames(CMA) <- c("Statistical methods for data estimation", "Data analysis and visualisation", "Sources", "Statistical processes", "Conceptualisation")

iCMA <- graph.incidence(CMA, mode=c("all"), weighted=TRUE )

V(iCMA)$shape[1:n] <- "square"
V(iCMA)$shape[(n+1):(n+ncol(CMA)+1)] <- "circle"
V(iCMA)$color[1:n] <- data[,58]
V(iCMA)$color[(n+1):(n+ncol(CMA)+1)] <- rgb(1,1,0,.5)
V(iCMA)$label <- V(iCMA)$name
V(iCMA)$label.color <- rgb(0,0,0,1)
V(iCMA)$label.cex <- degree(iCMA)/( max(degree(iCMA)))+0.4
V(iCMA)$size <- degree(iCMA)*0.5+1
V(iCMA)$frame.color <- rgb(0.5,0.5,0.5,0.5)

E(iCMA)$color <- rgb(.5,.5,.5,.5)
pdf("iCMA.pdf")
plot(iCMA, layout=layout.kamada.kawai, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA, layout=layout.fruchterman.reingold, main="Two-mode network of methodological areas
									 clusters by competent ESTAT staff") 
iCMA <- delete.vertices(iCMA, V(iCMA)[ degree(iCMA)==0 ])
plot(iCMA, layout=layout.kamada.kawai, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA, layout=layout.fruchterman.reingold, main="Two-mode network of methodological areas
									 clusters by competent ESTAT staff") 
plot(iCMA, layout=layout.auto, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA, layout=layout.lgl, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA, layout=layout.random, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA, layout=layout.circle, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA, layout=layout.sphere, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA, layout=layout.kamada.kawai, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA, layout=layout.reingold.tilford, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA, layout=layout.graphopt, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
dev.off()

SME<-cbind(MA[,10],MA[,14],MA[,16],MA[,19],MA[,21],MA[,25],MA[,27])
colnames(SME)<-c("Econometrics","Inferential Statistics","Linear Algebra","Model based Estimation","Seasonal adjustment","Sampling","Time Series")
DAV<-cbind(MA[,1],MA[,2],MA[,8],MA[,12],MA[,13])
colnames(DAV)<-c("Big Data Analytics, Data Mining, and Data Science","Data Analysis","Descriptive Statistics","Indices","Indicators, composite, and synthetic")
SOURCE<-cbind(MA[,24],MA[,26])
colnames(SOURCE)<-c("Survey Methodology","Administrative Data")
SP<-cbind(MA[,3],MA[,5],MA[,6],MA[,7],MA[,18],MA[,20],MA[,22],MA[,23])
colnames(SP)<-c("Data Dissemination","Data Processing","Data Validation","Data Warehousing","Micro data access","Quality","Statistical confidentiality","Statistical Software")
CONCEPT<-cbind(MA[,4],MA[,9],MA[,11],MA[,15],MA[,17])
colnames(CONCEPT)<-c("Data Integration","Design and optimisation of statistical processes","Enterprise Architecture","Information models and standards","Metadata models and standards")
row.names(SME) <-row.names(DAV) <-row.names(SOURCE) <-row.names(SP) <-row.names(CONCEPT) <- c(1:n)
iSME <- graph.incidence(SME, mode=c("all"), weighted=TRUE )
iDAV <- graph.incidence(DAV, mode=c("all"), weighted=TRUE )
iSOURCE <- graph.incidence(SOURCE, mode=c("all"), weighted=TRUE )
iSP <- graph.incidence(SP, mode=c("all"), weighted=TRUE )
iCONCEPT <- graph.incidence(CONCEPT, mode=c("all"), weighted=TRUE )

V(iSME)$shape[1:n] <- "square"
V(iSME)$shape[(n+1):(n+ncol(SME)+1)] <- "circle"
V(iSME)$color[1:n] <- data[,58]
V(iSME)$color[(n+1):(n+ncol(SME)+1)] <- rgb(1,1,0,.5)
V(iSME)$label <- V(iSME)$name
V(iSME)$label.color <- rgb(0,0,0,1)
V(iSME)$label.cex <- degree(iSME)/( max(degree(iSME)))+0.8
V(iSME)$size <- degree(iSME)+4
V(iSME)$frame.color <- rgb(.5,.5,.5,.5)
#iSME <- delete.vertices(iSME, V(iSME)[ degree(iSME)==0 ])

E(iSME)$color <- rgb(.5,.5,.5,.5)
pdf("iSME.pdf")
iSME <- delete.vertices(iSME, V(iSME)[ degree(iSME)==0 ])
plot(iSME, layout=layout.kamada.kawai, main="Two-mode network of statistical methodological areas
								 for data estimation by competent ESTAT staff")
plot(iSME, layout=layout.fruchterman.reingold, main="Two-mode network of statistical methodological areas
								 for data estimation by competent ESTAT staff") 
plot(iSME, layout=layout.auto, main="Two-mode network of statistical methodological areas
								 for data estimation by competent ESTAT staff")
plot(iSME, layout=layout.lgl, main="Two-mode network of statistical methodological areas
								 for data estimation by competent ESTAT staff")
plot(iSME, layout=layout.random, main="Two-mode network of statistical methodological areas
								 for data estimation by competent ESTAT staff")
plot(iSME, layout=layout.circle, main="Two-mode network of statistical methodological areas
								 for data estimation by competent ESTAT staff")
plot(iSME, layout=layout.sphere, main="Two-mode network of statistical methodological areas
								 for data estimation by competent ESTAT staff")
plot(iSME, layout=layout.kamada.kawai, main="Two-mode network of statistical methodological areas
								 for data estimation by competent ESTAT staff")
plot(iSME, layout=layout.spring, main="Two-mode network of statistical methodological areas
								 for data estimation by competent ESTAT staff")
plot(iSME, layout=layout.reingold.tilford, main="Two-mode network of statistical methodological areas
								 for data estimation by competent ESTAT staff")
plot(iSME, layout=layout.graphopt, main="Two-mode network of statistical methodological areas
								 for data estimation by competent ESTAT staff")
plot(iSME, layout=layout.svd, main="Two-mode network of statistical methodological areas
								 for data estimation by competent ESTAT staff")
dev.off()

V(iDAV)$shape[1:n] <- "square"
V(iDAV)$shape[(n+1):(n+ncol(DAV))] <- "circle"
V(iDAV)$color[1:n] <- data[,58]
V(iDAV)$color[(n+1):(n+ncol(DAV))] <- rgb(1,1,0,.5)
V(iDAV)$label <- V(iDAV)$name
V(iDAV)$label.color <- rgb(0,0,0,1)
V(iDAV)$label.cex <- degree(iDAV)/( max(degree(iDAV)))+0.4
V(iDAV)$size <- (degree(iDAV)/2)+2
V(iDAV)$frame.color <- rgb(.5,.5,.5,.5)
E(iDAV)$color <- rgb(.5,.5,.5,.5)
pdf("iDAV.pdf")
#iDAV <- delete.vertices(iDAV, V(iDAV)[ degree(iDAV)==0 ])
plot(iDAV, layout=layout.kamada.kawai, main="Two-mode network of Data Analysis and Visualisation
								 methodological areas by ESTAT staff")
plot(iDAV, layout=layout.fruchterman.reingold, main="Two-mode network of Data Analysis and Visualisation
								 methodological areas by ESTAT staff")
plot(iDAV, layout=layout.auto, main="Two-mode network of Data Analysis and Visualisation
								 methodological areas by ESTAT staff")
plot(iDAV, layout=layout.lgl, main="Two-mode network of Data Analysis and Visualisation
								 methodological areas by ESTAT staff")
plot(iDAV, layout=layout.random, main="Two-mode network of Data Analysis and Visualisation
								 methodological areas by ESTAT staff")
plot(iDAV, layout=layout.circle, main="Two-mode network of Data Analysis and Visualisation
								 methodological areas by ESTAT staff")
plot(iDAV, layout=layout.kamada.kawai, main="Two-mode network of Data Analysis and Visualisation
								 methodological areas by ESTAT staff")
plot(iDAV, layout=layout.reingold.tilford, main="Two-mode network of Data Analysis and Visualisation
								 methodological areas by ESTAT staff")
plot(iDAV, layout=layout.graphopt, main="Two-mode network of Data Analysis and Visualisation
								 methodological areas by ESTAT staff")
dev.off()

V(iSOURCE)$shape[1:n] <- "square"
V(iSOURCE)$shape[(n+1):(n+ncol(SOURCE))] <- "circle"
V(iSOURCE)$color[1:n] <- data[,58]
V(iSOURCE)$color[(n+1):(n+ncol(SOURCE))] <- rgb(1,1,0,.5)
V(iSOURCE)$label <- V(iSOURCE)$name
V(iSOURCE)$label.color <- rgb(0,0,0,1)
V(iSOURCE)$label.cex <- degree(iSOURCE)/( max(degree(iSOURCE)))+0.4
V(iSOURCE)$size <- (degree(iSOURCE)/2)+2
V(iSOURCE)$frame.color <- rgb(.5,.5,.5,.5)

E(iSOURCE)$color <- rgb(.5,.5,.5,.5)
pdf("iSOURCE.pdf")
#iSOURCE <- delete.vertices(iSOURCE, V(iSOURCE)[ degree(iSOURCE)==0 ])
plot(iSOURCE, layout=layout.kamada.kawai, main="Two-mode network of Data Sources by ESTAT staff")
plot(iSOURCE, layout=layout.fruchterman.reingold, main="Two-mode network of Data Sources by ESTAT staff")
plot(iSOURCE, layout=layout.auto, main="Two-mode network of Data Sources by ESTAT staff")
plot(iSOURCE, layout=layout.lgl, main="Two-mode network of Data Sources by ESTAT staff")
plot(iSOURCE, layout=layout.random, main="Two-mode network of Data Sources by ESTAT staff")
plot(iSOURCE, layout=layout.circle, main="Two-mode network of Data Sources by ESTAT staff")
plot(iSOURCE, layout=layout.kamada.kawai, main="Two-mode network of Data Sources by ESTAT staff")
plot(iSOURCE, layout=layout.reingold.tilford, main="Two-mode network of Data Sources by ESTAT staff")
plot(iSOURCE, layout=layout.graphopt, main="Two-mode network of Data Sources by ESTAT staff")

dev.off()


V(iSP)$shape[1:n] <- "square"
V(iSP)$shape[(n+1):(n+ncol(SP))] <- "circle"
V(iSP)$color[1:n] <- data[,58]
V(iSP)$color[(n+1):(n+ncol(SP))] <- rgb(1,1,0,.5)
V(iSP)$label <- V(iSP)$name
V(iSP)$label.color <- rgb(0,0,0,1)
V(iSP)$label.cex <- degree(iSP)/( max(degree(iSP)))+0.4
V(iSP)$size <- (degree(iSP)/2)+2
V(iSP)$frame.color <- rgb(.5,.5,.5,.5)

E(iSP)$color <- rgb(.5,.5,.5,.5)
pdf("iSP.pdf")
#iSP <- delete.vertices(iSP, V(iSP)[ degree(iSP)==0 ])
plot(iSP, layout=layout.kamada.kawai, main="Two-mode network of Practical process by ESTAT staff")
plot(iSP, layout=layout.fruchterman.reingold, main="Two-mode network of Practical process by ESTAT staff")
plot(iSP, layout=layout.auto, main="Two-mode network of Practical process by ESTAT staff")
plot(iSP, layout=layout.lgl, main="Two-mode network of Practical process by ESTAT staff")
plot(iSP, layout=layout.random, main="Two-mode network of Practical process by ESTAT staff")
plot(iSP, layout=layout.circle, main="Two-mode network of Practical process by ESTAT staff")
plot(iSP, layout=layout.kamada.kawai, main="Two-mode network of Practical process by ESTAT staff")
plot(iSP, layout=layout.reingold.tilford, main="Two-mode network of Practical process by ESTAT staff")
plot(iSP, layout=layout.graphopt, main="Two-mode network of Practical process by ESTAT staff")

dev.off()

V(iCONCEPT)$shape[1:n] <- "square"
V(iCONCEPT)$shape[(n+1):(n+ncol(CONCEPT))] <- "circle"
V(iCONCEPT)$color[1:n] <- data[,58]
V(iCONCEPT)$color[(n+1):(n+ncol(CONCEPT))] <- rgb(1,1,0,.5)
V(iCONCEPT)$label <- V(iCONCEPT)$name
V(iCONCEPT)$label.color <- rgb(0,0,0,1)
V(iCONCEPT)$label.cex <- degree(iCONCEPT)/( max(degree(iCONCEPT)))+0.4
V(iCONCEPT)$size <- (degree(iCONCEPT)/2)+2
V(iCONCEPT)$frame.color <- rgb(.5,.5,.5,.5)

E(iCONCEPT)$color <- rgb(.5,.5,.5,.5)
pdf("iCONCEPT.pdf")
iCONCEPT <- delete.vertices(iCONCEPT, V(iCONCEPT)[ degree(iCONCEPT)==0 ])
plot(iCONCEPT, layout=layout.kamada.kawai, main="Two-mode network of Conceptualisation by ESTAT staff")
plot(iCONCEPT, layout=layout.fruchterman.reingold, main="Two-mode network of Conceptualisation by ESTAT staff")
plot(iCONCEPT, layout=layout.auto, main="Two-mode network of Conceptualisation by ESTAT staff")
plot(iCONCEPT, layout=layout.lgl, main="Two-mode network of Conceptualisation by ESTAT staff")
plot(iCONCEPT, layout=layout.random, main="Two-mode network of Conceptualisation by ESTAT staff")
plot(iCONCEPT, layout=layout.circle, main="Two-mode network of Conceptualisation by ESTAT staff")
plot(iCONCEPT, layout=layout.kamada.kawai, main="Two-mode network of Conceptualisation by ESTAT staff")
plot(iCONCEPT, layout=layout.reingold.tilford, main="Two-mode network of Conceptualisation by ESTAT staff")
plot(iCONCEPT, layout=layout.graphopt, main="Two-mode network of Conceptualisation by ESTAT staff")

dev.off()

############ Clustering 2 ###############
n<-nrow(MA)
m<-ncol(MA)
row.names(MA) <- data[,1]
CMA1<-matrix(0,n,5)
for(i in 1:n){
if(MA[i,1]!=0) {CMA1[i,3]<-1}
if(MA[i,2]!=0) {CMA1[i,3]<-1}
if(MA[i,3]!=0) {CMA1[i,4]<-1}
if(MA[i,4]!=0) {CMA1[i,5]<-1}
if(MA[i,5]!=0) {CMA1[i,4]<-1}
if(MA[i,6]!=0) {CMA1[i,4]<-1}
if(MA[i,7]!=0) {CMA1[i,4]<-1}
if(MA[i,8]!=0) {CMA1[i,3]<-1}
if(MA[i,9]!=0) {CMA1[i,5]<-1}
if(MA[i,10]!=0) {CMA1[i,1]<-1}
if(MA[i,11]!=0) {CMA1[i,5]<-1}
if(MA[i,12]!=0) {CMA1[i,2]<-1}
if(MA[i,13]!=0) {CMA1[i,2]<-1}
if(MA[i,14]!=0) {CMA1[i,3]<-1}
if(MA[i,15]!=0) {CMA1[i,5]<-1}
if(MA[i,16]!=0) {CMA1[i,1]<-1}
if(MA[i,17]!=0) {CMA1[i,5]<-1}
if(MA[i,18]!=0) {CMA1[i,4]<-1}
if(MA[i,19]!=0) {CMA1[i,1]<-1}
if(MA[i,20]!=0) {CMA1[i,4]<-1}
if(MA[i,21]!=0) {CMA1[i,2]<-1}
if(MA[i,22]!=0) {CMA1[i,4]<-1}
if(MA[i,23]!=0) {CMA1[i,4]<-1}
if(MA[i,24]!=0) {CMA1[i,1]<-1}
if(MA[i,25]!=0) {CMA1[i,1]<-1}
if(MA[i,26]!=0) {CMA1[i,4]<-1}
if(MA[i,27]!=0) {CMA1[i,2]<-1}
}
row.names(CMA1) <- data[,1]
row.names(CMA1) <- c(1:n)    

colnames(CMA1) <- c("Microeconometrics", "Macroeconometrics", "Multidimensional data analysis", "Processes", "Conceptualisation")

iCMA1 <- graph.incidence(CMA1, mode=c("all"), weighted=TRUE )

V(iCMA1)$shape[1:n] <- "square"
V(iCMA1)$shape[(n+1):(n+ncol(CMA1)+1)] <- "circle"
V(iCMA1)$color[1:n] <- data[,58]
V(iCMA1)$color[(n+1):(n+ncol(CMA1)+1)] <- rgb(1,1,0,.5)
V(iCMA1)$label <- V(iCMA1)$name
V(iCMA1)$label.color <- rgb(0,0,0,1)
V(iCMA1)$label.cex <- degree(iCMA1)/( max(degree(iCMA1)))+0.5
V(iCMA1)$size <- degree(iCMA1)*0.5+1
V(iCMA1)$frame.color <- rgb(0.5,0.5,0.5,0.5)

E(iCMA1)$color <- rgb(.5,.5,.5,.5)
pdf("iCMA1.pdf")
iCMA1 <- delete.vertices(iCMA1, V(iCMA1)[ degree(iCMA1)==0 ])
#tkplot
plot(iCMA1, layout=layout.kamada.kawai, main="Two-mode network of clustered
								methodological areas by ESTAT staff")
plot(iCMA1, layout=layout.fruchterman.reingold, main="Two-mode network of clustered
								methodological areas by ESTAT staff") 

plot(iCMA1, layout=layout.kamada.kawai, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA1, layout=layout.fruchterman.reingold, main="Two-mode network of methodological areas
									 clusters by competent ESTAT staff") 
plot(iCMA1, layout=layout.auto, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA1, layout=layout.lgl, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA1, layout=layout.random, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA1, layout=layout.circle, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA1, layout=layout.sphere, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA1, layout=layout.kamada.kawai, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA1, layout=layout.reingold.tilford, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
plot(iCMA1, layout=layout.graphopt, main="Two-mode network of methodological areas
								 clusters by competent ESTAT staff")
dev.off()

MICRO<-cbind(MA[,10],MA[,16],MA[,19],MA[,24],MA[,25])
colnames(MICRO)<-c("Econometrics","Linear Algebra","Model based Estimation","Survey Methodology","Sampling")
MACRO<-cbind(MA[,12],MA[,13],MA[,21],MA[,27])
colnames(MACRO)<-c("Indices","Indicators, composite, and synthetic","Seasonal adjustment","Time Series")
MDA<-cbind(MA[,1],MA[,2],MA[,8],MA[,14],MA[,26])
colnames(MDA)<-c("Big Data Analytics, Data Mining, and Data Science","Data Analysis","Descriptive Statistics","Inferential Statistics","Administrative Data")

row.names(MICRO) <-row.names(MACRO) <-row.names(MDA) <- c(1:n)

iMICRO <- graph.incidence(MICRO, mode=c("all"), weighted=TRUE )
iMACRO <- graph.incidence(MACRO, mode=c("all"), weighted=TRUE )
iMDA <- graph.incidence(MDA, mode=c("all"), weighted=TRUE )
MICRO<-as_incidence_matrix(iMICRO)
MACRO<-as_incidence_matrix(iMACRO)
MDA<-as_incidence_matrix(iMDA)
SP<-as_incidence_matrix(iSP)
CONCEPT<-as_incidence_matrix(iCONCEPT)

V(iMICRO)$shape[1:n] <- "square"
V(iMICRO)$shape[(n+1):(n+ncol(MICRO)+1)] <- "circle"
V(iMICRO)$color[1:n] <- data[,58]
V(iMICRO)$color[(n+1):(n+ncol(MICRO)+1)] <- rgb(1,1,0,.5)
V(iMICRO)$label <- V(iMICRO)$name
V(iMICRO)$label.color <- rgb(0,0,0,1)
V(iMICRO)$label.cex <- degree(iMICRO)/( max(degree(iMICRO)))+0.4
V(iMICRO)$size <- degree(iMICRO)+2
V(iMICRO)$frame.color <- rgb(.5,.5,.5,.5)
iMICRO <- delete.vertices(iMICRO, V(iMICRO)[ degree(iMICRO)==0 ])

E(iMICRO)$color <- rgb(.5,.5,.5,.5)
pdf("iMICRO.pdf")
iMICRO <- delete.vertices(iMICRO, V(iMICRO)[ degree(iMICRO)==0 ])
plot(iMICRO, layout=layout.kamada.kawai, main="Two-mode network of Microeconometrics methodological areas
								 by competent ESTAT staff")
plot(iMICRO, layout=layout.fruchterman.reingold, main="Two-mode network of Microeconometrics methodological areas
								 by competent ESTAT staff") 
plot(iMICRO, layout=layout.auto, main="Two-mode network of Microeconometrics methodological areas
								 by competent ESTAT staff")
plot(iMICRO, layout=layout.lgl, main="Two-mode network of Microeconometrics methodological areas
								 by competent ESTAT staff")
plot(iMICRO, layout=layout.random, main="Two-mode network of Microeconometrics methodological areas
								 by competent ESTAT staff")
plot(iMICRO, layout=layout.circle, main="Two-mode network of Microeconometrics methodological areas
								 by competent ESTAT staff")
plot(iMICRO, layout=layout.sphere, main="Two-mode network of Microeconometrics methodological areas
								 by competent ESTAT staff")
plot(iMICRO, layout=layout.kamada.kawai, main="Two-mode network of Microeconometrics methodological areas
								 by competent ESTAT staff")
plot(iMICRO, layout=layout.spring, main="Two-mode network of Microeconometrics methodological areas
								 by competent ESTAT staff")
plot(iMICRO, layout=layout.reingold.tilford, main="Two-mode network of Microeconometrics methodological areas
								 by competent ESTAT staff")
plot(iMICRO, layout=layout.graphopt, main="Two-mode network of Microeconometrics methodological areas
								 by competent ESTAT staff")
plot(iMICRO, layout=layout.svd, main="Two-mode network of Microeconometrics methodological areas
								 by competent ESTAT staff")
dev.off()

V(iMACRO)$shape[1:n] <- "square"
V(iMACRO)$shape[(n+1):(n+ncol(MACRO))] <- "circle"
V(iMACRO)$color[1:n] <- data[,58]
V(iMACRO)$color[(n+1):(n+ncol(MACRO))] <- rgb(1,1,0,.5)
V(iMACRO)$label <- V(iMACRO)$name
V(iMACRO)$label.color <- rgb(0,0,0,1)
V(iMACRO)$label.cex <- degree(iMACRO)/( max(degree(iMACRO)))+0.4
V(iMACRO)$size <- (degree(iMACRO)/2)+2
V(iMACRO)$frame.color <- rgb(.5,.5,.5,.5)
E(iMACRO)$color <- rgb(.5,.5,.5,.5)
pdf("iMACRO.pdf")
#iMACRO <- delete.vertices(iMACRO, V(iMACRO)[ degree(iMACRO)==0 ])
plot(iMACRO, layout=layout.kamada.kawai, main="Two-mode network of Macroeconometrics methodological areas by ESTAT staff")
plot(iMACRO, layout=layout.fruchterman.reingold, main="Two-mode network of Macroeconometrics
								 methodological areas by ESTAT staff")
plot(iMACRO, layout=layout.auto, main="Two-mode network of Macroeconometrics
								 methodological areas by ESTAT staff")
plot(iMACRO, layout=layout.lgl, main="Two-mode network of Macroeconometrics
								 methodological areas by ESTAT staff")
plot(iMACRO, layout=layout.random, main="Two-mode network of Macroeconometrics
								 methodological areas by ESTAT staff")
plot(iMACRO, layout=layout.circle, main="Two-mode network of Macroeconometrics
								 methodological areas by ESTAT staff")
plot(iMACRO, layout=layout.kamada.kawai, main="Two-mode network of Macroeconometrics
								 methodological areas by ESTAT staff")
plot(iMACRO, layout=layout.reingold.tilford, main="Two-mode network of Macroeconometrics
								 methodological areas by ESTAT staff")
plot(iMACRO, layout=layout.graphopt, main="Two-mode network of Macroeconometrics
								 methodological areas by ESTAT staff")
dev.off()

V(iMDA)$shape[1:n] <- "square"
V(iMDA)$shape[(n+1):(n+ncol(MDA))] <- "circle"
V(iMDA)$color[1:n] <- data[,58]
V(iMDA)$color[(n+1):(n+ncol(MDA))] <- rgb(1,1,0,.5)
V(iMDA)$label <- V(iMDA)$name
V(iMDA)$label.color <- rgb(0,0,0,1)
V(iMDA)$label.cex <- degree(iMDA)/( max(degree(iMDA)))+0.4
V(iMDA)$size <- (degree(iMDA)/2)+2
V(iMDA)$frame.color <- rgb(.5,.5,.5,.5)

E(iMDA)$color <- rgb(.5,.5,.5,.5)
pdf("iMDA.pdf")
#iMDA <- delete.vertices(iMDA, V(iMDA)[ degree(iMDA)==0 ])
plot(iMDA, layout=layout.kamada.kawai, main="Two-mode network of Multidimentional data analysis methodological areas by ESTAT staff")
plot(iMDA, layout=layout.fruchterman.reingold, main="Two-mode network of Multidimentional data analysis methodological areas by ESTAT staff")
plot(iMDA, layout=layout.auto, main="Two-mode network of Multidimentional data analysis methodological areas by ESTAT staff")
plot(iMDA, layout=layout.lgl, main="Two-mode network of Multidimentional data analysis methodological areas by ESTAT staff")
plot(iMDA, layout=layout.random, main="Two-mode network of Multidimentional data analysis methodological areas by ESTAT staff")
plot(iMDA, layout=layout.circle, main="Two-mode network of Multidimentional data analysis methodological areas by ESTAT staff")
plot(iMDA, layout=layout.kamada.kawai, main="Two-mode network of Multidimentional data analysis methodological areas by ESTAT staff")
plot(iMDA, layout=layout.reingold.tilford, main="Two-mode network of Multidimentional data analysis methodological areas by ESTAT staff")
plot(iMDA, layout=layout.graphopt, main="Two-mode network of Multidimentional data analysis methodological areas by ESTAT staff")

dev.off()


MA<-as_incidence_matrix(iMA_SD_ST)
degr<-degree(iMA_SD_ST)
for(i in 1:n){
if(degr[i]<2) { MA<-MA[-i,]}
}
iMA <- graph.incidence(MA, mode=c("all"), weighted=TRUE )
dsaqwe<-degree(iMA)
n<-nrow(MA)
m<-ncol(MA)
centr<-matrix(0,nrow=1,ncol=n-1)
for(i in 1:(n-1)){
a<-NULL
cc_cc<-matrix(0,nc=m,nr=1)
	for(k in (i+1):n){
cc_c<-matrix(0,nc=m,nr=1)
		for(j in 1:m){
if(MA[i,j]==MA[k,j] && MA[i,j]>0) {cc_c[1,j]<-1}
}
if(sum(cc_c)>1) {cc_cc<-rbind(cc_cc,cc_c); a<-c(a,k)}

}
if(nrow(cc_cc)>1) {
cc_cc<-cc_cc[-1,]}
if(is.matrix(cc_cc)==FALSE) {cc_cc<-t(cc_cc)}
row.names(cc_cc)<-a
degr<-dsaqwe[a]
nn<-nrow(cc_cc)
c<-NULL
for(h in 1:nn){
q<-sum(cc_cc[h,])
ccc<-q/(degr[h]-q+1+dsaqwe[i]-q+1+q)
c<-c(c,ccc)
}
centr[i]<-mean(c)
}
mean(centr,na.rm=TRUE)

iMA_SD_ST <- graph.incidence(MA_SD_ST, mode=c("all"), weighted=TRUE )


MA<-as_incidence_matrix(iSD)
MA<-t(MA)
degr<-degree(iSD)
for(i in 1:n){
if(degr[i]<2) { MA<-MA[-i,]}
}
iMA <- graph.incidence(MA, mode=c("all"), weighted=TRUE )
dsaqwe<-degree(iMA)
n<-nrow(MA)
m<-ncol(MA)
centr<-matrix(0,nrow=1,ncol=n-1)
for(i in 1:(n-1)){
a<-NULL
cc_cc<-matrix(0,nc=m,nr=1)
	for(k in (i+1):n){
cc_c<-matrix(0,nc=m,nr=1)
		for(j in 1:m){
if(MA[i,j]==MA[k,j] && MA[i,j]>0) {cc_c[1,j]<-1}
}
if(sum(cc_c)>1) {cc_cc<-rbind(cc_cc,cc_c); a<-c(a,k)}

}
if(nrow(cc_cc)>1) {
cc_cc<-cc_cc[-1,]}
if(is.matrix(cc_cc)==FALSE) {cc_cc<-t(cc_cc)}
row.names(cc_cc)<-a
degr<-dsaqwe[a]
nn<-nrow(cc_cc)
c<-NULL
for(h in 1:nn){
q<-sum(cc_cc[h,])
ccc<-q/(degr[h]-q+1+dsaqwe[i]-q+1+q)
c<-c(c,ccc)
}
centr[i]<-mean(c)
}
mean(centr,na.rm=TRUE)
