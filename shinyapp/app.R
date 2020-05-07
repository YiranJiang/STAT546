list_of_packages <- c("shiny", "png", "visNetwork", "rsconnect")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(shiny)
library(rsconnect)
library(png)
library(visNetwork)

#####################################################################
## experiment data for node and link (to be changed)
node <- data.frame("id" = c(paste("s0", 1:9, sep = ""),
                            paste("s", 10:14, sep = "")), 
                   "symptom" = c("asymptomatic", "gender", "age",
                                 "dyspnea", "kidney injury",
                                 "septic shock", "heart failure",
                                 "fever", "cough", "anorxia",
                                 "chills", "body pain",
                                 "diarrhea", "rhinorrhea"),
                   "symptom_type" = c("asymptomatic", rep("basic info", 2),
                                      rep("severe symptoms", 4),
                                      rep("mild symptoms", 7)),
                   "symptom_size" = c(3, rep(3,2),rep(5,4), rep(7,7)))

link <- data.frame("from" = c("s03", "s03", "s02", "s02",
                              "s04","s06", "s08","s09",
                              "s11", "s12", "s14",
                              "s08", "s12", "s13", "s14"), 
                   "to" = c("s01", "s04", "s08", "s14",
                            "s08","s09", "s06","s07",
                            "s12", "s04", "s12",
                            "s12", "s08","s14", "s13"),
                   #"type" = c(),
                   "weight" = 1:15)

visNetwork(node, link, width="100%", height="400px", background="#eeefff",
           main="Network", submain="BN for Symptoms",
           footer= "Bayesian Network for Symptoms")

vis.nodes <- node
vis.links <- link

vis.nodes$shape  <- "dot"  
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$symptom # Text on click
vis.nodes$label  <- vis.nodes$symptom # Node label
vis.nodes$size   <- vis.nodes$symptom_size*5 # Node size
vis.nodes$borderWidth <- 2 # Node border width

colrs <- c("lightblue", "lightgreen", "orange", "red")
vis.nodes$color.background <- colrs[as.factor(node$symptom_type)]
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"

vis.links$width <- 1+link$weight/8 # line width
vis.links$color <- "gray"    # line color  
vis.links$arrows <- "to" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- FALSE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow


###############################################################################

ui <- fluidPage(theme = "bootstrap.min.css",
  tags$h1("Shiny App"),
  HTML("<a href=\"http://www.google.com\">Shiny Showcase</a>"),
  # tags$a("href=\"http://www.google.com\">Data Source<"),             
  visNetworkOutput("network")
)

################################################################################

server <- function(input, output){  #assemble input into output
  
  output$network <- renderVisNetwork({
    visOptions(visNetwork(vis.nodes, vis.links, main = "BN of Symptoms"),
               highlightNearest = TRUE, selectedBy = "symptom_type")
  })
  
}

################################################################################
shinyApp(ui = ui, server = server)


# rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
# deployApp(appTitle = "STAT546Project")
