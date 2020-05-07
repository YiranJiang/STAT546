list_of_packages <- c("shiny", "png", "visNetwork", "rsconnect",
                      "shinythemes", "shinycssloaders")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(shiny)
library(rsconnect)
library(png)
library(visNetwork)
library(shinythemes)
library(shinycssloaders)
################################################################################
path_to_images <- "https://raw.githubusercontent.com/YiranJiang/STAT546_Final_Project_COVID-19_BN/master/shinyapp/img/"
imagename <- c("Asymptomatic", "Gender", "Age",
               "Dyspnea", "Kidney",
               "Septic", "Heart",
               "Fever", "Cough", "Anorxia",
               "Chills", "Body",
               "Diarrhea", "Rhinorrhea")

node <- data.frame("id" = c(paste("s0", 1:9, sep = ""),
                            paste("s", 10:14, sep = "")), 
                   "shape" = c("circularImage"),
                   "image" = c(paste0(path_to_images, imagename, ".png")),
                   "symptom" = c("asymptomatic", "gender", "age",
                                 "dyspnea", "kidney injury",
                                 "septic shock", "heart failure",
                                 "fever", "cough", "anorexia",
                                 "chills", "body pain",
                                 "diarrhea", "rhinorrhea"),
                   "symptom_type" = c("asymptomatic", rep("basic info", 2),
                                      rep("severe symptoms", 4),
                                      rep("mild symptoms", 7)),
                   "symptom_size" = c(rep(3,3),rep(5,11)))

link <- data.frame("from" = c("s03", "s03", "s02", "s02",
                              "s04", "s08", "s08","s09",
                              "s09","s10","s11", "s12",
                              "s12", "s13","s14"), 
                   "to" = c("s01", "s04", "s08", "s14",
                            "s05", "s04", "s06", "s06",
                            "s07", "s08", "s12", "s08",
                            "s04", "s14","s12"))

vis.nodes <- node
vis.links <- link

vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$symptom # Text on click
vis.nodes$label  <- vis.nodes$symptom # Node label
vis.nodes$size   <- vis.nodes$symptom_size*7 # Node size
vis.nodes$borderWidth <- 2 # Node border width
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.border <- "darkred"

vis.links$color <- "gray"    # line color  
vis.links$arrows <- "to" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- FALSE   # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow


###############################################################################

ui <- navbarPage(
  theme = shinytheme('sandstone'),
  title = "Shiny App", 
  tabPanel(title = "Tab 1",
           sidebarPanel(
             h1("COVID-19"),
             a(href="https://github.com/beoutbreakprepared/nCoV2019/tree/master/latest_data",
               "Data Source"), # temp 
             img(height=100, width=1051/428*100,
                 src="covid-virus.png")
           ),
           mainPanel(
             visNetworkOutput("network")
           )
  )
)

################################################################################

server <- function(input, output){  #assemble input into output
  
  output$network <- renderVisNetwork({
    visOptions(visNetwork(vis.nodes, vis.links, 
                          main = "Bayesian Network of Symptoms",
                          submain = "Click nodes to see their connections",
                          footer = "(Zoom in to show all node names)"),
               highlightNearest = TRUE, selectedBy = "symptom_type")
  })
  
}

################################################################################
shinyApp(ui = ui, server = server)


# rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
# deployApp(appTitle = "STAT546Project")
