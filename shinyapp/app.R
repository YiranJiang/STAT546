
list_of_packages <- c("shiny", "png", "visNetwork",
                      "shinythemes", "shinycssloaders")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(shiny)
library(png)
library(visNetwork)
library(shinythemes)
library(shinycssloaders)

load("algo2_result.RData")
adjmat <- read.csv("adjmat.csv")
rownames(adjmat) <- adjmat[,1]
adjmat <- adjmat[,-1]
adjmat <- as.matrix(adjmat)
theta_list <- my_result$theta_list
source("functions.R")

################################################################################
path_to_images <- ""
imagename <- c("Asymptomatic", "Gender", "Age",
               "Dyspnea", "Kidney",
               "Septic", "Heart",
               "Fever", "Cough", "Anorxia",
               "Chills", "Body",
               "Diarrhea", "Rhinorrhea","Outcome")

node <- data.frame("id" = c(paste("s0", 1:9, sep = ""),
                            paste("s", 10:15, sep = "")), 
                   "shape" = c("circularImage"),
                   "image" = c(paste0(path_to_images, imagename, ".png")),
                   "symptom" = c("asymptomatic", "gender", "age",
                                 "dyspnea", "kidney injury",
                                 "septic shock", "heart failure",
                                 "fever", "cough", "anorexia",
                                 "chills", "body pain",
                                 "diarrhea", "rhinorrhea",
                                 "outcome"),
                   "symptom_type" = c("asymptomatic", rep("basic info", 2),
                                      rep("severe symptoms", 4),
                                      rep("mild symptoms", 7),
                                      'None'),
                   "symptom_size" = c(rep(3,3),rep(5,11),3))

link <- data.frame("from" = c("s03", "s03", "s02", "s02",
                              "s04", "s08", "s08","s09",
                              "s09","s10","s11", "s12",
                              "s12", "s13","s14",
                              "s03","s02","s09","s08","s04"), 
                   "to" = c("s01", "s04", "s08", "s14",
                            "s05", "s04", "s06", "s06",
                            "s07", "s08", "s12", "s08",
                            "s04", "s14","s12",
                            rep("s15",5)))

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

symps <- node$symptom[-c(2,3,15)]

################################################################################

ui <- navbarPage(
  theme = shinytheme('sandstone'),
  title = "BN for COVID-19",
  tabPanel(title = "Overview",
           sidebarPanel(
             h1("COVID-19"),
             a(href="https://github.com/beoutbreakprepared/nCoV2019/tree/master/latest_data",
               "Data Source"), 
             img(height=100, width=1051/428*100,
                 src="covid-virus.png")
           ),
           mainPanel(
             visNetworkOutput("network")
           )
  ),

  tabPanel(title = "Symptom Prediction",
           sidebarLayout(position = "left",
             sidebarPanel(
               strong("Please select all that apply."),
               hr(),

               selectInput(
                 inputId = "age",
                 label = "Age",
                 choices = list("0-19", "20-39",
                                "40-59", "60+",
                                "Secret"),
                 selected = "Secret"
               ),
               
               selectInput(
                 inputId = "gender",
                 label = "Gender",
                 choices = list("Male", "Female", "Others"),
                 selected = "Others"
               ),

               hr(),

               helpText("The default status of each symptom is \"Unsure\":"),

               selectInput(
                 inputId = "symptoms",
                 label = "Confirmed Symptoms",
                 choices = as.list(symps),
                 multiple = TRUE
               ),

             uiOutput(
               outputId = "nosuch"),

             actionButton(
               inputId = "button",
               label = "Predict")
             ),
             mainPanel(
               visNetworkOutput("network2")
             )
             )
  ),
  
  tabPanel(title = "Other Infomation",
           wellPanel(width = 5,
             radioButtons(
               inputId = "geninfo",
               label = "Select Information Type:",
               choices = list("GIS Outcomes", "Edge Plots")
             )
           ),
           
           uiOutput(outputId = "info_img")
           ),
  
  tabPanel(title = "Group Members",
           fluidRow(column(3,
                           br(),
                           br(),
                           br(),
                           br(),
                           wellPanel(
                             p("Yiran Jiang"),
                             p("Statistics Department"),
                             p("Purdue University"),
                             p("jiang693@purdue.edu")
                             ),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           wellPanel(
                             p("Sihui Wang"),
                             p("Statistics Department"),
                             p("Purdue University"),
                             p("wang1190@purdue.edu")
                             )
                           ),
                           
                    column(4,img(width = 910/1.8,
                                 height = 1053/1.8,
                                 src = "group3.png")),
                    column(3,
                           br(),
                           br(),
                           br(),
                           br(),
                           wellPanel(
                             p("Huiming Xie"),
                             p("Statistics Department"),
                             p("Purdue University"),
                             p("xie339@purdue.edu")
                           ),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           wellPanel(
                             p("Ziwei Su"),
                             p("Statistics Department"),
                             p("Purdue University"),
                             p("su230@purdue.edu")
                           )
                    )
                    )
           )
)

################################################################################

server <- function(input, output){  #assemble input into output

  output$network <- renderVisNetwork({
    visOptions(visNetwork(vis.nodes[1:14,], vis.links[1:14,],
                          main = "Bayesian Network of Symptoms",
                          submain = "Click nodes to see their nearby connections; drag nodes to reorganize",
                          footer = "(Zoom in to show all node names)"),
               highlightNearest = TRUE, selectedBy = "symptom_type")%>%
      visLayout(randomSeed = 3)
  })

  output$nosuch <- renderUI({
    selectInput(
      inputId = "nosuchsymptom",
      label = "No Such Symptoms",
      choices = as.list(symps[!symps %in% input$symptoms]),
      multiple = TRUE
    )
  })
  
  
  output$network2 <- renderVisNetwork({
    input$button
    isolate({
      
    in_vector <- rep(NA, 15)
    names(in_vector) <- c( "age", "gender", "asymptomatic",
                           "dyspnea", "kidney injury",
                           "septic shock", "heart failure",
                           "fever", "cough", "anorexia",
                           "chills", "body pain",
                           "diarrhea", "rhinorrhea", "outcome")
    
    if (input$age=="0-19") in_vector[1] <- 0
    else if(input$age=="20-39") in_vector[1] <- 1
    else if(input$age=="40-59") in_vector[1] <- 2
    else if(input$age=="60+") in_vector[1] <- 3
    
    if (input$gender=="Male") in_vector[2] <- 0
    else if (input$gender=="Female") in_vector[2] <- 1
    
    for(i in 1:length(symps)){
      if (symps[i] %in% input$symptoms) in_vector[i+2] <- 1
      if (symps[i] %in% input$nosuchsymptom) in_vector[i+2] <- 0
    }
    
    ## Warning! The Order Changed!!!!!!!
    this.order <- c(1,2,9,13,10,11,8,4,12,14,3,5,7,6,15)
    in_vector <- in_vector[this.order]
    # names(in_vector) changes accordingly
    prob.list <- get_prob(adjmat, theta_list, in_vector)
    ## Warning! The Order Changed!!!!!!!

    node_order <- rep(NA, length(in_vector))
    for (i in 1:length(in_vector)){
      node_order[i] <- which(node$symptom == names(in_vector)[i])
    }

    node$prob <- rep(NA, nrow(node))
    na_pos <- which(is.na(in_vector))

    for (i in na_pos){
        node$prob[node_order[i]] = ifelse(is.na(prob.list[[i]][1]),
                                          prob.list[[i]][2],
                                          1 - prob.list[[i]][1])
    }
    

    this.suffix <- c(rep("_locked",15))
    this.suffix[which(!is.na(node$prob))] <- ""
    
    vis.nodes2 <- vis.nodes
    vis.nodes2$label <- paste(vis.nodes$symptom, "\n",
                              ifelse(is.na(node$prob), "",
                              paste(round(node$prob,4)*100, "%", sep = "")),
                              sep = "")

    vis.nodes2$image <- c(paste0(path_to_images, imagename, this.suffix, ".png"))

    visOptions(visNetwork(vis.nodes2, vis.links,
                          main = "Bayesian Network of Symptoms with Predicted Proababilities",
                          submain = "Click nodes to see their nearby connections; drag nodes to reorganize",
                          footer = "(Zoom in to show all node names)"),
               highlightNearest = TRUE)%>%
    visLayout(randomSeed = 3)
    
    })
  })
  
  output$info_img <- renderUI({
    if ( input$geninfo == "GIS Outcomes"){
      mainPanel(
        fluidRow(column(1),
                 column(10, 
                        img(src = "map.png", 
                            width = 2483/2.65, 
                            height = 1072/2.65)
                        )
        )
      )
    }
    else {
      mainPanel(
        fluidRow(column(2),
                 column(10, 
                        img(src = "edgeplot.png",
                            width = 2248/3, 
                            height = 1240/3))
        )
      )
    }
  })

}

shinyApp(ui = ui, server = server)


