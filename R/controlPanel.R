chain_control_panel<- function(){

library(shiny)
library(shinyjs)
library(qcc)
library(dplyr)
library(data.table)
library(ggmap)
library(maps)
library(mapdata)
#install_github('ramnathv/rCharts@dev')
#install_github("ramnathv/rMaps")
library(sf)
  library(rhandsontable)
  
Sys.setlocale('LC_ALL','C')

tmp<- c("delv_plt","material","ship_to","qty")

ui <- fluidPage(
  titlePanel("AutoChain Control Panel")
  ,actionButton("cleanall","Clear all Data")
  ,navlistPanel(
    "Data Cleaning"
    ,tabPanel("Shipment",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   fileInput("file1", "Choose CSV File",multiple = TRUE,accept = c("text/csv",".csv")),
                   checkboxInput("header", "Header", TRUE),
                   radioButtons("sep", "Separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ","),
                   radioButtons("quote", "Quote",choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),selected = '"'),
                   tableOutput("a"),tableOutput("b"),tableOutput("c"),tableOutput("d")
                   ,actionButton("generate","Clean my shipment")
                   ,wellPanel(h3("Edit Data"),actionButton("edit", "Open Editor")
                   )
                   ,width = 3),#sidepanel
                 mainPanel(
                   rHandsontableOutput("hot"),
                   tableOutput("contents"),
                   tableOutput("summary_table")
                 )#mainpanel
               )#layout
             )#TabPage
    ),
    tabPanel("Shipment Pareto",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("obs","top %age of customers:",min = 1,max = 100,value = 10)),
                 mainPanel(
                   plotOutput("distPlot")
                 )
               )
             )),
    "Editing",
    tabPanel("Edit Table",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   helpText("Shiny app based on an example given in the rhandsontable package.", 
                            "Right-click on the table to delete/insert rows.", 
                            "Double-click on a cell to edit"),
                   
                   wellPanel(
                     h3("Table options"),
                     radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
                   ),
                   br(), 
                   
                   wellPanel(
                     h3("Save"), 
                     actionButton("save", "Save table")
                   )        
                   
                 ),#sidepanel
                 mainPanel(
                   
                   rHandsontableOutput("hot")
                   
                 )#mainpanel
               )#layout
             )#TabPage
    ),
    tabPanel("Component 4")
    ,widths = c(2,8))
)#ConrolPanelPage
  

server <- function(input, output, session) {
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    
    return(head(df))
    
    
  })
  
  output$a <-  renderUI({selectInput("a", paste0("Choose ",tmp[1]," colummn:"), as.list(colnames(read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote))))})
  output$b <-  renderUI({selectInput("b", paste0("Choose ",tmp[2]," colummn:"), as.list(colnames(read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote))))})
  output$c <-  renderUI({selectInput("c", paste0("Choose ",tmp[3]," colummn:"), as.list(colnames(read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote))))})
  output$d <-  renderUI({selectInput("d", paste0("Choose ",tmp[4]," colummn:"), as.list(colnames(read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote))))})
  
  data <- eventReactive(input$generate,{
    if(is.null(input$file1)){
      return()
    }
    
    df <- data.frame(read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote))[c(input$a,input$b,input$c,input$d)]
    colnames(df)<- tmp
    dir.create("files", showWarnings = FALSE)
    if(file.exists("./files/chain_shipmentfile.csv")){file.remove("./files/chain_shipmentfile.csv")}
    write.csv(df,file="./files/chain_shipmentfile.csv",row.names = F)
    return(head(read.csv("./files/chain_shipmentfile.csv")))
    
  })
  
  
  observeEvent(input$edit, {
    editTable(data.frame(read.csv("./files/chain_shipmentfile.csv")))
  })
    

  
  output$summary_table <- renderTable({
    data()
  })
  
  values <- reactiveValues()
  
  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else {
      if (is.null(values[["DF"]]))
        DF <- data.frame(read.csv("./files/chain_shipmentfile.csv"))
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  output$hot <- renderRHandsontable({
    DF <- values[["DF"]]
    if (!is.null(DF))
      rhandsontable(DF, useTypes = as.logical(input$useType), stretchH = "all")
  })
  
  ## Save 
  observeEvent(input$save, {
    finalDF <- isolate(values[["DF"]])
    saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
  })
  
  output$distPlot <- renderPlot({
    dist <- rnorm(input$obs)
    tmp<- data.table(read.csv("./files/chain_shipmentfile.csv"))
    a<- tmp[,sum(qty),by=(ship_to)]
    b<- a$V1
    names(b)<- a$ship_to 
    b<- head(b[order(-b)],((input$obs)/100)*length(b))
    pareto.chart((b), ylab = "Customers",cumperc = seq(0, 100, by = 5))
    
  })
  
  
  
}#ServerFunction

shinyApp(ui, server)
}



#####################################################333
b<- read.csv("./data/worldcities.csv")
c<- inner_join(a,b, by = c("ship_to" = "city"))[,c(1,4,5)]
d <- map_data("world2Hires")

ggplot() + 
  geom_polygon(data = d, aes(x=long, y = lat, group = group), color = "black", fill = NA)+
  geom_point(data = c, aes(x=lng, y = lat, group = 1), color = "red") + 
  coord_fixed(1.3)

################################3

chain_control_panel()
