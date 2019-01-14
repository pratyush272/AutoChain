chain_control_panel<- function(){

library(shiny)
library(shinyjs)
library(qcc)
library(dplyr)
library(data.table)
library(ggmap)
library(maps)
library(mapdata)
  
Sys.setlocale('LC_ALL','C')

tmp<- c("delv_plt","material","ship_to","qty")

jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
    var element = document.documentElement,
enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
enterFS.call(element);
} else {
exitFS.call(document);
}
}'

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsToggleFS),
  titlePanel("AutoChain Control Panel"),
  navlistPanel(
    "Shipment Data",
    tabPanel("Cleanup",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   fileInput("file1", "Choose CSV File",multiple = TRUE,accept = c("text/csv",".csv")),
                   checkboxInput("header", "Header", TRUE),
                   radioButtons("sep", "Separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ","),
                   radioButtons("quote", "Quote",choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),selected = '"'),
                   tableOutput("a"),tableOutput("b"),tableOutput("c"),tableOutput("d")
                   ,actionButton("generate","Clean my shipment")
                   
                   ,width = 3),#sidepanel
                 mainPanel(
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
    "Header B",
    tabPanel("Component 3"),
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
  
  output$summary_table <- renderTable({
    data()
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
