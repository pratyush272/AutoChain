###
# Install & Load Packages 
# Set Locale
###
list.of.packages <- c("rhandsontable", "data.table","ggmap","ggplot2","shiny","dplyr","")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

for(package_name in list.of.packages)
{library(package_name,character.only=TRUE, quietly = TRUE);}

Sys.setlocale('LC_ALL','C')

###
# Control Variables
###
headers_shipment <- c("date","delvering_node_id","customer_id","product_code","units","revenue")
headers_sites <- c("site_id","site_name","site_city")
headers_productmaster <- c("prod_code","Prod_name","units_per_load")
headers_eord<- c("product","from_location","to_location")
headers_productionmaster<- c("pp_code","product","production_CPU")


###
# Start Control Panel
###
ui <- fluidPage(
  titlePanel("AutoChain Control Panel")
  ,navlistPanel(
    "Data Cleaning",
    tabPanel("Shipment",
             chain_file_cleaning()
             )#end of shipment tab
    )#end of mainNavlist
)

server <- function(input, output, session) 
  {
    data <- eventReactive(input$generate,{
    if(is.null(input$file1)){
      return()
    }
    
      df <-   fread(input$file1$datapath,header = input$header,sep = input$sep, quote = input$quote)[c(input$a,input$b,input$c,input$d)]
    colnames(df)<- tmp
    dir.create("files", showWarnings = FALSE)
    if(file.exists("./files/chain_shipmentfile.csv")){file.remove("./files/chain_shipmentfile.csv")}
    write.csv(df,file="./files/chain_shipmentfile.csv",row.names = F)
    return(head(read.csv("./files/chain_shipmentfile.csv")))
    
  })
}


###
# runn app
###
runApp(list(ui=ui, server=server))

