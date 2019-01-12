tmp<- c("delv_plt","material","ship_to","qty")
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
      
    return(df)
    
   
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
    write.csv(df,file="../../../files/clean_shipmentfile.csv",row.names = F)
    return(read.csv(paste0(getwd(),"/files/clean_shipmentfile.csv")))
 
  })
  
  output$summary_table <- renderTable({
    data()
  })

    
  
  
}