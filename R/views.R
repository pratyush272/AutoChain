
chain_file_cleaning <- function(headers,outputfile)
{
  tabview<-
    fluidPage(
          
        mainPanel(
          fileInput("file1", "Choose CSV File",multiple = TRUE,accept = c("text/csv",".csv")),
          checkboxInput("header", "Header", TRUE),
          radioButtons("sep", "Separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ","),
          radioButtons("quote", "Quote",choices = c(None = "","Double Quote" = '"',"Single Quote" = "'"),selected = '"'),
          tableOutput("a"),tableOutput("b"),tableOutput("c"),tableOutput("d")
          ,actionButton("generate","Clean my file")
        )#mainpanel
    )#TabPage
return (tabview)
  }
  