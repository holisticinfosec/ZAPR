## ZAPR - ZAP API R Interface ##
## Russ McRee, @HolisticInfoSec, May 2017

## app.R ##
library(shinydashboard)
library(RJSONIO)
library(plyr)
library(rCharts)

zapapi <- fromJSON("http://localhost:8067/JSON/core/view/alerts/?zapapiformat=JSON&apikey=2v3tebdgojtcq3503kuoq2lq5g&formMethod=GET&baseurl=&start=&count=")
zapapi <- zapapi[['alerts']]

grabInfo<-function(var){
  print(paste("Alerts", var, sep=" "))  
  sapply(zapapi, function(x) returnData(x, var)) 
}

returnData<-function(x, var){
  if(!is.null( x[[var]])){
    return( trim(x[[var]]))
  }else{
    return(NA)
  }
}

ZapDataDF<-data.frame(sapply(1:19, grabInfo), stringsAsFactors=FALSE)

PluginID <- ZapDataDF[5]
CWEID <- ZapDataDF[6]
WASCID <- ZapDataDF[8]
Alert <- ZapDataDF[14]
Risk <- ZapDataDF[18]

reportDF <- cbind(Alert,Risk,WASCID,PluginID,CWEID)
colnames(reportDF) <- c("Alert","Risk","WASCID","PluginID","CWEID")

ui <- dashboardPage(
  dashboardHeader(title = "ZAPR: OWASP ZAP API R Interface", titleWidth = 400), 
  dashboardSidebar(
    sidebarMenu(
      menuItem(tags$div("Powered by:"), icon = icon("power-off")),
      tags$a(href='https://www.owasp.org/index.php/OWASP_Zed_Attack_Proxy_Project#tab=Main', tags$img(src='Zap128x128.png', height=128,width=128)),
      menuItem(tags$div("ZAPR Overview"), tabName = "overview", icon = icon("info")),
      menuItem(tags$div("ZAP API Alert Search"), tabName = "ZAPTable", icon = icon("search")),
      menuItem(tags$div("Risk"), tabName = "risk", icon = icon("fire")),
      menuItem(tags$div("CWEID"), tabName = "cweid", icon = icon("signal"))
      
    )
  ),
  dashboardBody(
    tabItems(
      
      # Data table
      tabItem(tabName = "ZAPTable",
              h1("ZAPR"),
              h3("ZAP API alerts search with filters."),
              fluidRow(
                column(width=1, 
                       selectInput("Risk", 
                                   "Risk:", 
                                   c("All", 
                                     unique(as.character(reportDF$Risk))))
                ),
                column(width=2, 
                       selectInput("CWEID", 
                                   "CWEID:", 
                                   c("All", 
                                     unique(as.character(reportDF$CWEID))))
                ),
                column(width=2, 
                       selectInput("WASCID", 
                                   "WASCID:", 
                                   c("All", 
                                     unique(as.character(reportDF$WASCID))))
                ),
                column(width=1, 
                       selectInput("PluginID", 
                                   "PluginID:", 
                                   c("All", 
                                     unique(as.character(reportDF$PluginID))))
                ),
                column(width=4, 
                       selectInput("Alert", 
                                   "Alert:", 
                                   c("All", 
                                     unique(as.character(reportDF$Alert))))       
                )
              ),
              # Create a new row for the table.
              fluidRow(
                dataTableOutput(outputId="table")
              )    
      ),
      # Risk plot
      tabItem(tabName = "risk",
              fluidRow(
                showOutput("plotRisk","Highcharts"),
                h3(" Alert Counts By Risk Level")
              )
      ),
      # Incidents by type plot
      tabItem(tabName = "cweid",
              fluidRow(
                showOutput("plotCWEID","nvd3"),
                h3(" CWEIDs by Count")
              )
      ),
      
      # Print Summary
      tabItem(tabName = "overview",
              h3("ZAPR Overview"),
              strong("Total OWASP ZAP Alerts:"),
              tags$p(fromJSON("http://localhost:8067/JSON/core/view/numberOfAlerts/?zapapiformat=JSON&apikey=2v3tebdgojtcq3503kuoq2lq5g&formMethod=GET&baseurl=")),
              strong("Hosts ZAP'd:"),
              tags$p(fromJSON("http://localhost:8067/JSON/core/view/hosts/?zapapiformat=JSON&apikey=2v3tebdgojtcq3503kuoq2lq5g&formMethod=GET")),
              strong("Total OWASP ZAP Messages:"),
              tags$p(fromJSON("http://localhost:8067/JSON/core/view/numberOfMessages/?zapapiformat=JSON&apikey=2v3tebdgojtcq3503kuoq2lq5g&formMethod=GET&baseurl=")),
              strong("Context:"),
              tags$p(fromJSON("http://localhost:8067/JSON/context/view/contextList/?zapapiformat=JSON&apikey=2v3tebdgojtcq3503kuoq2lq5g&formMethod=GET")),
              strong("Session Location:"),
              tags$p(fromJSON("http://localhost:8067/JSON/core/view/sessionLocation/?zapapiformat=JSON&apikey=2v3tebdgojtcq3503kuoq2lq5g&formMethod=GET"))
              )
      )
    )
  )


server <- function(input, output) {
  
  output$table <- renderDataTable({
    data <- reportDF
    if (input$Risk != "All"){
      data <- data[data$Risk == input$Risk,]
    }
    if (input$CWEID != "All"){
      data <- data[data$CWEID == input$CWEID,]
    }
    if (input$WASCID != "All"){
      data <- data[data$WASCID == input$WASCID,]
    }
    if (input$Alert != "All"){
      data <- data[data$Alert == input$Alert,]
    }
    data
  })
  
  ct1 <- count(reportDF$Risk)
  colnames(ct1) <- c("Risk_Level","Alert_Count")
  output$plotRisk <- renderChart2({
    n1 <- hPlot(Alert_Count ~ Risk_Level, group = 'Risk_Level', data = ct1, type = 'bar')
    return(n1)
  })  

  ct2 <- count(reportDF$CWEID)
  colnames(ct2) <- c("CWEID","CWEID_Count")
  output$plotCWEID <- renderChart2({
    n2 <- nPlot(CWEID_Count ~ CWEID, group = "CWEID_Count", data = ct2, type = "pieChart")
    return(n2)
  })
}

shinyApp(ui, server)