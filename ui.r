library(shiny)

ColorNames<-GraphColors$DisplayColor


shinyUI(
  fluidPage( theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", style="padding: 0px",
             title="NCRN Water Quality",
    
    column(12, id="NPSBanner", style="margin: 0px",
      tags$head(includeScript ("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js")),
      tags$head(tags$script(
        'type = "text/javascript"',' var ss = document.createElement("link"); ss.type="text/css"; ss.rel="stylesheet"; 
        ss.href = window.self === window.top ? "NCRN.css" : "NCRNframe.css"; document.getElementsByTagName("head")[0].appendChild(ss);'
      )),
      tags$head(HTML( '<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />')),
          
      div(
        h1(style="background-color: black; color: white; height: 125px; padding: 10px; margin: 0px",
            HTML('<img src="ah_large_black.gif", style="float:right; padding-right:25px"/>',
            'National Capital Region Network <br> Stream Water Quality'
        ))
      )
    ),
  
  column(3, wellPanel(style='overflow: hidden',
             
  #textOutput("Test"),  # For debugging purposes
    h3("Select Stream Data"),

    selectizeInput(inputId="ParkIn",label="1. Park:" , choices=NULL),     
      
    selectizeInput(inputId = "SiteIn", label="2. Stream:", choices=NULL),

    selectizeInput(inputId="ParamIn", label="3. Water Parameter:", choices=NULL),

    sliderInput(inputId="YearsShow", label= "4. Years to Display:", min=1900, max=2100, step=1, value=c(1900,2100),sep="",ticks=F),
    
    uiOutput("yearControl"),
       
    HTML('<hr >'),
     
    h3(id="ThreshHeader","Thresholds"),
      
    checkboxInput("ThreshLine","Show Water Quality Threshold Line",FALSE),
     
    checkboxInput("ThreshPoint","Indicate Points with Poor Water Quality",FALSE),

    HTML('<hr>'),
        
    h3(id="TrendHeader","Trends and Seasonal Patterns"),
     
    checkboxInput("Trends","Show Seasonal Patterns and Trends",FALSE),
     
    checkboxInput("Outliers","Indicate Outliers Not Used in Analysis",FALSE),    
     
    HTML('<hr>'),
     
    h3(id="DownloadHeader","Downloads"),
     
   # downloadButton("Data.Download","Save Data (.csv)", class="btn btn-primary") ,
     
    downloadButton("Plot.PNG","Save Plot (.png)", class="btn btn-primary"),
       
    downloadButton("Plot.JPG","Save Plot (.jpg)", class="btn btn-primary"),
     
    HTML('<hr>'),
    
    #### Graphics options ####
    
    h3(id="GraphOptHead","Graphics Options"),

    actionButton(inputId="GraphicsModal", label='Graphics Options', class="btn btn-primary")
    
  ) #end well panel
  ),
    
  
  mainPanel(
    tabsetPanel(  
      tabPanel(h4("Time Series Plot"),
               
         plotOutput("Plot2"),
      
        conditionalPanel(condition = "input.ThreshLine && !(output.Plot2==null)" , 
          h4("Threshold:"),
          textOutput ("ThresholdSummary"),
          textOutput ("ThresholdType")
        ),
               
        br(),
               
        conditionalPanel(condition = "input.Trends && !(output.Plot2==null)" , 
          h4("Trend Analysis"),
          textOutput("TrendsOut"),
          textOutput("SeasonOut")
        ),
        
        br(),
         
        conditionalPanel(condition = "input.ThreshLine && !(output.Plot2==null)" , 
          h4("Threshold Reference:"),
          textOutput("RefSummary")
        )
               
      ),
      
      tabPanel(h4("Box Plot")),
      
      tabPanel(h4("Raw Data"),DT::dataTableOutput("WaterTable")),
      
      tabPanel(h4("Project Information"),
        
               includeHTML(paste0(getwd(),"/www/","projectintro.html"))),
      
      tabPanel(h4("Citations & References"),
      
        includeHTML(paste0(getwd(),"/www/","citations.html"))
      )
    )
  )
))