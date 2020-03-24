library(shiny)
library(shinyjs)

ColorNames<-GraphColors$DisplayColor


shinyUI(
  fluidPage( theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", style="padding: 0px",
             title="NCRN Water Quality",
    
    column(12, id="NPSBanner", style="margin: 0px",
      useShinyjs(),
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
    h3("Select Stream Data"),
         
    uiOutput("parkControl"),
      
    uiOutput("streamControl"), 
    
    uiOutput("ParameterControl"),
    
    uiOutput("yearControl"),
       
    HTML('<hr >'),
     
    h4(id="ThreshHeader","Thresholds"),
      
    checkboxInput("ThreshLine","Show Water Quality Threshold Line",FALSE),
     
    checkboxInput("ThreshPoint","Indicate Points with Poor Water Quality",FALSE),

    HTML('<hr>'),
        
    h3(id="TrendHeader","Trends and Seasonal Patterns"),
     
    checkboxInput("Trends","Show Seasonal Patterns and Trends",FALSE),
     
    checkboxInput("Outliers","Indicate Outliers Not Used in Analysis",FALSE),    
     
    HTML('<hr>'),
     
    h3(id="DownloadHeader","Downloads"),
     
    downloadButton("Data.Download","Save Data (.csv)", class="btn btn-primary") ,
     
    downloadButton("Plot.PNG","Save Plot (.png)", class="btn btn-primary"),
       
    downloadButton("Plot.JPG","Save Plot (.jpg)", class="btn btn-primary"),
     
    HTML('<hr>'),
    
    ############### Graphics options
    
    h3(id="GraphOptHead","Graphics Options"),
    
    div(id="GraphBox",checkboxInput("GraphOptions","Show Graphics Options",FALSE)),
    
    column(6,
      checkboxInput("Legend","Show Legend",TRUE)
    ),
      
    column(6,
      sliderInput("FontSize", "Change Font Size", min=1, max=2.5,value=1.5, step=.25, width='130px')
    ),
      
   column(12, h4("Points", style="text-align: center",id="PointHeader")),
      
    column(6,
      selectInput("GoodColor","Measurement Color:",choices=ColorNames, selected="Blue", width='130px'),
      selectInput("BadColor","Poor Quality Color:",choices=ColorNames,selected="Orange", width='130px') 
    ),
    
    column(6,
      selectInput("OutColor","Outlier Color:",choices=ColorNames,selected="Vermillion", width='130px'),   
      sliderInput("PointSize", "Change Size", min=.5, max=2.5,value=1.5, step=.25, width='130px')
    ),
      
    column(12,h4("Lines",style="text-align: center", id="LineHeader")),
    
   column(6,
        selectInput("ThColor","Threshold Color:",choices=ColorNames,selected="Orange", width='130px'), 
        selectInput("TrColor","Trend Color:",choices=ColorNames,selected="Green", width='130px')
    ),
      
    column(6,
      sliderInput("LineWidth", "Change Width", min=.5, max=4,value=1, step=.5, width='130px')
    )    
  ) #end well panel
  ),
    
  
  mainPanel(
    tabsetPanel(  
      tabPanel(h4("Plot"), 
        plotOutput("WaterPlot"),
        
        conditionalPanel(condition = "input.ThreshLine && !(output.WaterPlot==null)" , 
          h4("Threshold:"),
          textOutput ("ThresholdSummary"),
          textOutput ("ThresholdType")
        ),
        
        br(),
        
        conditionalPanel(condition = "input.Trends && !(output.WaterPlot==null)" , 
        h4("Trend Analysis"),
        textOutput("TrendsOut"),
        textOutput("SeasonOut")
        ),
        
        br(),
        
       conditionalPanel(condition = "input.ThreshLine && !(output.WaterPlot==null)" , 
          h4("Threshold Reference:"),
          textOutput("RefSummary")
       )
        
      ),
      
      tabPanel(h4("Raw Data"),dataTableOutput("WaterTable")),
      
      tabPanel(h4("Project Information"),
        
               includeHTML(paste0(getwd(),"/www/","projectintro.html"))),
      
      tabPanel(h4("Citations & References"),
      
        includeHTML(paste0(getwd(),"/www/","citations.html"))
      )
      
    )
    
  )


))