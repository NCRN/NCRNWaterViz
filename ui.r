library(shiny)

ColorNames<-GraphColors$DisplayColor



shinyUI(
  pageWithSidebar(
    
    div(id="NPSBanner",
      headerPanel(
        windowTitle="NCRN Water Quality",
        list(
          tags$head(
            includeScript("./www/water-analytics.js"),
            tags$script(
              'type = "text/javascript"',' var ss = document.createElement("link"); ss.type="text/css"; ss.rel="stylesheet"; 
              ss.href = window.self === window.top ? "NCRN.css" : "NCRNframe.css"; document.getElementsByTagName("head")[0].appendChild(ss);'
            ),
            HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />')
          ),
          HTML('<img src="ah_large_black.gif", style="float:right; padding-right:25px"/>',
            '<p>  National Capital Region Network <br> Stream Water Quality </p>'
          )
        )
      )
    ),
  
  sidebarPanel(
    h4("Select Stream Data"),
         
    uiOutput("parkControl"),
      
    uiOutput("streamControl"), 
    
    uiOutput("ParameterControl"),
    
    uiOutput("yearControl"),
       
    HTML('<hr >'),
     
    h4(id="ThreshHeader","Thresholds"),
      
    checkboxInput("ThreshLine","Show Water Quality Threshold Line",FALSE),
     
    checkboxInput("ThreshPoint","Indicate Points with Poor Water Quality",FALSE),

    HTML('<hr>'),
        
    h4(id="TrendHeader","Trends and Seasonal Patterns"),
     
    checkboxInput("Trends","Show Seasonal Patterns and Trends",FALSE),
     
    checkboxInput("Outliers","Indicate Outliers Not Used in Analysis",FALSE),    
     
    HTML('<hr>'),
     
    h4(id="DownloadHeader","Downloads"),
     
    downloadButton("Data.Download","Save Data (.csv)") ,
     
    downloadButton("Plot.PNG","Save Plot (.png)"),
       
    downloadButton("Plot.JPG","Save Plot (.jpg)"),
     
    HTML('<hr>'),
    
    ############### Graphics options
    
    h4(id="GraphOptHead","Graphics Options"),
    
    div(id="GraphBox",checkboxInput("GraphOptions","Show Graphics Options",FALSE)),
  
    conditionalPanel(condition = "input.GraphOptions",

      div(id="LegCheck",checkboxInput("Legend","Show Legend",TRUE)),
      
      div(id="FontSli",sliderInput("FontSize", "Change Font Size", min=1, max=2.5,value=1.5, step=.25)),
      
      HTML('<hr>'),
      
      h5("Points"),
      
      div(id="GoodC",selectInput("GoodColor","Measurement Color:",choices=ColorNames, selected="Blue")),
  
      conditionalPanel(id="TheshPColor", condition = "input.ThreshPoint",
        selectInput("BadColor","Poor Quality Color:",choices=ColorNames,selected="Orange") 
      ),
    
      
      conditionalPanel(id="OutC", condition = "input.Outliers",
        selectInput("OutColor","Outlier Color:",choices=ColorNames,selected="Vermillion")   
      ),
    
      div(id="PointSli",sliderInput("PointSize", "Change Size", min=.5, max=2.5,value=1.5, step=.25)),
      
      HTML('<hr>'),
     
      conditionalPanel(condition = "input.ThreshLine || input.Trends",
        h5("Lines")),
      
      conditionalPanel(id="ThreshLCol",condition = "input.ThreshLine",
        selectInput("ThColor","Threshold Color:",choices=ColorNames,selected="Orange") 
      ),
    
      conditionalPanel(id="TrendLCol",condition = "input.Trends", 
        selectInput("TrColor","Trend Color:",choices=ColorNames,selected="Green") 
      ),
      
      conditionalPanel(id="LineSliPan",condition = "input.ThreshLine || input.Trends",
        sliderInput("LineWidth", "Change Width", min=.5, max=4,value=1, step=.5)
      )    
    )
  ),
    
  
  mainPanel(
    tabsetPanel(
      
      tabPanel(h4("Plot"), 
        plotOutput("Water.Plot"),
        
        conditionalPanel(condition = "input.ThreshLine &&  !(input['Park.in']==null) && input['Park.in']!=='Choose a Park' &&
            input.Stream!=='Choose a Stream' && !(input.Stream==null) && input.Param!=='Choose Water Parameter' &&
            !(input.Param==null)" , 
          h4("Threshold:"),
          textOutput ("ThresholdSummary"),
          textOutput ("ThresholdType")
        ),
        
        br(),
        
        conditionalPanel(condition = "input.Trends && !(input['Park.in']==null) && input['Park.in']!=='Choose a Park' &&
            input.Stream!=='Choose a Stream' && !(input.Stream==null) && input.Param!=='Choose Water Parameter' &&
            !(input.Param==null)" , 
        h4("Trend Analysis"),
        textOutput("TrendsOut"),
        textOutput("SeasonOut")
        ),
        
        br(),
        
        conditionalPanel(condition = "input.ThreshLine && !(input['Park.in']==null) && input['Park.in']!=='Choose a Park' &&
            input.Stream!=='Choose a Stream' && !(input.Stream==null) && input.Param!=='Choose Water Parameter' &&
            !(input.Param==null)", 
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