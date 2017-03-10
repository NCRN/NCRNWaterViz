library(shiny)
library(leaflet)

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
 # mainPanel(
    tabsetPanel(  
      tabPanel(h4("Time Series Plot"),
        column(3, div(style='padding: 5px 10px',class="panel panel-default", 
          
          #textOutput("Test"),  # For debugging purposes
          
          h3("Select Stream Data"),
          
          parkChooserUI("TimePark"),
          siteChooserUI("TimeSite"),
          paramChooserUI("TimeParam"),
          yearChooserUI("TimeYears"), 
          
          HTML('<hr >'),
             
          h3(id="ThreshHeader","Thresholds"),
          
          checkboxInput("SeriesThreshLine","Show Water Quality Threshold Line",FALSE),
          checkboxInput("ThreshPoint","Indicate Points with Poor Water Quality",FALSE),
          HTML('<hr>'),
           
          h3(id="TrendHeader","Trends and Seasonal Patterns"),
          checkboxInput("Trends","Show Seasonal Patterns and Trends",FALSE),
          checkboxInput("Outliers","Indicate Outliers Not Used in Analysis",FALSE),    
          HTML('<hr>'),
           
          splitLayout(h3(id="DownloadHeader","Downloads:"), cellWidths=c("35%","35%","30%"),
            downloadButton("Plot.PNG","Save Plot (.png)", class="btn btn-primary", style="margin-top: 15px"),
            downloadButton("Plot.JPG","Save Plot (.jpg)", class="btn btn-primary", style="margin-top: 15px")
          ),
           
          #### Graphics options and About ####
          splitLayout( cellWidths="35%",
            h3("Options:"),
            actionButton(inputId="GraphicsModal", label='Graphics Options', class="btn btn-primary",style="margin-top: 15px")
          ),
          splitLayout(cellWidths="35%",
            h3("About:"),
            actionButton(inputId="AboutTimeSeries", label="About this Graph...", class="btn btn-primary",style="margin-top: 15px")
          )
          ) #end controls div
        ),         
        
        column(9, 
          plotOutput("TimeSeries"),
          htmlOutput("SeriesThresholdSummary"),
          br(),
          htmlOutput("SeriesTrendsOut"),
          textOutput("SeasonOut"),
          br(),
          htmlOutput("SeriesRefSummary")
        )
      ),
      
      tabPanel(h4("Comparisons"),
        column(3, div(style='padding: 5px 10px',class="panel panel-default", 
                             
          h3("Comparison:"),
          radioButtons(inputId="BoxBy", label="Compare by:", choices=c('year', "month", "site"), selected = "year", inline = T),
          
          h3("Select Stream Data"),
           
          parkChooserUI("BoxPark"),
          siteChooserUI("BoxSite"),
          paramChooserUI("BoxParam"),
          yearChooserUI("BoxYears"),
          
          checkboxInput("BoxThreshLine","Show Water Quality Threshold Line",FALSE),
          
          HTML('<hr>'),
          
          splitLayout(h3(id="DownloadHeader","Downloads:"), cellWidths=c("35%","35%","30%"),
            downloadButton("BoxPlot.PNG","Save Plot (.png)", class="btn btn-primary", style="margin-top: 15px"),
            downloadButton("BoxPlot.JPG","Save Plot (.jpg)", class="btn btn-primary", style="margin-top: 15px")
          ),
          splitLayout( cellWidths="35%",
            h3("Options:"),
            actionButton(inputId="GraphicsModal2", label='Graphics Options', class="btn btn-primary",style="margin-top: 15px")
          ),
          splitLayout(cellWidths="35%",
            h3("About:"),
            actionButton(inputId="AboutComparisons", label="About this Graph...", class="btn btn-primary",style="margin-top: 15px")
          )
        )),
        
        column(9,
          plotOutput("BoxPlot"),
          htmlOutput("BoxThresholdSummary"),
          br(),
          htmlOutput("BoxRefSummary")
        )
      ),

      tabPanel(h4("Map"),
        column(2, div(style='padding: 5px 10px',class="panel panel-default",
          h3("Data to Map"),
          br(),
          strong("National Park Service Monitoring"),
          checkboxInput(inputId="MapNPS", label="Map NPS Water Monitoring", value=T),
          uiOutput("MapChars"),
          HTML('<hr >'),
          br(),
          strong("US Geological Survey Stream Gages"),
          checkboxInput(inputId="MapUSGS", label="Map USGS Gaging Stations (slow)", value=F),
          actionButton(inputId="AboutMap", label="About this Map...", class="btn btn-primary",style="margin-top: 15px")
          
        )),
        column(10, style="padding: 0",
               leafletOutput("WaterMap",width = "100%", height="900px")
        ) 
      ),
      tabPanel(h4("Raw Data"),
               column(3, div(style='padding: 5px 10px',class="panel panel-default", 
                             
                             h3("Select Stream Data"),
                             parkChooserUI("DataPark"),
                             siteChooserUI("DataSite"),
                             paramChooserUI("DataParam")
               )),
               column(9,              
                      DT::dataTableOutput("WaterTable"))
      ),
      
      tabPanel(h4("Project Information"),
      
        includeHTML(paste0(getwd(),"/www/","projectintro.html"))),
      
      tabPanel(h4("Citations & References"),
      
        includeHTML(paste0(getwd(),"/www/","citations.html"))
      )
    )
  )
)