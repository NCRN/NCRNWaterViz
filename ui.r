library(shiny)
library(leaflet)
library(plotly)

ColorNames<-GraphColors$DisplayColor

loading_facts <- c(
  "Did you know that NCRN has been monitoring streams since 2005! A",
  "Did you know that NCRN has been monitoring streams since 2005! B",
  "Did you know that NCRN has been monitoring streams since 2005! C",
  "Did you know that NCRN has been monitoring streams since 2005! D",
  "Did you know that NCRN has been monitoring streams since 2005! E",
  "Did you know that NCRN has been monitoring streams since 2005! F"
)


# random <- sample(facts, 1)
# text_length <- nchar(random)
#typing_duration <- paste0(0.1 * text_length, "s")
#typing_steps <- text_length


shinyUI(
  fluidPage( theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", style="padding: 0px",
             title=paste0(Network, " Water Quality"),
    
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
            Network_long, '<br>', Viz_name
        ))
      )
    ),
 
     tags$head(
       #custom styling for loading message
       tags$style(HTML("
                    .typing-text {
                    font-family: monospace;
                    overflow: hidden;
                    white-space: nowrap;
                    border-right: 2px solid black;
                    width: fit-content;
                    font-size: 24px;
                    color: #333;
                    }
                    .loader {
                    color: $4CAF50;
                    font-size: 60px;
                    text-indent: -9999em;
                    overflow: hidden;
                    width: 1em;
                    height: 1em;
                    border-radius: 50%;
                    margin: 72 auto;
                    position: center;
                    -webkit-transform: translateZ(0);
                    -ms-transform: translateZ(0);
                    transform: translateZ(0);
                    -webkit-animation: load6 1.7s infinite ease, round 1.7s infinite ease;
                    animation: load6 1.7s infinite ease, round 1.7s infinite ease;
                    }
                    @-webkit-keyframes load6 {
                      0% {
                        box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;  }
                      5%, 95% {
                        box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;  }
                      10%, 59% {
                        box-shadow: 0 -0.83em 0 -0.4em, -0.087em -0.825em 0 -0.42em, -0.173em -0.812em 0 -0.44em, -0.256em -0.789em 0 -0.46em, -0.297em -0.775em 0 -0.477em;  }
                      20% {
                        box-shadow: 0 -0.83em 0 -0.4em, -0.338em -0.758em 0 -0.42em, -0.555em -0.617em 0 -0.44em, -0.671em -0.488em 0 -0.46em, -0.749em -0.34em 0 -0.477em; }
                      38% {
                        box-shadow: 0 -0.83em 0 -0.4em, -0.377em -0.74em 0 -0.42em, -0.645em -0.522em 0 -0.44em, -0.775em -0.297em 0 -0.46em, -0.82em -0.09em 0 -0.477em; }
                      100% {
                        box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;  }
                    }
                    @keyframes load6 {
                      0% {
                        box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;  }
                      5%,
                      95% {
                        box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;  }
                      10%,
                      59% {
                        box-shadow: 0 -0.83em 0 -0.4em, -0.087em -0.825em 0 -0.42em, -0.173em -0.812em 0 -0.44em, -0.256em -0.789em 0 -0.46em, -0.297em -0.775em 0 -0.477em;  }
                      20% {
                        box-shadow: 0 -0.83em 0 -0.4em, -0.338em -0.758em 0 -0.42em, -0.555em -0.617em 0 -0.44em, -0.671em -0.488em 0 -0.46em, -0.749em -0.34em 0 -0.477em; }
                      38% {
                        box-shadow: 0 -0.83em 0 -0.4em, -0.377em -0.74em 0 -0.42em, -0.645em -0.522em 0 -0.44em, -0.775em -0.297em 0 -0.46em, -0.82em -0.09em 0 -0.477em; }
                      100% {
                        box-shadow: 0 -0.83em 0 -0.4em, 0 -0.83em 0 -0.42em, 0 -0.83em 0 -0.44em, 0 -0.83em 0 -0.46em, 0 -0.83em 0 -0.477em;  }
                    }
                    @-webkit-keyframes round {
                      0% {
                        -webkit-transform: rotate(0deg);
                        transform: rotate(0deg);
                      }
                      100% {
                        -webkit-transform: rotate(360deg);
                        transform: rotate(360deg);
                      }
                    }
                    @keyframes round {
                      0% {
                        -webkit-transform: rotate(0deg);
                        transform: rotate(0deg);
                      }
                      100% {
                        -webkit-transform: rotate(360deg);
                        transform: rotate(360deg);
                      }
                    }
                    "))

                    #' progress-container {
                    #' border-radius: 10px;
                    #' width: 200px;
                    #' margin-top: 30px;
                    #' background-color: #eee;
                    #' }
                    #' .progress-bar {
                    #' height: 10px;
                    #' background-color: $4CAF50;
                    #' border-radius: 10px;
                    #' animation: loading 2s infinite alternate;
                    #' }
                    #' @keyframes loading {
                    #' from { width: 0px; }
                    #' to { width: 200px; }
                    #' }
                    #' "))
       ),

       #Loading screen HTML div (initially visible)
       div(id = "loading_screen", #"Loading, please wait...",
           style = "position: fixed; top: 0;
        left: 0; width: 100%; height: 100%; background-color: white; opacity: 0.8; z-index: 9999; display: flex; align-items: center; justify-content: center;
        flex-direction: column;",
          
        tags$div(id =  "typingText", class = "typing-text", style = "margin: 75px"),
        tags$div(class = "loader"),
      #  tags$div(class = "progress-bar"),
       
      
      tags$script(HTML(sprintf("
                     const facts = %s;
                     const fact = facts[Math.floor(Math.random() * facts.length)];
                     let i=0;
                     
                     function typeFact() {
                     if (i < fact.length) {
                     document.getElementById('typingText').textContent += fact.charAt(i);
                     i++;
                     setTimeout(typeFact, 150);
                      }
                     }
                     document.addEventListener('DOMContentLoaded', typeFact);",
                               jsonlite::toJSON(loading_facts, auto_unbox = TRUE))))
  ),

 #mainPanel(
  tabsetPanel(  
    tabPanel(h4("Summary"),
      column(3, div(style='padding: 5px 10px',class="panel panel-default", 
                             
        h3("Select Site Data"),
                    
        radioButtons(inputId="SummaryBoxBy", label="Compare by:", choices=c("year", "month", "site"), selected = "year", inline = T),
                    
                            parkChooserUI("SummaryPark"),
                            siteChooserUI("SummarySite"),
                            paramChooserUI("SummaryParam"),
                            yearChooserUI("SummaryYears"),
      splitLayout(cellWidths="35%",
                  h3("About:"),
                  actionButton(inputId="AboutSummary", label="About this Table...", class="btn btn-primary",style="margin-top: 15px")
      ))),
      
      column(9,
            div(class = "summary-box",
              uiOutput("summary_text")),
             DT::dataTableOutput("SummaryTable")

       ,tags$head(
        tags$style(HTML("
          .summary-box {
          background-color: #f5f3e5;
          padding: 10px;
          border-radius: 5px;
          margin-bottom: 15px;
          }
          .dt-buttons {
          float: right !important;
          }
          .shiny-notification {
          background-color: #D0342C;
          color: white;
          font-size: 18px;
          border-radius: 10px;
          padding: 15px;
          }
          .shiny-notification-close {
           color: white;
           } 
           "))),
        ),
      ),
      tabPanel(h4("Time Series Plot"),
        column(3, div(style='padding: 5px 10px',class="panel panel-default", 
          
          #textOutput("Test"),  # For debugging purposes
          
          h3("Select Site Data"),
          
          parkChooserUI("TimePark"),
          siteChooserUI("TimeSite"),
          paramChooserUI("TimeParam"),
          yearChooserUI("TimeYears"), 
          
          HTML('<hr >'),
             
          h3(id="ThreshHeader","Thresholds"),
          
          checkboxInput("SeriesThreshLine","Show Water Quality Threshold Line",FALSE),
          checkboxInput("ThreshPoint","Indicate Points with Poor Water Quality",FALSE),
          HTML('<hr>'),
          
          # to hide the "Trends" checkbox, we'll make it only conditionally-visible
          # this approach leaves all of the downstream code intact to avoid breaking dependencies
          # take the contents out of the conditionalPanel and reload the app to restore functionality
          conditionalPanel( 
            condition = "1===2", # a condition that only ever evaluates to FALSE
            # then we move everything that should be conditionally-visible into the body of the panel
            h3(id="TrendHeader","Trends and Seasonal Patterns")
            ,checkboxInput("Trends","Show Seasonal Patterns and Trends",FALSE)
            ,checkboxInput("Outliers","Indicate Outliers Not Used in Analysis",FALSE)
            ,HTML('<hr>')
            ),
          
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
          radioButtons(inputId="BoxBy", label="Compare by:", choices=c("year", "month", "site"), selected = "year", inline = T),
          
          h3("Select Site Data"),
           
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
          
          # to hide the "US Geological Survey Stream Gages" checkbox, we'll make it only conditionally-visible
          # this approach leaves all of the downstream code intact to avoid breaking dependencies
          # take the contents out of the conditionalPanel and reload the app to restore functionality
          conditionalPanel( 
            condition = "1===2", # a condition that only ever evaluates to FALSE
            HTML('<hr >'),
            br(),
            strong("US Geological Survey Stream Gages"),
            checkboxInput(inputId="MapUSGS", label="Map USGS Gaging Stations (slow)", value=F)
          ),
          actionButton(inputId="AboutMap", label="About this Map...", class="btn btn-primary",style="margin-top: 15px")
          
        )),
        column(10, style="padding: 0",
               leafletOutput("WaterMap",width = "100%", height="900px")
        ) 
      ),
      tabPanel(h4("Raw Data"),
               column(3, div(style='padding: 5px 10px',class="panel panel-default", 
                             
                             h3("Select Site Data"),
                             parkChooserUI("DataPark"),
                             siteChooserUI("DataSite"),
                             paramChooserUI("DataParam")
               )),
               column(9,              
                      DT::dataTableOutput("WaterTable"))
      ),
      tabPanel(h4("Exceedances"),
               column(3, div(style='padding: 5px 10px',class="panel panel-default", 
                             
                             h3("Select Site Data"),
                             parkChooserUI("DataParkExceedances"),
                             siteChooserUI("DataSiteExceedances"),
                             paramChooserUI("DataParamExceedances"),
                             actionButton("hist_button", "Show Histogram")
               )),
               column(9,              
                      uiOutput("exceedances_summary"),
                      uiOutput("exceedances_hist"),
                      DT::dataTableOutput("ExceedancesTable"),
                      
                      tags$style(HTML("
    .shiny-notification {
      background-color: #D0342C;
      color: white;
      font-size: 18px;
      border-radius: 10px;
      padding: 15px;
    }
    .shiny-notification-close {
      color: white;
    }
    "))
                      
                      ),
                      
    
               
      ),
      
      tabPanel(h4("Project Information"),
      
        includeHTML(paste0(getwd(),"/www/","projectintro.html"))),
      
      tabPanel(h4("Citations & References"),
      
        includeHTML(paste0(getwd(),"/www/","citations.html"))
      )
    )
  )
)