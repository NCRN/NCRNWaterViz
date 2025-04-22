library(shiny)
library(leaflet)
library(plotly)

ColorNames<-GraphColors$DisplayColor

#Facts and images for loading screen
loading_facts <- c(
  "Did you know that NCRN has been monitoring streams since 2005! A",
  "Did you know that NCRN has been monitoring streams since 2005! B",
  "Did you know that NCRN has been monitoring streams since 2005! C",
  "Did you know that NCRN has been monitoring streams since 2005! D",
  "Did you know that NCRN has been monitoring streams since 2005! E",
  "Did you know that NCRN has been monitoring streams since 2005! F"
)

loading_images <- c(
  "dwq_NCRN_ANTI_SHCK_2024-06-04_20240604-131442.jpg"
  ,"dwq_NCRN_MONO_BUCK_2024-06-04_20240604-084406.jpg"
  ,"dwq_NCRN_PRWI_BONE_2024-06-11_20240611-130821.jpg"
)
  
shinyUI(
  fluidPage(
    theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css"
    ,style="padding: 0px"
    ,title=paste0(Network, " Water Quality")
    ,column(
      12
      ,id="NPSBanner"
      ,style="margin: 0px"
      ,tags$head(includeScript ("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js"))
      ,tags$head(
        tags$script(
        'type = "text/javascript"'
        ,'
        var ss = document.createElement("link"); ss.type="text/css"; ss.rel="stylesheet"; 
        ss.href = window.self === window.top ? "NCRN.css" : "NCRNframe.css"; document.getElementsByTagName("head")[0].appendChild(ss);
        var dimension = [0, 0];
        $(document).on("shiny:connected", function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
        });
        $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
        });
        '
        )
      )
      ,tags$head(HTML( '<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />'))
      ,div(
        h1(
          style="background-color: black; color: white; height: 125px; padding: 10px; margin: 0px"
          ,HTML('<img src="ah_large_black.gif", style="float:right; padding-right:25px"/>', Network_long, '<br>', Viz_name)
          )
      )
    ),
    
    tags$head(
      #custom styling for loading message
      tags$style(HTML("
                    .typing-text {
                    font-family: monospace;
                    #overflow: hidden;
                    #white-space: nowrap;
                    #width: fit-content;
                    font-size: 24px;
                    color: #333;
                    text-align: center;
                    position: relative;
                    margin-top: 50px
                    }
                    .loading-image {
                    max-width: 800px;
                    max-height: 500px;
                    margin: 0 auto;
                    display: block;
                    }
                    .loader {
                    color: $4CAF50;
                    font-size: 60px;
                    text-indent: -9999em;
                    overflow: hidden;
                    width: 1em;
                    height: 1em;
                    border-radius: 50%;
                    margin-top: 40px;
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
    ),
    
    #Loading screen HTML div (initially visible)
    div(id = "loading_screen", #"Loading, please wait...",
        style = "position: fixed; top: 0;
        left: 0; width: 100%; height: 100%; background-color: white; opacity: 0.8; z-index: 9999; display: flex; align-items: center; justify-content: center;
        flex-direction: column;",
        
        tags$div(id =  "typingText", class = "typing-text", style = "margin: 40px"),
        tags$img(id = "loadingImage", class = "loading-image", src = "", alt = "Water monitoring Image"),
        tags$div(class = "loader"),
        
        tags$script(HTML(sprintf("
                     const facts = %s;
                     const images = %s;
                     const fact_index = Math.floor(Math.random() * facts.length);
                     const image_index = Math.floor(Math.random() * images.length);
                   
              document.addEventListener('DOMContentLoaded', () => {
                     document.getElementById('typingText').textContent = facts[fact_index];
                     document.getElementById('loadingImage').src = images[image_index];})",
                 jsonlite::toJSON(loading_facts, auto_unbox = TRUE),
                 jsonlite::toJSON(loading_images, auto_unbox = TRUE))))
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
      splitLayout(cellWidths="100%",
                  # h3("About:"),
                  actionButton(inputId="AboutSummary", label="About this Table...", class="btn btn-primary",style="margin-top: 15px")
      ))),
      
      column(9,
           # div(class = "summary-box",
              uiOutput("summary_box_ui")
           #  DT::dataTableOutput("SummaryTable")

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
           ")))
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
          
          checkboxInput("SeriesThreshLine","Show Water Quality Threshold Line", TRUE),
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
          
          # splitLayout(cellWidths=c("33%","33%", "33%"),
          splitLayout(cellWidths=c("50%","50%"),
            # downloadButton("BoxPlot.PNG","Save Plot (.png)", class="btn btn-primary", style="margin-top: 15px")
            # ,downloadButton("BoxPlot.JPG","Save Plot (.jpg)", class="btn btn-primary", style="margin-top: 15px")
            actionButton(inputId="GraphicsModal2", label='Graphics Options', class="btn btn-primary",style="margin-top: 15px")
            ,actionButton(inputId="AboutComparisons", label="About this Graph...", class="btn btn-primary",style="margin-top: 15px")
          ),
          br(),
          htmlOutput("SeriesThresholdSummaryMultiple"),
          br(),
          htmlOutput("SeriesRefSummaryMultiple")
          ) #end controls div
        ),         
        
        column(9, 
          plotlyOutput("SeriesPlotMultiple")
        )
      ),
      
      tabPanel(h4("Boxplot"),
        column(3, div(style='padding: 5px 10px',class="panel panel-default", 
                             
          h3("Compare by"),
          radioButtons(inputId="BoxBy", label="", choices=c("year", "month", "site"), selected = "year", inline = T),
          
          h3("Select Site Data"),
           
          parkChooserUI("BoxPark"),
          siteChooserUI("BoxSite"),
          paramChooserUI("BoxParam"),
          yearChooserUI("BoxYears"),
          
          checkboxInput("BoxThreshLine","Show Water Quality Threshold Line", TRUE),
          
          HTML('<hr>'),
          
          # splitLayout(cellWidths=c("25%","25%", "25%", "25%"),
          splitLayout(cellWidths=c("50%","50%"),
            # downloadButton("BoxPlot.PNG","Save Plot (.png)", class="btn btn-primary", style="margin-top: 15px")
            # ,downloadButton("BoxPlot.JPG","Save Plot (.jpg)", class="btn btn-primary", style="margin-top: 15px")
            actionButton(inputId="GraphicsModal2", label='Graphics Options', class="btn btn-primary",style="margin-top: 15px")
            ,actionButton(inputId="AboutComparisons", label="About this Graph...", class="btn btn-primary",style="margin-top: 15px")
          ),
          br(),
          htmlOutput("BoxThresholdSummaryMultiple"),
          br(),
          htmlOutput("BoxRefSummaryMultiple")
          # ,splitLayout( cellWidths="50%",
          #   # h3("Options:"),
          #   actionButton(inputId="GraphicsModal2", label='Graphics Options', class="btn btn-primary",style="margin-top: 15px")
          # )
          # ,splitLayout(cellWidths="50%",
          #   # h3("About:"),
          #   actionButton(inputId="AboutComparisons", label="About this Graph...", class="btn btn-primary",style="margin-top: 15px")
          # )
        )
        ),
        
        column(9,
          plotlyOutput("BoxPlotMultiple", width="auto", height="auto"),
        )
      ),

      tabPanel(h4("Map"),
        column(2, div(style='padding: 5px 10px',class="panel panel-default",
          h3("Data to Map"),
          br(),
          strong("National Park Service Monitoring"),
         # checkboxInput(inputId="MapNPS", label="Map NPS Water Monitoring", value=T),
          uiOutput("MapChars"),

         div(style = "display: block", 
             checkboxInput(inputId="InactiveSites", label="Display inactive sites", value=F), 
             checkboxGroupInput(inputId="MapIn",label="Select Parks:" , choices=NULL, inline = FALSE),
             actionButton("refreshParks", "Reset park selections", class = "btn btn-primary", style = "margin-top: 10px;")),
              
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