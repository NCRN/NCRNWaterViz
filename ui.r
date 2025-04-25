library(shiny)
library(leaflet)
library(plotly)

ColorNames<-GraphColors$DisplayColor

#Facts and images for loading screen
loading_facts <- c(
  "Did you know that NCRN has been monitoring streams since 2005 A",
  "Did you know that NCRN has been monitoring streams since 2005 B",
  "Did you know that NCRN has been monitoring streams since 2005 C",
  "Did you know that NCRN has been monitoring streams since 2005 D",
  "Did you know that NCRN has been monitoring streams since 2005 E",
  "Did you know that NCRN has been monitoring streams since 2005 F"
)
#Loading screen images with captions
loading_images <- list(
   list(src= "dwq_NCRN_ANTI_SHCK_2024-06-04_20240604-131442.jpg", location = "Sharpsburg Creek, Antietam", date = "June 4, 2024")
  ,list(src= "dwq_NCRN_MONO_BUCK_2024-06-04_20240604-084406.jpg", location = "Bush Creek, Monocacy", date = "June 4, 2024")
  ,list(src= "dwq_NCRN_PRWI_BONE_2024-06-11_20240611-130821.jpg", location = "Boneyard Run, Prince William", date = "June 11, 2024")
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
                    margin-top: 50px;
                    margin: 40px;
                    display: none;
                    }
                    .loading-image {
                    max-width: 700px;
                    width: 40%;
                    max-height: 45%;
                    margin: 0 auto;
                    display: none;
                    }
                    .caption-text {
                    margin-top: 12px; font-size: 18px; color: #333; display: none
                    }
                    .loading-bar-container {
                    margin-top: 20px;
                    height: 12px;
                    width: 40%;
                    background-color: #e0e0e0;
                    overflow: hidden;
                    border-radius: 5px;
                    display: none;
                    }
                    .loading-bar {
                    height: 100%;
                    width: 0%;
                    background-color: #333;
                   # transition: width 0.5s ease;
                    }
                    ")),
      
      #Loading screen HTML div (initially visible)
      div(id = "loading_screen", #"Loading, please wait...",
          style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: white; opacity: 0.8; 
          z-index: 9999; display: flex; align-items: center; justify-content: center; flex-direction: column;",
          
          tags$div(id =  "typingText", class = "typing-text"),
          tags$img(id = "loadingImage", class = "loading-image", alt = "Water monitoring Image"),
          tags$div(id = "captionText"),
          tags$div(id = "progressContainer", class = "loading-bar-container",
                   tags$div(id = "progressBar", class = "loading-bar"))),
      
      tags$script(HTML(sprintf("
              const facts = %s;
              const images = %s;
 
              if (!window.shinyAppLoaded) {
              setTimeout(() => {
                     const fact_index = Math.floor(Math.random() * facts.length);
                     const image_index = Math.floor(Math.random() * images.length);
                   
                   document.getElementById('loading_screen').style.display = 'flex';
                   
                    const fact1 = document.getElementById('typingText'); 
                      fact1.textContent = facts[fact_index];
                      fact1.style.display = 'block';
                    const img1 = document.getElementById('loadingImage')
                      img1.src = images[image_index].src;
                      img1.style.display = 'block';
                    const caption1 = document.getElementById('captionText')
                      caption1.textContent = images[image_index].location + ' - ' + images[image_index].date;
                      caption1.style.display = 'block';  
                    const progressContainer1 = document.getElementById('progressContainer');
                    progressContainer1.style.display = 'block';
                      let progress = 0;
                    const progressBar = document.getElementById('progressBar');
                      let progressInterval;
                 
                function fillTo(target, speed, next) {
                progressInterval = setInterval(() => {
                  if (progress < target) {
                    progress += 1;
                    progressBar.style.width = progress + '%%';
                  } else {
                    clearInterval(progressInterval);
                    if (next) setTimeout(next, 1000);
                    }
                  }, speed);
                }
              
               function waitandfill() {
               if (!window.shinyAppLoaded) {
                    const wait = setInterval(() => {
                    if (window.ShinyAppLoaded) {
                    clearInterval(wait);
                    fillTo(100, 30, () => {
                    setTimeout(() => {
                    document.getElementById('loading_screen').style.display = 'none';
                    }, 1000);
                   });
                  }
                 }, 100);
               } else {
               fillTo(100, 30, () => {
                  setTimeout(() => {
                    document.getElementById('loading_screen').style.display = 'none';
                    }, 1000);
                  });
                 }
               }
              fillTo(30, 60, () => {
              fillTo(85, 80, () => {
              fillTo(99, 100, () => {
              waitandfill();
              });
              });
              });
              }, 1500);
                setTimeout(() => {
                 window.shinyAppLoaded = true; }, 15000);
                               }",
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