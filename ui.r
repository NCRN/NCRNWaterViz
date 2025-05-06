library(shiny)
library(leaflet)
library(plotly)

ColorNames<-GraphColors$DisplayColor

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
      tabPanel(h4("Time Series"),
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
          # checkboxInput("ThreshPoint","Indicate Points with Poor Water Quality",FALSE),
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
          # br(),
          htmlOutput("SeriesRefSummaryMultiple")
          ) #end controls div
        ),         
        
        column(9, 
          plotlyOutput("SeriesPlotMultiple")
        )
      ),
      
      tabPanel(h4("Boxplot"),
        column(3, div(style='padding: 5px 10px',class="panel panel-default", 

        h3("Select Site Data"),
                    
        radioButtons(inputId="BoxBy", label="Compare by:", choices=c("year", "month", "site"), selected = "year", inline = T),
           
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
          # br(),
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
      tabPanel(h4("Correlations"),
        column(3, div(style='padding: 5px 10px',class="panel panel-default", 

        h3("Select Site Data"),
                    
        parkChooserUI("CorrPark"),
        siteChooserUI("CorrSite"),
        paramChooserUI("CorrParam1"),
        paramChooserUI2("CorrParam2"),
        yearChooserUI("CorrYears"),

        HTML('<hr>'),
        splitLayout(cellWidths=c("50%","50%"),
          actionButton(inputId="GraphicsModal2", label='Graphics Options', class="btn btn-primary",style="margin-top: 15px")
          ,actionButton(inputId="AboutComparisons", label="About this Graph...", class="btn btn-primary",style="margin-top: 15px")
        )
        )
        )
        ,column(9,
          plotlyOutput("CorrPlot", width="auto", height="auto"),
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
            #  checkboxInput(inputId="InactiveSites", label="Display inactive sites", value=F), 
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
      tabPanel(
        h4("Data")
        ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,h3("Select Site Data")
          ,uiOutput('DatasetURL')
          ,fluidRow(
            column(width=4, parkChooserUI("DataPark"))
            ,column(width=4, siteChooserUI("DataSite"))
            ,column(width=4, paramChooserUI("DataParam"))
          )
        )
        ,DT::dataTableOutput("WaterTable")
      ),
      tabPanel(
        h4("Photos")
        ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,h3("Select Site Data")
          # ,uiOutput('DatasetURL')
          ,fluidRow(
            column(width=4, parkChooserUI("PhotoPark"))
            ,column(width=4, siteChooserUI("PhotoSite"))
            # ,column(width=3, siteChooserUI("PhotoSite"))
            ,column(width=4, yearChooserUI("PhotoYears"))
          )
          ,fluidRow(
            column(width=4, siteVisitChooserUI("PhotoSiteVisit"))
          )
        )
        ,htmlOutput("Photos")
        ,selectInput("image_select", "Select Image:",
                list.files("Data/NCRN/img", pattern = "\\.(jpg|JPG)$", full.names = FALSE))
        ,imageOutput("image_plot")
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
      
      tabPanel(h4("About"),
      
        includeHTML(paste0(getwd(),"/www/","projectintro.html")))
      
      # tabPanel(h4("Citations & References"),
      
      #   includeHTML(paste0(getwd(),"/www/","citations.html"))
      # )
    )
  )
)