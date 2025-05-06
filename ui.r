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

      tabPanel(
        h4("Time Series")
        ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI("TimePark"))
            ,column(width=4, siteChooserUI("TimeSite"))
            ,column(width=4, paramChooserUI("TimeParam"))
          )
          ,fluidRow(
            column(width=4, yearChooserUI("TimeYears"))
            ,column(width=4, checkboxInput("SeriesThreshLine","Show Water Quality Threshold Line", TRUE))
            ,column(width=2, actionButton(inputId="GraphicsModal2", label='Graphics Options', class="btn btn-primary",style="margin-top: 15px"))
            ,column(width=2, actionButton(inputId="AboutComparisons", label="About this Graph...", class="btn btn-primary",style="margin-top: 15px"))
          )
          ,fluidRow(
            column(width=6, htmlOutput("SeriesThresholdSummaryMultiple"))
            ,column(width=6, htmlOutput("SeriesRefSummaryMultiple"))
          )
        )
        ,fluidRow(
          column(width=12, plotlyOutput("SeriesPlotMultiple"))
        )
      ),  

      tabPanel(
        h4("Boxplot")
        ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI("BoxPark"))
            ,column(width=4, siteChooserUI("BoxSite"))
            ,column(width=4, paramChooserUI("BoxParam"))
          )
          ,fluidRow(
            column(width=4, yearChooserUI("BoxYears"))
            ,column(width=2, radioButtons(inputId="BoxBy", label="Compare by:", choices=c("year", "month", "site"), selected = "year", inline = F))
            ,column(width=2, checkboxInput("BoxThreshLine","Show Water Quality Threshold Line", TRUE))
            ,column(width=2, actionButton(inputId="GraphicsModal2", label='Graphics Options', class="btn btn-primary",style="margin-top: 15px"))
            ,column(width=2, actionButton(inputId="AboutComparisons", label="About this Graph...", class="btn btn-primary",style="margin-top: 15px"))
          )
          ,fluidRow(
            column(width=6, htmlOutput("BoxThresholdSummaryMultiple"))
            ,column(width=6, htmlOutput("BoxRefSummaryMultiple"))
          )
        )
        ,fluidRow(
          column(width=12, plotlyOutput("BoxPlotMultiple"))
        )
      ),  

      tabPanel(
        h4("Correlations")
        ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI("CorrPark"))
            ,column(width=4, siteChooserUI("CorrSite"))
            ,column(width=4, yearChooserUI("CorrYears"))
          )
          ,fluidRow(
            column(width=4, paramChooserUI("CorrParam1"))
            ,column(width=4, paramChooserUI2("CorrParam2"))
            ,column(width=2, actionButton(inputId="GraphicsModal2", label='Graphics Options', class="btn btn-primary",style="margin-top: 15px"))
            ,column(width=2, actionButton(inputId="AboutComparisons", label="About this Graph...", class="btn btn-primary",style="margin-top: 15px"))
          )
        )
        ,fluidRow(
          column(width=12, plotlyOutput("CorrPlot"))
        )
      ),  

      tabPanel(h4("Map"),
        column(2, div(style='padding: 5px 10px',class="panel panel-default",
          uiOutput("MapChars"),

         div(style = "display: block", 
            #  checkboxInput(inputId="InactiveSites", label="Display inactive sites", value=F), 
             checkboxGroupInput(inputId="MapIn",label="Park(s)" , choices=NULL, inline = FALSE),
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
        h4("Summary")
        ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI("SummaryPark"))
            ,column(width=4, siteChooserUI("SummarySite"))
            ,column(width=4, paramChooserUI("SummaryParam"))
          )
          ,fluidRow(
            column(width=4, yearChooserUI("SummaryYears"))
            ,column(width=4, radioButtons(inputId="SummaryBoxBy", label="Compare by:", choices=c("year", "month", "site"), selected = "year", inline = T))
            ,column(width=4, actionButton(inputId="AboutSummary", label="About this Table...", class="btn btn-primary",style="margin-top: 15px"))
          )
        )
        ,uiOutput("summary_box_ui")
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
      
  tabPanel(
    h4("Exceedances")
    ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI("DataParkExceedances"))
            ,column(width=4, siteChooserUI("DataSiteExceedances"))
            ,column(width=4, paramChooserUI("DataParamExceedances"))
          ),fluidRow(
            column(width=4, actionButton("hist_button", "Show Figure"))
          )
        )
    ,uiOutput("mytabs")
     
        ),
        tabPanel(
        h4("Photos")
        ,tabsetPanel(
          tabPanel(
          h4("First Photo")
          ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI("PhotoPark"))
            ,column(width=4, siteChooserUI("PhotoSite"))
            ,column(width=4, yearChooserUI("PhotoYears"))
          )
          ,fluidRow(
            column(width=4, siteVisitChooserUI("PhotoSiteVisit"))
            ,column(width=4, photoChooserUI("PhotoPhoto"))
            ,column(width=4, uiOutput('NoPhotos'))
          )
        )  ,imageOutput("image_plot")      
          )
          ,tabPanel(
          h4("Second Photo")
          ,div(
          style='padding: 5px 10px'
          ,class="panel panel-default"
          ,fluidRow(
            column(width=4, parkChooserUI2("PhotoPark2"))
            ,column(width=4, siteChooserUI2("PhotoSite2"))
            ,column(width=4, yearChooserUI2("PhotoYears2"))
          )
          ,fluidRow(
            column(width=4, siteVisitChooserUI2("PhotoSiteVisit2"))
            ,column(width=4, photoChooserUI2("PhotoPhoto2"))
            ,column(width=4, uiOutput('NoPhotos2'))
          )
        )  ,imageOutput("image_plot2")      
          )
      )
      ),
      
      tabPanel(h4("About"),
      
        includeHTML(paste0(getwd(),"/www/","projectintro.html")))
      
      # tabPanel(h4("Citations & References"),
      
      #   includeHTML(paste0(getwd(),"/www/","citations.html"))
      # )
    )
  )
)