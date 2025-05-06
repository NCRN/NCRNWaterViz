library(shiny)
library(lattice)
library(dplyr)
library(lubridate)
library(NCRNWater)
library(DT)
library(htmltools)
library(ggplot2)
library(leaflet)
library(jsonlite)
library(purrr)
library(magrittr)
library(openair)
library(NADA)
library(plotly)

### Filtering data to active Characteristics and Sites ####

# NCRN maintains 'active' and 'inactive' characteristics and sites.

# Through time, NCRN has monitoried different water quality characteristics.
# This means that characteristics that are measured in 2025 may not have been measured in other years
# and characteristics that were measured in 2006 may no longer be measured.
# In Apr 2025, NCRN decided that including "all" characteristics was confusing
# because so many picklist items were inactive.
# To solve this problem, we filter the webapp's dataset to focus on what NCRN does right now.
# For this reason, the lines below filter out inactive characteristics.
# The full dataset (i.e., inactive and active) will be available in IRMA for
# anyone interested in deprecated characteristics.

# Through time, NCRN has monitored different sites; some have been retired and others added.
# Since characteristics also change through time, sites usually have different recordsets (number of rows, unique CharacteristicNames, etc.) depending on which time they came from.
# For example, Donaldson Run (NCRN_GWMP_DORU) has only two site visits from 2005 and is, therefore, difficult to reconcile with modern records because DORU returns zero rows for most queries.
# The result is that the Shiny app is unstable if retired sites are left in the dataset.
# Since the main reason NCRN retires sites is that they do not fit our protocol, we elected to filter-out retired sites instead of
# building the app to accomodate the numerous edge cases caused by retired sites that are fundamentally apples-and-oranges to active sites.

mname2 <- file.path('Data',Network, metadataname2)
dname2 <- file.path('Data',Network, dataname2)
if (file.exists(mname2)==F | file.exists(dname2)==F){
  # filter the metadata
  mname <- file.path('Data',Network,metadataname)
  metadata_df <- read.csv(mname)
  metadata_active <- metadata_df %>%
    dplyr::filter(IsActiveCharacteristicName == "True") %>%
    dplyr::filter(IsActiveSiteCode == "True") 
  write.csv(metadata_active, mname2, row.names = FALSE)
  # filter the data
  active_chars <- metadata_active %>%
    dplyr::pull(DataName) %>% unique
  active_sites <- metadata_active %>%
    dplyr::pull(SiteCode) %>% unique
  dname <- file.path('Data',Network,dataname)
  filtered_data <- read.csv(dname) %>%
    dplyr::filter(CharacteristicName %in% active_chars) %>%
    dplyr::filter(MonitoringLocationIdentifier %in% active_sites)
  write.csv(filtered_data, dname2, row.names = FALSE)
} else {
  metadata_active <- read.csv(mname2)
}

#### Get data ####
WaterData<-suppressWarnings(importNCRNWater(paste0("./Data/", Network), Data=dataname2, MetaData = metadataname2, wqx=wqx_bool))

#### Get photos ####

  dir <- file.path('Data',Network,'img')
  imgs <- list()
  for (f in list.files(dir)){
    # we need the park, site, and date given a filename

    # we have three naming conventions to deal with
    # 1. "WATER_ANTI_SHCK_20240201 (1).JPG"
    # 2. "dwq_NCRN_MONO_BUCK_2024-06-04_20240604-084406.jpg"
    # 3. "ANTI_SHCK_20181210 (8).JPG"

    # to start with, we'll use what the string starts with
    if (base::endsWith(base::tolower(f), 'jpg')){
        
      # step 1: break the filename into pieces

      if (base::startsWith(f, 'WATER')){ # 1. "WATER_ANTI_SHCK_20240201 (1).JPG"
        tmp <- base::strsplit(f, '_')
        # park and site
        park <- tmp[[1]][2]
        site <- tmp[[1]][3]
        # date and index
        tmp <- base::strsplit(tmp[[1]][4], ' ')
        dt <- tmp[[1]][1]
        idx <- base::sub('.JPG', '', tmp[[1]][2])
        idx <- base::sub('.*\\((.*)\\).*', '\\1', idx)
      } else if(base::startsWith(f, 'dwq') | base::startsWith(f, 'cwq')){ # 2. "dwq_NCRN_MONO_BUCK_2024-06-04_20240604-084406.jpg"
        tmp <- base::strsplit(f, '_')
        # park and site
        park <- tmp[[1]][3]
        site <- tmp[[1]][4]
        # date and index
        tmp <- base::strsplit(tmp[[1]][6], '-')
        dt <- tmp[[1]][1]
        idx <- base::sub('.jpg', '', tmp[[1]][2])
      } else { # 3. "ANTI_SHCK_20181210 (8).JPG"
        tmp <- base::strsplit(f, '_')
        # park and site
        park <- tmp[[1]][1]
        site <- tmp[[1]][2]
        # date and index
        tmp <- base::strsplit(tmp[[1]][3], ' ')
        dt <- tmp[[1]][1]
        idx <- base::sub('.JPG', '', tmp[[1]][2])
        idx <- base::sub('.*\\((.*)\\).*', '\\1', idx)
      }

      site <- paste0('NCRN_',park,'_',site)
      yr <- base::substr(dt, 1,4)
      mo <- base::substr(dt, 5,6)
      day <- base::substr(dt, 7,8)
      sitevisit <- paste0(NCRNWater::getSiteInfo(WaterData, parkcode=park, sitecode=site, info="SiteName"), ' ', yr, '-', mo, '-', day)

      # step 2: build the data structure

      # add the park if it does not exist
      if (park %in% names(imgs)==F){
        imgs[[park]] <- list()
      }
      # add the site if it does not exist
      if (site %in% names(imgs[[park]])==F) {
        imgs[[park]][[site]] <- list()
      }
      # add the year if it does not exist
      if (yr %in% names(imgs[[park]][[site]])==F) {
        imgs[[park]][[site]][[yr]] <- list()
      }
      # add the date if it does not exist
      if (sitevisit %in% names(imgs[[park]][[site]][[yr]])==F) {
        imgs[[park]][[site]][[yr]][[sitevisit]] <- list()
      }
      # add the filename if it does not exist
      if (f %in% names(imgs[[park]][[site]][[yr]][[sitevisit]])==F) {
        imgs[[park]][[site]][[yr]][[sitevisit]][[f]] <- list()
      }

      imgs[[park]][[site]][[yr]][[sitevisit]][[f]]$rel_fpath <- file.path(dir, f)
      imgs[[park]][[site]][[yr]][[sitevisit]][[f]]$sortorder <- idx

    }
  }

####getThresholdText Function
getTresholdText<-function(object, parkcode,sitecode,charname){    
 x<-c(getCharInfo(object, parkcode=parkcode, sitecode=sitecode, charname=charname, info="LowerDescription"),
    getCharInfo(object, parkcode=parkcode, sitecode=sitecode, charname=charname, info="UpperDescription"))
  return(x[!is.na(x)])
}

##### Shiny Server ####

shinyServer(function(input,output,session){

  #### Reactive Values for Graphics Options with Defaults ####
  GraphOpts<-shiny::reactiveValues(Legend=TRUE, FontSize=20, GoodColor="Blue", BadColor="Orange",OutColor="Vermillion",PointSize=10,
                              ThColor="Orange", TrColor="Green", LineWidth=2, ShowHidePoint=F, FigureHorizontalScaling=0.7, FigureVerticalScaling=0.7)
  
  #### Reactive Values for Choosing Data ####
  DataOpts<-shiny::reactiveValues(Park=NA, Site=NA, Param=NA, Agg=NA, DateRange=NA, Years=NA, USGSload=FALSE, USGSdata=NA, Param2=NA, SiteVisit=NA, Photo=NA, Park2=NA, Site2=NA, Years2=NA, SiteVisit2=NA, Photo2=NA)

  #### UI Controls ####  
  # Have the threshold lines observe each other so they stay in-sync
  shiny::observeEvent(input$SeriesThreshLine,  {
    updateCheckboxInput(session = session, inputId = "BoxThreshLine", value = input$SeriesThreshLine)
  })
  shiny::observeEvent(input$BoxThreshLine,  {
    updateCheckboxInput(session = session, inputId = "SeriesThreshLine", value = input$BoxThreshLine)
  })
  # Have the group-by (i.e., "Compare by:") radio buttons observe each other so they stay in-sync
  shiny::observeEvent(input$BoxBy,  {
    updateRadioButtons(session = session, inputId = "SummaryBoxBy", selected = input$BoxBy)
  })
  shiny::observeEvent(input$SummaryBoxBy,  {
    updateRadioButtons(session = session, inputId = "BoxBy", selected = input$SummaryBoxBy)
  })

  shiny::observeEvent(input$FigureHorizontalScaling,  {
    GraphOpts$FigureHorizontalScaling <- input$FigureHorizontalScaling
   })
  shiny::observeEvent(input$FigureVerticalScaling,  {
    GraphOpts$FigureVerticalScaling <- input$FigureVerticalScaling
   })

  #### Time Series Controls ####
  # callModule tells the conditional picklists to update
  # e.g., input$TimeSite should only show sites that correspond to the value of input$TimePark
  # and, since `chosen` is always the DataOpts value, the picklist of one tab (e.g., Summary) is synced with all other tabs
  TimePark<-shiny::callModule(
    parkChooser
    ,id="TimePark"
    ,data=WaterData
    ,chosen=reactive(DataOpts$Park)
    )
  TimeSite<-shiny::callModule(
    siteChooser
    ,id="TimeSite"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,chosen=reactive(DataOpts$Site)
    )
  TimeParam<-shiny::callModule(
    paramChooser
    ,id="TimeParam"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,site=reactive(DataOpts$Site)
    ,chosen=reactive(DataOpts$Param)
    )
  TimeYears<-shiny::callModule(
    yearChooser
    ,id="TimeYears"
    ,data=DataUse
    ,chosen=reactive(DataOpts$Years)
    )
  # observeEvent resets the values of conditional picklists to their starting point so downstream code (e.g., data()) won't error
  # when the input specified in the function updates it triggers one or more values to update
  # e.g., when input$TimePark changes in the app, it updates DataOpts$Park, DataOpts$Site, DataOpts$Param, and DataOpts$Years
  shiny::observeEvent(
    TimePark()
    ,{
      DataOpts$Park<-TimePark()
      ;DataOpts$Site<-NA
      # ;DataOpts$Param<-NA
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    TimeSite()
    ,{
      DataOpts$Site<-TimeSite()
      # ;DataOpts$Param<-NA
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    TimeParam()
    ,{
      DataOpts$Param<-TimeParam()
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    TimeYears()
      ,{
        DataOpts$Years<-TimeYears()
        ;shiny::callModule(yearChooser, id="SummaryYears", data=DataUseMultiple, chosen=reactive(DataOpts$Years))
      }
    )
  ### Summary Controls ###
  SummaryPark<-shiny::callModule(
    parkChooser
    ,id="SummaryPark"
    ,data=WaterData
    ,chosen=reactive(DataOpts$Park)
    )
  SummarySite<-shiny::callModule(
    siteChooser
    ,id="SummarySite"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,chosen=reactive(DataOpts$Site)
    )
  SummaryParam<-shiny::callModule(
    paramChooser
    ,id="SummaryParam"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,site=reactive(DataOpts$Site)
    ,chosen=reactive(DataOpts$Param)
    )
  SummaryYears<-shiny::callModule(
    yearChooser
    ,id="SummaryYears"
    ,data=DataUseMultiple
    ,chosen=reactive(DataOpts$Years)
    )
  shiny::observeEvent(
    SummaryPark()
    ,{
      DataOpts$Park<-SummaryPark()
      ;DataOpts$Site<-NA
      # ;DataOpts$Param<-NA
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    SummarySite()
    ,{
      DataOpts$Site<-SummarySite()
      # ;DataOpts$Param<-NA
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    SummaryParam()
    ,{
      DataOpts$Param<-SummaryParam()
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    SummaryYears()
    ,DataOpts$Years<-SummaryYears()
    )
  ### Boxplot Controls ###
  BoxPark<-shiny::callModule(
    parkChooser
    ,id="BoxPark"
    ,data=WaterData
    ,chosen=reactive(DataOpts$Park)
    )
  BoxSite<-shiny::callModule(
    siteChooser
    ,id="BoxSite"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,chosen=reactive(DataOpts$Site)
    )
  BoxParam<-shiny::callModule(
    paramChooser
    ,id="BoxParam"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,site=reactive(DataOpts$Site)
    ,chosen=reactive(DataOpts$Param)
    )
  BoxYears<-shiny::callModule(
    yearChooser
    ,id="BoxYears"
    ,data=DataUseMultiple
    ,chosen=reactive(DataOpts$Years)
    )
  shiny::observeEvent(
    BoxPark()
    ,{
      DataOpts$Park<-BoxPark()
      ;DataOpts$Site<-NA
      # ;DataOpts$Param<-NA
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    BoxSite()
    ,{
      DataOpts$Site<-BoxSite()
      # ;DataOpts$Param<-NA
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    BoxParam()
    ,{
      DataOpts$Param<-BoxParam()
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    BoxYears()
    ,DataOpts$Years<-BoxYears()
    )
  ### Corrplot Controls ###
  CorrPark<-shiny::callModule(
    parkChooser
    ,id="CorrPark"
    ,data=WaterData
    ,chosen=reactive(DataOpts$Park)
    )
  CorrSite<-shiny::callModule(
    siteChooser
    ,id="CorrSite"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,chosen=reactive(DataOpts$Site)
    )
  CorrParam1<-shiny::callModule(
    paramChooser
    ,id="CorrParam1"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,site=reactive(DataOpts$Site)
    ,chosen=reactive(DataOpts$Param)
    )
  CorrParam2<-shiny::callModule(
    paramChooser2
    ,id="CorrParam2"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,site=reactive(DataOpts$Site)
    ,chosen=reactive(DataOpts$Param2)
    )
  CorrYears<-shiny::callModule(
    yearChooser
    ,id="CorrYears"
    ,data=DataUseMultiple
    ,chosen=reactive(DataOpts$Years)
    )
  shiny::observeEvent(
    CorrPark()
    ,{
      DataOpts$Park<-CorrPark()
      ;DataOpts$Site<-NA
      }
    )
  shiny::observeEvent(
    CorrSite()
    ,{
      DataOpts$Site<-CorrSite()
      }
    )
  shiny::observeEvent(
    CorrParam1()
    ,{
      DataOpts$Param<-CorrParam1()
      }
    )
  shiny::observeEvent(
    CorrParam2()
    ,{
      DataOpts$Param2<-CorrParam2()
      }
    )
  shiny::observeEvent(
    CorrYears()
    ,DataOpts$Years<-CorrYears()
    )
  # Data controls
  DataPark<-shiny::callModule(
    parkChooser
    ,id="DataPark"
    ,data=WaterData
    ,chosen=reactive(DataOpts$Park)
    )
  DataSite<-shiny::callModule(
    siteChooser
    ,id="DataSite"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,chosen=reactive(DataOpts$Site)
    )
  DataParam<-shiny::callModule(
    paramChooser
    ,id="DataParam"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,site=reactive(DataOpts$Site)
    ,chosen=reactive(DataOpts$Param)
    )
  shiny::observeEvent(
    DataPark()
    ,{
      DataOpts$Park<-DataPark()
      ;DataOpts$Site<-NA
      # ;DataOpts$Param<-NA
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    DataSite()
    ,{
      DataOpts$Site<-DataSite()
      # ;DataOpts$Param<-NA
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    DataParam()
    ,{
      DataOpts$Param<-DataParam()
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  # Photo controls
  PhotoPark<-shiny::callModule(
    parkChooser
    ,id="PhotoPark"
    ,data=WaterData
    ,chosen=reactive(DataOpts$Park)
    )
  PhotoSite<-shiny::callModule(
    siteChooser
    ,id="PhotoSite"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,chosen=reactive(DataOpts$Site)
    )
  PhotoYears<-shiny::callModule(
    yearChooser
    ,id="PhotoYears"
    ,data=DataUseMultiple
    ,chosen=reactive(DataOpts$Years)
    )
  PhotoSiteVisit<-shiny::callModule(
    siteVisitChooser
    ,id="PhotoSiteVisit"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,site=reactive(DataOpts$Site)
    ,years=reactive(DataOpts$Years)
    ,imgs=reactive(imgs)
    ,chosen=reactive(DataOpts$SiteVisit)
    )
  PhotoPhoto<-shiny::callModule(
    photoChooser
    ,id="PhotoPhoto"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,site=reactive(DataOpts$Site)
    ,years=reactive(DataOpts$Years)
    ,imgs=reactive(imgs)
    ,sitevisit=reactive(DataOpts$SiteVisit)
    )
  shiny::observeEvent(
    PhotoPark()
    ,{
      DataOpts$Park<-PhotoPark()
      ;DataOpts$Site<-NA
      ;DataOpts$SiteVisit<-NA
      # ;DataOpts$Param<-NA
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    PhotoSite()
    ,{
      DataOpts$Site<-PhotoSite()
      ;DataOpts$SiteVisit<-NA
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    PhotoYears()
    ,DataOpts$Years<-PhotoYears()
    )
  shiny::observeEvent(
    PhotoSiteVisit()
    ,{
      DataOpts$SiteVisit<-PhotoSiteVisit()
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    PhotoPhoto()
    ,{
      DataOpts$Photo<-PhotoPhoto()
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  # Photo controls
  PhotoPark2<-shiny::callModule(
    parkChooser2
    ,id="PhotoPark2"
    ,data=WaterData
    ,chosen=reactive(DataOpts$Park2)
    )
  PhotoSite2<-shiny::callModule(
    siteChooser2
    ,id="PhotoSite2"
    ,data=WaterData
    ,park=reactive(DataOpts$Park2)
    ,chosen=reactive(DataOpts$Site2)
    )
  PhotoYears2<-shiny::callModule(
    yearChooser2
    ,id="PhotoYears2"
    ,data=DataUseMultiple
    ,chosen=reactive(DataOpts$Years2)
    )
  PhotoSiteVisit2<-shiny::callModule(
    siteVisitChooser2
    ,id="PhotoSiteVisit2"
    ,data=WaterData
    ,park=reactive(DataOpts$Park2)
    ,site=reactive(DataOpts$Site2)
    ,years=reactive(DataOpts$Years2)
    ,imgs=reactive(imgs)
    ,chosen=reactive(DataOpts$SiteVisit2)
    )
  PhotoPhoto2<-shiny::callModule(
    photoChooser2
    ,id="PhotoPhoto2"
    ,data=WaterData
    ,park=reactive(DataOpts$Park2)
    ,site=reactive(DataOpts$Site2)
    ,years=reactive(DataOpts$Years2)
    ,imgs=reactive(imgs)
    ,sitevisit=reactive(DataOpts$SiteVisit2)
    )
  shiny::observeEvent(
    PhotoPark2()
    ,{
      DataOpts$Park2<-PhotoPark2()
      ;DataOpts$Site2<-NA
      ;DataOpts$SiteVisit2<-NA
      # ;DataOpts$Param<-NA
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    PhotoSite2()
    ,{
      DataOpts$Site2<-PhotoSite2()
      ;DataOpts$SiteVisit2<-NA
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    PhotoYears2()
    ,DataOpts$Years2<-PhotoYears2()
    )
  shiny::observeEvent(
    PhotoSiteVisit2()
    ,{
      DataOpts$SiteVisit2<-PhotoSiteVisit2()
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  shiny::observeEvent(
    PhotoPhoto2()
    ,{
      DataOpts$Photo2<-PhotoPhoto2()
      # ;DataOpts$Years<-c(1900,2100)
      }
    )
  #### Exceedances Controls ####
  ExceedancesPark<-shiny::callModule(
    parkChooser
    ,id="DataParkExceedances"
    ,data=WaterData
    ,chosen=reactive(DataOpts$Park)
    )
  ExceedancesSite<-shiny::callModule(
    siteChooser
    ,id="DataSiteExceedances"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,chosen=reactive(DataOpts$Site)
    )
  ExceedancesParam<-shiny::callModule(
    paramChooser
    ,id="DataParamExceedances"
    ,data=WaterData
    ,park=reactive(DataOpts$Park)
    ,site=reactive(DataOpts$Site)
    ,chosen=reactive(DataOpts$Param)
    )
  shiny::observeEvent(
    ExceedancesPark()
    ,{
      DataOpts$Park<-ExceedancesPark()
      ;DataOpts$Site<-NA
    }
    )
  shiny::observeEvent(
    ExceedancesSite()
    ,DataOpts$Site<-ExceedancesSite()
    )
  shiny::observeEvent(
    ExceedancesParam()
    ,DataOpts$Param<-ExceedancesParam()
    )
#### Graphics Modal Control ####
  
  observeEvent(eventExpr = c( input$GraphicsModal,input$GraphicsModal2), ignoreInit = TRUE,
    showModal(modalDialog(title="Graphics Options", footer=tagAppendAttributes( modalButton(tags$div("Close")), class="btn btn-primary"),
      column(12,hr()),
      column(12,h4("Figure size:"),
        # column(3,checkboxInput("Legend","Show Legend",GraphOpts$Legend)),
        column(6,sliderInput("FigureHorizontalScaling", "Figure Horizontal Scaling", min=0.1, max=1,value=GraphOpts$FigureHorizontalScaling, step=0.1))
        ,column(6,sliderInput("FigureVerticalScaling", "Figure Vertical Scaling", min=0.1, max=1,value=GraphOpts$FigureVerticalScaling, step=0.1))
      ),
      # column(12,hr()),
      column(12,h4("Figure Text:"),
        # column(3,checkboxInput("Legend","Show Legend",GraphOpts$Legend)),
        column(12,sliderInput("FontSize", "Title, legend, and axis label font size", min=10, max=50,value=GraphOpts$FontSize, step=1))
      
      ),
      # column(12,hr()),
      # column(12,hr()),
      column(12, h4("Points:"),
        column(6,sliderInput("PointSize", "Point Size", min=1, max=30,value=GraphOpts$PointSize, step=1))
        # column(3,selectInput("GoodColor","Measurement Color:",choices=GraphColors$DisplayColor, 
        #                    selected=GraphOpts$GoodColor, width='130px')
        # ),
        # ,column(3,selectInput("BadColor","Poor Quality Color:",choices=GraphColors$DisplayColor,selected=GraphOpts$BadColor,
        #                      width='130px') )
        ,column(6,checkboxInput("ShowHidePoint", "Show/hide points", value=GraphOpts$ShowHidePoint))
        # column(3,selectInput("OutColor","Outlier Color:",choices=GraphColors$DisplayColor,selected=GraphOpts$OutColor, width='130px')),   
        
      ),
      # column(12,hr()),
      column(12, h4("Lines:"),
        column(6,sliderInput("LineWidth", "Line Width", min=1, max=10,value=GraphOpts$LineWidth, step=1))
        ,column(6,selectInput("ThColor","Threshold Line Color:",choices=GraphColors$DisplayColor,selected=GraphOpts$ThColor))
        # column(3,selectInput("TrColor","Trend Color:",choices=GraphColors$DisplayColor,selected=GraphOpts$TrColor, width='130px')),
      )
      ,column(12) # required to make the below hr() show up
      ,column(12,hr())
    )
    )
  )
  
  observeEvent(input$Legend, GraphOpts$Legend<-input$Legend)
  observeEvent(input$FontSize, GraphOpts$FontSize<-input$FontSize)
  observeEvent(input$GoodColor, GraphOpts$GoodColor<-input$GoodColor)
  observeEvent(input$BadColor, GraphOpts$BadColor<-input$BadColor)
  observeEvent(input$OutColor, GraphOpts$OutColor<-input$OutColor)
  observeEvent(input$PointSize, GraphOpts$PointSize<-input$PointSize)
  observeEvent(input$ThColor, GraphOpts$ThColor<-input$ThColor)
  observeEvent(input$TrColor, GraphOpts$TrColor<-input$TrColor)
  observeEvent(input$LineWidth, GraphOpts$LineWidth<-input$LineWidth)
  observeEvent(input$ShowHidePoint, GraphOpts$ShowHidePoint<-input$ShowHidePoint)
  
  #### About this ... modals ####
  
  observeEvent(input$AboutTimeSeries, showModal(
    modalDialog(title="About Time Series Graphs", footer=tagAppendAttributes( modalButton(tags$div("Close")), class="btn btn-primary"),
      includeHTML("./www/AboutTimeSeries.html")                  
    )
  ))
  
  observeEvent(input$AboutComparisons, showModal(
    modalDialog(title="About Comparison Graphs", footer=tagAppendAttributes( modalButton(tags$div("Close")), class="btn btn-primary"),
                includeHTML("./www/AboutComparisons.html")                  
    )
  ))
  
  observeEvent(input$AboutMap, showModal(
    modalDialog(title="About the Map", footer=tagAppendAttributes( modalButton(tags$div("Close")), class="btn btn-primary"),
                includeHTML("./www/AboutMap.html")                  
    )
  ))
  
  observeEvent(input$AboutSummary, showModal(
    modalDialog(title="About the Table", footer=tagAppendAttributes( modalButton(tags$div("Close")), class="btn btn-primary"),
                includeHTML("./www/AboutSummary.Rhtml")                  
    )
  ))
  
#### Housekeeping of data ####
  DataUse<-reactive({ 
    # DataUse() is deprecated as of 2025-04-25 because
    # this function will only accept one site but
    # all picklists in the app now allow a user to choose >1 site
    # choosing >1 site and calling DataUse() will crash the app
     shiny::validate(
       need(DataOpts$Park, message="Choose a Park"),
       need(DataOpts$Site, message="Choose a Site"),
       need(DataOpts$Param, message="Choose a Water Quality Parameter")
     )  
    df1 <- getWData(WaterData, parkcode=DataOpts$Park, sitecode=NA, charname=DataOpts$Param)
    df <- suppressWarnings(df1 %>% mutate(year.dec = julian(Date)/365, month = as.factor(months(Date))) %>% 
                              group_by(month) %>% mutate(num_meas = sum(!is.na(Value))) %>% 
                              ungroup()) %>% mutate(num_mos = length(unique(month)))
    
    return(df)
    })
  
#### Housekeeping of data --Multiple site selections ####
 DataUseMultiple <-reactive({
   # Get data to use in a tab that allows for multiple site selections and reacts to user input.
   # Args:
   #  DataOpts$Park, chr, required. The character string provided by parkChooser() in global.R.
   #  DataOpts$Site, chr or vector if multiple sites selected, required. The character string provided by siteChooser() in global.R.
   #  DataOpts$Param, chr, required. The character string provided by paramChooser() in global.R.
   # 
   # Return:
   #  df, data.frame. A dataframe that includes water records for multiple sites.
   #
   # Examples:
   #  table <- DataUseMultiple (
   #    DataOpts$Park = "ANTI",
   #    DataOpts$Site = "NCRN_ANTI_ANCR",
   #    DataOpts$Param = "DOmg"
   #    )
   #    SummaryParam<-shiny::callModule(paramChooser, id="SummaryParam",data=WaterData, park=reactive(DataOpts$Park), site=reactive(DataOpts$Site), chosen=reactive(DataOpts$Param))
   #
    req(DataOpts$Park, DataOpts$Site, DataOpts$Param)

    combined_data <- data.frame()
    for (site in DataOpts$Site) {
      site_data <- getWData(WaterData, parkcode=DataOpts$Park, sitecode= site, charname=DataOpts$Param)
      combined_data <- dplyr::bind_rows(combined_data, site_data)
    }

    df <- suppressWarnings(combined_data %>% 
                             dplyr::mutate(Year = lubridate::year(Date)) %>%
                             dplyr::mutate(year.dec = julian(Date)/365, month = as.factor(months(Date))) %>% 
                             group_by(month) %>% dplyr::mutate(num_meas = sum(!is.na(Value))) %>% 
                             ungroup()) %>% dplyr::mutate(num_mos = length(unique(month)))
    return(df)
  })

 DataUseMultipleParam2 <-reactive({
   # Get data to use in a tab that allows for multiple site selections and reacts to user input.
   # Args:
   #  DataOpts$Park, chr, required. The character string provided by parkChooser() in global.R.
   #  DataOpts$Site, chr or vector if multiple sites selected, required. The character string provided by siteChooser() in global.R.
   #  DataOpts$Param, chr, required. The character string provided by paramChooser() in global.R.
   # 
   # Return:
   #  df, data.frame. A dataframe that includes water records for multiple sites.
   #
   # Examples:
   #  table <- DataUseMultipleParam2 (
   #    DataOpts$Park = "ANTI",
   #    DataOpts$Site = "NCRN_ANTI_ANCR",
   #    DataOpts$Param2 = "DOmg"
   #    )
   #    SummaryParam<-shiny::callModule(paramChooser, id="SummaryParam",data=WaterData, park=reactive(DataOpts$Park), site=reactive(DataOpts$Site), chosen=reactive(DataOpts$Param2))
   #
     req(DataOpts$Park, DataOpts$Site, DataOpts$Param)

    combined_data <- data.frame()
    for (site in DataOpts$Site) {
      site_data <- getWData(WaterData, parkcode=DataOpts$Park, sitecode= site, charname=DataOpts$Param2)
      combined_data <- dplyr::bind_rows(combined_data, site_data)
    }

    df <- suppressWarnings(combined_data %>% 
                             dplyr::mutate(Year = lubridate::year(Date)) %>%
                             dplyr::mutate(year.dec = julian(Date)/365, month = as.factor(months(Date))) %>% 
                             group_by(month) %>% dplyr::mutate(num_meas = sum(!is.na(Value))) %>% 
                             ungroup()) %>% dplyr::mutate(num_mos = length(unique(month)))
    return(df)
  })

#### Thresholds ####
  
  Thresholds<-reactive({
    c(getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="LowerPoint"),
      getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="UpperPoint"))
  })
  
  Units<-reactive({getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="Units") %>% 
    iconv("","UTF-8") 
  })
  
  TrendType <-reactive({
    req(DataOpts$Park, DataOpts$Site, DataOpts$Param, DataUse(), DataUse()$num_meas)
    
    if(input$Trends == FALSE || (input$Trends == TRUE && max(DataUse()$num_meas)< 6)){"notrends"}
    else if(input$Trends == TRUE && max(DataUse()$num_meas >= 6)){
               if(nrow(DataUse()) >= 24 && DataUse()$num_mos >= 6){"wcosinor"}
               else if((nrow(DataUse()) < 24 || DataUse()$num_mos < 6) && any(DataUse()$Censored) == TRUE){"nonparCens"}
               else if((nrow(DataUse()) < 24 || DataUse()$num_mos < 6) && all(DataUse()$Censored) == FALSE){"nonpar"}
    }
    
      })
       
  TrendsOut<-reactive({
    req(DataOpts$Park, DataOpts$Site, DataOpts$Param, DataUse(), TrendType(), input$Trends)

    if(TrendType() == 'wcosinor'){
      wcosinor(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param)
    } else if(TrendType() == 'nonpar'){
      nonparTrends(WaterData, parkcode = DataOpts$Park, 
                   sitecode = DataOpts$Site, charname = DataOpts$Param, 
                   censored = FALSE) %>% arrange(month)
    } else if(TrendType() == 'nonparCens'){
      nonparTrends(WaterData, parkcode = DataOpts$Park, 
                   sitecode = DataOpts$Site, charname = DataOpts$Param, 
                   censored = TRUE) %>% arrange(month)
    } else if(TrendType() == 'notrends'){paste0("notrends")}
    
  })
  
  Title<-reactive({
    paste(getSiteInfo(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, info="SiteName"),
        getCharInfo(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="DisplayName"),sep=": ")
    })
  

#### Get Colors from user inputs ####
  BadCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$BadColor,]$Rcolor})
  GoodCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$GoodColor,]$Rcolor})
  OutCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$OutColor,]$Rcolor})
  ThCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$ThColor,]$Rcolor})
  TrCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$TrColor,]$Rcolor})
  
#### Summary Table ####
  summary <- shiny::reactive({
    # A reactive function that calculates summary statistics based on user selected site(s) and aggregation method (year, month, or site). 
    # Args:
    #  DataOpts$Years, int, required. The character string provided by yearChooser() in global.R. 
    #  input$SummaryBoxBy, chr, required. Determines aggregation type and formats accordingly if month or year is selected. 
    #  
    # Returns:
    #  data_values, data.frame. Calculated statistical values and contains: 
    #     Aggregation, chr, "compare by" grouping variable (year, month, site)
    #     Site, chr, name for selected sites
    #     Minimum, num, minimum value
    #     Q1, num, 1st Quartile (25th percentile)
    #     Mean, num, mean value
    #     Median, num, median value
    #     Q3, num, 3rd Quartile (75th percentile)
    #     Maximum, num, maximum value
    #     SD, num, standard deviation
    #     Site_Visits, int, count of observations for unique site visits
    #     Total_Measurements, int, count of total observations 
    #     Missing_Values, int, count of missing values
    #
    # Example:
    #   DataOpts$Years <- 2010,
    #   input$SummaryBoxBy <- "month"
    #   
    #   summary <- reactive({
    #     DataOpts$Years, 
    #     input$SummaryBoxBy
    #    })
    #
    
    data_values <- DataUseMultiple () %>%
    
    dplyr::filter(Year >= DataOpts$Years[1] & Year <= DataOpts$Years[2]) %>% #year filtering
      
    dplyr::mutate(Date = as.Date(Date)) %>%
    dplyr::mutate(Aggregation = dplyr::case_when(input$SummaryBoxBy == "month" ~ format(Date, "%b"),
                                                   input$SummaryBoxBy == "year" ~ format(Date, "%Y"), TRUE ~ as.character(Site))) %>%
      dplyr::select(Site, Date, Value, Aggregation) %>%
    #   dplyr::arrange(desc(Aggregation)) 
    
   # data_values <- data_values %>%
      
    dplyr::group_by(Aggregation, Site, Date) %>%
    dplyr::summarise(
        SiteVisitMean = mean(Value, na.rm = TRUE))

    data_values_summary <- data_values %>%
    dplyr::group_by(Aggregation, Site) %>%
      dplyr::summarise(
        Minimum = min(SiteVisitMean, na.rm = TRUE),
        Q1 = stats::quantile(SiteVisitMean, 0.25, na.rm = TRUE),
        Mean = mean(SiteVisitMean, na.rm = TRUE),
        Median = stats::median(SiteVisitMean, na.rm = TRUE),
        Q3 = stats::quantile(SiteVisitMean, 0.75, na.rm = TRUE),
        Maximum = max(SiteVisitMean, na.rm = TRUE),
        Standard_Deviation = stats::sd(SiteVisitMean, na.rm = TRUE),
        Site_Visits = dplyr::n_distinct(Date), .groups = "drop")
    
      data_values_summary <- data_values_summary %>%
        dplyr::mutate(
          Standard_Deviation = as.character(Standard_Deviation),
          Standard_Deviation = dplyr::case_when(
            (!is.na(Mean) & is.na(as.numeric(Standard_Deviation))) ~ "Not available",
          is.na(as.numeric(Standard_Deviation)) ~ "Data not collected",
          TRUE ~ formatC(as.numeric(Standard_Deviation), format = "f", digits = 2)),
          Minimum = ifelse(is.na(Minimum) | Minimum == Inf | Minimum == -Inf, "Data not collected",
                           formatC(Minimum, format = "f", digits = 2)),   
          Maximum = ifelse(is.na(Maximum) | Maximum == Inf | Maximum == -Inf, "Data not collected",
                           formatC(Maximum, format = "f", digits = 2))) %>%
      dplyr::mutate(dplyr::across(c(Q1, Mean, Median, Q3), ~ifelse(is.na(.), "Data not collected", formatC(., format = "f", digits = 2))))

      n_count = DataUseMultiple() %>%
        dplyr::filter(Year >= DataOpts$Years[1] & Year <= DataOpts$Years[2]) %>% #year filtering
        
        dplyr::mutate(Date = as.Date(Date)) %>%
      dplyr::mutate(Aggregation = dplyr::case_when(input$SummaryBoxBy == "month" ~ format(Date, "%b"),
                                 input$SummaryBoxBy == "year" ~ format(Date, "%Y"), TRUE ~ as.character(Site))) %>%
      dplyr::group_by(Aggregation, Site) %>%
        dplyr::summarise(Total_Measurements = dplyr::n(), .groups = "drop")
      
      missing_count = DataUseMultiple() %>%
       # dplyr::filter(Year >= DataOpts$Years[1] & Year <= DataOpts$Years[2]) %>% #year filtering
        
        dplyr::mutate(Date = as.Date(Date)) %>%
        dplyr::mutate(Aggregation = dplyr::case_when(input$SummaryBoxBy == "month" ~ format(Date, "%b"),
                                                     input$SummaryBoxBy == "year" ~ format(Date, "%Y"), TRUE ~ as.character(Site))) %>%
        dplyr::filter(is.na(Value)) %>%
        dplyr::group_by(Site, Aggregation) %>%
        dplyr::summarise(Missing_Values = dplyr::n(), .groups = "drop")
      
      final_summary <- data_values_summary %>%
        dplyr::left_join(n_count, by = c("Aggregation", "Site")) %>%
        dplyr::left_join(missing_count, by = c("Aggregation", "Site")) %>%
        dplyr::mutate(Missing_Values = tidyr::replace_na(Missing_Values, 0)) 

      #dplyr::arrange(factor(Aggregation, levels = month.name), Site) 
  return(final_summary) 
})

  output$summary_box_ui <- renderUI({
    req(DataUseMultiple())
    tagList(
    div(class = "summary-box",
        uiOutput("summary_text")),
    DT::dataTableOutput("SummaryTable")
  )
  })
### Summary table output ####
  output$SummaryTable <-DT::renderDataTable({
    # Outputs a table with summary values from summary() and handles missing, infinite, and NAN values. 
    # Converts abbreviated site names, characteristics, and months to full names, as well as grouping within table based on selected aggregation.  
    # Args:
    #  DataOpts$Park, chr, required. The character string provided by parkChooser() in global.R. 
    #  input$SummaryBoxBy, chr, required. Determines aggregation type and formats table style according to selection. 
    # 
    # Return:
    #  DT::datatable. A formatted data table with statistical values according to user input and grouped according to aggregation type (year, month, site).
    # 
    # Example:
    #   DataOpts$Park <- "ANTI",
    #   input$SummaryBoxBy <- "year"
    #   
    #   output$SummaryTable <-DT::renderDataTable({
    #      DataOpts$Park,
    #      input$SummaryBoxBy
    #   })
    
    table <- summary()

    # table <- table %>%
    #   dplyr::mutate(Standard_Deviation = ifelse(!is.na(Mean) & is.na(Standard_Deviation), "Not available", Standard_Deviation),
    #          Minimum = ifelse(is.na(Minimum) | Minimum == Inf | Minimum == -Inf, "Data not collected", Minimum),
    #          Maximum = ifelse(is.na(Maximum) | Maximum == Inf | Maximum == -Inf, "Data not collected", Maximum),
    #          dplyr::across(everything(), ~ifelse(is.na(.), "Data not collected", .)))

    table <- table %>%
      dplyr::mutate(Aggregation = ifelse(Aggregation %in% month.abb, 
                                month.name[match(Aggregation, month.abb)], Aggregation))

   #SiteCodes to full site names
    site_codes <- NCRNWater::getSiteInfo(WaterData, parkcode = DataOpts$Park, info = "SiteCode")
    site_names <- NCRNWater::getSiteInfo(WaterData, parkcode = DataOpts$Park, info = "SiteName")
    
    site_info <- data.frame(SiteCode = site_codes, SiteName = site_names, stringsAsFactors = FALSE)
 
    table <- table %>%
      dplyr::left_join(site_info, by = c("Site" = "SiteCode")) %>%
      dplyr::mutate(Site = ifelse(!is.na(SiteName), SiteName, Site)) %>% 
      dplyr::select(-SiteName)
      
    #Table grouping
    if (input$SummaryBoxBy %in% c("month", "year")) {
      group_headers <- table %>%
      dplyr::distinct(Aggregation) %>%
      dplyr::mutate(Site = Aggregation, Minimum = NA, Q1 = NA, Mean = NA, Median = NA, Q3 = NA, Maximum = NA, 
                    Standard_Deviation = NA, Site_Visits = NA, Total_Measurements = NA, Missing_Values = NA)
      
      aggregation_levels <- if (input$SummaryBoxBy == "month") {
        month.name
      } else {
        sort(unique(as.character(table$Aggregation)), decreasing = TRUE)
      }

    summary_table <- dplyr::bind_rows(group_headers, table) %>%
      dplyr::mutate(Aggregation = factor(Aggregation, levels = aggregation_levels),
                    is_group = Site == Aggregation) %>%
    #                                       c(month.name, 
    #                                                      sort(unique(as.character
    #                                                                  (table$Aggregation[!table$Aggregation %in% month.name])))))) %>%
      
      dplyr::arrange(Aggregation, dplyr::desc(is.na(Minimum)), Site) %>%
      dplyr::mutate(is_group = Site == Aggregation)
    } else {
    summary_table <- table %>%
      dplyr::arrange(Site) %>%
      dplyr::mutate(is_group = FALSE)
    }
    
    # summary_table <- summary_table %>%
    #   ig (input$SummaryBoxBy == "year")
    # list(0, 'desc')
    # else
    #   list(0, 'asc')
    
    summary_table <- summary_table %>%
      dplyr::select(-Aggregation, -is_group)

      tooltips <- list(
        "Site" = "Monitoring location where data was collected",
        "Minimum" = "Lowest recorded value",
        "Q1" = "The first quartile (25th percentile)",
        "Mean" = "The average value",
        "Median" = "Central value in the sorted dataset",
        "Q3" = "The third quartile (75th percentile)",
        "Maximum" = "Highest recorded value",
        "Standard<br>Deviation" = "Measure of variability",
        "Site<br>Visits" = "Number of site visits",
        "Total<br>Measurements" = "Number of measurements",
        "Missing<br>Values" = "Number of missing values"
      )
      tooltips_json <- jsonlite::toJSON(tooltips, auto_unbox = TRUE)
      dt<- DT::datatable(summary_table, colnames = c("Site", "Minimum", "Q1", "Mean", "Median", "Q3", "Maximum", "Standard<br>Deviation", "Site<br>Visits", "Total<br>Measurements", "Missing<br>Values"), escape = FALSE,
                 extensions=c("Buttons", "KeyTable"),
                 #caption=tags$caption(h3(Title())),
                 class="stripe hover order-column cell-border", #filter="top",
                 rownames=F, options=list(paging = FALSE, autoWidth=TRUE, ordering= FALSE,
                                          dom= "<'dt-buttons'B>ltipr", buttons=c("copy","csv","excel","pdf","print"), keys = TRUE,
                                          headerCallback = JS("function(thead, data, start, end, display){", 
                                                              "$(thead).find('th').css('text-align', 'center');",
                                                              "$(thead).find('th').filter(function() { 
                                                              return $(this).html().trim() === 'Site'; }).css('text-align', 'left');",
                                                              "$('th', thead).each(function(index){",
                                                              " var tooltips = ",
                                                              tooltips_json,";",
                                                              " var colName = $
                                                              (this).html().trim();",
                                                              " if(tooltips[colName]) {",
                                                              " $(this).attr('title', tooltips[colName]);",
                                                              " }",
                                                              "});",
                                                              "}"))) %>%
    #,server=F)

      DT::formatStyle(columns = setdiff(names(summary_table), "Site"), textAlign = "center") %>%
      DT::formatStyle(columns = "Site", textAlign = "left", fontWeight = if(input$SummaryBoxBy %in% c("month", "year")) {
      DT::styleEqual(group_headers$Site, rep("bold", nrow(group_headers)))
      } else if (input$SummaryBoxBy == "site") { "bold" 
      } else { 
        NULL }
      ,backgroundColor = if(input$SummaryBoxBy %in% c("month", "year")) {
       DT::styleEqual(group_headers$Site, rep("#f0f0f0", nrow(group_headers)))
      } else {
        NULL
      }
     )
    })
  
  #Notification pop-up when all data is missing in table
  shiny::observe({
    # An observation that outputs a pop-up notification if all data in summary table is NA for selected site(s) and parameter. Only accounts for numeric columns and cleans up inf and NaN values.
    # Args:
    #  session$userData$popup, session, required. Stores user specific session data and controls whether a notification should or should not be displayed.
    #  DataOpts$Param, chr, required. The character string provided by paramChooser() in global.R that indicates user parameter selection. 
    # 
    # Returns:
    #  NULL, no pop-up notification if numeric value is present in summary table.
    #  or
    #  Pop-up notification, indicates that no data was recorded at selected site(s) for selected parameter. 
    #
    # Example:
    #  DataOpts$Param <- "DOmg"
    #
    #  shiny::observe({
    #     if (!session$userData$popup)
    #     shiny::showNotification(
    #     paste("No data collected for", DataOpts$Param, "at selected sites."), type = "warning")
    #     session$userData$popup <- TRUE
    #   })
    
    shiny::req(summary(), DataOpts$Param)
    notifs <- summary()
    
    numeric_table <- notifs %>%
      dplyr::select(-c(Total_Measurements, Site_Visits, Missing_Values, Aggregation, Site))
    
    numeric_table <- as.data.frame(numeric_table)
    numeric_table[is.infinite(as.matrix(numeric_table)) | is.nan(as.matrix(numeric_table))] <- NA
    
    session$userData$popup <- FALSE 
    
    if (nrow(numeric_table) > 0 &&
        all(is.na(numeric_table))) {
       if (!isTRUE(shiny::isolate(session$userData$popup))) {
      shiny::showNotification(
        paste("No data collected for", DataOpts$Param, "at selected sites."), 
        type = "warning", 
        duration = 10, 
        id= "noDataPopup")

    session$userData$popup <- TRUE }
    } else {
    session$userData$popup <- FALSE }
    }, priority = 1)

  
### Summary text ###
  summary_text_data <- reactive({
    # Generates a reactive summary text to display mean and median values for the selected parameter and site(s), along with the 
    #       highest and lowest recorded values across sites, including the month and year they occurred. 
    # Args:
    #   DataOpts$Park, chr, required. The character string provided by parkChooser() in global.R.
    #   DataOpts$Site, chr or vector if multiple sites selected, required. The character string provided by siteChooser() in global.R.
    #   DataOpts$Param, chr, required. The character string provided by paramChooser() in global.R.
    #   DataOpts$Years, int. The character string provided by yearChooser() in global.R. 
    #                              Default set to the minimum and maximum recorded years for selected sites and parameter, can be modified accordingly.
    #   
    # Returns:
    #   chr. A character string containing reactive formatted HTML text.
    #
    # Example:
    #     DataOpts$Park<-"ANTI"
    #     DataOpts$Site<- "NCRN_ANTI_ANCR",
    #     DataOpts$Param<- "DOmg",
    #     DataOpts$Years<- 2008,   
    #
    #   summary_text_data <- reactive({
    #     DataOpts$Park,
    #     DataOpts$Site,
    #     DataOpts$Param,
    #     DataOpts$Years
    #   })
    
    req(DataOpts$Park, DataOpts$Site, DataOpts$Param)
    raw_data <- DataUseMultiple () %>%
      dplyr::filter(Year >= DataOpts$Years[1] & Year <= DataOpts$Years[2])
    
    #Full characteristic name
    char_info <- data.frame(
      Char = NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, info = "CharName"),
      CharName = NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, info = "DisplayName"), stringsAsFactors = FALSE)
    
    FullParamName <- char_info$CharName[match(DataOpts$Param, char_info$Char)]
    
    #Full site names
    site_info <- data.frame(
      Site = NCRNWater::getSiteInfo(WaterData, parkcode = DataOpts$Park, info = "SiteCode"),
      SiteName = NCRNWater::getSiteInfo(WaterData, parkcode = DataOpts$Park, info = "SiteName"), stringsAsFactors = FALSE)

    #Site visit average
    site_visit_data <- raw_data %>%
      dplyr::group_by(Site, Date) %>%
      dplyr::summarise(text_sitevisit_mean = mean(Value, na.rm = TRUE), .groups = "drop")
    
    #Summary text calculated values
    text_summary_stats <- site_visit_data %>%
      dplyr::group_by(Site) %>%
      dplyr::summarise(
        Mean = mean(text_sitevisit_mean, na.rm = TRUE),
        Median = stats::median(text_sitevisit_mean, na.rm = TRUE),
        Maximum = max(text_sitevisit_mean, na.rm = TRUE),
        Minimum = min(text_sitevisit_mean, na.rm = TRUE),
        Date_max = Date[which.max(text_sitevisit_mean)],
        Date_min = Date[which.min(text_sitevisit_mean)],
        .groups = "drop") %>%
      dplyr::right_join(site_info %>%
      dplyr::filter(Site %in% DataOpts$Site), by = "Site")
      

    summary_units <- unique(NCRNWater::getCharInfo(WaterData,parkcode=DataOpts$Park, charname=DataOpts$Param, info="Units"))
    
    site_list <- paste(
      sapply(1:length(text_summary_stats$Site), function(i) { 
          site_name <- site_info$SiteName[match(text_summary_stats$Site[i], site_info$Site)]
          site_code <- text_summary_stats$Site[i]
          site_data <- text_summary_stats %>%
            dplyr::filter(Site == site_code)
          
          if (nrow(site_data) == 0 ||
              all(is.na(site_data$Mean))) {
                        paste0("<li><b>", site_name, " -</b> Data not collected </li>")
          } else {
                        paste0("<li><b>", site_name, " -</b> Mean: ", round(site_data$Mean, 2), " ", summary_units,
                        ", Median: ", round(site_data$Median, 2), " ", summary_units, ".</li>") }
      }), 
      collapse = "")

    #Highest/Lowest values
    highest_value <- text_summary_stats[which.max(text_summary_stats$Maximum), ]
    lowest_value <- text_summary_stats[which.min(text_summary_stats$Minimum), ]

    highest_lowest_sentence <- ""
      if (nrow(highest_value) > 0 & nrow(lowest_value) > 0) {
    highest_month <- format(as.Date(highest_value$Date_max), "%B")
    highest_year <- format(as.Date(highest_value$Date_max), "%Y")
    lowest_month <- format(as.Date(lowest_value$Date_min), "%B")
    lowest_year <- format(as.Date(lowest_value$Date_min), "%Y")
    
    highest_lowest_sentence <- paste0(
            "<p>The highest ", FullParamName, " value across selected sites was ", round(highest_value$Maximum, 2), " " 
            ,summary_units, " at ", highest_value$SiteName, " in ", highest_month, " ", highest_year
            ,", while the lowest value was ", round(lowest_value$Minimum, 2), " ", summary_units, " at ", lowest_value$SiteName 
            ," in ", lowest_month, " ", lowest_year, ".</p>")}

    #Generate text
    HTML(paste0(
            "<p><b><span style='font-size: 18px;'>Summary Report:</b></p>"
            ,"<p>The mean and median values for ", FullParamName, " at the selected sites and years are as follows:</p>" 
            ,"<ul>", site_list, "</ul>", highest_lowest_sentence, 
            "<p>*Summary statistics of site visit averages.</p>")
            )
    })
  
  output$summary_text <- shiny::renderText({
    # Renders the reactive summary text created by summary_text_data(). Displays mean and median values for parameter at selected site(s), along with the 
    #       highest and lowest recorded values across sites, including the month and year they occurred.
    # Args: 
    #   None
    #
    # Return:
    #  chr. A character string containing the reactive HTML text from summary_text_data() for output in UI.  
    # 
    # Example:
    #  output$summary_text <-shiny::renderText({
    #      summary_text_data()
    #   })
    
    summary_text_data()
})
  
#### Summaries of Seasonality and Trends ####
  # output$SeasonOut<-renderText({

  #   req(input$Trends, isTruthy(TrendsOut()), 
  #       TrendType())
  #   if(TrendType() == 'wcosinor') {
  #   switch(class(TrendsOut()$Analysis),
  #     "lm" =      c("There is no seasonal pattern in the data."),
  #     "Cosinor" = c("There is a seasonal pattern in the data. The peak is", strsplit(summary(TrendsOut()$Analysis)$phase," ")[[1]][3],
  #                 strsplit(summary(TrendsOut()$Analysis)$phase," ")[[1]][7], "and the low point is ",
  #       strsplit(summary(TrendsOut()$Analysis)$lphase," ")[[1]][3],paste0(strsplit(summary(TrendsOut()$Analysis)$lphase," ")[[1]][7] ,"." 
  #     )), NULL)
      
  #   }
  # })

  # SeriesTrendsOut<-reactive({
  #   req(input$Trends, isTruthy(TrendsOut()), isTruthy(TrendType()))

  #   outmessage <- 
  #     paste(h4("Trend Analysis:"),"\n",
  #       if(TrendType() == "wcosinor" && !is.na(TrendsOut()$Analysis)){
  #       paste(switch(class(TrendsOut()$Analysis),
  #       "lm" =  {
  #         if(summary(TrendsOut()$Analysis)$coefficients[2,4]>.05) {("There is no significant trend in the data.")} 
  #         else {
  #           paste("There is a significant", 
  #           ifelse (summary(TrendsOut()$Analysis)$coefficients[2,1] > 0, "increasing", "decreasing"),
  #           "trend of",c(signif(summary(TrendsOut()$Analysis)$coefficients[2,1]*365.24, digits=3)),
  #           Units(), "per year.")
  #         }
  #       }, 
  #       "Cosinor"=  {
  #         if(summary(TrendsOut()$Analysis$glm)$coefficients[2,4]>.05){("There is no significant trend in the data")}
  #         else {
  #           paste("There is a significant",
  #           ifelse (summary(TrendsOut()$Analysis$glm)$coefficients[2,1]>0,"increasing","decreasing"), 
  #           "trend of",c(signif(summary(TrendsOut()$Analysis$glm)$coefficients[2,1]*365.24,digits=3)),
  #           Units(), "per year."
  #           )
  #         }
  #         }, NULL))
  #       } else if(TrendType() %in% c("nonparCens", "nonpar")){
          
  #         paste(
  #         if(TrendType() == "nonparCens" && !all(TrendsOut()$modeled == FALSE)){ 
  #           "Data were separated by month for censored Mann-Kendall test. 
  #           Solid lines are significant trends. Dashed lines are non-significant trends."
  #         } else if(TrendType() == "nonpar" && !all(TrendsOut()$modeled == FALSE)){
  #           "Data were separated by month for Mann-Kendall test. 
  #           Solid lines are significant trends. Dashed lines are non-significant trends."},
          
  #         if(any(TrendsOut()$message == "no trend")){
  #          paste(br(), "The following months were modeled and found no significant trends: ",
  #                paste0(TrendsOut()$month[TrendsOut()$message=="no trend"], collapse=", "), ". ", sep = "")},

  #         if(all(TrendsOut()$modeled == FALSE)){
  #           paste(br(), "There were too few non-censored measurements to analyze for trends.")},
          
  #         if(any(TrendsOut()$modeled == FALSE) && any(!is.na(TrendsOut()$pval))){
  #           paste(br(), "The following months had too few non-censored measurements to analyze for trends and were not plotted: ",
  #                 paste0(TrendsOut()$month[TrendsOut()$modeled == FALSE], collapse=", "), ".", sep = "")},
          
          
  #         if(any(grepl("There", TrendsOut()$message))){
  #          paste(TrendsOut()$message[TrendsOut()$modeled==TRUE & grepl("There", TrendsOut()$message)], sep="")
  #         }
  #         ) #end of nonparCen/nonpar paste

  #       } else if(TrendType() == 'notrends' & TrendsOut() == 'notrends'){
  #         paste("There were too few non-censored measurements to plot and analyze for trends.")}
  #         ) 
  # return(outmessage)
  # })
  
  # output$SeriesTrendsOut<-renderUI(HTML(SeriesTrendsOut()))
  
#### Threshold Summary ####
  # ThresholdSummary<-reactive({    
  #   req(input$SeriesThreshLine | input$ThreshPoint)
  #   paste(h4("Threshold:"),"\n", 
  #         if(all(is.na(Thresholds()))){ "There is no water quality threshold for this parameter." } else {
  #     c(getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="LowerDescription"),
  #     getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, 
  #                 info="UpperDescription"))[!is.na(Thresholds())] }
  #   ) 
  # })
  
  # output$SeriesThresholdSummary<-renderUI( HTML(ThresholdSummary()) )
  
  
  # RefSummary<-reactive({
  #   req(input$SeriesThreshLine | input$ThreshPoint) 
  #   paste(h4("Threshold Reference:"),"\n",
  #     if (all(is.na(Thresholds()))) {"None"} else {
  #     getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="AssessmentDetails")}
  #   ) 
  # })      
  
  # output$SeriesRefSummary<-renderUI(HTML(RefSummary()))

  
#### Time Series Plot 2.0 ####
WaterSeriesOutMultiple <- reactive({
    # A reactive function that a plotly of boxplots based on user selected site(s) and aggregation method (year, month, or site). 
    # Args:
    #  DataOpts$Years, c(int), required. The character string provided by yearChooser() in global.R. 
    #  input$SummaryBoxBy, chr, required. Determines aggregation type and formats accordingly if month or year is selected. 
    #  input$BoxThreshLine, bool, optional. Default False. If True, looks up the water quality threshold.
    #  DataOpts$Park, chr, required. A park acronym. E.g., 'ROCR'.
    #  DataOpts$Site, chr or c(chr), required. A site code. E.g., 'NCRN_ROCR_KLVA'
    #  DataOpts$Param, chr, required. A characteristic abbreviation. E.g., 'DOper'.
    #  
    # Returns:
    #  Plotly figure
    # 
    # Example:
    #   DataOpts$Years <- c(2010,2024),
    #   input$SummaryBoxBy <- "year"
    #   input$BoxThreshLine <- T
    #   DataOpts$Park <- 'ROCR'
    #   DataOpts$Site <- c('NCRN_ROCR_KLVA', 'NCRN_ROCR_FEBR')
    #   DataOpts$Param <- 'DOper'
    #   
    #   myfigure <- BoxPlotMutlipleOut(
    #     DataOpts$Years
    #     ,input$SummaryBoxBy
    #     ,input$SeriesThreshLine
    #     ,DataOpts$Park
    #     ,DataOpts$Site
    #     ,DataOpts$Param
    #   )
    #
  req(DataOpts$Park, DataOpts$Site, DataOpts$Param)

  # https://github.com/NCRN/NCRNWater/blob/87a16069713e2ea188d8bb8a2ae0cab97a43af4f/R/waterbox.R#L73
  series_df <- DataUseMultiple() %>%
    dplyr::filter(Year >= DataOpts$Years[1] & Year <= DataOpts$Years[2]) %>% #year filtering
    dplyr::arrange(MonitoringLocationName, Date)

  # initialize variables
  ynames <- c()
  xname <- NA
  labels <- NA
  assessment <- input$SeriesThreshLine
  assessments <- c()
  threshold <- NA
  units <- c()
  displaynames <- c()

  # https://github.com/NCRN/NCRNWater/blob/87a16069713e2ea188d8bb8a2ae0cab97a43af4f/R/waterbox.R#L75-L98
  for (site in DataOpts$Site){
    displayname <- getCharInfo(
        object=WaterData
        ,parkcode=DataOpts$Park
        ,sitecode = site
        ,charname=DataOpts$Param
        , info="DisplayName"
        )
      displaynames <- c(displaynames, displayname)
      unit <- getCharInfo(
        object=WaterData
        ,parkcode=DataOpts$Park
        ,sitecode = site
        ,charname=DataOpts$Param
        ,info="Units"
        )
      units <- c(units, unit)
    yname<-paste0(displayname," (", unit,")")
    ynames <- c(yname, ynames)
  }

  units <- units %>% unique
  displaynames <- displaynames %>% unique

  # resolve conflicts that would happen if the metadata file was messed up
  # e.g., if one characteristic had multiple units
  n_ynames <- length(ynames %>% unique)
  if (n_ynames == 1){
    yname <- ynames %>% unique
  } else if (n_ynames == 0){
    yname <- ''
  } else {
    yname <- ynames[1]
  }

  xname <- 'Date'

  if(assessment){
      for (site in DataOpts$Site){
        tmp<-c(getCharInfo(object=WaterData,parkcode=DataOpts$Park, sitecode=site, charname=DataOpts$Param, info="LowerPoint"),
          getCharInfo(object=WaterData,parkcode=DataOpts$Park, sitecode=site, charname=DataOpts$Param, info="UpperPoint")) %>%
          unlist %>% unique
        assessments <- c(tmp, assessments)
    }
    threshold <- assessments %>% unique
    threshold <- threshold[!is.na(threshold)] # needed if there is no upper or lower threshold.
  }
  
  # https://github.com/NCRN/NCRNWater/blob/87a16069713e2ea188d8bb8a2ae0cab97a43af4f/R/waterbox.R#L106-L127

  n_not_na <- nrow(series_df %>% dplyr::filter(is.na(Value)==F))
  n_na <- nrow(series_df %>% dplyr::filter(is.na(Value)))
  title <- paste0(NCRNWater::getParkInfo(object=WaterData, parkcode=DataOpts$Park, info="ParkLongName"), ': ', yname, '\nYears: ',DataOpts$Years[1], '-', DataOpts$Years[2],'; Total measurements: ',n_not_na+n_na,' (non-NA: ', n_not_na, ', NA: ', n_na,')')

  m <- list( # figure margins
    l = 100,
    r = 50,
    b = 100,
    t = 100,
    pad = 20
  )
  t <- list(
    size = global_textsize
    )
  baseplot <-
    plotly::plot_ly(
      series_df
      ,y= ~Value
      ,x= ~Date
      ,color= ~MonitoringLocationName
      ,symbol= ~MonitoringLocationName
      ,type='scatter'
      ,mode='lines'
      ,connectgaps=TRUE # set to FALSE to create breaks in the line for NAs
      ,width = (GraphOpts$FigureHorizontalScaling*as.numeric(input$dimension[1])) # to dynamically resize fig
      ,height = (GraphOpts$FigureVerticalScaling*as.numeric(input$dimension[2]))
      ,line=list(width=input$LineWidth)
      ,marker=list(
        size=input$PointSize
        ,opacity=as.numeric(input$ShowHidePoint)
        )
      ,hovertemplate = paste(
        "<br>Date :", series_df$Date
        ,"<br>Site :", series_df$MonitoringLocationName
        ,"<br>", yname, ": ", series_df$Value
        # extra is a secondary bit of hovertext that's visible on the right-ide of the main hovertext
        # https://community.plotly.com/t/disabling-default-tooltip-while-using-a-hovertemplate-in-python/85824/3
        ,'<extra></extra>'
        )
      ,text=NULL
    ) %>% layout(
      font=list(size=input$FontSize)
      ,margin=m
      ,title = list(text=title ,font=list(size=input$FontSize))
      ,legend = list(
        title=list(text='<br>Site<br>')
        ,font=list(size=input$FontSize)
        )
      ,showlegend=T
      ,yaxis = list(title=list(text=paste0(yname, '<br>'), font=list(size=input$FontSize)), font=list(size=input$FontSize))
      ,xaxis = list(title=list(text=paste0(xname, '<br>'), font=list(size=input$FontSize)), font=list(size=input$FontSize))
    )   

  if (assessment==T & identical(threshold, numeric(0))==F) {
    # a <- list( # commented-out because the annotation doesn't look great
    #   x = 1,
    #   y = 0.95*threshold,
    #   text = paste0(stringr::str_split_1(yname, '[(]')[1], 'threshold: ', threshold, ' ', stringr::str_extract(yname, '(?<=\\()[^\\^\\)]+')),
    #   xref = "x",
    #   yref = "y",
    #   showarrow = F,
    #   ax = 20,
    #   ay = -40
    # )
    if (length(threshold)==2){
      baseplot %>% layout(
      shapes = list(
        hline(threshold[1])
        ,hline(threshold[2])
        )
      # ,annotations = a # commented-out because the annotation doesn't look great
    )

    } else if (length(threshold)==1){
      baseplot %>% layout(
        shapes = list(hline(threshold))
      # ,annotations = a # commented-out because the annotation doesn't look great
      )
    }
  } else {
    baseplot
  }
  })

output$SeriesPlotMultiple<-renderPlotly({   WaterSeriesOutMultiple() })

CorrPlotOutMultiple <- reactive({
    # A reactive function that returns a plotly scatterplot of two user-selected parameters. 
    # Args:
    #  DataOpts$Years, c(int), required. The character string provided by yearChooser() in global.R. 
    #  DataOpts$Park, chr, required. A park acronym. E.g., 'ROCR'.
    #  DataOpts$Site, chr or c(chr), required. A site code. E.g., 'NCRN_ROCR_KLVA'
    #  DataOpts$Param, chr, required. A characteristic abbreviation. E.g., 'DOper'.
    #  DataOpts$Param2, chr, required. A characteristic abbreviation. E.g., 'TN'.
    #  
    # Returns:
    #  Plotly figure
    # 
    # Example:
    #   DataOpts$Years <- c(2010,2024),
    #   DataOpts$Park <- 'ROCR'
    #   DataOpts$Site <- c('NCRN_ROCR_KLVA', 'NCRN_ROCR_FEBR')
    #   DataOpts$Param <- 'DOper'
    #   DataOpts$Param2 <- 'TN'
    #   
    #   myfigure <- CorrPlotOutMultiple(
    #     DataOpts$Years
    #     ,input$SummaryBoxBy
    #     ,input$SeriesThreshLine
    #     ,DataOpts$Park
    #     ,DataOpts$Site
    #     ,DataOpts$Param
    #     ,DataOpts$Param2
    #   )
    #
  req(DataOpts$Park, DataOpts$Site, DataOpts$Param, DataOpts$Param2)

  # https://github.com/NCRN/NCRNWater/blob/87a16069713e2ea188d8bb8a2ae0cab97a43af4f/R/waterbox.R#L73
  series_df <- DataUseMultiple() %>%
    dplyr::filter(Year >= DataOpts$Years[1] & Year <= DataOpts$Years[2]) %>% #year filtering
    dplyr::arrange(MonitoringLocationName, Date)
  
  # process the dataframe
  # n will vary depending on which `Param` is chosen (e.g., there will only ever be 1 air temp but there could be >1 water temp for one site visit)
  # so we need to normalize the data: calculate the mean `Value` per site visit
  # then we can plot the means against each other
  colname_lookup <- c(Characteristic_y = 'Characteristic', sitevisit_meanvalue_y = 'sitevisit_meanvalue')
  df_firstparam <- DataUseMultiple() %>%
    dplyr::filter(Year >= DataOpts$Years[1] & Year <= DataOpts$Years[2]) %>% #year filtering
    dplyr::arrange(MonitoringLocationName, Date) %>%
    dplyr::group_by(ActivityMediaSubdivisionName, MonitoringLocationName, Characteristic, Date) %>%
    dplyr::summarize(sitevisit_meanvalue = mean(Value)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(all_of(colname_lookup))
  # print(head(df_firstparam))

  colname_lookup <- c(Characteristic_x = 'Characteristic', sitevisit_meanvalue_x = 'sitevisit_meanvalue')
  df_secondparam <- DataUseMultipleParam2() %>%
    dplyr::filter(Year >= DataOpts$Years[1] & Year <= DataOpts$Years[2]) %>% #year filtering
    dplyr::arrange(MonitoringLocationName, Date) %>%
    dplyr::group_by(ActivityMediaSubdivisionName, MonitoringLocationName, Characteristic, Date) %>%
    dplyr::summarize(sitevisit_meanvalue = mean(Value)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(all_of(colname_lookup)) %>%
    dplyr::select(ActivityMediaSubdivisionName, Characteristic_x, sitevisit_meanvalue_x)
  # print(head(df_secondparam))
  
  print(paste0('Is the row count equal in each dataframe? , ',nrow(df_firstparam) == nrow(df_secondparam)))
  print(paste0('nrow df_firstparam: ',nrow(df_firstparam)))
  print(paste0('nrow df_secondparam: ',nrow(df_secondparam)))

  df <- dplyr::inner_join(df_firstparam, df_secondparam, by=dplyr::join_by(ActivityMediaSubdivisionName))
  print(head(df))
  
  # initialize variables
  ynames <- c()
  xnames <- c()
  labels <- NA
  assessment <- input$SeriesThreshLine
  assessments <- c()
  threshold <- NA
  units <- c()
  displaynames <- c()

  # https://github.com/NCRN/NCRNWater/blob/87a16069713e2ea188d8bb8a2ae0cab97a43af4f/R/waterbox.R#L75-L98
  for (site in DataOpts$Site){
    displayname <- getCharInfo(
        object=WaterData
        ,parkcode=DataOpts$Park
        ,sitecode = site
        ,charname=DataOpts$Param
        , info="DisplayName"
        )
      displaynames <- c(displaynames, displayname)
      unit <- getCharInfo(
        object=WaterData
        ,parkcode=DataOpts$Park
        ,sitecode = site
        ,charname=DataOpts$Param
        ,info="Units"
        )
      units <- c(units, unit)
    yname<-paste0(displayname," (", unit,")")
    ynames <- c(yname, ynames)
  }

  units <- units %>% unique
  displaynames <- displaynames %>% unique

  # resolve conflicts that would happen if the metadata file was messed up
  # e.g., if one characteristic had multiple units
  n_ynames <- length(ynames %>% unique)
  if (n_ynames == 1){
    yname <- ynames %>% unique
  } else if (n_ynames == 0){
    yname <- ''
  } else {
    yname <- ynames[1]
  }
  n_displaynames <- length(displaynames %>% unique)
  if (n_displaynames == 1){
    yvarname <- displaynames %>% unique
  } else if (n_displaynames == 0){
    yvarname <- ''
  } else {
    yvarname <- displaynames[1]
  }

  displaynames <- c()
  for (site in DataOpts$Site){
    displayname <- getCharInfo(
        object=WaterData
        ,parkcode=DataOpts$Park
        ,sitecode = site
        ,charname=DataOpts$Param2
        , info="DisplayName"
        )
      displaynames <- c(displaynames, displayname)
      unit <- getCharInfo(
        object=WaterData
        ,parkcode=DataOpts$Park
        ,sitecode = site
        ,charname=DataOpts$Param2
        ,info="Units"
        )
      units <- c(units, unit)
    xname<-paste0(displayname," (", unit,")")
    xnames <- c(xname, xnames)
  }

  units <- units %>% unique
  displaynames <- displaynames %>% unique

  # resolve conflicts that would happen if the metadata file was messed up
  # e.g., if one characteristic had multiple units
  n_xnames <- length(xnames %>% unique)
  if (n_xnames == 1){
    xname <- xnames %>% unique
  } else if (n_xnames == 0){
    xname <- ''
  } else {
    xname <- xnames[1]
  }
  n_displaynames <- length(displaynames %>% unique)
  if (n_displaynames == 1){
    xvarname <- displaynames %>% unique
  } else if (n_displaynames == 0){
    xvarname <- ''
  } else {
    xvarname <- displaynames[1]
  }
  
  # https://github.com/NCRN/NCRNWater/blob/87a16069713e2ea188d8bb8a2ae0cab97a43af4f/R/waterbox.R#L106-L127
  x_nas <- nrow(df_secondparam %>% dplyr::filter(is.na(sitevisit_meanvalue_x)))
  y_nas <- nrow(df_firstparam %>% dplyr::filter(is.na(sitevisit_meanvalue_y)))
  n_measurements <-  nrow(df %>% dplyr::filter(is.na(sitevisit_meanvalue_x)==F & is.na(sitevisit_meanvalue_y)==F))
  title <- paste0(NCRNWater::getParkInfo(object=WaterData, parkcode=DataOpts$Park, info="ParkLongName"), '\n', yname, ' ~ ',xname, '; Years: ',DataOpts$Years[1], '-', DataOpts$Years[2],'\nTotal measurements: ',n_measurements, ' (NAs: ', yvarname, ': ', y_nas,'; ', xvarname,': ',x_nas, ')')

  m <- list( # figure margins
    l = 100,
    r = 50,
    b = 100,
    t = 100,
    pad = 20
  )
  baseplot <-
    plotly::plot_ly(
      df
      ,y= ~sitevisit_meanvalue_y
      ,x= ~sitevisit_meanvalue_x
      ,color= ~MonitoringLocationName
      ,symbol= ~MonitoringLocationName
      ,type='scatter'
      # ,mode='lines'
      # ,connectgaps=TRUE # set to FALSE to create breaks in the line for NAs
      ,width = (GraphOpts$FigureHorizontalScaling*as.numeric(input$dimension[1])) # to dynamically resize fig
      ,height = (GraphOpts$FigureVerticalScaling*as.numeric(input$dimension[2]))
      # ,line=list(width=input$LineWidth)
      ,marker=list(
        size=GraphOpts$PointSize
        ,opacity=1
        )
      ,hovertemplate = paste(
        "<br>Date :", df$Date
        ,"<br>Site :", df$MonitoringLocationName
        ,"<br>",xname, ": ", df$sitevisit_meanvalue_x
        ,"<br>",yname, ": ", df$sitevisit_meanvalue_y
        # extra is a secondary bit of hovertext that's visible on the right-ide of the main hovertext
        # https://community.plotly.com/t/disabling-default-tooltip-while-using-a-hovertemplate-in-python/85824/3
        ,'<extra></extra>'
        )
      ,text=NULL
    ) %>% layout(
      font=list(size=input$FontSize)
      ,margin=m
      ,title = list(text=title ,font=list(size=input$FontSize))
      ,legend = list(
        title=list(text='<br>Site<br>')
        ,font=list(size=input$FontSize)
        )
      ,showlegend=T
      ,yaxis = list(title=list(text=paste0(yname, '<br>'), font=list(size=input$FontSize)), font=list(size=input$FontSize))
      ,xaxis = list(title=list(text=paste0(xname, '<br>'), font=list(size=input$FontSize)), font=list(size=input$FontSize))
    )   

    return(baseplot)

  })

output$CorrPlot<-renderPlotly({   CorrPlotOutMultiple() })

#### Time Series Plot ####
  #   WaterSeriesOut<-reactive({
  #     req(DataUse()$Date, TrendType(), DataUse()$Value)
      
  #     SeriesPlot<- if(TrendType() == "notrends"){  
  #       cens <- ifelse(any(DataUse()$Censored==TRUE), TRUE, FALSE)
  #       waterseries(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, char=DataOpts$Param,
  #                   censored=cens,
  #                   years=DataOpts$Years[1]:DataOpts$Years[2],layers=c("points"),
  #                   assessment=input$SeriesThreshLine, title=Title(),
  #                   colors=(GoodCol()),assesscolor=ThCol(), 
  #                   sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth),
  #                   legend=if(GraphOpts$Legend) "bottom" else "none") +
  #                   theme(text=element_text(size=GraphOpts$FontSize*10))+
          
  #         {if(input$ThreshPoint && !is.na(Thresholds()[1])) geom_point(data=DataUse()[DataUse()$Value<Thresholds()[1],], 
  #                                                                      aes(Date,Value), pch=16,size=GraphOpts$PointSize, color=BadCol()) } +
          
  #         {if(input$ThreshPoint && !is.na(Thresholds()[2])) geom_point(data=DataUse()[DataUse()$Value>Thresholds()[2],], 
  #                                                                      aes(Date,Value), pch=16, size=GraphOpts$PointSize, color=BadCol())} 
          
  #       } else if(TrendType() == "wcosinor"){  
         
  #       waterseries(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, char=DataOpts$Param, 
  #                   #years=DataOpts$Years[1]:DataOpts$Years[2],
  #                   layers=c("points"),
  #                   assessment=input$SeriesThreshLine, title=Title(),
  #                   colors=(GoodCol()),assesscolor=ThCol(), 
  #                   sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth),
  #                   legend=if(GraphOpts$Legend) "bottom" else "none") +
  #                   theme(text=element_text(size=GraphOpts$FontSize*10))+
        
  #           {if(input$Outliers && exists("TrendsOut")) geom_point(data=TrendsOut()[["Outliers"]], aes(Date,Value),pch=1,
  #                 size=GraphOpts$PointSize+2,color=OutCol(),stroke=1.5)} +
        
  #           {if(input$ThreshPoint && !is.na(Thresholds()[1])) geom_point(data=DataUse()[DataUse()$Value<Thresholds()[1],], 
  #                  aes(Date,Value), pch=16,size=GraphOpts$PointSize, color=BadCol()) } +
        
  #           {if(input$ThreshPoint && !is.na(Thresholds()[2])) geom_point(data=DataUse()[DataUse()$Value>Thresholds()[2],], 
  #                 aes(Date,Value), pch=16, size=GraphOpts$PointSize, color=BadCol())} +
      
  #           {if(input$Trends && exists("TrendsOut") && class(TrendsOut()$Analysis)=="lm") geom_line(data=data.frame(
  #           Value=TrendsOut()$Analysis$fitted.values,Date=TrendsOut()$CDates), aes(Date,Value), color=TrCol(), 
  #           lwd=GraphOpts$LineWidth) } +
        
  #           {if(input$Trends && exists("TrendsOut") && class(TrendsOut()$Analysis)=="Cosinor") geom_line(data=data.frame(
  #           Value=TrendsOut()$PredLine$Preds,Date=TrendsOut()$PredLine$PreDates.Date),  aes(Date,Value), col=TrCol(), 
  #           lwd=GraphOpts$LineWidth)}
        
  #       } else if(TrendType() == "nonparCens"){ 
          
  #           df <- DataUse() %>% group_by(Category, Characteristic, Site, Park, month) %>% 
  #             mutate(num_meas=sum(!is.na(ValueCen)), 
  #                    pct_true= sum(ifelse(Censored==FALSE, 1, 0))/num_meas,
  #                    adjValueCen = ifelse(Censored==TRUE, max(ValueCen), Value)) %>% 
  #             filter(num_meas >=6) %>% 
  #             ungroup() %>% droplevels()
            
  #           df2 <- merge(df, TrendsOut()[,c('month','intercept','slope', 'message')], 
  #                        by = 'month', all.x = T) %>%
  #             mutate(pred_y = intercept + slope * year.dec,
  #                    sign=as.factor(ifelse(message=="no trend", 0, 1)),
  #                    month = as.factor(lubridate::month(Date, label = TRUE, abbr = FALSE))) %>% 
  #             arrange(month) 
            
  #           ylabel = paste0(getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site,
  #                                              charname = DataOpts$Param, info = "CategoryDisplay"), " (",
  #                           getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site,
  #                                       charname = DataOpts$Param, info = "Units"), ")")
          
  #           waterseries(df2, parkcode=DataOpts$Park, sitecode=DataOpts$Site, 
  #                       char=DataOpts$Param, censored = TRUE, deseason = TRUE,
  #                       #years=DataOpts$Years[1]:DataOpts$Years[2],
  #                       layers=c("points"),
  #                       assessment=input$SeriesThreshLine, 
  #                       title=Title(),
  #                       #colors=(GoodCol()),
  #                       assesscolor=ThCol(),
  #                       sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth),
  #                       legend=if(GraphOpts$Legend) "bottom" else "none") +
  #                       labs(y = ylabel)+
  #                       theme(text=element_text(size=GraphOpts$FontSize*10)) +
  #                       geom_smooth(data=df2, method = 'lm', se = FALSE,
  #                       aes(x = Date, y = pred_y, linetype = sign), formula = y~x, color='black')+
  #                       scale_linetype_manual(values = c('dashed', 'solid'), guide = 'none')+
                        

  #           {if(input$ThreshPoint && !is.na(Thresholds()[1]))
  #             geom_point(data=df2[df2$AdjValueCen<Thresholds()[1],],
  #             aes(Date,AdjValueCen), pch=16,size=GraphOpts$PointSize, color=BadCol())} +

  #           {if(input$ThreshPoint && !is.na(Thresholds()[2]))
  #             geom_point(data=df2[df2$AdjValueCen>Thresholds()[2],],
  #             aes(Date,AdjValueCen), pch=16, size=GraphOpts$PointSize, color=BadCol())}
        

  #       } else if(TrendType() == "nonpar"){ 

  #            df <- merge(DataUse(), TrendsOut()[,c('month','intercept','slope', 'message')], by = 'month', all.x = T) %>%
  #              mutate(pred_y = intercept + slope * year.dec,
  #                     sign=as.factor(ifelse(message=="no trend", 0, 1)),
  #                     month = as.factor(lubridate::month(Date, label = TRUE, abbr = FALSE))) %>% 
  #              filter(message != "Too few data points.") %>% arrange(month) %>% droplevels()
             
  #            if(nrow(df)==0){ #catches case when mann-kendall bootstrap fails due to too little data and/or too many ties
  #              waterseries(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, char=DataOpts$Param, 
  #                          layers=c("points"),
  #                          #years=DataOpts$Years[1]:DataOpts$Years[2],
  #                          assessment=input$SeriesThreshLine, title=Title(),
  #                          colors=(GoodCol()),
  #                          assesscolor=ThCol(), 
  #                          sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth),
  #                          legend=if(GraphOpts$Legend) "bottom" else "none") +
  #                          theme(text=element_text(size=GraphOpts$FontSize*10))+
                 
  #                {if(input$ThreshPoint && !is.na(Thresholds()[1])) geom_point(data=DataUse()[DataUse()$Value<Thresholds()[1],], 
  #                                                                             aes(Date,Value), pch=16,size=GraphOpts$PointSize, color=BadCol()) } +
                 
  #                {if(input$ThreshPoint && !is.na(Thresholds()[2])) geom_point(data=DataUse()[DataUse()$Value>Thresholds()[2],], 
  #                                                                             aes(Date,Value), pch=16, size=GraphOpts$PointSize, color=BadCol())} 
  #            } else{
             
  #            ylabel = paste0(getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site,
  #                                        charname = DataOpts$Param, info = "CategoryDisplay"), " (",
  #                            getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site,
  #                                        charname = DataOpts$Param, info = "Units"), ")") 
             
  #             pointlab = paste0(getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site,
  #                                           charname = DataOpts$Param, info = "DisplayName"))
             
  #            waterseries(df, parkcode=DataOpts$Park, sitecode=DataOpts$Site, 
  #                        char=DataOpts$Param, censored = FALSE, deseason = TRUE,
  #                        #years=DataOpts$Years[1]:DataOpts$Years[2],
  #                        layers=c("points"), 
  #                        assessment=input$SeriesThreshLine, title=Title(),
  #                        colors=(GoodCol()),
  #                        assesscolor=ThCol(), 
  #                        sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth),
  #                        legend=if(GraphOpts$Legend) "bottom" else "none") +
  #                        labs(y = ylabel, color = pointlab)+
  #                        theme(text=element_text(size=GraphOpts$FontSize*10)) +
  #                        geom_smooth(data=df, method = 'lm', se = FALSE, 
  #                          aes(x = Date, y = pred_y, linetype = sign), formula = y~x, color='black')+
  #                        scale_linetype_manual(values = c('dashed', 'solid'), guide = 'none')+ 
                         
               
  #              {if(input$ThreshPoint && !is.na(Thresholds()[1])) 
  #                geom_point(data=df[df$Value<Thresholds()[1],],
  #                           aes(Date,Value), pch=16,size=GraphOpts$PointSize, color=BadCol())} +
               
  #              {if(input$ThreshPoint && !is.na(Thresholds()[2])) 
  #                geom_point(data=df[df$Value>Thresholds()[2],],
  #                           aes(Date,Value), pch=16, size=GraphOpts$PointSize, color=BadCol())}
             
  #          }
  #       }
      
      
  #     SeriesPlot  #forces ggplot to draw graph after all the conditionals
    
  #       })
  
  # output$TimeSeries<-renderPlot({
  #   WaterSeriesOut()
  # })

  
  #### Plot downloads ####
  output$Plot.PNG<-downloadHandler(
    filename=function(){paste(Title(), ".png", sep="")}, 
    content=function (file){
      png(file,width=960, height=480)
      print(WaterSeriesOut())
      dev.off()
    }
  )
  
  output$Plot.JPG<-downloadHandler(
    filename=function(){paste(Title(), ".jpeg", sep="")}, 
    content=function (file){
      jpeg(file,width=960, height=480,quality=100)
      print(WaterSeriesOut())
      dev.off()
    }
  )
  
  
#### Box Plot Controls ####
  BoxPark<-callModule(parkChooser, id="BoxPark", data=WaterData, chosen=reactive(DataOpts$Park))
  BoxSite<-callModule(siteChooser, id="BoxSite", data=WaterData, park=reactive(DataOpts$Park), chosen=reactive(DataOpts$Site))
  BoxParam<-callModule(paramChooser, id="BoxParam",data=WaterData, park=reactive(DataOpts$Park), site=reactive(DataOpts$Site), 
                        chosen=reactive(DataOpts$Param))
  BoxYears<-callModule(yearChooser, id="BoxYears", data=DataUseMultiple, chosen=reactive(DataOpts$Years) )
  
  observeEvent(BoxPark(), DataOpts$Park<-BoxPark() )
  observeEvent(BoxSite(), DataOpts$Site<-BoxSite() )
  observeEvent(BoxParam(), DataOpts$Param<-BoxParam() )
  observeEvent(BoxYears(), DataOpts$Years<-BoxYears() )

#### Box Plot 2.0 ####

hline <- function(y = 0, color = "red", dash = 'dash', size=1) {
    # Make a list of parameters to be passed to plotly to generate a horizontal dashed line for water quality threshold. 
    # Args:
    #  y: int, optional. Default 0. The vertical position at which the horizontal line should be drawn.
    #  color: chr or c(int), optional. Default 'red'. The color of the horizontal line. Can pass str E.g., 'red' or str hex, or c(R,G,B).
    #  dash: chr, optional. Default 'dash'. The linetype for the horizontal line.
    #  
    # Returns:
    #  list
    # 
    # Example:
    #   myhline <- hline(y=10)
    #
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = input$ThColor, dash = dash, width=input$LineWidth)
  )
}

hbox <- function(y = 0, color = "red", opacity=0.1) {
    # Make a list of parameters to be passed to plotly to generate a horizontal rectangle for water quality threshold. 
    # Args:
    #  y: int, optional. Default 0. The vertical position at which the rectangle should be drawn.
    #  color: chr or c(int), optional. Default 'red'. The color of the rectangle fill. Can pass str E.g., 'red' or str hex, or c(R,G,B).
    #  opacity: num (0,1), optional. Default 0.1. How transparent the rectangle should be. 0 fully transparent to 1 fully opaque.
    #  
    # Returns:
    #  list
    # 
    # Example:
    #   myhbox <- hbox(y=10)
    #
  list(
    type = "rect"
    ,x0 = 0
    ,x1 = 1
    ,xref = "paper"
    ,y0 = y
    ,y1 = y+10
    ,opacity=opacity
    ,fillcolor = input$ThColor
    ,line = list(color = input$ThColor)
  )
}

BoxPlotMultipleOut<-reactive({
    # A reactive function that a plotly of boxplots based on user selected site(s) and aggregation method (year, month, or site). 
    # Args:
    #  DataOpts$Years, c(int), required. The character string provided by yearChooser() in global.R. 
    #  input$SummaryBoxBy, chr, required. Determines aggregation type and formats accordingly if month or year is selected. 
    #  input$BoxThreshLine, bool, optional. Default False. If True, looks up the water quality threshold.
    #  DataOpts$Park, chr, required. A park acronym. E.g., 'ROCR'.
    #  DataOpts$Site, chr or c(chr), required. A site code. E.g., 'NCRN_ROCR_KLVA'
    #  DataOpts$Param, chr, required. A characteristic abbreviation. E.g., 'DOper'.
    #  
    # Returns:
    #  Plotly figure
    # 
    # Example:
    #   DataOpts$Years <- c(2010,2024),
    #   input$SummaryBoxBy <- "year"
    #   input$BoxThreshLine <- T
    #   DataOpts$Park <- 'ROCR'
    #   DataOpts$Site <- c('NCRN_ROCR_KLVA', 'NCRN_ROCR_FEBR')
    #   DataOpts$Param <- 'DOper'
    #   
    #   myfigure <- BoxPlotMutlipleOut(
    #     DataOpts$Years
    #     ,input$SummaryBoxBy
    #     ,input$BoxThreshLine
    #     ,DataOpts$Park
    #     ,DataOpts$Site
    #     ,DataOpts$Param
    #   )
    #
  req(DataOpts$Park, DataOpts$Site, DataOpts$Param)

  # https://github.com/NCRN/NCRNWater/blob/87a16069713e2ea188d8bb8a2ae0cab97a43af4f/R/waterbox.R#L73
  table <- summary()

  table <- table %>%
    dplyr::mutate(Aggregation = ifelse(Aggregation %in% month.abb, 
                              month.name[match(Aggregation, month.abb)], Aggregation))

  #SiteCodes to full site names
  site_codes <- NCRNWater::getSiteInfo(WaterData, parkcode = DataOpts$Park, info = "SiteCode")
  site_names <- NCRNWater::getSiteInfo(WaterData, parkcode = DataOpts$Park, info = "SiteName")
  
  site_info <- data.frame(SiteCode = site_codes, SiteName = site_names, stringsAsFactors = FALSE)

  table <- table %>%
    dplyr::left_join(site_info, by = c("Site" = "SiteCode")) %>%
    dplyr::mutate(Site = ifelse(!is.na(SiteName), SiteName, Site)) %>% 
    dplyr::select(-SiteName)
    
  #Table grouping
  if (input$SummaryBoxBy %in% c("month", "year")) {
    
    aggregation_levels <- if (input$SummaryBoxBy == "month") {
      month.name
    } else {
      sort(unique(as.character(table$Aggregation)), decreasing = F)
    }

  summary_table <- table %>%
    dplyr::mutate(Aggregation = factor(Aggregation, levels = aggregation_levels),
                  is_group = Site == Aggregation) %>%
    dplyr::arrange(Aggregation, dplyr::desc(is.na(Minimum)), Site) %>%
    dplyr::mutate(is_group = Site == Aggregation)
  } else {
  summary_table <- table %>%
    dplyr::arrange(Site) %>%
    dplyr::mutate(Aggregation = Site) %>%
    dplyr::mutate(is_group = FALSE)
  }
  
  # summary_table <- summary_table %>%
  #   dplyr::select(-Aggregation, -is_group)

  print(head(summary_table))

  # initialize variables
  ynames <- c()
  xname <- NA
  labels <- NA
  assessment <- input$BoxThreshLine
  assessments <- c()
  threshold <- NA

  # https://github.com/NCRN/NCRNWater/blob/87a16069713e2ea188d8bb8a2ae0cab97a43af4f/R/waterbox.R#L75-L98
  for (site in DataOpts$Site){
    yname<-paste0(
      getCharInfo(
        object=WaterData
        ,parkcode=DataOpts$Park
        ,sitecode = site
        ,charname=DataOpts$Param
        , info="DisplayName"
        )
      ," ("
      ,getCharInfo(
        object=WaterData
        ,parkcode=DataOpts$Park
        ,sitecode = site
        ,charname=DataOpts$Param
        ,info="Units"
        )
      ,")"
      )

    ynames <- c(yname, ynames)

  }

  # resolve conflicts that would happen if the metadata file was messed up
  # e.g., if one characteristic had multiple units
  n_ynames <- length(ynames %>% unique)
  if (n_ynames == 1){
    yname <- ynames %>% unique
  } else if (n_ynames == 0){
    yname <- ''
  } else {
    yname <- ynames[1]
  }

  xname<-switch(
    input$BoxBy
    ,year="Year"
    ,month="Month"
    ,site="Site"
    )

  if(assessment){
      for (site in DataOpts$Site){
        tmp<-c(getCharInfo(object=WaterData,parkcode=DataOpts$Park, sitecode=site, charname=DataOpts$Param, info="LowerPoint"),
          getCharInfo(object=WaterData,parkcode=DataOpts$Park, sitecode=site, charname=DataOpts$Param, info="UpperPoint")) %>%
          unlist %>% unique
        assessments <- c(tmp, assessments)
    }
    threshold <- assessments %>% unique
    threshold <- threshold[!is.na(threshold)] # needed if there is no upper or lower threshold.
  }
  
  # https://github.com/NCRN/NCRNWater/blob/87a16069713e2ea188d8bb8a2ae0cab97a43af4f/R/waterbox.R#L106-L127

  # Grouper<-switch(
  #   input$BoxBy # the "Compare by:" selection (year, month, site)
  #   ,year=boxplot_df$Date %>% lubridate::year() %>% factor
  #   ,month=boxplot_df$Date %>% lubridate::month(label=T) %>% factor
  #   ,site=boxplot_df$MonitoringLocationName
  #   )

  n_not_na <- sum(summary_table$Total_Measurements) - sum(summary_table$Missing_Values)
  n_na <- sum(summary_table$Missing_Values)
  title <- paste0(NCRNWater::getParkInfo(object=WaterData, parkcode=DataOpts$Park, info="ParkLongName"), ': ', yname, '\nYears: ',DataOpts$Years[1], '-', DataOpts$Years[2],'; Total measurements: ', sum(summary_table$Total_Measurements),' (non-NA: ', n_not_na, ', NA: ', n_na,')')

  m <- list(
    l = 100,
    r = 50,
    b = 100,
    t = 100,
    pad = 20
  )
  t <- list(
    size = global_textsize
    )
  baseplot <-
    plotly::plot_ly(
      summary_table
      ,x= ~Aggregation
      ,color= ~Site
      ,width = (GraphOpts$FigureHorizontalScaling*as.numeric(input$dimension[1])) # to dynamically resize fig
      ,height = (GraphOpts$FigureVerticalScaling*as.numeric(input$dimension[2]))
      # boxplot_df
      # ,y= ~Value
      # ,x= ~Grouper
      # ,color= ~MonitoringLocationName
      # ,type='box'
      # # ,height = global_figure_height
      # # ,width = global_figure_width
    ) %>% add_trace(
      lowerfence= ~Minimum
      ,q1= ~Q1
      ,median= ~Median
      ,q3= ~Q3
      ,upperfence= ~Maximum
      ,type='box'
    ) %>% layout(
      boxmode = 'group'
      ,font=list(size=input$FontSize)
      ,margin=m
      ,title = list(text=title ,font=list(size=input$FontSize))
      ,legend = list(
        title=list(text='<br>Site<br>')
        ,font=list(size=input$FontSize)
        )
      ,showlegend=T
      ,yaxis = list(title=list(text=paste0(yname, '<br>'), font=list(size=input$FontSize)), font=list(size=input$FontSize))
      ,xaxis = list(title=list(text=paste0(xname, '<br>'), font=list(size=input$FontSize)), font=list(size=input$FontSize))
    )

  if (assessment==T & identical(threshold, numeric(0))==F) {
    # a <- list( # commented-out because the annotation doesn't look great
    #   x = 1,
    #   y = 0.95*threshold,
    #   text = paste0(stringr::str_split_1(yname, '[(]')[1], 'threshold: ', threshold, ' ', stringr::str_extract(yname, '(?<=\\()[^\\^\\)]+')),
    #   xref = "x",
    #   yref = "y",
    #   showarrow = F,
    #   ax = 20,
    #   ay = -40
    # )
    if (length(threshold)==2){
      baseplot %>% layout(
      shapes = list(
        hline(threshold[1])
        ,hline(threshold[2])
        # ,hbox(y=threshold[1], opacity=0.5)
        # ,hbox(threshold[2])
        )
      # ,annotations = a # commented-out because the annotation doesn't look great
    )

    } else if (length(threshold)==1){
      baseplot %>% layout(
        shapes = list(
          hline(threshold)
          # ,hbox(y=threshold, opacity=0.5)
          )
      # ,annotations = a # commented-out because the annotation doesn't look great
      )
    }
  } else {
    baseplot
  }
  # OutPlot<-ggplot( # commented out because replaced with plot_ly; saved just in case
  #   boxplot_df
  #   ,aes(
  #     x=Grouper
  #     ,y=Value
  #     ,fill=MonitoringLocationName
  #     # note: custom hovertext is not available for boxplots
  #     # https://github.com/ua-snap/northern-climate-reports/issues/85
  #     # https://github.com/plotly/plotly.R/issues/1636
  #     # ,text=MonitoringLocationName
  #     )
  #   ) +
  #   geom_boxplot(alpha=1) +
  #   # geom_boxplot(outlier.size=sizes[1], outlier.color=outliercolor, lwd=sizes[2]) +
  #   {if (is.numeric(assessment)) geom_hline(yintercept=assessment,color='red',linetype="dashed",size=2)}+
  #   labs(title=title,y=yname)+
  #   scale_x_discrete(name=xname)+
  #   scale_fill_discrete(name = "Site<br>")+
  #   theme_bw()+
  #   theme(
  #     panel.grid = element_blank()
  #     ,text = element_text(size=global_textsize)
  #     )

  # plotly::ggplotly(OutPlot) %>% layout(boxmode = "group", height = global_figure_height, width = global_figure_width)

  })

output$BoxPlotMultiple<-renderPlotly({   BoxPlotMultipleOut() })

#### Box Plot ####
  
  # BoxPlotOut<-reactive({
  #   req(DataOpts$Park, DataOpts$Site, DataOpts$Param)
  #   p <- waterbox(
  #     object=WaterData
  #     ,parkcode=DataOpts$Park
  #     ,sitecode=if(input$BoxBy !="site") DataOpts$Site else NA
  #     ,charname = DataOpts$Param
  #     ,by=input$BoxBy
  #     ,title=Title()
  #     ,years=DataOpts$Years[1]:DataOpts$Years[2]
  #     ,assessment=input$BoxThreshLine
  #     ,assesscolor=ThCol()
  #     ,outliercolor = BadCol()
  #     # ,webplot=T
  #     ,sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth)
  #     ,labels=if(input$BoxBy=="site") getSiteInfo(WaterData, parkcode= DataOpts$Park, info="SiteName") else NA) +
  #   theme(text=element_text(size=GraphOpts$FontSize*10))
    
  #   # plotly::ggplotly(p, tooltip = "text")
  #   plotly::ggplotly(p)

  # })
   
  # output$BoxPlot<-renderPlotly({   BoxPlotOut() })

  
  #### BoxThreshold Summary ####
  
  # BoxThresholdSummary<-reactive({    
  #   req(input$BoxThreshLine)
  #   paste(h4("Threshold:"),"\n",
  #         c(getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="LowerDescription"),
  #           getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, 
  #                       info="UpperDescription"))[!is.na(Thresholds())])
  # })
  
  # output$BoxThresholdSummary<-renderUI( HTML(BoxThresholdSummary()) )

  BoxThresholdSummaryMultiple<-reactive({    
    # Make an html string of water quality thresholds to be displayed when the user asks for the thresholds. 
    # Args:
    #  input$BoxThreshLine, bool, optional. Default False. If True, looks up the water quality threshold.
    #  DataOpts$Park, chr, required. A park acronym. E.g., 'ROCR'.
    #  DataOpts$Site, chr or c(chr), required. A site code. E.g., 'NCRN_ROCR_KLVA'
    #  DataOpts$Param, chr, required. A characteristic abbreviation. E.g., 'DOper'.
    #  
    # Returns:
    #  chr
    # 
    # Example:
    #   input$BoxThreshLine <- T
    #   DataOpts$Park <- 'ROCR'
    #   DataOpts$Site <- c('NCRN_ROCR_KLVA', 'NCRN_ROCR_FEBR')
    #   DataOpts$Param <- 'DOper'
    #   
    #   mythresholds <- 
    #     BoxThresholdSummaryMultiple(
    #       ,input$BoxThreshLine
    #       ,DataOpts$Park
    #       ,DataOpts$Site
    #       ,DataOpts$Param
    #     )
    #
    req(input$BoxThreshLine, DataOpts$Park, DataOpts$Site, DataOpts$Param)

    sitethreshes <- c()

    if(input$BoxThreshLine){
        for (site in DataOpts$Site){
          tmp<-c(getCharInfo(object=WaterData,parkcode=DataOpts$Park, sitecode=site, charname=DataOpts$Param, info="LowerDescription"),
            getCharInfo(object=WaterData,parkcode=DataOpts$Park, sitecode=site, charname=DataOpts$Param, info="UpperDescription")) %>%
            unlist %>% unique
          sitethreshes <- c(tmp, sitethreshes)
      }
      sitethresh <- sitethreshes %>% unique
      sitethresh <- sitethresh[!is.na(sitethresh)] # needed if there is no upper or lower sitethresh.
    }

    if (length(sitethresh)>0){
      paste(h4("Threshold:"),"\n",sitethresh)
    } else {
      paste(h4("This parameter has no water quality threshold."),"\n")
    }
  })
  
  output$BoxThresholdSummaryMultiple<-renderUI( HTML(BoxThresholdSummaryMultiple()) )

  SeriesThresholdSummaryMultiple<-reactive({    
    # Make an html string of water quality thresholds to be displayed when the user asks for the thresholds. 
    # Args:
    #  input$SeriesThreshLine, bool, optional. Default False. If True, looks up the water quality threshold.
    #  DataOpts$Park, chr, required. A park acronym. E.g., 'ROCR'.
    #  DataOpts$Site, chr or c(chr), required. A site code. E.g., 'NCRN_ROCR_KLVA'
    #  DataOpts$Param, chr, required. A characteristic abbreviation. E.g., 'DOper'.
    #  
    # Returns:
    #  chr
    # 
    # Example:
    #   input$SeriesThreshLine <- T
    #   DataOpts$Park <- 'ROCR'
    #   DataOpts$Site <- c('NCRN_ROCR_KLVA', 'NCRN_ROCR_FEBR')
    #   DataOpts$Param <- 'DOper'
    #   
    #   mythresholds <- 
    #     SeriesThresholdSummaryMultiple(
    #       ,input$SeriesThreshLine
    #       ,DataOpts$Park
    #       ,DataOpts$Site
    #       ,DataOpts$Param
    #     )
    #
    req(input$SeriesThreshLine, DataOpts$Park, DataOpts$Site, DataOpts$Param)

    sitethreshes <- c()

    if(input$SeriesThreshLine){
        for (site in DataOpts$Site){
          tmp<-c(getCharInfo(object=WaterData,parkcode=DataOpts$Park, sitecode=site, charname=DataOpts$Param, info="LowerDescription"),
            getCharInfo(object=WaterData,parkcode=DataOpts$Park, sitecode=site, charname=DataOpts$Param, info="UpperDescription")) %>%
            unlist %>% unique
          sitethreshes <- c(tmp, sitethreshes)
      }
      sitethresh <- sitethreshes %>% unique
      sitethresh <- sitethresh[!is.na(sitethresh)] # needed if there is no upper or lower sitethresh.
    }

    if (length(sitethresh)>0){
      paste(h4("Threshold:"),"\n",sitethresh)
    } else {
      paste(h4("This parameter has no water quality threshold."),"\n")
    }
  })
  
  output$SeriesThresholdSummaryMultiple<-renderUI( HTML(SeriesThresholdSummaryMultiple()) )
    
  # BoxRefSummary<-reactive({
  #   req(input$BoxThreshLine) 
  #   paste(h4("Threshold Reference:"),"\n",
  #         getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="AssessmentDetails")) 
  # })      
  
  # output$BoxRefSummary<-renderUI(HTML(BoxRefSummary()))     
  
  BoxRefSummaryMultiple<-reactive({    
    # Make an html string of water quality threshold references to be displayed when the user asks for the thresholds. 
    # Args:
    #  input$BoxThreshLine, bool, optional. Default False. If True, looks up the water quality threshold.
    #  DataOpts$Park, chr, required. A park acronym. E.g., 'ROCR'.
    #  DataOpts$Site, chr or c(chr), required. A site code. E.g., 'NCRN_ROCR_KLVA'
    #  DataOpts$Param, chr, required. A characteristic abbreviation. E.g., 'DOper'.
    #  
    # Returns:
    #  chr
    # 
    # Example:
    #   input$BoxThreshLine <- T
    #   DataOpts$Park <- 'ROCR'
    #   DataOpts$Site <- c('NCRN_ROCR_KLVA', 'NCRN_ROCR_FEBR')
    #   DataOpts$Param <- 'DOper'
    #   
    #   mythreshold_references <- 
    #     BoxRefSummaryMultiple(
    #       ,input$BoxThreshLine
    #       ,DataOpts$Park
    #       ,DataOpts$Site
    #       ,DataOpts$Param
    #   )
    #
    req(input$BoxThreshLine, DataOpts$Park, DataOpts$Site, DataOpts$Param)

    sitethreshes <- c()

    if(input$BoxThreshLine){
      for (site in DataOpts$Site){
        tmp<-c(getCharInfo(object=WaterData,parkcode=DataOpts$Park, sitecode=site, charname=DataOpts$Param, info="AssessmentDetails"),
          getCharInfo(object=WaterData,parkcode=DataOpts$Park, sitecode=site, charname=DataOpts$Param, info="AssessmentDetails")) %>%
          unlist %>% unique
        sitethreshes <- c(tmp, sitethreshes)
      }
      sitethresh <- sitethreshes %>% unique
      sitethresh <- sitethresh[!is.na(sitethresh)] # needed if there is no upper or lower sitethresh.
      
      if (length(sitethresh)>0){
        paste(h4("Threshold Reference:"),"\n",sitethresh)
      }
    }
  })
  
output$BoxRefSummaryMultiple<-renderUI(HTML(BoxRefSummaryMultiple()))

SeriesRefSummaryMultiple<-reactive({    
  # Make an html string of water quality threshold references to be displayed when the user asks for the thresholds. 
  # Args:
  #  input$SeriesThreshLine, bool, optional. Default False. If True, looks up the water quality threshold.
  #  DataOpts$Park, chr, required. A park acronym. E.g., 'ROCR'.
  #  DataOpts$Site, chr or c(chr), required. A site code. E.g., 'NCRN_ROCR_KLVA'
  #  DataOpts$Param, chr, required. A characteristic abbreviation. E.g., 'DOper'.
  #  
  # Returns:
  #  chr
  # 
  # Example:
  #   input$SeriesThreshLine <- T
  #   DataOpts$Park <- 'ROCR'
  #   DataOpts$Site <- c('NCRN_ROCR_KLVA', 'NCRN_ROCR_FEBR')
  #   DataOpts$Param <- 'DOper'
  #   
  #   mythreshold_references <- 
  #     SeriesRefSummaryMultiple(
  #       ,input$SeriesThreshLine
  #       ,DataOpts$Park
  #       ,DataOpts$Site
  #       ,DataOpts$Param
  #   )
  #
  req(input$SeriesThreshLine, DataOpts$Park, DataOpts$Site, DataOpts$Param)

  sitethreshes <- c()

  if(input$SeriesThreshLine){
    for (site in DataOpts$Site){
      tmp<-c(getCharInfo(object=WaterData,parkcode=DataOpts$Park, sitecode=site, charname=DataOpts$Param, info="AssessmentDetails"),
        getCharInfo(object=WaterData,parkcode=DataOpts$Park, sitecode=site, charname=DataOpts$Param, info="AssessmentDetails")) %>%
        unlist %>% unique
      sitethreshes <- c(tmp, sitethreshes)
    }
    sitethresh <- sitethreshes %>% unique
    sitethresh <- sitethresh[!is.na(sitethresh)] # needed if there is no upper or lower sitethresh.
    
    if (length(sitethresh)>0){
      paste(h4("Threshold Reference:"),"\n",sitethresh)
    }
  }
})

output$SeriesRefSummaryMultiple<-renderUI(HTML(SeriesRefSummaryMultiple()))

  #### Plot downloads ####
  output$BoxPlot.PNG<-downloadHandler(
    filename=function(){paste(Title(), ".png", sep="")}, 
    content=function (file){
      png(file,width=960, height=480)
      print(BoxPlotOut())
      dev.off()
    }
  )
  
  output$BoxPlot.JPG<-downloadHandler(
    filename=function(){paste(Title(), ".jpeg", sep="")}, 
    content=function (file){
      jpeg(file,width=960, height=480,quality=100)
      print(BoxPlotOut())
      dev.off()
    }
  )
    
### Data table output ####
  output$DatasetURL <- renderUI({tagList(DATASET_URL)}) # a constant stored in global.R
  DATATABLE_COLNAME_LOOKUP <- c( # a lookup used in output$WaterTable
    Site = 'MonitoringLocationName'
    ,Latitude = 'ActivityLocation.LatitudeMeasure'
    ,Longitude = 'ActivityLocation.LongitudeMeasure'
    ,SampleDate = 'Date'
    ,SampleTime = 'ActivityStartTime.Time'
    ,Value = 'Value'
    ,Units = 'ResultMeasure.MeasureUnitCode'
    )

  output$WaterTable <-DT::renderDataTable(
    # Generate a data table for the `Data` tab given user inputs
    # 
    # Args:
    #   DataOpts$Park, chr, required. The character string provided by ExceedancesPark() and parkChooser() in global.R
    #   DataOpts$Site, chr, required. The character string provided by ExceedancesSite() and siteChooser() in global.R
    #   DataOpts$Param, chr, required. The character string provided by ExceedancesParam() and paramChooser() in global.R
    # 
    # Returns:
    #   data.frame
    # 
    # Example:
    #   DataOpts$Park<- "GWMP"
    #   DataOpts$Site<- "NCRN_GWMP_TURU"
    #   DataOpts$Param<- "TotalP"
    #   
    expr=datatable(
        DataUseMultiple() %>% dplyr::select(
        MonitoringLocationName
        ,ActivityLocation.LatitudeMeasure
        ,ActivityLocation.LongitudeMeasure
        ,Date
        ,ActivityStartTime.Time
        ,Value
        ,ResultMeasure.MeasureUnitCode
      ) %>% dplyr::arrange(
        dplyr::desc(Date)
        ,MonitoringLocationName
      ) %>% dplyr::rename(
        dplyr::all_of(DATATABLE_COLNAME_LOOKUP)
      ) %>% dplyr::mutate(
        Park = NCRNWater::getParkInfo(object=WaterData, parkcode=DataOpts$Park, info="ParkLongName")
        ,Parameter = NCRNWater::getCharInfo(WaterData, parkcode=DataOpts$Park, sitecode=if(length(DataOpts$Site)>1) DataOpts$Site[1] else DataOpts$Site, charname=DataOpts$Param, info="DisplayName")
      ) %>% dplyr::select(
        Park
        ,Site
        ,Latitude
        ,Longitude
        ,SampleDate
        ,SampleTime
        ,Parameter
        ,Value
        ,Units
      )
      ,extensions=c(
        "Buttons"
        ,"KeyTable"
        )
      # ,caption=htmltools::tags$caption(htmltools::h3(Title()))
      ,class="stripe hover order-column cell-border"
      ,filter="top"
      ,rownames=F
      ,options=list(
        autoWidth=F
        ,dom="Bltirp"
        ,buttons=c(
          "copy"
          ,"csv"
          ,"excel"
        ),keys=TRUE
      )
    )
    ,server=F
  )
  
### Exceedances Data Use Multiple ###
  exDUM <-shiny::reactive({
    # Get threshold exceedance data to use in a tab that allows for multiple site selections and reacts to user input.
    # Args:
    #  DataOpts$Park, chr, required. The character string provided by parkChooser() in global.R.
    #  DataOpts$Site, chr or vector if multiple sites selected, required. The character string provided by siteChooser() in global.R.
    #  DataOpts$Param, chr, required. The character string provided by paramChooser() in global.R.
    #  WaterData, list, required. The list of NCRN water quality data assembled from /wqp_ncrnwater_metadata.csv and /wqp.csv
    # 
    # Return:
    #  mydatastrucure, list. A temporary package containing:
    #    [[site]], list. A temporary package for each selected site containing:
    #      df, data.frame. A data.frame that includes water records for a [[site]].
    #      exdf, data.frame. A data.frame that includes water records for threshold exceedances at a [[site]].
    #
    # Examples:
    #
    shiny::validate(
      need(DataOpts$Park, message="Choose a Park"),
      need(DataOpts$Site, message="Choose a Site"),
      need(DataOpts$Param, message="Choose a Water Quality Parameter")
    )  
    
    mydatastructure <- list()
    for (site in DataOpts$Site) {
      mydatastructure[[site]]<-list()
      
      # Building df
      mydatastructure[[site]][["df"]] <- getWData(WaterData, parkcode=DataOpts$Park, sitecode = site, charname=DataOpts$Param)
      mydatastructure[[site]][["df"]] <- mydatastructure[[site]][["df"]][, c("MonitoringLocationName", "Date", "Characteristic", "Value", "ResultMeasure.MeasureUnitCode")]
      mydatastructure[[site]][["df"]] <- subset(mydatastructure[[site]][["df"]], !is.na(Value))
      
      # Getting characters
      mydatastructure[[site]][["LowerThreshold"]]<- NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = site, charname = DataOpts$Param, info="LowerDescription")
      mydatastructure[[site]][["UpperThreshold"]]<- NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = site, charname = DataOpts$Param, info="UpperDescription")
      mydatastructure[[site]][["LowerPoint"]]<- NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = site, charname = DataOpts$Param, info="LowerPoint")
      mydatastructure[[site]][["UpperPoint"]]<- NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = site, charname = DataOpts$Param, info="UpperPoint")
      mydatastructure[[site]][["Sitename"]]<- NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = site, charname = DataOpts$Param, info = "SiteName")
      mydatastructure[[site]][["Characteristic"]]<- NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = site, charname = DataOpts$Param, info = "DisplayName")
      mydatastructure[[site]][["Unit"]]<- NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = site, charname = DataOpts$Param, info="Units")
      mydatastructure[[site]][["notification_text"]] <- dplyr::case_when(
        !is.na(mydatastructure[[site]][["UpperPoint"]]) == TRUE & !is.na(mydatastructure[[site]][["LowerPoint"]]) == TRUE & all(mydatastructure[[site]][["df"]]$Value > mydatastructure[[site]][["LowerPoint"]]) & all(mydatastructure[[site]][["df"]]$Value < mydatastructure[[site]][["UpperPoint"]]) ~ 
          paste0("No measurements of ", mydatastructure[[site]][["Characteristic"]], " at ", mydatastructure[[site]][["Sitename"]], " fall below the lower water quality threshold of ", mydatastructure[[site]][["LowerPoint"]], " ", mydatastructure[[site]][["Unit"]], ", nor exceed the upper water quality threshold of ", mydatastructure[[site]][["UpperPoint"]], " ", mydatastructure[[site]][["Unit"]])
        ,!is.na(mydatastructure[[site]][["LowerPoint"]]) == TRUE & all(mydatastructure[[site]][["df"]]$Value > mydatastructure[[site]][["LowerPoint"]]) ~ 
          paste0("No measurements of ", mydatastructure[[site]][["Characteristic"]], " at ", mydatastructure[[site]][["Sitename"]],
                 "fall below the water quality threshold of ", mydatastructure[[site]][["LowerPoint"]], " ", mydatastructure[[site]][["Unit"]])
        ,!is.na(mydatastructure[[site]][["UpperPoint"]]) == TRUE & all(mydatastructure[[site]][["df"]]$Value < mydatastructure[[site]][["UpperPoint"]]) ~
          paste0("No measurements of ", mydatastructure[[site]][["Characteristic"]], " at ", mydatastructure[[site]][["Sitename"]],
                 "exceed the water quality threshold of ", mydatastructure[[site]][["UpperPoint"]], " ", mydatastructure[[site]][["Unit"]])
        ,is.na(mydatastructure[[site]][["LowerPoint"]]) == TRUE & is.na(mydatastructure[[site]][["UpperPoint"]]) == TRUE ~
          paste0("There is no recorded water quality threshold for ", mydatastructure[[site]][["Characteristic"]], " at ", mydatastructure[[site]][["Sitename"]])
      )
      
      # Building exdf
      if(any(mydatastructure[[site]][["df"]]$Value <= mydatastructure[[site]][["LowerPoint"]], na.rm = TRUE)) {
        lower_sd<- mydatastructure[[site]][["df"]][mydatastructure[[site]][["df"]]$Value < mydatastructure[[site]][["LowerPoint"]], ]
        lower_sd$LowerThreshold <- mydatastructure[[site]][["LowerThreshold"]]
      } else{
        lower_sd<- mydatastructure[[site]][["df"]][0, ]
      }
      if(any(mydatastructure[[site]][["df"]]$Value >= mydatastructure[[site]][["UpperPoint"]], na.rm = TRUE)) {
        upper_sd<- mydatastructure[[site]][["df"]][mydatastructure[[site]][["df"]]$Value > mydatastructure[[site]][["UpperPoint"]], ]
        upper_sd$UpperThreshold <- mydatastructure[[site]][["UpperThreshold"]]
      } else {
        upper_sd<- mydatastructure[[site]][["df"]][0, ]
      }
      mydatastructure[[site]][["exdf"]] <- dplyr::bind_rows(lower_sd, upper_sd)
      
      ### ...and desc_exdf ###
      mydatastructure[[site]][["desc_exdf"]]<- mydatastructure[[site]][["exdf"]] %>%
        dplyr::rename("Units" = "ResultMeasure.MeasureUnitCode") %>%
        dplyr::rename("Site" = "MonitoringLocationName") %>%
        dplyr::rename("Parameter" = "Characteristic") %>%
        dplyr::rename("SampleDate" = "Date") %>%
        dplyr::mutate("Difference" = case_when(
          !is.na(mydatastructure[[site]][["UpperPoint"]])==TRUE & is.na(mydatastructure[[site]][["LowerPoint"]])==TRUE ~ 
            mydatastructure[[site]][["exdf"]]$Value - mydatastructure[[site]][["UpperPoint"]]
          ,!is.na(mydatastructure[[site]][["LowerPoint"]])==TRUE & is.na(mydatastructure[[site]][["UpperPoint"]])==TRUE ~ 
            mydatastructure[[site]][["exdf"]]$Value - mydatastructure[[site]][["LowerPoint"]]
          ,!is.na(mydatastructure[[site]][["UpperPoint"]])==TRUE & !is.na(mydatastructure[[site]][["LowerPoint"]])==TRUE & any(mydatastructure[[site]][["df"]]$Value >= mydatastructure[[site]][["UpperPoint"]], na.rm = TRUE) ~ 
            mydatastructure[[site]][["exdf"]]$Value - mydatastructure[[site]][["UpperPoint"]]
          ,!is.na(mydatastructure[[site]][["UpperPoint"]])==TRUE & !is.na(mydatastructure[[site]][["LowerPoint"]])==TRUE & any(mydatastructure[[site]][["df"]]$Value <= mydatastructure[[site]][["LowerPoint"]], na.rm = TRUE) ~ 
            mydatastructure[[site]][["exdf"]]$Value - mydatastructure[[site]][["LowerPoint"]]
        )) %>%
        dplyr::mutate("Difference" = round(Difference, 4)) %>%
        dplyr::arrange(desc(SampleDate)) %>%
        dplyr::select("Site", "SampleDate", "Parameter", "Value", "Difference", "Units", any_of(c("UpperThreshold", "LowerThreshold")))
        
      
      # Data wrangle for text and hist
      mydatastructure[[site]][["histyears"]]<- data.frame(Year = lubridate::year(mydatastructure[[site]][["df"]]$Date))
      mydatastructure[[site]][["totcount"]]<- mydatastructure[[site]][["histyears"]] %>%
        dplyr::count(Year) %>%
        dplyr::rename("ntot" = "n")
      mydatastructure[[site]][["totcount"]] <- subset(mydatastructure[[site]][["totcount"]], !is.na(Year))
      
      mydatastructure[[site]][["excount"]] <- mydatastructure[[site]][["exdf"]] %>%
        dplyr::mutate(Year = lubridate::year(Date)) %>%
        dplyr::count(Year) %>%
        dplyr::rename("nex" = "n")
      mydatastructure[[site]][["excount"]] <- subset(mydatastructure[[site]][["excount"]], !is.na(Year))
      
      mydatastructure[[site]][["histdata"]] <- dplyr::left_join(mydatastructure[[site]][["totcount"]], mydatastructure[[site]][["excount"]], by = "Year")
      mydatastructure[[site]][["histdata"]][is.na(mydatastructure[[site]][["histdata"]])] <- 0
      mydatastructure[[site]][["histdata"]] <- mydatastructure[[site]][["histdata"]] %>%
        dplyr::mutate(percent_ex = (nex / ntot) * 100) %>%
        dplyr::mutate(formatted_percent_ex = scales::percent(percent_ex / 100, accuracy = 0.01))
      
      # Text prep
      mydatastructure[[site]][["recent_year"]]<- max(mydatastructure[[site]][["histdata"]]$Year)
      mydatastructure[[site]][["oldest_year"]]<- min(mydatastructure[[site]][["histdata"]]$Year)
      mydatastructure[[site]][["nex"]]<- mydatastructure[[site]][["histdata"]][mydatastructure[[site]][["histdata"]]$Year == mydatastructure[[site]][["recent_year"]], "nex"]
      mydatastructure[[site]][["ntot"]]<- mydatastructure[[site]][["histdata"]][mydatastructure[[site]][["histdata"]]$Year == mydatastructure[[site]][["recent_year"]], "ntot"]
      # recent_freq<- sprintf("%.2f%%", (nex/ntot)*100)
      mydatastructure[[site]][["sum_nex"]]<- sum(mydatastructure[[site]][["histdata"]]$nex)
      mydatastructure[[site]][["sum_ntot"]]<- sum(mydatastructure[[site]][["histdata"]]$ntot)
      # sum_freq<- sprintf("%.2f%%", (sum_nex/sum_ntot)*100)
      # freq_comp<- ifelse((nex/ntot) > (sum_nex/sum_ntot), "greater than",
      #                      ifelse((nex/ntot) == (sum_nex/sum_ntot), "equal to", "less than"))
      mydatastructure[[site]][["grammar1"]]<- if(mydatastructure[[site]][["nex"]]==1) {
        paste0("There was ", "<b>", mydatastructure[[site]][["nex"]], "</b>", " exceedance of the ")
      } else {
        paste0("There were ", "<b>", mydatastructure[[site]][["nex"]], "</b>", " exceedances of the ")
      }
      mydatastructure[[site]][["grammar2"]]<- if(mydatastructure[[site]][["sum_nex"]]==1) {
        paste0("There has been ", "<b>", mydatastructure[[site]][["sum_nex"]], "</b>", " exceedance of the ")
      } else {
        paste0("There have been ", "<b>", mydatastructure[[site]][["sum_nex"]], "</b>", " exceedances of the ")
      }
      
      # Writing text
      mydatastructure[[site]][["extext"]]<- c(
        paste0(mydatastructure[[site]][["grammar1"]], mydatastructure[[site]][["Characteristic"]], " water quality threshold among ", mydatastructure[[site]][["ntot"]], " observations at ", mydatastructure[[site]][["Sitename"]], " in ", mydatastructure[[site]][["recent_year"]], ".")
        ,paste0(mydatastructure[[site]][["grammar2"]], mydatastructure[[site]][["Characteristic"]], " water quality threshold among ", mydatastructure[[site]][["sum_ntot"]], " observations at ", mydatastructure[[site]][["Sitename"]], " since monitoring began in ", mydatastructure[[site]][["oldest_year"]], ".")
        # ,paste0("<u>", recent_freq, "</u>", " of observations exceeded the water quality threshold in ", recent_year, ", ", freq_comp, " the overall exceedance percentage of ", "<u>", sum_freq, "</u>", ".")
      )
      mydatastructure[[site]][["extext_bullets"]]<- paste0("<li>", mydatastructure[[site]][["extext"]], "</li>", collapse = "")
      
      mydatastructure[[site]][["html_extext"]]<- paste0(
        "<p><b><span style='font-size: 18px;'>Exceedances Report:</b></p>",
        "<ul>", mydatastructure[[site]][["extext_bullets"]], "</ul>"
      )
      
      # Histogram
      mydatastructure[[site]][["ExPoint"]]<- dplyr::case_when(
        is.na(mydatastructure[[site]][["UpperPoint"]]) == TRUE & !is.na(mydatastructure[[site]][["LowerPoint"]]) == TRUE ~ mydatastructure[[site]][["LowerPoint"]]
        ,!is.na(mydatastructure[[site]][["UpperPoint"]]) == TRUE & is.na(mydatastructure[[site]][["LowerPoint"]]) == TRUE ~ mydatastructure[[site]][["UpperPoint"]]
        ,!is.na(mydatastructure[[site]][["UpperPoint"]]) == TRUE & !is.na(mydatastructure[[site]][["LowerPoint"]]) == TRUE & any(mydatastructure[[site]][["exdf"]]$Value <= mydatastructure[[site]][["LowerPoint"]], na.rm=TRUE) & all(mydatastructure[[site]][["exdf"]]$Value < mydatastructure[[site]][["UpperPoint"]], na.rm=TRUE) ~ mydatastructure[[site]][["LowerPoint"]]
        ,!is.na(mydatastructure[[site]][["UpperPoint"]]) == TRUE & !is.na(mydatastructure[[site]][["LowerPoint"]]) == TRUE & any(mydatastructure[[site]][["exdf"]]$Value >= mydatastructure[[site]][["UpperPoint"]], na.rm=TRUE) & all(mydatastructure[[site]][["exdf"]]$Value > mydatastructure[[site]][["LowerPoint"]], na.rm=TRUE) ~ mydatastructure[[site]][["UpperPoint"]]
        # ,!is.na(mydatastructure[[site]][["UpperPoint"]]) == TRUE & !is.na(mydatastructure[[site]][["LowerPoint"]]) == TRUE & any(mydatastructure[[site]][["exdf"]]$Value >= mydatastructure[[site]][["UpperPoint"]], na.rm=TRUE) & any(mydatastructure[[site]][["exdf"]]$Value <= mydatastructure[[site]][["LowerPoint"]], na.rm=TRUE) ~ paste0(mydatastructure[[site]][["LowerPoint"]], " and ", mydatastructure[[site]][["UpperPoint"]])
      )
      
      mydatastructure[[site]][["p"]]<- ggplot2::ggplot(mydatastructure[[site]][["histdata"]], aes(x = Year, y = percent_ex,
                                                                                                  text = paste0(mydatastructure[[site]][["histdata"]]$Year, ", ", mydatastructure[[site]][["Characteristic"]], "\n",
                                                                                                                mydatastructure[[site]][["histdata"]]$ntot, " total observation(s)", "\n",
                                                                                                                mydatastructure[[site]][["histdata"]]$formatted_percent_ex, " of observations exceeding ", mydatastructure[[site]][["ExPoint"]], " ", mydatastructure[[site]][["Unit"]]))) +
        geom_bar(stat = "identity", fill = "lightgray") +
        ylim(0, 100) +
        labs(
          title = paste0("Percent of ", mydatastructure[[site]][["Characteristic"]], " observations exceeding the water quality threshold at ", mydatastructure[[site]][["Sitename"]]),
          x = "Year",
          y = "% observations") +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
      
      # Hist Alt Text
      alt_text <- c()
      for (i in seq_len(nrow(mydatastructure[[site]][["histdata"]]))) {
        if(mydatastructure[[site]][["histdata"]]$nex[i] != 0) {
          alt_text <- c(alt_text, paste0("<b>", mydatastructure[[site]][["histdata"]]$Year[i], "</b>: ", mydatastructure[[site]][["histdata"]]$ntot[i], " total observation(s), ", mydatastructure[[site]][["histdata"]]$formatted_percent_ex[i], " of observations exceeding ", mydatastructure[[site]][["ExPoint"]], " ", mydatastructure[[site]][["Unit"]]))
        }
      }
      mydatastructure[[site]][["alt_raw"]] <- alt_text
      mydatastructure[[site]][["alt_bullets"]] <- paste0("<li>", mydatastructure[[site]][["alt_raw"]], "</li>", collapse = "")
      mydatastructure[[site]][["alt_bullets2"]] <- paste0("<ul>", mydatastructure[[site]][["alt_bullets"]], "</ul>")
      
      mydatastructure[[site]][["recent_ex"]] <- max(mydatastructure[[site]][["histdata"]]$Year[mydatastructure[[site]][["histdata"]]$nex != 0])
      mydatastructure[[site]][["oldest_ex"]] <- min(mydatastructure[[site]][["histdata"]]$Year[mydatastructure[[site]][["histdata"]]$nex != 0])
      mydatastructure[[site]][["highest_ex_rate"]] <- mydatastructure[[site]][["histdata"]]$formatted_percent_ex[which.max(mydatastructure[[site]][["histdata"]]$percent_ex)]
      mydatastructure[[site]][["hry_vec"]] <- mydatastructure[[site]][["histdata"]]$Year[mydatastructure[[site]][["histdata"]]$percent_ex == max(mydatastructure[[site]][["histdata"]]$percent_ex)]
      vec_format <- function(vec) {
        n <- length(vec)
        if (n == 1) return(as.character(vec[1]))
        if (n == 2) return(paste(vec, collapse = " and "))
        paste(paste(vec[-n], collapse = ", "), "and", vec[n])
      }
      mydatastructure[[site]][["highest_rate_year"]] <- vec_format(mydatastructure[[site]][["hry_vec"]])
      
      mydatastructure[[site]][["n_ex_year"]] <- sum(mydatastructure[[site]][["histdata"]]$nex != 0)
      if (mydatastructure[[site]][["n_ex_year"]] == 1) {
        mydatastructure[[site]][["alt_text_line2"]] <- paste0("Exceedances of the water quality threshold of ", mydatastructure[[site]][["ExPoint"]], " ", mydatastructure[[site]][["Unit"]], " were measured in only ", mydatastructure[[site]][["recent_ex"]], ".")
      } else {
        mydatastructure[[site]][["alt_text_line2"]] <- paste0("Exceedances of the water quality threshold of ", mydatastructure[[site]][["ExPoint"]], " ", mydatastructure[[site]][["Unit"]], " were measured first in ", mydatastructure[[site]][["oldest_ex"]], ", and most recently in ", mydatastructure[[site]][["recent_ex"]], ".")
      }
      
      if (mydatastructure[[site]][["n_ex_year"]] != 0) {
        mydatastructure[[site]][["alt_text"]] <- paste0(
          "<b>Figure Description:</b> Acceptable water quality measurements for ", mydatastructure[[site]][["Characteristic"]], " at ", mydatastructure[[site]][["Sitename"]], " have occurred between ", mydatastructure[[site]][["oldest_year"]], " and ", mydatastructure[[site]][["recent_year"]], ". ",
          mydatastructure[[site]][["alt_text_line2"]],
          " The highest proportion of threshold exceedances per measurements taken in a single year was ", mydatastructure[[site]][["highest_ex_rate"]], " in ", mydatastructure[[site]][["highest_rate_year"]], ".",
          " See exceedances profiles per year below:",
          mydatastructure[[site]][["alt_bullets2"]])
      } else {
        mydatastructure[[site]][["alt_text"]] <- paste0(
          "<b>Figure Description:</b> Acceptable water quality measurements for ", mydatastructure[[site]][["Characteristic"]], " at ", mydatastructure[[site]][["Sitename"]], " have occurred between ", mydatastructure[[site]][["oldest_year"]], " and ", mydatastructure[[site]][["recent_year"]], ". ",
          "There are no exceedances data to describe."
        )
      }
      
    }
      return(mydatastructure)
  })

### Table Hover Text ###
  exceedances_tooltips <- list(
    "Site" = "Site name of monitoring event"
    ,"SampleDate" = "Date of monitoring event"
    ,"Parameter" = "Water quality parameter"
    ,"Value" = "Numeric measurement of a water quality parameter"
    ,"Difference" = "Magnitude of the exceedance (Value - Threshold)"
    ,"Units" = "Units of the measured value"
    ,"UpperThreshold" = "Description of the upper threshold for this site and parameter"
    ,"LowerThreshold" = "Description of the lower threshold for this site and parameter"
  )
  exceedances_tooltips_json <- jsonlite::toJSON(exceedances_tooltips, auto_unbox = TRUE)
  
### Output ###
  show_plot<- shiny::reactiveVal(FALSE)
  
  output$mytabs <- shiny::renderUI({
    req(DataOpts$Park, DataOpts$Site, DataOpts$Param) # execute output$mytabs only if user makes selections

    mydatastructure <- exDUM() # get the data
    
    # if (!is.na(mydatastructure[[site]][["notification_text"]])==TRUE) {shiny::showNotification(
    #   mydatastructure[[site]][["notification_text"]], type = "error", duration = 10)}

    
    nTabs = length(names(mydatastructure))
    myTabs = lapply(seq_len(nTabs), function(i) { # i is the index (e.g., 1, 2, 3)
      shiny::tabPanel(
        mydatastructure[[i]][['Sitename']] # this is the name displayed on the tab
        ,div(
          class = "summary-box"
          ,shiny::uiOutput(paste0("dynamic_text_",i)))
        ,if (show_plot()) {plotlyOutput(paste0("hist_",i))}
        ,if (show_plot()) {uiOutput(paste0("alt_text_",i))}
        ,DT::dataTableOutput(paste0("datatable_",i))
        
        
        )
      })
    do.call(tabsetPanel, myTabs) # make a tabsetPanel containing one tab per site
    
    
    
    })

  shiny::observe(
    lapply(seq_len(length(DataOpts$Site)), function(i) { # i is the index (e.g., 1, 2, 3)

      mydatastructure <- exDUM() # get the data, again
      site <- DataOpts$Site[i] # site ID (e.g., 'NCRN_GWMP_TURU')
      output[[paste0("dynamic_text_",i)]] <- shiny::renderUI({HTML(mydatastructure[[site]][["html_extext"]])})
      output[[paste0("hist_",i)]]<- renderPlotly({plotly::ggplotly(mydatastructure[[site]][["p"]], tooltip = "text")})
      output[[paste0("alt_text_",i)]] <- shiny::renderUI({HTML(mydatastructure[[site]][["alt_text"]])})
      output[[paste0("datatable_",i)]] <- DT::renderDataTable({
        DT::datatable(mydatastructure[[site]][['desc_exdf']]
        ,extensions=c("Buttons","KeyTable")
        # ,caption=htmltools::tags$caption(htmltools::h3(Title()))
        ,class="stripe hover order-column cell-border"
        # ,filter="top"
        ,rownames=F
        ,options=list(
          autoWidth=TRUE
          ,dom="Bltirp"
          ,buttons=c("copy","csv","excel","pdf","print")
          ,keys=TRUE
          ,headerCallback = JS("function(thead, data, start, end, display){",
                               "$(thead).find('th').css('text-align', 'center');",
                               "$(thead).find('th').filter(function() {
                                          return $(this).html().trim() === 'Site'; }).css('text-align', 'left');",
                               "$('th', thead).each(function(index){",
                               " var tooltips = ",
                               exceedances_tooltips_json,";",
                               " var colName = $
                                          (this).html().trim();",
                               " if(tooltips[colName]) {",
                               " $(this).attr('title', tooltips[colName]);",
                               " }",
                               "});",
                               "}"
          )
        )
        )
        })
      
      
      

      }
    )
  )
  
  shiny::observeEvent(input$hist_button, {
    show_plot(!show_plot())
    new_label<- ifelse(show_plot(), "Hide Figure", "Show Figure")
    shiny::updateActionButton(session, "hist_button", label = new_label)
  })
  
  
#### Mapping ####
  
#### USGS Functions ####
  
  getUSGSVal<-function(x){
    read.table(x, sep="\t", header=T)[-1,] %>% select(Site=site_no, Discharge=5)
  }
  
  getUSGSDaily<-function(x) {read.table(x, sep="\t", header=T)[-1,]  %>% 
      filter(month_nu==month(Sys.Date()) & day_nu==day(Sys.Date())) %>% 
      dplyr::select(Site=site_no, P05=p05_va, P25=p25_va, P50=p50_va, P75=p75_va, P95=p95_va )
  }
  
  
  USGScut<-function (value, p05, p25,p50,p75,p95) {
    if (!is.na(p05) & value<=p05) return("<5th percentile") 
    if ( (is.na(p05) & value <=p25) | (!is.na (p05) & value > p05 & value<=p25)) return("5th - 25th percentile")
    if (value > p25 & value <= p50) return("25th - 50th percentile")
    if (value > p50 & value <= p75) return ("50th - 75th percentile")
    if ( (value > p75 & is.na(p95)) | (!is.na(p95) & value > p75 & value <=p95))  return("75th - 95th percentile")
    if(!is.na(p95) & value > p95 ) return ("> 95th percentile")
  }
  
  observeEvent(input$MapUSGS, ignoreInit = TRUE, {if (!DataOpts$USGSload) {
      withProgress(message="Loading USGS data, please be patient.", expr={
        USGSCodes<-read.csv("./Data/USGSSites.csv", header=T, as.is=T, colClasses = "character")$Code %>% paste(collapse=",")
        
        USGSdata<-read.table(paste0("http://waterservices.usgs.gov/nwis/site/?sites=",USGSCodes,
                                    "&format=rdb&siteStatus=active&siteType=ST&hasDataTypeCd=iv&parameterCD=00060"), 
                             sep="\t", header=TRUE)[-1,] %>% select(Site=site_no, Station=station_nm, lat=dec_lat_va, long=dec_long_va)
        
        incProgress(1/3)
        
        USGSdata<-USGSdata %>% mutate(CurrentURL=paste0("http://waterservices.usgs.gov/nwis/iv/?site=",Site,
                                                        "&format=rdb&siteStatus=active&siteType=ST&parameterCd=00060"),
                                      StatURL=paste0("http://waterservices.usgs.gov/nwis/stat/?sites=",USGSdata$Site,
                                                     "&format=rdb&parameterCd=00060&statType=P05,P25,P50,P75,P95&statReportType=daily"), 
                                      SiteURL=USGSSiteURL<-paste0("'https://waterdata.usgs.gov/nwis/uv?",USGSdata$Site,"'"))
        
        USGSdata<-USGSdata %>% left_join(map_df(USGSdata$CurrentURL, possibly(getUSGSVal, otherwise=NULL)), by="Site")
        
        incProgress(1/3)
        
        USGSdata<-USGSdata %>% left_join(map_df(USGSdata$StatURL, possibly(getUSGSDaily, otherwise=NULL)), by="Site") %>%
          mutate(lat=lat %>% as.character %>% as.numeric(), long = long %>% as.character %>% as.numeric, 
                 Discharge=Discharge %>% as.numeric,P05=P05 %>% as.numeric, P25=P25 %>% as.numeric, P50=P50 %>% as.numeric, 
                 P75=P75 %>% as.numeric, P95=P95 %>% as.numeric ) %>% 
          rowwise %>%  mutate(DLevel=USGScut(Discharge,P05,P25,P50,P75,P95))
      })
      
      DataOpts$USGSload<-TRUE
      
      DataOpts$USGSdata<-USGSdata
  }
  })
  
 #### NPS Data ####
  NPSGeoData <- data.frame(ParkCode=getSiteInfo(WaterData, info="ParkCode"), SiteCode=getSiteInfo(WaterData, info="SiteCode"), ParkName=getSiteInfo(WaterData, info = "ParkShortName"), SiteName=getSiteInfo(WaterData, info= "SiteName"), 
                          latitude=getSiteInfo(WaterData, info="lat"), longitude=getSiteInfo(WaterData, info="long"), stringsAsFactors = F)

  active_sites <- metadata_active %>%
    dplyr::filter(IsActiveSiteCode == "True") %>%
    dplyr::pull(SiteCode)
  
  NPSGeoData_filtered <- NPSGeoData %>%
      dplyr::filter(SiteCode %in% active_sites)
  
  #CharIndex is a true/false of characters that have thresholds
  CharIndex<-{getCharInfo(WaterData,info="LowerPoint") %>% is.na %>% not} | {getCharInfo(WaterData,info="UpperPoint") %>% is.na %>% not} 
  NPSchars<-getCharInfo(WaterData, info="CharName")[CharIndex] %>% unique
  names(NPSchars)<-getCharInfo(WaterData, info="DisplayName")[CharIndex] %>% unique
  output$MapChars<-renderUI( selectizeInput(inputId="MapChar",label="Parameter", choices=NPSchars[order(names(NPSchars))] ))
  
  #coloring
  MapColors<- colorBin(
    palette = c("red", "yellow", "blue"),
    domain = c(0,1)
    ,bins = c(1, 0.66, 0.33, 0)
  )
  MapColors2<-colorFactor(palette="viridis", domain=c("<5th percentile","5th - 25th percentile", 
          "25th - 50th percentile", "50th - 75th percentile", "75th - 95th percentile", "> 95th percentile" ), ordered = T )  # USGS percentile category for discharge
  
  ExceedData<-reactive({
    req(input$MapChar)
    exceed(WaterData, charname=input$MapChar)
  })
  
  map_values <- reactiveValues(
    # Initializes a reactiveValues object to store dynamic map settings including longitude, latitude, and zoom level. 
    # Args:
    #   netlon, num, required. Longitude for map centering, initially set to NA. 
    #   netlat, num, required. Latitude for map centering, initially set to NA. 
    #   netzoom, num, required. Zoom level for map centering, initially set to NA. 
    #
    # Returns:
    #   A reactiveValues object, 'map_values', with fields netlon, netlat, and netzom. 
    #
    # Example:
    #   map_values$netlat <- mean(NPSGeoData$latitude, na.rm = TRUE)
    #   map_values$netlon <- mean(NPSGeoData$longitude, na.rm = TRUE)
    #   map_values$netzoom <- 9
    
    netlon = NA, 
    netlat = NA, 
    netzoom = NA
  )
  
  #### the Map ####
  output$WaterMap<-leaflet::renderLeaflet({ 
    # Renders a map widget centered on water monitoring sites, with dynamic zoom and basemap options.  
    # Args:
    #   NPSGeoData$latitude, num, required. Numeric vector containing the latitude coordinates for water monitoring sites. 
    #   NPSGeoData$longitude, num, required. Numeric vector containing the longitude coordinates for water monitoring sites. 
    #   map_values$netlat, num, required. Reactive value used to store the map's central latitude.
    #   map_values$netlon, num, required. Reactive value used to store the map's central longitude.
    #   map_values$netzoom, num, required. Reactive value used to store the zoom level dynamically based on coordinate range. 
    # 
    # Returns:
    #  A Leaflet map with configured basemap layers ("map", "slate") and zoom extent.
    #
    # Example:
    #   output$WaterMap<-leaflet::renderLeaflet({ 
    #   netlat<- mean(NPSGeoData$latitude, na.rm = TRUE)
    #   netlon<- mean(NPSGeoData$longitude, na.rm = TRUE)
    #   netzoom <- 8
    #
    #   map_values$netlat <- netlat
    #   map_values$netlon <- netlon
    #   map_values$netzoom <- netzoom
    #
    #   leaflet::leaflet() %>%
    #   leaflet::setView(lng = netlon, lat = netlat , zoom = netzoom) 
    #   })
    
    #buffer_factor<- 0.05
    netlat<- mean(NPSGeoData$latitude, na.rm = TRUE)
    netlon<- mean(NPSGeoData$longitude, na.rm = TRUE)

    min_lat<- min(NPSGeoData$latitude, na.rm = TRUE)
    max_lat<- max(NPSGeoData$latitude, na.rm = TRUE)
    min_lon<- min(NPSGeoData$longitude, na.rm = TRUE)
    max_lon<- max(NPSGeoData$longitude, na.rm = TRUE)

    lat_range<- max_lat - min_lat
    lon_range<- max_lon - min_lon
    
    max_range<- max(lat_range, lon_range)

    netzoom<- dplyr::case_when(
      max_range > 10 ~5,
      max_range > 5 ~7,
      max_range > 2.5 ~8,
      max_range > 1.5 ~9,
      max_range > 1 ~9,
      max_range > 0.5 ~10,
      TRUE ~10
    )

    map_values$netlat <- netlat
    map_values$netlon <- netlon
    map_values$netzoom <- netzoom

  #netzoom<- ifelse(max(lat_range, long_range) >5, 7, 9)

  # netlat<-dplyr::case_when(Network == "NCRN" ~ 39.25,
  #                          Network == "NETN" ~ 42.5)
  # netlon<-dplyr::case_when(Network == "NCRN" ~ -77,
  #                          Network == "NETN" ~ -71.6)
  # netzoom<-dplyr::case_when(Network == "NCRN" ~ 9,
  #                          Network == "NETN" ~ 7)

    leaflet() %>%
      leaflet::addTiles(group="Map", urlTemplate="https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg", attribution="National Park Service, © Mapbox, and © OpenStreetMap") %>%
    #  leaflet::addTiles(group="Imagery", urlTemplate="https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck72fwp2642dv07o7tbqinvz4/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg", attribution="National Park Service, © Mapbox, and © OpenStreetMap") %>%
      leaflet::addTiles(group="Slate", urlTemplate = "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg", attribution ="National Park Service, © Mapbox, and © OpenStreetMap") %>%
      leaflet::addLayersControl(map=., baseGroups=c("Map","Slate"), options=layersControlOptions(collapsed=T)) %>%

    # addTiles() # temporary workaround to provide a basemap
    # broken map tiles:
    # addTiles(group="Map", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.397cfb9a,nps.3cf3d4ab,nps.b0add3e6/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",attribution=NPSAttrib, options=tileOptions(minZoom=netzoom)) %>%
    # addTiles(group="Imagery", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.2c589204,nps.25abf75b,nps.7531d30a/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",attribution=NPSAttrib, options=tileOptions(minZoom=netzoom)) %>%
    # addTiles(group="Slate", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.9e521899,nps.17f575d9,nps.e091bdaf/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q", attribution=NPSAttrib, options=tileOptions(minZoom=netzoom) ) %>%
    # addLayersControl(map=., baseGroups=c("Map","Imagery","Slate"), options=layersControlOptions(collapsed=T)) %>%

    leaflet::setView(lng = netlon, lat = netlat , zoom = netzoom) 
     })

  NPSAttrib<-HTML("<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> | 
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles' 
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>")
   
  #  observe({
  #   if(input$MapNPS){
  #     leafletProxy("WaterMap") %>% 
  #       clearGroup("NPS") %>% 
  #       addCircleMarkers(data=NPSGeoData, group="NPS", 
  #                        layerId=NPSGeoData$SiteCode, 
  #                        fillColor=MapColors(ExceedData()$Acceptable/ExceedData()$Total),
  #                        fillOpacity=.8, stroke=FALSE) %>% 
  #       
  #       addLegend(position="topright", pal=MapColors, values=c(0,1), opacity=1,
  #                   layerId="npsLegend",title=paste0("<svg height='15' width='20'>
  #                   <circle cx='10' cy='10' r='5', stroke='black' fill='black'/></svg> NPS: % of Acceptable <br>Measurements"),
  #                 labFormat=labelFormat(suffix="%", transform= function(x) 100*x))
  #       } else {leafletProxy("WaterMap") %>% clearGroup("NPS") %>% removeControl(layerId="npsLegend")}
  # })
  
  observe({
    # Updates the "MapIn" checkbox group input dynamically based on unique park names found in NPSGeoData dataset.  
    # Args:
    #   NPSGeoData, dataframe, required. Contains park, site, latitude and longitude information.  
    #
    # Returns:
    #   None. An observer that triggers UI changes when the app loads.  
    #
    # Example:
    #   NPSGeoData <- data.frame(ParkCode=getSiteInfo(WaterData, info="ParkCode"), ParkName=getSiteInfo(WaterData, info = "ParkShortName"), stringsAsFactors = F)
    #   unique_park_names <- unique(NPSGeoData$ParkName)
    
    req(NPSGeoData)
    updateCheckboxGroupInput(session, "MapIn", choices = unique(NPSGeoData$ParkName), inline = FALSE)
  })
  
  directions <- c("top", "bottom", "left", "right", "tr", "tl", "br", "bl")
  offset <- list(
    top = c(0, -18),
    bottom = c(0, 18),
    left = c(-18, 0),
    right = c(18, 0),
    tr = c(18, -18),
    tl = c(-18, -18),
    br = c(18, 18),
    bl = c(-18, 18)
    #,center = c(0,0)
  )
  
  NPSGeoData$label_dir <- directions[ (seq_len(nrow(NPSGeoData)) %%
                                         length(directions)) +1]
  NPSGeoData$xoffset <- sapply(NPSGeoData$label_dir, function(dir) offset[[dir]][1])
  NPSGeoData$yoffset <- sapply(NPSGeoData$label_dir, function(dir) offset[[dir]][2])
  
  
  observe({
    # Handles updates to the map's zoom level, the visibility of labels, and the display of active or inactive sites based on user input.  
    # Args:
    #   input$WaterMap_zoom, int, required. The map zoom level. 
    #   input$MapIn, chr, required. User-selected park names from UI input.
    #   input$InactiveSites, logical, required. A checkbox input for "Display inactive sites" that indicates whether to display inactive sites.   
    #   input$WaterMap_groups, chr, required. Indicates which map overlay groups are currently visible on map.  
    #
    # Returns:
    #   Updates map zoom level, adds/removes markers, and updates labels based on the filtered data and user input. 
    #
    # Examples:
    #   input$WaterMap_zoom <- 12
    #   input$MapIn <- "Catoctin"
    #   input$InactiveSites <- FALSE
    #   input$WaterMap_groups <- c("Map", "NPS")
    #   
    #   show_labels <- if (is.null(input$MapIn) || length(input$MapIn) == 0) {
    #     input$WaterMap_zoom >= 13
    #     } else {
    #     input$WaterMap_zoom >= 11
    #   }
    # 
    #   site_data <- if (input$InactiveSites) {
    #     NPSGeoData
    #     } else{
    #     NPSGeoData_filtered
    #   }
    # 
    #   label_color <- if ("Slate" %in% input$WaterMap_groups) {
    #     "white" 
    #     } else {
    #     "black"
    #   }
    
    req(input$WaterMap_zoom)
    
    zoom_level <- input$WaterMap_zoom
    parks <- input$MapIn

    show_labels <- if (is.null(parks) || length(parks) == 0) {
                    zoom_level >= 13
    } else {
      zoom_level >= 11
    }

    site_data <- NPSGeoData

    # site_data <- if (input$InactiveSites) {
    #   NPSGeoData
    # } else{
    #   NPSGeoData_filtered
    # }

    filtered_data <- if (is.null(parks) || length(parks) == 0) {
      site_data
    } else {
      site_data[site_data$ParkName %in% parks, ]}
    
    filtered_exceed <- ExceedData()
    
    merge_data <- dplyr::left_join(filtered_data, filtered_exceed, by = c("SiteCode"="Site"))
    
    label_color <- if ("Slate" %in% input$WaterMap_groups) {
      "white" 
    } else {
      "black"
    }

    #When parks are selected
    leaflet::leafletProxy("WaterMap") %>%
      leaflet::clearGroup("NPS") %>%
      leaflet::addCircleMarkers(
        data = merge_data
        ,group = "NPS"
        ,layerId = merge_data$SiteCode
        ,fillColor = ~MapColors(merge_data$Acceptable/merge_data$Total)
        ,fillOpacity = 1
        , stroke = FALSE
        ) %>%
      leaflet::addLegend(
        position="topright"
        ,pal=MapColors
        ,values=c(0,1)
        ,opacity=1
        ,layerId="npsLegend"
        ,title=paste0(
          "
          <svg height='15' width='20'>
          <circle cx='10' cy='10' r='5', stroke='black' fill='black'/></svg>
          Percent of Acceptable <br>Measurements
          "
        ),labFormat=labelFormat(suffix="%", transform= function(x) 100*x))

    if (show_labels) {
          leaflet::leafletProxy("WaterMap") %>%    
            leaflet::clearGroup("Sites")
      for (i in seq_len(nrow(merge_data))) {
        leaflet::leafletProxy("WaterMap") %>%
        leaflet::addLabelOnlyMarkers(
          group = "Sites"
          ,lng = merge_data$longitude[i]
          ,lat = merge_data$latitude[i]
          ,label = merge_data$SiteName[i]
          ,labelOptions = labelOptions(
            noHide = TRUE
            ,textOnly = TRUE
            ,direction = merge_data$label_dir[i]
            ,offset= c(merge_data$xoffset[i], merge_data$yoffset[i])
            ,style = list("font-weight"="bold", "font-size"="13px", "color"=label_color)
            )
          )
        } 
          } else {
          leaflet::leafletProxy("WaterMap") %>%
              leaflet::clearGroup("Sites")
          }
  })

  observe({
    # Adjusts map view to selected park(s)
    # Args:
    #   input$MapIn, chr, required. User-selected park names from UI input. 
    #   selected_parks$latitude, num, required. Latitude values for the selected park(s).
    #   selected_parks$longitude, num, required. Longitude values for the selected park(s). 
    #
    # Returns:
    #   Updated Leaflet map ("WaterMap") that zooms to extent of all selected park locations. 
    #
    # Example:
    #   req(input$MapIn)
    #   selected_parks <- NPSGeoData %>%
    #   filter(ParkName %in% input$MapIn)
    #   if (nrow(selected_parks) > 0 ) {
    #   leafletProxy("WaterMap") %>%
    #   fitBounds(
    #     lat1 = min(selected_parks$latitude, na.rm = TRUE),
    #     lat2 = max(selected_parks$latitude, na.rm = TRUE),
    #     lng1 = min(selected_parks$longitude, na.rm = TRUE),
    #     lng2 = max(selected_parks$longitude, na.rm = TRUE)
    #   )}
    
    req(input$MapIn)
    selected_parks <- NPSGeoData %>%
    dplyr::filter(ParkName %in% input$MapIn)
    if (nrow(selected_parks) > 0 ) {
      minLat <- min(selected_parks$latitude, na.rm = TRUE)
      maxLat <- max(selected_parks$latitude, na.rm = TRUE)
      minLon <- min(selected_parks$longitude, na.rm = TRUE)
      maxLon <- max(selected_parks$longitude, na.rm = TRUE)

    latPadding <- (maxLat - minLat) * 0.1
    lonPadding <- (maxLon - minLon) * 0.1

    leaflet::leafletProxy("WaterMap") %>%
      leaflet::fitBounds(lng1 = minLon - lonPadding,
                lat1 = minLat - latPadding,
                lng2 = maxLon + lonPadding,
                lat2 = maxLat + latPadding)
    }
  })
  
  observeEvent(
    # Resets park selection and restores the default Leaflet map view.   
    # Args:
    #   input$refreshParks, chr, required. Reactive input triggered by clicking the "Refresh park selections" button.  
    #   map_values$netlon, chr, required. Default longitude for resetting the map view. 
    #   map_values$netlat, num, required. Default latitude for resetting the map view. 
    #   map_values$netzoom, num, required. Default zoom level. 
    #
    # Returns:
    #   Clears the selected parks checkbox group.
    #   Resets the map view to default latitude/longitude/zoom. 
    #
    # Example:
    #   map_values <- list(netlon = -77.25951, netlat = 38.92281, netzoom = 9)
    #   observeEvent(
    #    input$refreshParks, {
    #    updateCheckboxGroupInput(session, "MapIn", selected = character(0))
    #    leafletProxy("WaterMap") %>%
    #    setView(lng = map_values$netlon, lat = map_values$netlat, zoom = map_values$netzoom)
    #   })
    
    input$refreshParks, {
    shiny::updateCheckboxGroupInput(session, "MapIn", selected = character(0))
     leaflet::leafletProxy("WaterMap") %>%
       leaflet::setView(lng = map_values$netlon, lat = map_values$netlat, zoom = map_values$netzoom) %>%
       leaflet::clearGroup("Sites") %>%
       leaflet::clearGroup("NPS") %>%
       leaflet::removeControl(layerId="npsLegend")
  })
  
  observe({
    if(input$MapUSGS){
      req(DataOpts$USGSdata)
      leafletProxy("WaterMap") %>%
      clearGroup("USGS") %>%
      addCircleMarkers(data=DataOpts$USGSdata, group="USGS", layerId=DataOpts$USGSdata$Site,
                 label=DataOpts$USGSdata$Site,
                 color=MapColors2(DataOpts$USGSdata$DLevel),opacity=.8, fillOpacity=0, stroke=TRUE, weight=8) %>% 
                 addLegend(position="topright", opacity=1,colors=MapColors2(c("<5th percentile","5th - 25th percentile", 
                 "25th - 50th percentile", "50th - 75th percentile", "75th - 95th percentile", "> 95th percentile" )), 
                 labels=c("<5th percentile","5th - 25th percentile", 
                                     "25th - 50th percentile", "50th - 75th percentile", "75th - 95th percentile", "> 95th percentile" ),
                 layerId="usgsLegend",title=" <svg height='15' width='20'> <circle cx='10' cy='10' r='4' stroke='black' stroke-width='3'
                            fill='transparent'/></svg>USGS: Discharge" )
    } else {leafletProxy("WaterMap") %>% clearGroup("USGS") %>% removeControl(layerId="usgsLegend")}
  })

  
  observeEvent(input$WaterMap_marker_click,{
    MarkerClick<-input$WaterMap_marker_click
    ClickData<-if(MarkerClick$group == "NPS") {NPSGeoData %>% filter(SiteCode==MarkerClick$id)} else {DataOpts$USGSdata %>% filter(Site==MarkerClick$id)}
    
    leafletProxy("WaterMap") %>%
      clearPopups() %>% {
       switch(MarkerClick$group[1],
          NPS= addPopups(map=.,lat=MarkerClick$lat, lng=MarkerClick$lng,
            popup= paste("<b>",ClickData$SiteName,"</b>", br(),
            getCharInfo(WaterData, parkcode=ClickData$ParkCode, sitecode=ClickData$SiteCode,charname=input$MapChar, 
            info="DisplayName"),":", br(), round(100*ExceedData()[ExceedData()$Site==ClickData$SiteCode,]$Acceptable/ExceedData()[ExceedData()$Site==ClickData$SiteCode,]$Total,1),"% of measurements meet water quality standards",br(),br(),
            getTresholdText(WaterData, ClickData$ParkCode, ClickData$SiteCode, input$MapChar), br(),br(),
            "<b>References:</b>",br(),
            getCharInfo(WaterData, ClickData$ParkCode, ClickData$SiteCode, input$MapChar, info="AssessmentDetails")
            )),
          USGS=addPopups(map=.,lat=MarkerClick$lat, lng=MarkerClick$lng,
              popup=paste("<b><a href=",ClickData$SiteURL,"target='_blank'>",ClickData$Station, "</a></b>",br(),
                    "USGS: Current Discharge: ", ClickData$Discharge,"cfs",br(), ClickData$DLevel, br(), br(),
                    "<a href=",ClickData$SiteURL,"target='_blank'>Click here</a> to visit USGS gage website"
              )
          )
        )
    }
  })


  PhotoSiteVisits<-reactive({    
    # Make a vector of site visits to be displayed in a picklist in the photos tab 
    # Args:
    #  DataOpts$Park, chr, required. A park acronym. E.g., 'ROCR'.
    #  DataOpts$Site, chr or c(chr), required. A site code. E.g., 'NCRN_ROCR_KLVA'
    #  DataOpts$Years, c(int), required. Vector of integers from the app's year slider.
    #  
    # Returns:
    #  vector
    # 
    # Example:
    #   DataOpts$Park <- 'ROCR'
    #   DataOpts$Site <- c('NCRN_ROCR_KLVA', 'NCRN_ROCR_FEBR')
    #   DataOpts$Years <- c(2010,2020)
    #   
    #   sitevisits <- 
    #     PhotoSiteVisits(
    #       ,DataOpts$Park
    #       ,DataOpts$Site
    #       ,DataOpts$Years
    #   )
    #
    req(DataOpts$Park, DataOpts$Site, DataOpts$Years)

    sitevisits <- c()
    if (DataOpts$Park %in% names(imgs)){
      for (site in DataOpts$Site){
        if (site %in% names(imgs[[DataOpts$Park]])){
          for (yr in names(imgs[[DataOpts$Park]][[site]])){
            if (as.numeric(yr) >= as.numeric(DataOpts$Years[1]) & as.numeric(yr) <= as.numeric(DataOpts$Years[2])){
              for (sitevisit in names(imgs[[DataOpts$Park]][[site]][[yr]])){
                sitevisits <- c(sitevisits, sitevisit)
              }
            }
          }
        }
      }
    }
    sitevisits <- base::sort(sitevisits, decreasing=T)
    sitevisits <- paste(sitevisits, collapse=', ')
  })
  
  output$PhotoSiteVisits<-renderUI(PhotoSiteVisits())

  NoPhotos<-reactive({    
    # Make a html message telling the user if the parameters they chose have no photos
    # Args:
    #  DataOpts$Park, chr, required. A park acronym. E.g., 'ROCR'.
    #  DataOpts$Site, chr or c(chr), required. A site code. E.g., 'NCRN_ROCR_KLVA'
    #  DataOpts$Years, c(int), required. Vector of integers from the app's year slider.
    #  
    # Returns:
    #  vector
    # 
    # Example:
    #   DataOpts$Park <- 'ROCR'
    #   DataOpts$Site <- c('NCRN_ROCR_KLVA', 'NCRN_ROCR_FEBR')
    #   DataOpts$Years <- c(2010,2020)
    #   
    #   sitevisits <- 
    #     PhotoSiteVisits(
    #       ,DataOpts$Park
    #       ,DataOpts$Site
    #       ,DataOpts$Years
    #   )
    #
    req(DataOpts$Park, DataOpts$Site, DataOpts$Years)

    sitevisits <- c()
    if (DataOpts$Park %in% names(imgs)){
      for (site in DataOpts$Site){
        if (site %in% names(imgs[[DataOpts$Park]])){
          for (yr in names(imgs[[DataOpts$Park]][[site]])){
            if (as.numeric(yr) >= as.numeric(DataOpts$Years[1]) & as.numeric(yr) <= as.numeric(DataOpts$Years[2])){
              for (sitevisit in names(imgs[[DataOpts$Park]][[site]][[yr]])){
                sitevisits <- c(sitevisit, sitevisits)
              }
            }
          }
        }
      }
    }
    msg <- NULL
    if (length(sitevisits)==0){
      msg <- 'There are no photos for this combination of park, site, and year.\nPlease try again.'
    }
  })
  
  output$NoPhotos<-renderUI(NoPhotos())

  NoPhotos2<-reactive({    
    # Make a html message telling the user if the parameters they chose have no photos
    # Args:
    #  DataOpts$Park, chr, required. A park acronym. E.g., 'ROCR'.
    #  DataOpts$Site, chr or c(chr), required. A site code. E.g., 'NCRN_ROCR_KLVA'
    #  DataOpts$Years, c(int), required. Vector of integers from the app's year slider.
    #  
    # Returns:
    #  vector
    # 
    # Example:
    #   DataOpts$Park <- 'ROCR'
    #   DataOpts$Site <- c('NCRN_ROCR_KLVA', 'NCRN_ROCR_FEBR')
    #   DataOpts$Years <- c(2010,2020)
    #   
    #   sitevisits <- 
    #     PhotoSiteVisits(
    #       ,DataOpts$Park
    #       ,DataOpts$Site
    #       ,DataOpts$Years
    #   )
    #
    req(DataOpts$Park2, DataOpts$Site2, DataOpts$Years2)

    sitevisits <- c()
    if (DataOpts$Park2 %in% names(imgs)){
      for (site in DataOpts$Site2){
        if (site %in% names(imgs[[DataOpts$Park2]])){
          for (yr in names(imgs[[DataOpts$Park2]][[site]])){
            if (as.numeric(yr) >= as.numeric(DataOpts$Years2[1]) & as.numeric(yr) <= as.numeric(DataOpts$Years2[2])){
              for (sitevisit in names(imgs[[DataOpts$Park2]][[site]][[yr]])){
                sitevisits <- c(sitevisit, sitevisits)
              }
            }
          }
        }
      }
    }
    msg <- NULL
    if (length(sitevisits)==0){
      msg <- 'There are no photos for this combination of park, site, and year.\nPlease try again.'
    }
  })
  
  output$NoPhotos2<-renderUI(NoPhotos2())

  output$image_plot <- renderImage({
    req(DataOpts$Park, DataOpts$Site, DataOpts$Years, DataOpts$SiteVisit, DataOpts$Photo)
    # https://cran.r-project.org/web/packages/slickR/vignettes/shiny.html possible 2.0 version?

    filenames <- c()
    if (DataOpts$Park %in% names(imgs)){
      for (site in DataOpts$Site){
        if (site %in% names(imgs[[DataOpts$Park]])){
          for (yr in names(imgs[[DataOpts$Park]][[site]])){
            if (as.numeric(yr) >= as.numeric(DataOpts$Years[1]) & as.numeric(yr) <= as.numeric(DataOpts$Years[2])){
              for (sitevisit in names(imgs[[DataOpts$Park]][[site]][[yr]])){
                if (sitevisit == DataOpts$SiteVisit){
                  for (f in names(imgs[[DataOpts$Park]][[site]][[yr]][[sitevisit]])){
                    fname <- imgs[[DataOpts$Park]][[site]][[yr]][[sitevisit]][[f]]$rel_fpath
                    filenames <- c(filenames, fname)
                  }
                }
              }
            }
          }
        }
      }
    }
    # filenames <- paste(filenames, collapse=', ')

    # Get the current slider value
    current_image_index <- DataOpts$Photo
    # Get the path to the corresponding image
    image_path <- filenames[current_image_index]
    # Return the image as a list
    list(src = image_path,
         contentType = "image/jpeg", # Or appropriate image type
         width = "100%" # Or desired width
    )
        
    },
    deleteFile=F)
  
  output$image_plot2 <- renderImage({
    req(DataOpts$Park2, DataOpts$Site2, DataOpts$Years2, DataOpts$SiteVisit2, DataOpts$Photo2)
    # https://cran.r-project.org/web/packages/slickR/vignettes/shiny.html possible 2.0 version?

    filenames <- c()
    if (DataOpts$Park2 %in% names(imgs)){
      for (site in DataOpts$Site2){
        if (site %in% names(imgs[[DataOpts$Park2]])){
          for (yr in names(imgs[[DataOpts$Park2]][[site]])){
            if (as.numeric(yr) >= as.numeric(DataOpts$Years2[1]) & as.numeric(yr) <= as.numeric(DataOpts$Years2[2])){
              for (sitevisit in names(imgs[[DataOpts$Park2]][[site]][[yr]])){
                if (sitevisit == DataOpts$SiteVisit2){
                  for (f in names(imgs[[DataOpts$Park2]][[site]][[yr]][[sitevisit]])){
                    fname <- imgs[[DataOpts$Park2]][[site]][[yr]][[sitevisit]][[f]]$rel_fpath
                    filenames <- c(filenames, fname)
                  }
                }
              }
            }
          }
        }
      }
    }
    # filenames <- paste(filenames, collapse=', ')

    # Get the current slider value
    current_image_index <- DataOpts$Photo2
    # Get the path to the corresponding image
    image_path <- filenames[current_image_index]
    # Return the image as a list
    list(src = image_path,
         contentType = "image/jpeg", # Or appropriate image type
         width = "100%" # Or desired width
    )
        
    },
    deleteFile=F)

}) #End of Shiny Server function
    