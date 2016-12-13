library(shiny)
library(lattice)
library(dplyr)
library(lubridate)
library(NCRNWater)
library(DT)
library(htmltools)
library(ggplot2)
library(colourpicker)
library(leaflet)


#### Get data ####
WaterData<-importNCRNWater("./Data/")

##### Shiny Server ####

shinyServer(function(input,output,session){


#output$Test<-renderText(TimeSite())   #For debugging purposes

#### Reactive Values for Graphics Optiions with Defaults ####
  
GraphOpts<-reactiveValues(Legend=TRUE, FontSize=1.5, GoodColor="blue", BadColor="Orange",OutColor="Vermillion",PointSize=3,
                            ThColor="Orange", TrColor="Green", LineWidth=1)
  
 
#### Reactive Values for Choosing Data ####

DataOpts<-reactiveValues(Park=NA, Site=NA, Param=NA, Years=NA)

#### UI Controls ####  

#### Time Series Controls ####
TimePark<-callModule(parkChooser, id="TimePark", data=WaterData, chosen=reactive(DataOpts$Park))
TimeSite<-callModule(siteChooser, id="TimeSite", data=WaterData, park=reactive(DataOpts$Park), chosen=reactive(DataOpts$Site))
TimeParam<-callModule(paramChooser, id="TimeParam",data=WaterData, park=reactive(DataOpts$Park), site=reactive(DataOpts$Site), 
                      chosen=reactive(DataOpts$Param))
TimeYears<-callModule(yearChooser, id="TimeYears", data=DataUse, chosen=reactive(DataOpts$Years) )


observeEvent(TimePark(), DataOpts$Park<-TimePark() )
observeEvent(TimeSite(), DataOpts$Site<-TimeSite() )
observeEvent(TimeParam(), DataOpts$Param<-TimeParam() )
observeEvent(TimeYears(), DataOpts$Years<-TimeYears() )

#### Graphics Modal Control ####
  
  observeEvent(input$GraphicsModal,
    showModal(modalDialog(title="Graphics Options", footer=tagAppendAttributes( modalButton(tags$div("Close")), class="btn btn-primary"),
      column(12,hr()),
      column(12,h4("General:"),
        column(3,checkboxInput("Legend","Show Legend",GraphOpts$Legend)),
        column(3,sliderInput("FontSize", "Font Size", min=1, max=2.5,value=GraphOpts$FontSize, step=.25, width='130px'))
      
      ),
      column(12,hr()),
      column(12, h4("Points:"),
        column(3,#selectInput("GoodColor","Measurement Color:",choices=GraphColors$DisplayColor, 
                           #selected=GraphOpts$GoodColor, width='130px')
              colourInput(inputId="GoodColor", label="Measurement Color", value=GraphOpts$GoodColor, palette="limited",showColour = "background")
              
        ),
        column(3,selectInput("BadColor","Poor Quality Color:",choices=GraphColors$DisplayColor,selected=GraphOpts$BadColor,
                             width='130px') ),
        column(3,selectInput("OutColor","Outlier Color:",choices=GraphColors$DisplayColor,selected=GraphOpts$OutColor, width='130px')),   
        column(3,sliderInput("PointSize", "Change Size", min=1, max=6,value=GraphOpts$PointSize, step=.5, width='130px'))
      ),
      column(12,hr()),
      column(12, h4("Lines:"),
        column(3,selectInput("ThColor","Threshold Color:",choices=GraphColors$DisplayColor,selected=GraphOpts$ThColor, width='130px')), 
        column(3,selectInput("TrColor","Trend Color:",choices=GraphColors$DisplayColor,selected=GraphOpts$TrColor, width='130px')),
        column(3,sliderInput("LineWidth", "Change Width", min=.5, max=4,value=GraphOpts$LineWidth, step=.5, width='130px'))
      )
    ))
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
  
#### Housekeeping of data ####
  DataUse<-reactive({ 
     validate(
       need(DataOpts$Park, message="Choose a Park"),
       need(DataOpts$Site, message="Choose a Stream"),
       need(DataOpts$Param, message="Choose a Water Quality Parameter")
     )  
    getWData(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param)
  })
  
  Thresholds<-reactive({
    c(getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="LowerPoint"),
      getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="UpperPoint"))
  })
  
  Units<-reactive({getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="Units") %>% 
    iconv("","UTF-8") 
  })
  
  TrendsOut<-reactive({
    req(DataOpts$Park, DataOpts$Site, DataOpts$Param)
    wcosinor(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param)
  })
  
  Title<-reactive({
    paste(getSiteInfo(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, info="SiteName"),
        getCharInfo(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="DisplayName"),sep=": ")
    })
 
#### Get Colors from user inputs ####
  BadCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$BadColor,]$Rcolor})
  GoodCol<-reactive({GraphOpts$GoodColor})#reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$GoodColor,]$Rcolor})
  OutCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$OutColor,]$Rcolor})
  ThCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$ThColor,]$Rcolor})
  TrCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$TrColor,]$Rcolor})

#### Summaries of Seaonality and Trends ####
  output$SeasonOut<-renderText({
    validate(
      need (input$Trends==TRUE, message=FALSE),
      need(is.atomic(TrendsOut())==FALSE, message=FALSE)
    )
    switch(class(TrendsOut()$Analysis),
      "lm" =      c("There is no seasonal pattern in the data."),
      "Cosinor" = c("There is a seasonal pattern in the data. The peak is", strsplit(summary(TrendsOut()$Analysis)$phase," ")[[1]][3],
                  strsplit(summary(TrendsOut()$Analysis)$phase," ")[[1]][7], "and the low point is ",
        strsplit(summary(TrendsOut()$Analysis)$lphase," ")[[1]][3],paste0(strsplit(summary(TrendsOut()$Analysis)$lphase," ")[[1]][7] ,"." 
      )),
    NULL)
  })

  SeriesTrendsOut<-reactive({
    req(input$Trends, is.atomic(TrendsOut())==FALSE)
    
    paste(h4("Trend Analysis:"),"/n",
      switch(class(TrendsOut()$Analysis),
        "lm" =  {
          if(summary(TrendsOut()$Analysis)$coefficients[2,4]>.05) {("There is no significant trend in the data.")} 
          else {
            paste("There is a significant", 
            ifelse (summary(TrendsOut()$Analysis)$coefficients[2,1] > 0, "increasing", "decreasing"),
            "trend of",c(signif(summary(TrendsOut()$Analysis)$coefficients[2,1]*365.24, digits=3)),
            Units(), "per year.")
          }
        }, 
        "Cosinor"=  {
          if(summary(TrendsOut()$Analysis$glm)$coefficients[2,4]>.05){("There is no significant trend in the data")}
          else {
            c("There is a significant",
            ifelse (summary(TrendsOut()$Analysis$glm)$coefficients[2,1]>0,"increasing","decreasing"), 
            "trend of",c(signif(summary(TrendsOut()$Analysis$glm)$coefficients[2,1]*365.24,digits=3)),
            Units(), "per year."
            )
          }
        },
      NULL))
  })
  
  output$TrendsOut<-renderUI(HTML(SeriesTrendsOut() ))
  
#### Threshold Summary ####
  ThresholdSummary<-reactive({
    req(input$SeriesThreshLine | input$ThreshPoint)
    paste(h4("Threshold:"),"\n",
      c(getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="LowerDescription"),
      getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, 
                  info="UpperDescription"))[!is.na(Thresholds())])
  })
  
  output$SeriesThresholdSummary<-renderUI(HTML(ThresholdSummary()))
  
  
  RefSummary<-reactive({
    req(input$SeriesThreshLine | input$ThreshPoint) 
    paste(h4("Threshold Reference:"),"\n",
    getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="AssessmentDetails")) 
  })      
  
  output$SeriesRefSummary<-renderUI(HTML(RefSummary()))
  
  
#### Time Series Plot ####
    WaterSeriesOut<-reactive({
      req( DataUse()$Date, DataUse()$Value)
      SeriesPlot<-waterseries(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, char=DataOpts$Param, 
            years=DataOpts$Years[1]:DataOpts$Years[2],layers=c("points"),assessment=input$SeriesThreshLine, title=Title(),
            colors=(GoodCol()),assesscolor=ThCol(), sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth),
            legend=if(GraphOpts$Legend) "bottom" else "none") +
      theme(text=element_text(size=GraphOpts$FontSize*10))+
        
      {if(input$Outliers && exists("TrendsOut")) geom_point(data=TrendsOut()[["Outliers"]], aes(Date,Value),pch=1,
                  size=GraphOpts$PointSize+2,color=OutCol(),stroke=1.5)} +
        
      {if(input$ThreshPoint && !is.na(Thresholds()[1])) geom_point(data=DataUse()[DataUse()$Value<Thresholds()[1],], 
                   aes(Date,Value), pch=16,size=GraphOpts$PointSize, color=BadCol()) } +
        
      {if(input$ThreshPoint && !is.na(Thresholds()[2])) geom_point(data=DataUse()[DataUse()$Value>Thresholds()[2],], 
                  aes(Date,Value), pch=16, size=GraphOpts$PointSize, color=BadCol())} +
      
      {if(input$Trends && exists("TrendsOut") && class(TrendsOut()$Analysis)=="lm") geom_line(data=data.frame(
        Value=TrendsOut()$Analysis$fitted.values,Date=TrendsOut()$CDates), aes(Date,Value), color=TrCol(), lwd=GraphOpts$LineWidth) } +
        
      {if(input$Trends && exists("TrendsOut") && class(TrendsOut()$Analysis)=="Cosinor") geom_line(data=data.frame(
        Value=TrendsOut()$PredLine$Preds,Date=TrendsOut()$PredLine$PreDates.Date),  aes(Date,Value), col=TrCol(), 
        lwd=GraphOpts$LineWidth)}
     
      SeriesPlot  #forces ggplot to draw graph after all the conditionals
  })
  
  output$TimeSeries<-renderPlot({
    WaterSeriesOut()
  })

  
#### Box Plot Controls ####
  BoxPark<-callModule(parkChooser, id="BoxPark", data=WaterData, chosen=reactive(DataOpts$Park))
  BoxSite<-callModule(siteChooser, id="BoxSite", data=WaterData, park=reactive(DataOpts$Park), chosen=reactive(DataOpts$Site))
  BoxParam<-callModule(paramChooser, id="BoxParam",data=WaterData, park=reactive(DataOpts$Park), site=reactive(DataOpts$Site), 
                        chosen=reactive(DataOpts$Param))
  BoxYears<-callModule(yearChooser, id="BoxYears", data=DataUse, chosen=reactive(DataOpts$Years) )
  
  
  observeEvent(BoxPark(), DataOpts$Park<-BoxPark() )
  observeEvent(BoxSite(), DataOpts$Site<-BoxSite() )
  observeEvent(BoxParam(), DataOpts$Param<-BoxParam() )
  observeEvent(BoxYears(), DataOpts$Years<-BoxYears() )
  
#### Box Plot ####
  
  output$BoxPlot<-renderPlot({
    req(DataOpts$Park, DataOpts$Site, DataOpts$Param)
    waterbox(object=WaterData, parkcode=DataOpts$Park, sitecode=if(input$BoxBy !="site") DataOpts$Site else NA, 
             charname = DataOpts$Param, by=input$BoxBy, title=Title(),
             years=DataOpts$Years[1]:DataOpts$Years[2], assessment=input$BoxThreshLine, assesscolor=ThCol(), outliercolor = BadCol(),
             sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth))
    
  })
  
#### Data table controls #### 
  DataPark<-callModule(parkChooser, id="DataPark", data=WaterData, chosen=reactive(DataOpts$Park))
  DataSite<-callModule(siteChooser, id="DataSite", data=WaterData, park=reactive(DataOpts$Park), chosen=reactive(DataOpts$Site))
  DataParam<-callModule(paramChooser, id="DataParam",data=WaterData, park=reactive(DataOpts$Park), site=reactive(DataOpts$Site), 
                       chosen=reactive(DataOpts$Param))
  #DataYears<-callModule(yearChooser, id="DataYears", data=DataUse, chosen=reactive(DataOpts$Years) )
  
  
  observeEvent(DataPark(), DataOpts$Park<-DataPark() )
  observeEvent(DataSite(), DataOpts$Site<-DataSite() )
  observeEvent(DataParam(), DataOpts$Param<-DataParam() )
  #observeEvent(DataYears(), DataOpts$Years<-DataYears() )
  
### Data table output ####
 output$WaterTable <-DT::renderDataTable(
   expr=datatable(DataUse(), extensions=c("Buttons","KeyTable"),caption=htmltools::tags$caption(htmltools::h3(Title())),
                  class="stripe hover order-column cell-border",filter="top",
      rownames=F, options=list(autoWidth=TRUE, dom="Bltirp", buttons=c("copy","csv","excel","pdf","print"), keys=TRUE)
                  ),server=F
  )

  
  
#### Mapping ####
  output$WaterMap<-renderLeaflet({ 
    leaflet() %>%
    setView(lng=-77, lat=39.25, zoom=9) %>% 
      
      addTiles(group="Map", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.2yxv8n84,nps.jhd2e8lb/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",attribution=NPSAttrib, options=tileOptions(minZoom=8))%>% 
      addTiles(group="Imagery", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.2c589204,nps.25abf75b,nps.7531d30a/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",attribution=NPSAttrib, options=tileOptions(minZoom=8)) %>% 
      addTiles(group="Slate", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.68926899,nps.502a840b/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q", attribution=NPSAttrib, options=tileOptions(minZoom=8) ) %>% 
      addLayersControl(map=., baseGroups=c("Map","Imagery","Slate"), options=layersControlOptions(collapsed=F))  
      
         #lng=mean(c(ParkBounds[ParkBounds$ParkCode==Network,]$LongE,ParkBounds[ParkBounds$ParkCode==Network,]$LongW)), 
      #         lat=mean(c(ParkBounds[ParkBounds$ParkCode==Network,]$LatN,ParkBounds[ParkBounds$ParkCode==Network,]$LatS)),
      #         zoom=8 ) %>% 
     # setMaxBounds(#lng1=ParkBounds[ParkBounds$ParkCode==Network,]$LongE,lng2=ParkBounds[ParkBounds$ParkCode==Network,]$LongW, 
                   #lat1=ParkBounds[ParkBounds$ParkCode==Network,]$LatN, lat2=ParkBounds[ParkBounds$ParkCode==Network,]$LatS)
       # lng1=-79.5,lng2=-76.1, lat1=37.7, lat2=40.36)
  })
  
  NPSAttrib<-HTML("<a href='https://www.nps.gov/npmap/disclaimer' target='_blank'>Disclaimer</a>
     &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a> 
    &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors | 
    <a class='improve-park-tiles' href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4.00/-95.98/39.03'
    target='_blank'>Improve Park Tiles</a>")

#### Get USGS Data ####
  #http://waterservices.usgs.gov/nwis/iv/?StateCd=MD&format=json
  
  
  #### Plot downloads ####
  output$Plot.PNG<-downloadHandler(
    filename=function(){paste(Title(), ".png", sep="")}, 
    content=function (file){
      png(file,width=960, height=480)
      print(OutPlot())
      dev.off()
    }
  )

  output$Plot.JPG<-downloadHandler(
    filename=function(){paste(Title(), ".jpeg", sep="")}, 
    content=function (file){
      jpeg(file,width=960, height=480,quality=100)
      print(OutPlot())
      dev.off()
    }
  )

}) #End of Shiny Server function
    