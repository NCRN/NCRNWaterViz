library(shiny)
library(lattice)
library(dplyr)
library(lubridate)
library(NCRNWater)
library(DT)
library(htmltools)
library(ggplot2)
library(colourpicker)



#### Get data ####
WaterData<-importNCRNWater("./Data/")

##### Shiny Server ####

shinyServer(function(input,output,session){


output$Test<-renderText(DataUse()$Date)   #For debugging purposes

#### Reactive Values for Graphics Optiions with Defaults ####
  
GraphOpts<-reactiveValues(Legend=TRUE, FontSize=1.5, GoodColor="blue", BadColor="Orange",OutColor="Vermillion",PointSize=2.5,
                            ThColor="Orange", TrColor="Green", LineWidth=1)
  
 

#### UI Controls ####  

#### Time Series Controls ####
TimePark<-callModule(parkChooser, id="TimePark", WaterData)  
TimeSite<-callModule(siteChooser, id="TimeSite", data=WaterData, park=TimePark)
TimeParam<-callModule(paramChooser, id="TimeParam",data=WaterData, park=TimePark, site=TimeSite)
TimeYears<-callModule(yearChooser, id="TimeYears", data=DataUse) 

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
        column(3,sliderInput("PointSize", "Change Size", min=1, max=5,value=GraphOpts$PointSize, step=.5, width='130px'))
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
       need(TimePark(), message="Choose a Park"),
       need(TimeSite(), message="Choose a Stream"),
       need(TimeParam(), message="Choose a Water Quality Parameter")
     )  
    getWData(WaterData, parkcode=TimePark(), sitecode=TimeSite(), charname=TimeParam())
  })
  
  Thresholds<-reactive({
    c(getCharInfo(WaterData,parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="LowerPoint"),
      getCharInfo(WaterData,parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="UpperPoint"))
  })
  
  Units<-reactive({getCharInfo(WaterData,parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="Units") %>% 
    iconv("","UTF-8") 
  })
  
  TrendsOut<-reactive({
    req(input$ParkIn, input$SiteIn, input$ParamIn)
    wcosinor(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn)
  })
  
  Title<-reactive({
    paste(getSiteInfo(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, info="SiteName"),
        getCharInfo(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="DisplayName"),sep=": ")
    })
 
#### Get Colors from user inputs ####
  BadCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$BadColor,]$Rcolor})
  GoodCol<-reactive({GraphOpts$GoodColor})#reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$GoodColor,]$Rcolor})
  OutCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$OutColor,]$Rcolor})
  ThCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$ThColor,]$Rcolor})
  TrCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$TrColor,]$Rcolor})
      


#### Plot Output ####
 
  output$WaterPlot<-renderPlot({
    print(OutPlot())
  }) 

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

  output$TrendsOut<-renderText({
    req(input$Trends)
    req(is.atomic(TrendsOut())==FALSE)
  
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
    NULL) 
  })

#### Threshold Summary ####
  output$ThresholdSummary<-renderText({
    req(input$ThreshLine | input$ThreshPoint)
    paste(
      c(getCharInfo(WaterData,parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="LowerDescription"),
      getCharInfo(WaterData,parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, 
                  info="UpperDescription"))[!is.na(Thresholds())])
  })

  output$RefSummary<-renderText({
    req(input$ThreshLine | input$ThreshPoint) 
    getCharInfo(WaterData,parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="AssessmentDetails")
  })      
  
#### Time Series Plot ####
    WaterSeriesOut<-reactive({
      req( DataUse()$Date, DataUse()$Value)
      SeriesPlot<-waterseries(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, char=input$ParamIn, 
            years=input$YearsShow[1]:input$YearsShow[2],layers=c("points"),assessment=input$ThreshLine, title=Title(),
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

  
#### Box Plot ####
  
  output$BoxPlot<-renderPlot({
    req(input$ParkIn, input$SiteIn, input$ParamIn)
    waterbox(object=WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, charname = input$ParamIn, 
             years=input$YearsShow[1]:input$YearsShow[2], assessment=input$ThreshLine,assesscolor=ThCol(), outliercolor = BadCol(),
             sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth))
    
  })
  
  
#### Raw data table ####
 output$WaterTable <-DT::renderDataTable(
   expr=datatable(DataUse(), extensions=c("Buttons","KeyTable"),caption=htmltools::tags$caption(htmltools::h3(Title())),
                  class="stripe hover order-column cell-border",filter="top",
      rownames=F, options=list(autoWidth=TRUE, dom="Bftirp", buttons=c("copy","csv","excel","pdf","print"), keys=TRUE)
                  ),server=F
  )


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
    