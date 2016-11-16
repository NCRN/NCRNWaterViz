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


  #output$Test<-renderText(class(TrendsOut()$Analysis))   #For debugging purposes

#### Reactive Values for Graphics Optiions with Defaults ####
  
  GraphOpts<-reactiveValues(Legend=TRUE, FontSize=1.5, GoodColor="blue", BadColor="Orange",OutColor="Vermillion",PointSize=2.5,
                            ThColor="Orange", TrColor="Green", LineWidth=1)
  
  
#### Make UI controls ####
    
#### Park control update ####
  observe({
    updateSelectizeInput(session, inputId="ParkIn", choices=c("Choose a Park"="", 
      c(`names<-`(getParkInfo(WaterData, info="ParkCode"),getParkInfo(WaterData, info="ParkShortName")))))
  })
  
#### Stream Control ####
  observe({
    req(input$ParkIn)
    updateSelectizeInput(session, inputId = "SiteIn",  choices=c("Choose a Site"="",
      c(`names<-`(getSiteInfo(WaterData, parkcode=input$ParkIn, info="SiteCode"), 
                  getSiteInfo(WaterData, parkcode=input$ParkIn, info="SiteName")  ))))
  })
  
#### Parameter Choices ####
  PChoices<-reactive({
    Choice<-getCharInfo(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn,info="CharName")
    ChoiceName<-paste(getCharInfo(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, info="DisplayName"),
        getCharInfo(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn,info="Units") %>% iconv("","UTF-8"))
   names(Choice)<-ChoiceName
   return(Choice)
  })
  
#### Parameter control ####
  
  observe({
    req(input$ParkIn, input$SiteIn)
    updateSelectizeInput(session, inputId="ParamIn",choices=c("Choose a Parameter"="",as.list(PChoices())))
  })
  
#### Year control  - NOTE - this control needs DataUse() below to be populated before it is created. ####
  
observe({
    req(input$ParkIn,input$SiteIn, input$ParamIn)
    updateSliderInput(session=session, inputId="YearsShow",min=min(year(DataUse()$Date), na.rm=T),  
      max=max(year(DataUse()$Date), na.rm=T), value=c(min(year(DataUse()$Date), na.rm=T), max=max(year(DataUse()$Date), na.rm=T)))
  })
  
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
               colourInput(inputId="GoodColor", label="Measurement Color", value=GraphOpts$GoodColor, showColour = "background")
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
       need(input$ParkIn !="", message="Choose a Park"),
       need(input$SiteIn !="", message="Choose a Stream"),
       need(input$ParamIn !="", message="Choose a Water Quality Parameter")
      # need(input$YearsShow !="", message="Choose the Years to Display")
     )  
    getCharInfo(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="Data")[[1]] })
  
  Thresholds<-reactive({
    c(getCharInfo(WaterData,parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="LowerPoint"),
      getCharInfo(WaterData,parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="UpperPoint"))
  })
  
  Units<-reactive({getCharInfo(WaterData,parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="Units") %>% 
    iconv("","UTF-8") 
  })
  
  TrendsOut<-reactive({
    wcosinor(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn)
  })
  
  Title<-reactive({paste(getSiteInfo(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, info="SiteName"),
        getCharInfo(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="DisplayName"),sep=": ")})
 
#### Get Colors from user inputs ####
  BadCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$BadColor,]$Rcolor})
  GoodCol<-reactive({GraphOpts$GoodColor})#reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$GoodColor,]$Rcolor})
  OutCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$OutColor,]$Rcolor})
  ThCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$ThColor,]$Rcolor})
  TrCol<-reactive({GraphColors[GraphColors$DisplayColor==GraphOpts$TrColor,]$Rcolor})
      
#### PlotElements determines what is being plotted and is used in the key= argument of lattice  ####
  PlotElements=reactive({                         
    c(input$ThreshPoint, input$ThreshLine, input$Trends, input$Outliers)
  })

 OutPlot<-reactive({
    xyplot(Value~Date, data=DataUse() %>% filter(between(year(Date), input$YearsShow[1],input$YearsShow[2])), 
      cex=GraphOpts$PointSize, 
      pch=16,col=GoodCol(),
      prepanel=function(x,y,...){
        list(ylim=range(y, if(input$ThreshLine) Thresholds() else NA ,na.rm=T))},
      main=list(Title(),cex=GraphOpts$FontSize),
      ylab=list(label=Units(),cex=GraphOpts$FontSize),
      xlab=list(label="Sample Date",cex=GraphOpts$FontSize),
      scales=list(cex=GraphOpts$FontSize, alternating=1, tck=c(1,0)),
      key=if(GraphOpts$Legend==TRUE) {
        key=list(border=FALSE, cex=GraphOpts$PointSize, space="top",columns=min(3,1+sum(PlotElements())),                           
          lines=list(
            pch=c(16,c(16,16,16,1)[PlotElements()]),
            type=c("p",c("p","l","l","p")[PlotElements()] ),
            col=c(GoodCol(),c(BadCol(),ThCol(),TrCol(),OutCol())[PlotElements()]),
            lwd=c(GraphOpts$LineWidth,c(rep(GraphOpts$LineWidth,4))[PlotElements()])
          ),
          text=list(
            c("Measurement",c("Fails Threshold","Threshold","Trend Line","Outliers")[PlotElements()])
          )
        ) #end Key
      },
      panel=function(x,y,...){
        panel.xyplot(x,y,...)
       
 #### Threshold lines  ####
        if(input$ThreshLine==TRUE & !is.na(Thresholds()[1])) {
          panel.abline(Thresholds()[1],col=ThCol(), lwd=GraphOpts$LineWidth)
        }
        if(input$ThreshLine==TRUE & !is.na(Thresholds()[2])) {      
          panel.abline(Thresholds()[2],col=ThCol(), lwd=GraphOpts$LineWidth)
        }
   
 #### Threshold Points ####
        if(input$ThreshPoint==TRUE & !is.na(Thresholds()[1])) {
          panel.points(x=DataUse()$Date[DataUse()$Value<Thresholds()[1]], y=DataUse()$Value[DataUse()$Value<Thresholds()[1]],
                       col=BadCol(), pch=16, cex=GraphOpts$PointSize)
        }  
          
        if(input$ThreshPoint==TRUE & !is.na(Thresholds()[2])) {
          panel.points(x=DataUse()$Date[DataUse()$Value>Thresholds()[2]], y=DataUse()$Value[DataUse()$Value>Thresholds()[2]],
                     col=BadCol(), pch=16, cex=GraphOpts$PointSize)
        } 
          
#### Trend lines ####
        if(input$Trends==TRUE){
          if(class(TrendsOut()$Analysis)=="lm"){
            panel.lines(TrendsOut()$Analysis$fitted.values~TrendsOut()$CDates, col=TrCol(), lwd=GraphOpts$LineWidth)
          }
          if(class(TrendsOut()$Analysis)=="Cosinor"){
            panel.lines(TrendsOut()$PredLine$Preds~TrendsOut()$PredLine$PreDates.Date, col=TrCol(), lwd=GraphOpts$LineWidth)
          }
        }     
#### Outlier Points ####
        if(input$Outliers==TRUE){
          if(nrow(TrendsOut()$Outliers) > 0){
            panel.points(x=TrendsOut()$Outliers$Date, y=TrendsOut()$Outliers$Value, col=OutCol(), pch=1, cex=1.5+GraphOpts$PointSize,lwd=2)
          }
        }  
      } #ends panel funciton
    )# ends xyplot
  })# end reactive

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
  
#### Plot2 ####
    WaterSeriesOut<-reactive({
      SeriesPlot<-waterseries(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, char=input$ParamIn, 
            years=input$YearsShow[1]:input$YearsShow[2],layers=c("points"),assessment=input$ThreshLine, title=Title(),
            colors=c(GoodCol(), "black", ThCol()), sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth )) +
      #geom_point(size=GraphOpts$PointSize, color=GoodCol(), pch=16)+
      #{if(input$ThreshLine) geom_hline(size=GraphOpts$LineWidth)}+
      theme(text=element_text(size=GraphOpts$FontSize*10))+
        
      {if(input$Outliers) geom_point(data=TrendsOut()[["Outliers"]], aes(Date,Value),pch=1,size=GraphOpts$PointSize+2,
                  color=OutCol(),stroke=1.5)} +
        
      {if(input$ThreshPoint & !is.na(Thresholds()[1])) geom_point(data=DataUse()[DataUse()$Value<Thresholds()[1],], 
                   aes(Date,Value), pch=16,size=GraphOpts$PointSize, color=BadCol()) } +
        
      {if(input$ThreshPoint & !is.na(Thresholds()[2])) geom_point(data=DataUse()[DataUse()$Value>Thresholds()[2],], 
                  aes(Date,Value), pch=16, size=GraphOpts$PointSize, color=BadCol())} +
      
      {if(input$Trends & class(TrendsOut()$Analysis)=="lm") geom_line(data=data.frame(Value=TrendsOut()$Analysis$fitted.values,
                Date=TrendsOut()$CDates), aes(Date,Value), color=TrCol(), lwd=GraphOpts$LineWidth) } +
      {if(input$Trends & class(TrendsOut()$Analysis)=="Cosinor") geom_line(data=data.frame(Value=TrendsOut()$PredLine$Preds,
               Date=TrendsOut()$PredLine$PreDates.Date),  aes(Date,Value), col=TrCol(), lwd=GraphOpts$LineWidth)}
     SeriesPlot  #forces ggplot to draw graph after all the conditionals
  })
  
  output$Plot2<-renderPlot({
    WaterSeriesOut()
  })
  
  
#### Raw data table   #####
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
    