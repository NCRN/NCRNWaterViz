library(shiny)
library(lattice)
library(dplyr)
library(lubridate)
library(shinyjs)
library(NCRNWater)
library(DT)

source("Water_Cosinor.r")

setClass("NPSDate")
setAs("character","NPSDate", function(from) as.Date(from, format="%m/%d/%Y") )  #explains to read.csv the date format from NPStoret

#### Get data ####
WaterData<-importNCRNWater("./Data/")

##### Shiny Server ####

shinyServer(function(input,output,session){

#### shinyjs toggles ####
  observe ({
    toggle(id="Legend", condition = input$GraphOptions)
    toggle(id="FontSize", condition = input$GraphOptions)
    toggle(id="PointHeader", condition = input$GraphOptions)
    toggle(id="GoodColor", condition = input$GraphOptions)  
    toggle(id="BadColor", condition = input$GraphOptions && input$ThreshPoint)
    toggle(id="OutColor", condition = input$GraphOptions && input$Outliers)
    toggle(id="PointSize", condition = input$GraphOptions)
    toggle(id="LineHeader", condition = input$GraphOptions && (input$ThreshLine || input$Trends) )
    toggle(id="ThColor", condition = input$GraphOptions && input$ThreshLine )
    toggle(id="TrColor", condition = input$GraphOptions && input$Trends )
    toggle(id="LineWidth", condition = input$GraphOptions && (input$ThreshLine || input$Trends) )
  })

 # output$Test<-renderText(input$ParamIn)   #For debugging purposes
#### Make UI controls ####
    
#### Park control ####
  output$parkControl<-renderUI({
    selectizeInput(inputId="ParkIn",label="Park:", choices=c("Choose a Park"="",
                  c(`names<-`(getParkInfo(WaterData, info="ParkCode"),getParkInfo(WaterData, info="ParkShortName"))
                  )))
  })     
  
#### Stream Control ####
  output$streamControl<-renderUI({
    req(input$ParkIn)
    selectizeInput(inputId = "SiteIn", label="Stream:", 
        choices=c("Choose a Site"="",c(`names<-`(getSiteInfo(WaterData, parkcode=input$ParkIn, info="SiteCode"), 
                                                getSiteInfo(WaterData, parkcode=input$ParkIn, info="SiteName")  )))
    )
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
  
  output$ParameterControl<-renderUI({
    req(input$ParkIn, input$SiteIn)
    selectizeInput(inputId="ParamIn", label="Water Parameter:",
      choices=c("Choose a Parameter"="",as.list(PChoices())))
  })
  
#### Year control  - NOTE - this control needs DataUse() below to be populated before it is created. ####
  
  output$yearControl<-renderUI({
    req(input$ParkIn,input$SiteIn, input$ParamIn)
    sliderInput(inputId="YearsShow", label= "Years to Display:", min=min(year(DataUse()$Date), na.rm=T),  
      max=max(year(DataUse()$Date), na.rm=T), value=c(min(year(DataUse()$Date), na.rm=T),  
                max=max(year(DataUse()$Date), na.rm=T)), sep="",ticks=F)
  })
  
#### Housekeeping of data ####
  DataUse<-reactive({ getCharInfo(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="Data")[[1]] })
  
  Thresholds<-reactive({
    c(getCharInfo(WaterData,parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="LowerPoint"),
      getCharInfo(WaterData,parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="UpperPoint"))
  })
  
  Units<-reactive({getCharInfo(WaterData,parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="Units") %>% 
    iconv("","UTF-8") 
  })
  
  TrendsOut<-reactive({
    req(input$Trends | input$Outliers)
    wcosinor(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn)
  })
  
  Title<-reactive({paste(getSiteInfo(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, info="SiteName"),
        getCharInfo(WaterData, parkcode=input$ParkIn, sitecode=input$SiteIn, charname=input$ParamIn, info="DisplayName"),sep=": ")})
 
#### Get Colors from user inputs ####
  BadCol<-reactive({GraphColors[GraphColors$DisplayColor==input$BadColor,]$Rcolor})
  GoodCol<-reactive({GraphColors[GraphColors$DisplayColor==input$GoodColor,]$Rcolor})
  OutCol<-reactive({GraphColors[GraphColors$DisplayColor==input$OutColor,]$Rcolor})
  ThCol<-reactive({GraphColors[GraphColors$DisplayColor==input$ThColor,]$Rcolor})
  TrCol<-reactive({GraphColors[GraphColors$DisplayColor==input$TrColor,]$Rcolor})
      
#### PlotElements determines what is being plotted and is used in the key= argument of lattice  ####
  PlotElements=reactive({                         
    c(input$ThreshPoint, input$ThreshLine, input$Trends, input$Outliers)
  })

 OutPlot<-reactive({
   validate(
     need(input$ParkIn !="", message="Choose a Park"),
     need(input$SiteIn !="", message="Choose a Stream"),
     need(input$ParamIn !="", message="Choose a Water Quality Parameter"),
     need(input$YearsShow !="", message="Choose the Years to Display")
    )
    
    xyplot(Value~Date, data=DataUse() %>% filter(between(year(Date), input$YearsShow[1],input$YearsShow[2])), 
      cex=input$PointSize, 
      pch=16,col=GoodCol(),
      prepanel=function(x,y,...){
        list(ylim=range(y, if(input$ThreshLine) Thresholds() else NA ,na.rm=T))},
      main=list(Title(),cex=input$FontSize),
      ylab=list(label=Units(),cex=input$FontSize),
      xlab=list(label="Sample Date",cex=input$FontSize),
      scales=list(cex=input$FontSize, alternating=1, tck=c(1,0)),
      key=if(input$Legend==TRUE) {
        key=list(border=FALSE, cex=input$PointSize, space="top",columns=min(3,1+sum(PlotElements())),                           
          lines=list(
            pch=c(16,c(16,16,16,1)[PlotElements()]),
            type=c("p",c("p","l","l","p")[PlotElements()] ),
            col=c(GoodCol(),c(BadCol(),ThCol(),TrCol(),OutCol())[PlotElements()]),
            lwd=c(input$LineWidth,c(rep(input$LineWidth,4))[PlotElements()])
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
          panel.abline(Thresholds()[1],col=ThCol(), lwd=input$LineWidth)
        }
        if(input$ThreshLine==TRUE & !is.na(Thresholds()[2])) {      
          panel.abline(Thresholds()[2],col=ThCol(), lwd=input$LineWidth)
        }
   
 #### Threshold Points ####
        if(input$ThreshPoint==TRUE & !is.na(Thresholds()[1])) {
          panel.points(x=DataUse()$Date[DataUse()$Value<Thresholds()[1]], y=DataUse()$Value[DataUse()$Value<Thresholds()[1]],
                       col=BadCol(), pch=16, cex=input$PointSize)
        }  
          
        if(input$ThreshPoint==TRUE & !is.na(Thresholds()[2])) {
          panel.points(x=DataUse()$Date[DataUse()$Value>Thresholds()[2]], y=DataUse()$Value[DataUse()$Value>Thresholds()[2]],
                     col=BadCol(), pch=16, cex=input$PointSize)
        } 
          
#### Trend lines ####
        if(input$Trends==TRUE){
          if(class(TrendsOut()$Analysis)=="lm"){
            panel.lines(TrendsOut()$Analysis$fitted.values~TrendsOut()$CDates, col=TrCol(), lwd=input$LineWidth)
          }
          if(class(TrendsOut()$Analysis)=="Cosinor"){
            panel.lines(TrendsOut()$PredLine$Preds~TrendsOut()$PredLine$PreDates.Date, col=TrCol(), lwd=input$LineWidth)
          }
        }     
#### Outlier Points ####
        if(input$Outliers==TRUE){
          if(nrow(TrendsOut()$Outliers) > 0){
            panel.points(x=TrendsOut()$Outliers$Date, y=TrendsOut()$Outliers$Value, col=OutCol(), pch=1, cex=1.5+input$PointSize,lwd=2)
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

#### Raw data table   #####
 output$WaterTable <-renderDataTable({
    validate(
      need(input$ParkIn !="", message="Choose a Park"),
      need(input$SiteIn !="", message="Choose a Stream"),
      need(input$ParamIn !="", message="Choose a Water Quality Parameter"),
      need(input$YearsShow !="", message="Choose the Years to Display")
    )   
   DataUse()
  })

#### Data download ####
  output$Data.Download<-downloadHandler(
    filename=function(){paste(Title(),".csv",sep="")},
    content=function(file){ 
      write.csv(DataUse(),file) #DataOut()
    }
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
    