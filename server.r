library(shiny)
library(lattice)
library(dplyr)
library(lubridate)

source("Water_Cosinor.r")

setClass("NPSDate")
setAs("character","NPSDate", function(from) as.Date(from, format="%m/%d/%Y") )  #explains to read.csv the date format from NPStoret

############### get data
WaterData<-tbl_df(
  read.csv("Water Data.csv", header=TRUE, col.names=c("Network","StationID","StationName","VisitDate","Parameter","Result"),
                    colClasses=c("factor","character","character","NPSDate","character","numeric" ),comment.char="")
)
WaterData<-WaterData %>% mutate(VistiDate=ymd(VisitDate), Year=year(VisitDate))
Parameters<-tbl_df(read.csv("Parameters.csv", header=TRUE, as.is=TRUE))
Thresholds<-tbl_df(read.csv("Thresholds.csv", header=TRUE, as.is =TRUE))


################ Shiny Server

shinyServer(function(input,output,session){

  Parks<-unique(Thresholds$ParkName)

##############Make UI controls###############################
    
####### Park control
  output$parkControl<-renderUI({
    selectizeInput(inputId="ParkIn",label="Park:", choices=Parks, 
      options = list(placeholder='Choose a Park', onInitialize = I('function() { this.setValue(""); }'))
    )
  })     
  
############# Stream Control
  output$streamControl<-renderUI({
    validate(
      need(input$ParkIn != "", message=FALSE)
    ) 
    selectizeInput(inputId = "Stream", label="Stream:", choices=c(Thresholds %>% filter(ParkName==input$ParkIn, Use==1))$DisplayName,
      options = list(placeholder='Choose a Stream' , onInitialize = I('function() { this.setValue(""); }'))
    )
  })
  
############ Parameter control
  
  output$ParameterControl<-renderUI({
    validate(
        need (input$ParkIn !="", message=FALSE),
        need (input$Stream !="", message=FALSE)
    )
    selectizeInput(inputId="Parameter", label="Water Parameter:", choices=Parameters$Display,
      options = list(placeholder='Choose a Parameter' , onInitialize = I('function() { this.setValue(""); }'))
    )
  })
  
########## Year control  - NOTE - this control needs DataUse() below to be populated before it is created.
  
  output$yearControl<-renderUI({
    validate(
      need(input$ParkIn!="", message = FALSE),
      need(input$Stream!="", message = FALSE),
      need(input$Parameter!="", message=FALSE)
    )

    sliderInput(inputId="YearsShow", label= "Years to Display:", min=min(DataUse()$Year, na.rm=T),  max=max(DataUse()$Year, na.rm=T),
      value=c( min(DataUse()$Year,na.rm=T), max=max(DataUse()$Year,na.rm=T) ), format="####"
    )
  })
  
#####housekeeping of data

  DataUse<-reactive({ data.frame(
  #  WaterData %>% filter (StationName== filter(Thresholds, DisplayName == input$Stream)$StationName &  ###dplyr and season are not friends
  #                       Parameter==filter(Parameters, Display == input$Parameter)$Parameter)
    WaterData[WaterData$StationName==Thresholds[Thresholds$DisplayName==input$Stream,]$StationName &
               WaterData$Parameter==Parameters[Parameters$Display==input$Parameter,]$Parameter,]
  )})
  DataGraph<-reactive({DataUse()[DataUse()$Year>=input$YearsShow[1] & DataUse()$Year<=input$YearsShow[2],]})
  ThreshUse<-reactive({Thresholds[Thresholds$DisplayName==input$Stream,]})
  TrendsOut<-reactive({WaterCosinor(DataIn=DataUse(), DateVar="VisitDate", Measure="Result", Formula=Result~VisitDate)})
  DataMissing<-reactive({!is.na(DataUse()$Result)})
  DataOut<-reactive({setNames( data.frame(as.character(DataUse()$VisitDate),DataUse()$Result), c("Date",input$Parameter))})

 
#### Get Colors from user inputs
 BadCol<-reactive({GraphColors[GraphColors$DisplayColor==input$BadColor,]$Rcolor})
 GoodCol<-reactive({GraphColors[GraphColors$DisplayColor==input$GoodColor,]$Rcolor})
 OutCol<-reactive({GraphColors[GraphColors$DisplayColor==input$OutColor,]$Rcolor})
 ThCol<-reactive({GraphColors[GraphColors$DisplayColor==input$ThColor,]$Rcolor})
 TrCol<-reactive({GraphColors[GraphColors$DisplayColor==input$TrColor,]$Rcolor})
      
##### PlotElements determines what is being plotted and is used in the key= argument of lattice 
  PlotElements=reactive({                         
    c(input$ThreshPoint, input$ThreshLine, input$Trends, input$Outliers)
  })

 OutPlot<-reactive({

   validate(
     need(input$ParkIn !="", message="Choose a Park"),
     need(input$Stream !="", message="Choose a Stream"),
     need(input$Parameter !="", message="Choose a Water Quality Parameter"),
     need(input$YearsShow !="", message="Choose the Years to Display")
    )
    
    xyplot(Result~VisitDate, data=DataGraph(), 
      cex=input$PointSize, 
      pch=16,col=GoodCol(),
      ylim=c(min(Parameters[Parameters$Display==input$Parameter,]$Ymin, min(0.98*DataGraph()$Result, na.rm=TRUE)), 
        max(Parameters[Parameters$Display==input$Parameter,]$Ymax,1.03*DataGraph()$Result,na.rm=T)),
      main=list(paste(input$Stream,input$Parameter,sep=": "),cex=input$FontSize),
      ylab=list(label=Parameters[Parameters$Display==input$Parameter,]$Units,cex=input$FontSize),
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
       
 #######Threshold lines 
        if(input$ThreshLine==TRUE) {
          switch(Parameters[Parameters$Display==input$Parameter,]$Parameter,
            ANC =                     panel.abline(h=ThreshUse()$ANCt, col=ThCol(),lwd=input$LineWidth),
            "DO (mg/L)" =             panel.abline(h=ThreshUse()$DOmgt, col=ThCol(), lwd=input$LineWidth),
            pH={                      panel.abline(h=ThreshUse()$pHmint, col=ThCol(), lwd=input$LineWidth)
                                    panel.abline(h=ThreshUse()$pHmaxt, col=ThCol(), lwd=input$LineWidth)
            },
            "Specific conductance"=   panel.abline(h=ThreshUse()$SCt, col=ThCol(), lwd=input$LineWidth),
            "Water Temperature" =     panel.abline(h=ThreshUse()$Tempt,col=ThCol(), lwd=input$LineWidth),
            "Nitrate 2007" =          panel.abline(h=ThreshUse()$Nt,col=ThCol(), lwd=input$LineWidth),
            "Total Phosphorus 2009" = panel.abline(h=ThreshUse()$Pt,col=ThCol(), lwd=input$LineWidth)
          )
        }
   
 #######Threshold Points
        if(input$ThreshPoint==TRUE) {
          switch(Parameters[Parameters$Display==input$Parameter,]$Parameter,
            ANC=  panel.points(x=DataUse()[DataUse()$Result<ThreshUse()$ANCt,]$VisitDate, y=DataUse()[DataUse()$Result<ThreshUse()$ANCt,]$Result,
                                   col=BadCol(),pch=16, cex=input$PointSize),
            "DO (mg/L)" = panel.points(x=DataUse()[DataUse()$Result<ThreshUse()$DOmgt,]$VisitDate,
                  y=DataUse()[DataUse()$Result<ThreshUse()$DOmgt,]$Result, col=BadCol(),pch=16, cex=input$PointSize),
            pH=   panel.points(x=DataUse()[DataUse()$Result<ThreshUse()$pHmint | DataUse()$Result>ThreshUse()$pHmaxt,]$VisitDate, 
                    y=DataUse()[DataUse()$Result<ThreshUse()$pHmint | DataUse()$Result>ThreshUse()$pHmaxt,]$Result,
                    col=BadCol(),pch=16, cex=input$PointSize),
            "Specific conductance" = panel.points(x=DataUse()[DataUse()$Result>ThreshUse()$SCt,]$VisitDate, 
                                    y=DataUse()[DataUse()$Result>ThreshUse()$SCt,]$Result, col=BadCol(),pch=16, cex=input$PointSize),
            "Water Temperature" = panel.points(x=DataUse()[DataUse()$Result>ThreshUse()$Tempt,]$VisitDate, 
                                               y=DataUse()[DataUse()$Result>ThreshUse()$Tempt,]$Result, col=BadCol(),pch=16, cex=input$PointSize),
            "Nitrate 2007" = panel.points(x=DataUse()[DataUse()$Result>ThreshUse()$Nt,]$VisitDate,
                                          y=DataUse()[DataUse()$Result>ThreshUse()$Nt,]$Result, col=BadCol(),pch=16, cex=input$PointSize),
            "Total Phosphorus 2009" =  panel.points(x=DataUse()[DataUse()$Result>ThreshUse()$Pt,]$VisitDate, 
                                                    y=DataUse()[DataUse()$Result>ThreshUse()$Pt,]$Result, col=BadCol(),pch=16, cex=input$PointSize)
            
          )
        }

############# Trend lines   
    if(input$Trends==TRUE){
      if(class(TrendsOut()$Analysis)=="lm"){
        panel.lines(TrendsOut()$Analysis$fitted.values~TrendsOut()$CDate, col=TrCol(), lwd=input$LineWidth)
      }
      if(class(TrendsOut()$Analysis)=="Cosinor"){
        panel.lines(TrendsOut()$PredLine$Preds~TrendsOut()$PredLine$PreDates.VisitDate, col=TrCol(), lwd=input$LineWidth)
      }
    }     
    
########### Outlier Points
  if(input$Outliers==TRUE){
      if(nrow(TrendsOut()$Outliers) > 0){
        panel.points(x=TrendsOut()$Outliers$VisitDate, y=TrendsOut()$Outliers$Result, col=OutCol(), pch=1, cex=1.5+input$PointSize,lwd=2)
      }
  }  

     
} #ends panel funciton
)# ends xyplot
})# end reactive

##################################### Plot Output
 
output$WaterPlot<-renderPlot({
  print(OutPlot())
}) 

################################ Summaries of Seaonality and Trends


output$SeasonOut<-renderText({
    validate(
      need (input$Trends==TRUE, message=FALSE),
      need(is.atomic(TrendsOut())==FALSE, message=FALSE)
    )
  switch(class(TrendsOut()$Analysis),
    "lm" =      c("There is no seasonal pattern in the data."),
    "Cosinor" = c("There is a seasonal pattern in the data. The peak is", strsplit(summary(TrendsOut()$Analysis)$phase," ")[[1]][3],
                  strsplit(summary(TrendsOut()$Analysis)$phase," ")[[1]][7], "and the low point is ",
        strsplit(summary(TrendsOut()$Analysis)$lphase," ")[[1]][3],paste0(strsplit(summary(TrendsOut()$Analysis)$lphase," ")[[1]][7] ,".")),
    NULL)
})

output$TrendsOut<-renderText({
  validate(
    need (input$Trends==TRUE, message=FALSE),
    need(is.atomic(TrendsOut())==FALSE, message=FALSE)
  )
    switch(class(TrendsOut()$Analysis),
      "lm" =  {
        if(summary(TrendsOut()$Analysis)$coefficients[2,4]>.05) {("There is no significant trend in the data.")} 
        else {
          c("There is a significant", 
          ifelse (summary(TrendsOut()$Analysis)$coefficients[2,1] > 0, "increasing", "decreasing"),
            "trend of",c(signif(summary(TrendsOut()$Analysis)$coefficients[2,1]*365.24, digits=3)),
            Parameters[Parameters$Display==input$Parameter,]$Units, "per year.")
        }
    }, 
    "Cosinor"=  {
      if(summary(TrendsOut()$Analysis$glm)$coefficients[2,4]>.05){("There is no significant trend in the data")}
      else {
        c("There is a significant",
        ifelse (summary(TrendsOut()$Analysis$glm)$coefficients[2,1]>0,"increasing","decreasing"), 
        "trend of",c(signif(summary(TrendsOut()$Analysis$glm)$coefficients[2,1]*365.24,digits=3)),
        Parameters[Parameters$Display==input$Parameter,]$Units, "per year.")
      }
    },
  NULL) 
})

###################### Threshold Summary


output$ThresholdSummary<-renderText({
  if(input$ThreshLine==TRUE ){
    c(Parameters[Parameters$Display==input$Parameter,]$ThreshText)
  }
})
    
output$ThresholdType<-renderText({  
    if(input$ThreshLine==TRUE  ){ 
      switch(Parameters[Parameters$Display==input$Parameter,"Parameter"], #$Parameter,
        "ANC" =                   c(input$Stream,"is in",ThreshUse()$Karst,"terrain."),
        "DO (mg/L)" =             c(input$Stream,"is a",ThreshUse()$Water,"water stream."),
        "Water Temperature" =     c(input$Stream,"is a",ThreshUse()$Water,"water stream."),
        "Nitrate 2007" =          c(input$Stream,"is in nutrient ecoregion ",paste0(ThreshUse()$Ecoregion,".")),
        "Total Phosphorus 2009" = c(input$Stream,"is in nutrient ecoregion ",paste0(ThreshUse()$Ecoregion,".")),
     NULL)
  
    }
})
      
output$RefSummary<-renderText({
  if(input$ThreshLine==TRUE  ) {
   c(Parameters[Parameters$Display==input$Parameter,]$Refs)
  }
})      


 
###########Raw data table   ##################################
 output$WaterTable <-renderDataTable({
   validate(
     need(input$ParkIn !="", message="Choose a Park"),
     need(input$Stream !="", message="Choose a Stream"),
     need(input$Parameter !="", message="Choose a Water Quality Parameter"),
     need(input$YearsShow !="", message="Choose the Years to Display")
   )   
   DataOut()[DataMissing(),]
})

#############Data download   #########################
 output$Data.Download<-downloadHandler(
  filename=function(){paste(input$Stream, "_", input$Parameter, ".csv", sep="")},
  content=function(file){ 
    write.csv(DataOut(),file)
    }
 )   

########Plot downloads   ############################

output$Plot.PNG<-downloadHandler(
  filename=function(){paste(input$Stream, "_", input$Parameter, ".png", sep="")}, 
  content=function (file){
  png(file,width=960, height=480)
  print(OutPlot())
  dev.off()
  }
)

output$Plot.JPG<-downloadHandler(
  filename=function(){paste(input$Stream, "_", input$Parameter, ".jpeg", sep="")}, 
  content=function (file){
  jpeg(file,width=960, height=480,quality=100)
  print(OutPlot())
  dev.off()
  }
)


}) #End of Shiny Server function
    