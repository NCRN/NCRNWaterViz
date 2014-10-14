library(shiny)
library(lattice)
library(dplyr)
library(lubridate)

source("Water_Cosinor.r")

#setClass("NPSDate")
#setAs("character","NPSDate", function(from) as.Date(from, format="%m/%d/%Y") )  #explains to read.csv the date format from NPStoret

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

  
  W.Data<-reactiveFileReader(10000,session,"Water Data.csv",read.csv,  header=TRUE,
                             col.names=c("Network","StationID","Station.Name","Visit.Date","Parameter","Result"),
      colClasses=c("factor","charaTRcter","character","NPSDate","character","numeric" ),comment.char="")
  Param.info<-reactiveFileReader(10000,session,"Parameters.csv",read.csv, header=TRUE, as.is=TRUE)
  Thresh.in<-reactiveFileReader(10000,session,"Thresholds.csv", read.csv, header=TRUE, as.is=TRUE)

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
  
## Old
  Data.Use<-reactive({
    W.Data()[W.Data()$Station.Name==Thresh.in()[Thresh.in()$Display.Name==input$Stream,]$Station.Name &
                                  W.Data()$Parameter==Param.info()[Param.info()$Display==input$Param,]$Parameter,]}) 
  Data.Years<-reactive({   as.numeric(format(Data.Use()$Visit.Date, format="%Y"))    })
  Data.Graph<-reactive({Data.Use()[Data.Years()>=input$YearsShow[1] & Data.Years()<=input$YearsShow[2],]})
  Thresh.Use<-reactive({Thresh.in()[Thresh.in()$Display.Name==input$Stream,]})
#  Trends.Out<-reactive({W.Cosinor(Df.In=Data.Use(), DateVar="Visit.Date", Measure="Result", Formula=Result~Visit.Date)})

  Data.Missing<-reactive({!is.na(Data.Use()$Result)})
  Data.Out<-reactive({setNames( data.frame(as.character(Data.Use()$Visit.Date),Data.Use()$Result), c("Date",input$Param))})
 

  
### New
  DataUse<-reactive({ data.frame(
  #  WaterData %>% filter (StationName== filter(Thresholds, DisplayName == input$Stream)$StationName &
  #                       Parameter==filter(Parameters, Display == input$Parameter)$Parameter)
    WaterData[WaterData$StationName==Thresholds[Thresholds$DisplayName==input$Stream,]$StationName &
               WaterData$Parameter==Parameters[Parameters$Display==input$Parameter,]$Parameter,]
  )})
  DataGraph<-reactive({DataUse()[DataUse()$Year>=input$YearsShow[1] & DataUse()$Year<=input$YearsShow[2],]})
  ThreshUse<-reactive({Thresholds[Thresholds$DisplayName==input$Stream,]})
  TrendsOut<-reactive({WaterCosinor(DataIn=DataUse(), DateVar="VisitDate", Measure="Result", Formula=Result~VisitDate)})
############## ShowGraph switch

 #ShowGraph<-reactive(!(is.null(input$ParkIn) || is.null(input$Stream)  ||        
#          is.null(input$Parameter) ||is.null(input$YearsShow) ) ) #if all the conditions are true than the whole thing is true




 #####################           Make Plot #####################
 BadCol<-reactive({GraphColors[GraphColors$DisplayColor==input$BadColor,]$Rcolor})
 GoodCol<-reactive({GraphColors[GraphColors$DisplayColor==input$GoodColor,]$Rcolor})
 OutCol<-reactive({GraphColors[GraphColors$DisplayColor==input$OutColor,]$Rcolor})
 ThCol<-reactive({GraphColors[GraphColors$DisplayColor==input$ThColor,]$Rcolor})
 TrCol<-reactive({GraphColors[GraphColors$DisplayColor==input$TrColor,]$Rcolor})
      
 OutPlot<-reactive({

   validate(
     need(input$ParkIn !="", message="Choose a Park"),
     need(input$Stream !="", message="Choose a Stream"),
     need(input$Parameter !="", message="Choose a Water Quality Parameter"),
     need(input$YearsShow !="", message="Choose the Years to Display")
    )
    
    xyplot(Result~VisitDate, data=DataGraph(), cex=input$PointSize, pch=16,col=GoodCol(),
        ylim=c(min(Parameters[Parameters$Display==input$Parameter,]$Ymin, min(0.98*DataGraph()$Result, na.rm=TRUE)), 
            max(Parameters[Parameters$Display==input$Parameter,]$Ymax,1.03*DataGraph()$Result,na.rm=T)),
        main=list(paste(input$Stream,input$Parameter,sep=": "),cex=input$FontSize),
        ylab=list(label=Parameters[Parameters$Display==input$Parameter,]$Units,cex=input$FontSize),
        xlab=list(label="Sample Date",cex=input$FontSize),
        scales=list(cex=input$FontSize, alternating=1, tck=c(1,0)),
        key=if(input$Legend==TRUE) {
          key=list(border=FALSE, cex=input$PointSize, space="top",columns=3,                           
          lines=list(
                pch=c(16,
                    if(input$ThreshPoint==T){16},
                    if(input$ThreshLine==T){16},
                    if(input$Trends==T){16},
                    if(input$Outliers==T){1}
                ),
                type=c("p",
                    if(input$ThreshPoint==T){"p"},
                    if(input$ThreshLine==T){"l"},
                    if(input$Trends==T){"l"},
                    if(input$Outliers==T){"p"}
                ),
                col=c(GoodCol(),
                  if(input$ThreshPoint==T){BadCol()},
                  if(input$ThreshLine==T){ThCol()},
                  if(input$Trends==T) {TrCol()},
                  if(input$Outliers==T) {OutCol()}
                ),
                 lwd=c(input$LineWidth,
                  if(input$ThreshPoint==T){input$LineWidth},
                  if(input$ThreshLine==T){input$LineWidth},
                  if(input$Trends==T) {input$LineWidth},
                  if(input$Outliers==T) {input$LineWidth}
                )
          ),
          text=list(c("Measurement",
            if (input$ThreshPoint==T){"Fails Threshold"},
            if (input$ThreshLine==T){"Threshold"},
            if(input$Trends==T){"Trend Line"},
            if(input$Outliers==T){"Outliers"}
            )
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
 
output$Water.Plot<-renderPlot({
  print(OutPlot())
}) 

################################ Summaries of Seaonality and Trends


output$SeasonOut<-renderText({
  if(input$Trends==FALSE )   {invisible()}
  else{
  switch(class(Trends.Out()$Analysis),
    "lm" =      c("There is no seasonal pattern in the data."),
    "Cosinor" = c("There is a seasonal pattern in the data. The peak is", strsplit(summary(Trends.Out()$Analysis)$phase," ")[[1]][3] ,strsplit(summary(Trends.Out()$Analysis)$phase," ")[[1]][7], "and the low point is ",
        strsplit(summary(Trends.Out()$Analysis)$lphase," ")[[1]][3],paste0(strsplit(summary(Trends.Out()$Analysis)$lphase," ")[[1]][7] ,".")),
    NULL)
    }
})

output$TrendsOut<-renderText({
  if (input$Trends==FALSE  ) {invisible()} else{
  switch(class(Trends.Out()$Analysis),
  "lm" =  {if(summary(Trends.Out()$Analysis)$coefficients[2,4]>.05) {("There is no significant trend in the data.")} 
          else {
            c("There is a significant", 
            ifelse (summary(Trends.Out()$Analysis)$coefficients[2,1] > 0, "increasing", "decreasing"),
            "trend of",c(signif(summary(Trends.Out()$Analysis)$coefficients[2,1]*365.24, digits=3)),Param.info()[Param.info()$Display==input$Param,]$Units, "per year.")
          }}, 
  "Cosinor"=  {if(summary(Trends.Out()$Analysis$glm)$coefficients[2,4]>.05){("There is no significant trend in the data")}
              else {
                c("There is a significant",
                ifelse (summary(Trends.Out()$Analysis$glm)$coefficients[2,1]>0,"increasing","decreasing"), 
                "trend of",c(signif(summary(Trends.Out()$Analysis$glm)$coefficients[2,1]*365.24,digits=3)),Param.info()[Param.info()$Display==input$Param,]$Units, "per year.")
              }},
  NULL) 
  }
})

###################### Threshold Summary


output$ThresholdSummary<-renderText({
  if(input$ThreshLine==TRUE ){
    c(Parameters[Parameters$Display==input$Param,]$ThreshText)
  }
})
    
output$ThresholdType<-renderText({  
    if(input$ThreshLine==TRUE  ){ 
      switch(Param.info()[Param.info()$Display==input$Param,]$Parameter,
        "ANC" =                   c(input$Stream,"is in",Thresh.Use()$Karst,"terrain."),
        "DO (mg/L)" =             c(input$Stream,"is a",Thresh.Use()$Water,"water stream."),
        "Water Temperature" =     c(input$Stream,"is a",Thresh.Use()$Water,"water stream."),
        "Nitrate 2007" =          c(input$Stream,"is in nutrient ecoregion ",paste0(Thresh.Use()$Ecoregion,".")),
        "Total Phosphorus 2009" = c(input$Stream,"is in nutrient ecoregion ",paste0(Thresh.Use()$Ecoregion,".")),
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
  if(!ShowGraph()){
   return()
  }
  else{
   Data.Out()[Data.Missing(),]
  }    
})

#############Data download   #########################
 output$Data.Download<-downloadHandler(
  filename=function(){paste(input$Stream, "_", input$Param, ".csv", sep="")},
  content=function(file){ 
    write.csv(Data.Out(),file)
    }
 )   

########Plot downloads   ############################

output$Plot.PNG<-downloadHandler(
  filename=function(){paste(input$Stream, "_", input$Param, ".png", sep="")}, 
  content=function (file){
  png(file,width=960, height=480)
  print(OutPlot())
  dev.off()
  }
)

output$Plot.JPG<-downloadHandler(
  filename=function(){paste(input$Stream, "_", input$Param, ".jpeg", sep="")}, 
  content=function (file){
  jpeg(file,width=960, height=480,quality=100)
  print(OutPlot())
  dev.off()
  }
)


}) #End of Shiny Server function
    