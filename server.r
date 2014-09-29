library(shiny)
library(lattice)

setwd("L:/WaterShiny")  #remove this line before uploading to server
source("Water_Cosinor.r")

setClass("NPSDate")
setAs("character","NPSDate", function(from) as.Date(from, format="%m/%d/%Y") )  #explains to read.csv the date format from NPStoret


################Actual shiny stuff



shinyServer(function(input,output,session){
  
  
  
  W.Data<-reactiveFileReader(10000,session,"Water Data.csv",read.csv,  header=TRUE,col.names=c("Network","StationID","Station.Name","Visit.Date","Parameter","Result"),
      colClasses=c("factor","character","character","NPSDate","character","numeric" ),comment.char="")
  Param.info<-reactiveFileReader(10000,session,"Parameters.csv",read.csv, header=TRUE, as.is=TRUE)
  Thresh.in<-reactiveFileReader(10000,session,"Thresholds.csv", read.csv, header=TRUE, as.is=TRUE)


#####housekeeping of data
  
  Data.Use<-reactive({ W.Data()[W.Data()$Station.Name==Thresh.in()[Thresh.in()$Display.Name==input$Stream,]$Station.Name & W.Data()$Parameter==Param.info()[Param.info()$Display==input$Param,]$Parameter,] }) 
  Data.Years<-reactive({   as.numeric(format(Data.Use()$Visit.Date, format="%Y"))    })
 
  Thresh.Use<-reactive({Thresh.in()[Thresh.in()$Display.Name==input$Stream,]})
  Data.Missing<-reactive({!is.na(Data.Use()$Result)})
  Data.Out<-reactive({setNames( data.frame(as.character(Data.Use()$Visit.Date),Data.Use()$Result), c("Date",input$Param))})
  Trends.Out<-reactive({W.Cosinor(Df.In=Data.Use(), DateVar="Visit.Date", Measure="Result", Formula=Result~Visit.Date)})
  Parks<-reactive({unique(Thresh.in()$ParkName) })
  Data.Graph<-reactive({Data.Use()[Data.Years()>=input$YearsShow[1] & Data.Years()<=input$YearsShow[2],]})
  
##############Make UI controls


####### Park control
output$parkControl<-renderUI({
  selectInput("Park.in",label="Park:", c("Choose a Park",Parks()))
})     

############# Stream Control
output$streamControl<-renderUI({
  if( is.null(input$Park.in) || input$Park.in == "Choose a Park") {
    return()
  }  
  else {
    selectInput("Stream", "Stream:", c("Choose a Stream",unique(Thresh.in()$Display.Name[Thresh.in()$ParkName==input$Park.in & Thresh.in()$Use==1])))
  }
})

############ Parameter control

output$ParameterControl<-renderUI({
  if( is.null(input$Park.in) || input$Park.in=="Choose a Park" || is.null(input$Stream) || input$Stream =="Choose a Stream") {
    return()
  }
  else {
      selectInput("Param", "Water Parameter:", c("Choose Water Parameter", unique(Param.info()$Display)))
  }
})

########## Year contorol

output$yearControl<-renderUI({
  if(is.null(input$Park.in) ||input$Park.in=="Choose a Park" || is.null(input$Stream) || input$Stream =="Choose a Stream" || is.null(input$Param) || input$Param=="Choose Water Parameter") {
    return()
  }
 else {
   sliderInput("YearsShow", "Years to Display:", min=min(as.numeric(format(Data.Use()$Visit.Date, format="%Y")), na.rm=T), max=max(as.numeric(format(Data.Use()$Visit.Date, format="%Y")), na.rm=T),
    value=c(min(as.numeric(format(Data.Use()$Visit.Date, format="%Y")),na.rm=T),  max=max(as.numeric(format(Data.Use()$Visit.Date,format="%Y" )),na.rm=T)), format="####")
  }
})



############## ShowGraph switch

 ShowGraph<-reactive(!(is.null(input$Park.in) || input$Park.in=="Choose a Park" || is.null(input$Stream) || input$Stream =="Choose a Stream" ||        #if all the conditions are true thanthe whole thing is true
          is.null(input$Param) || input$Param=="Choose Water Parameter" ||is.null(input$YearsShow)))
 


 #####################           Make Plot #####################
 BadCol<-reactive({GraphColors[GraphColors$DisplayColor==input$BadColor,]$Rcolor})
 GoodCol<-reactive({GraphColors[GraphColors$DisplayColor==input$GoodColor,]$Rcolor})
 OutCol<-reactive({GraphColors[GraphColors$DisplayColor==input$OutColor,]$Rcolor})
 ThCol<-reactive({GraphColors[GraphColors$DisplayColor==input$ThColor,]$Rcolor})
 TrCol<-reactive({GraphColors[GraphColors$DisplayColor==input$TrColor,]$Rcolor})
      
 OutPlot<-reactive( 
 if(!ShowGraph()){
  return()
  }
   else{
 
        xyplot(Result~Visit.Date, data=Data.Graph(), cex=input$PointSize, pch=16,col=GoodCol(),
        ylim=c(min(Param.info()[Param.info()$Display==input$Param,]$Ymin,min(0.98*Data.Graph()$Result,na.rm=TRUE)), max(Param.info()[Param.info()$Display==input$Param,]$Ymax,1.03*Data.Graph()$Result,na.rm=T)),
        main=list(paste(input$Stream,input$Param,sep=": "),cex=input$FontSize),
        ylab=list(label=Param.info()[Param.info()$Display==input$Param,]$Units,cex=input$FontSize),
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
    
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="ANC"){
        panel.abline(h=Thresh.Use()$ANCt,col=ThCol(),lwd=input$LineWidth)
      }
    
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="DO (mg/L)"){
        panel.abline(h=Thresh.Use()$DOmgt,col=ThCol(), lwd=input$LineWidth)
      }
      
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="pH"){
        panel.abline(h=Thresh.Use()$pHmint, col=ThCol(), lwd=input$LineWidth)
        panel.abline(h=Thresh.Use()$pHmaxt,col=ThCol(), lwd=input$LineWidth)
      }
      
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="Specific conductance"){
        panel.abline(h=Thresh.Use()$SCt,col=ThCol(), lwd=input$LineWidth)
      }
      
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="Water Temperature"){
        panel.abline(h=Thresh.Use()$Tempt,col=ThCol(), lwd=input$LineWidth)
      }
      
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="Nitrate 2007"){
        panel.abline(h=Thresh.Use()$Nt,col=ThCol(), lwd=input$LineWidth)
      }
      
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="Total Phosphorus 2009"){
        panel.abline(h=Thresh.Use()$Pt,col=ThCol(), lwd=input$LineWidth)

      }
    }
   
 #######Threshold Points
     if(input$ThreshPoint==TRUE) {
      
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="ANC"){
        panel.points(x=Data.Use()[Data.Use()$Result<Thresh.Use()$ANCt,]$Visit.Date, y=Data.Use()[Data.Use()$Result<Thresh.Use()$ANCt,]$Result, col=BadCol(),pch=16, cex=input$PointSize)
      }
      
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="DO (mg/L)"){
        panel.points(x=Data.Use()[Data.Use()$Result<Thresh.Use()$DOmgt,]$Visit.Date, y=Data.Use()[Data.Use()$Result<Thresh.Use()$DOmgt,]$Result, col=BadCol(),pch=16, cex=input$PointSize)
      }
      
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="pH"){
        panel.points(x=Data.Use()[Data.Use()$Result<Thresh.Use()$pHmint | Data.Use()$Result>Thresh.Use()$pHmaxt,]$Visit.Date, y=Data.Use()[Data.Use()$Result<Thresh.Use()$pHmint | Data.Use()$Result>Thresh.Use()$pHmaxt,]$Result,
          col=BadCol(),pch=16, cex=input$PointSize)
      }
      
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="Specific conductance"){
        panel.points(x=Data.Use()[Data.Use()$Result>Thresh.Use()$SCt,]$Visit.Date, y=Data.Use()[Data.Use()$Result>Thresh.Use()$SCt,]$Result, col=BadCol(),pch=16, cex=input$PointSize)
      }
      
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="Water Temperature"){
          panel.points(x=Data.Use()[Data.Use()$Result>Thresh.Use()$Tempt,]$Visit.Date, y=Data.Use()[Data.Use()$Result>Thresh.Use()$Tempt,]$Result, col=BadCol(),pch=16, cex=input$PointSize)
      }
      
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="Nitrate 2007"){
        panel.points(x=Data.Use()[Data.Use()$Result>Thresh.Use()$Nt,]$Visit.Date, y=Data.Use()[Data.Use()$Result>Thresh.Use()$Nt,]$Result, col=BadCol(),pch=16, cex=input$PointSize)
      }
      
      if(Param.info()[Param.info()$Display==input$Param,]$Parameter=="Total Phosphorus 2009"){
        panel.points(x=Data.Use()[Data.Use()$Result>Thresh.Use()$Pt,]$Visit.Date, y=Data.Use()[Data.Use()$Result>Thresh.Use()$Pt,]$Result, col=BadCol(),pch=16, cex=input$PointSize)
      }
     }

############# Trend lines   
    if(input$Trends==TRUE){
      if(class(Trends.Out()$Analysis)=="lm"){
        panel.lines(Trends.Out()$Analysis$fitted.values~Trends.Out()$CDate, col=TrCol(), lwd=input$LineWidth)
      }
      if(class(Trends.Out()$Analysis)=="Cosinor"){
        panel.lines(Trends.Out()$PredLine$Preds~Trends.Out()$PredLine$PreDates.Visit.Date, col=TrCol(), lwd=input$LineWidth)
      }
    }     
    
########### Outlier Points
  if(input$Outliers==TRUE){
      if(nrow(Trends.Out()$Outliers) > 0){
        panel.points(x=Trends.Out()$Outliers$Visit.Date, y=Trends.Out()$Outliers$Result, col=OutCol(), pch=1, cex=1.5+input$PointSize,lwd=2)
      }
  }  

     
} #ends panel funciton
)# ends xyplot
} #ends else associted with YearsShow check at top of graph
)# end reactive

##################################### Plot Output
 
output$Water.Plot<-renderPlot({
  print(OutPlot())
}) 

################################ Summaries of Seaonality and Trends


output$SeasonOut<-renderText({
  if(input$Trends==FALSE | !ShowGraph() )   {invisible()}
  else{
  switch(class(Trends.Out()$Analysis),
    "lm" =      c("There is no seasonal pattern in the data."),
    "Cosinor" = c("There is a seasonal pattern in the data. The peak is", strsplit(summary(Trends.Out()$Analysis)$phase," ")[[1]][3] ,strsplit(summary(Trends.Out()$Analysis)$phase," ")[[1]][7], "and the low point is ",
        strsplit(summary(Trends.Out()$Analysis)$lphase," ")[[1]][3],paste0(strsplit(summary(Trends.Out()$Analysis)$lphase," ")[[1]][7] ,".")),
    NULL)
    }
})

output$TrendsOut<-renderText({
  if (input$Trends==FALSE | !ShowGraph()) {invisible()} else{
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
  if(input$ThreshLine==TRUE & ShowGraph()){
    c(Param.info()[Param.info()$Display==input$Param,]$ThreshText)
  }
})
    
output$ThresholdType<-renderText({  
    if(input$ThreshLine==TRUE & ShowGraph() ){ 
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
  if(input$ThreshLine==TRUE & ShowGraph()) {
   c(Param.info()[Param.info()$Display==input$Param,]$Refs)
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
    