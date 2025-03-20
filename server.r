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

#### Get data ####
WaterData<-suppressWarnings(importNCRNWater(paste0("./Data/", Network), Data=dataname, MetaData = metadataname, wqx=wqx_bool))

####getThresholdText Function
getTresholdText<-function(object, parkcode,sitecode,charname){    
 x<-c(getCharInfo(object, parkcode=parkcode, sitecode=sitecode, charname=charname, info="LowerDescription"),
    getCharInfo(object, parkcode=parkcode, sitecode=sitecode, charname=charname, info="UpperDescription"))
  return(x[!is.na(x)])
}

##### Shiny Server ####

shinyServer(function(input,output,session){


#output$Test<-renderText(exists("TrendsOut()$Analysis"))   #For debugging purposes

#### Reactive Values for Graphics Options with Defaults ####
  
GraphOpts<-reactiveValues(Legend=TRUE, FontSize=1.5, GoodColor="Blue", BadColor="Orange",OutColor="Vermillion",PointSize=3,
                            ThColor="Orange", TrColor="Green", LineWidth=1)
 
#### Reactive Values for Choosing Data ####

DataOpts<-reactiveValues(Park=NA, Site=NA, Param=NA, Years=NA, USGSload=FALSE, USGSdata=NA)

#### UI Controls ####  

#### Time Series Controls ####
TimePark<-callModule(parkChooser, id="TimePark", data=WaterData, chosen=reactive(DataOpts$Park))
TimeSite<-callModule(siteChooser, id="TimeSite", data=WaterData, park=reactive(DataOpts$Park), 
                     chosen=reactive(DataOpts$Site))
TimeParam<-callModule(paramChooser, id="TimeParam",data=WaterData, park=reactive(DataOpts$Park), 
                      site=reactive(DataOpts$Site), chosen=reactive(DataOpts$Param))
TimeYears<-callModule(yearChooser, id="TimeYears", data=DataUse, chosen=reactive(DataOpts$Years) )


observeEvent(TimePark(), {DataOpts$Park<-TimePark(); DataOpts$Site<-NA; DataOpts$Param<-NA; DataOpts$Years<-c(1900,2100)} )
observeEvent(TimeSite(), {DataOpts$Site<-TimeSite(); DataOpts$Param<-NA; DataOpts$Years<-c(1900,2100)} )
observeEvent(TimeParam(), {DataOpts$Param<-TimeParam(); DataOpts$Years<-c(1900,2100) })
observeEvent(TimeYears(), DataOpts$Years<-TimeYears() )

#### Graphics Modal Control ####
  
  observeEvent(eventExpr = c( input$GraphicsModal,input$GraphicsModal2), ignoreInit = TRUE,
    showModal(modalDialog(title="Graphics Options", footer=tagAppendAttributes( modalButton(tags$div("Close")), class="btn btn-primary"),
      column(12,hr()),
      column(12,h4("General:"),
        column(3,checkboxInput("Legend","Show Legend",GraphOpts$Legend)),
        column(3,sliderInput("FontSize", "Font Size", min=1, max=2.5,value=GraphOpts$FontSize, step=.25, width='130px'))
      
      ),
      column(12,hr()),
      column(12, h4("Points:"),
        column(3,selectInput("GoodColor","Measurement Color:",choices=GraphColors$DisplayColor, 
                           selected=GraphOpts$GoodColor, width='130px')
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
  
#### Housekeeping of data ####
  DataUse<-reactive({ 
     shiny::validate(
       need(DataOpts$Park, message="Choose a Park"),
       need(DataOpts$Site, message="Choose a Site"),
       need(DataOpts$Param, message="Choose a Water Quality Parameter")
     )  
    df1 <- getWData(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param)
    df <- suppressWarnings(df1 %>% mutate(year.dec = julian(Date)/365, month = as.factor(months(Date))) %>% 
                              group_by(month) %>% mutate(num_meas = sum(!is.na(Value))) %>% 
                              ungroup()) %>% mutate(num_mos = length(unique(month)))
    
    return(df)
    
    })
  
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

#### Summaries of Seaonality and Trends ####
  output$SeasonOut<-renderText({

    req(input$Trends, isTruthy(TrendsOut()), 
        TrendType())
    if(TrendType() == 'wcosinor') {
    switch(class(TrendsOut()$Analysis),
      "lm" =      c("There is no seasonal pattern in the data."),
      "Cosinor" = c("There is a seasonal pattern in the data. The peak is", strsplit(summary(TrendsOut()$Analysis)$phase," ")[[1]][3],
                  strsplit(summary(TrendsOut()$Analysis)$phase," ")[[1]][7], "and the low point is ",
        strsplit(summary(TrendsOut()$Analysis)$lphase," ")[[1]][3],paste0(strsplit(summary(TrendsOut()$Analysis)$lphase," ")[[1]][7] ,"." 
      )), NULL)
      
    }
  })

  SeriesTrendsOut<-reactive({
    req(input$Trends, isTruthy(TrendsOut()), isTruthy(TrendType()))

    outmessage <- 
      paste(h4("Trend Analysis:"),"\n",
        if(TrendType() == "wcosinor" && !is.na(TrendsOut()$Analysis)){
        paste(switch(class(TrendsOut()$Analysis),
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
            paste("There is a significant",
            ifelse (summary(TrendsOut()$Analysis$glm)$coefficients[2,1]>0,"increasing","decreasing"), 
            "trend of",c(signif(summary(TrendsOut()$Analysis$glm)$coefficients[2,1]*365.24,digits=3)),
            Units(), "per year."
            )
          }
          }, NULL))
        } else if(TrendType() %in% c("nonparCens", "nonpar")){
          
          paste(
          if(TrendType() == "nonparCens" && !all(TrendsOut()$modeled == FALSE)){ 
            "Data were separated by month for censored Mann-Kendall test. 
            Solid lines are significant trends. Dashed lines are non-significant trends."
          } else if(TrendType() == "nonpar" && !all(TrendsOut()$modeled == FALSE)){
            "Data were separated by month for Mann-Kendall test. 
            Solid lines are significant trends. Dashed lines are non-significant trends."},
          
          if(any(TrendsOut()$message == "no trend")){
           paste(br(), "The following months were modeled and found no significant trends: ",
                 paste0(TrendsOut()$month[TrendsOut()$message=="no trend"], collapse=", "), ". ", sep = "")},

          if(all(TrendsOut()$modeled == FALSE)){
            paste(br(), "There were too few non-censored measurements to analyze for trends.")},
          
          if(any(TrendsOut()$modeled == FALSE) && any(!is.na(TrendsOut()$pval))){
            paste(br(), "The following months had too few non-censored measurements to analyze for trends and were not plotted: ",
                  paste0(TrendsOut()$month[TrendsOut()$modeled == FALSE], collapse=", "), ".", sep = "")},
          
          
          if(any(grepl("There", TrendsOut()$message))){
           paste(TrendsOut()$message[TrendsOut()$modeled==TRUE & grepl("There", TrendsOut()$message)], sep="")
          }
          ) #end of nonparCen/nonpar paste

        } else if(TrendType() == 'notrends' & TrendsOut() == 'notrends'){
          paste("There were too few non-censored measurements to plot and analyze for trends.")}
          ) 
  return(outmessage)
  })
  
  output$SeriesTrendsOut<-renderUI(HTML(SeriesTrendsOut()))
  
#### Threshold Summary ####
  ThresholdSummary<-reactive({    
    req(input$SeriesThreshLine | input$ThreshPoint)
    paste(h4("Threshold:"),"\n", 
          if(all(is.na(Thresholds()))){ "There is no water quality threshold for this parameter." } else {
      c(getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="LowerDescription"),
      getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, 
                  info="UpperDescription"))[!is.na(Thresholds())] }
    ) 
  })
  
  output$SeriesThresholdSummary<-renderUI( HTML(ThresholdSummary()) )
  
  
  RefSummary<-reactive({
    req(input$SeriesThreshLine | input$ThreshPoint) 
    paste(h4("Threshold Reference:"),"\n",
      if (all(is.na(Thresholds()))) {"None"} else {
      getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="AssessmentDetails")}
    ) 
  })      
  
  output$SeriesRefSummary<-renderUI(HTML(RefSummary()))
  
  
#### Time Series Plot ####
    WaterSeriesOut<-reactive({
      req(DataUse()$Date, TrendType(), DataUse()$Value)
      
      SeriesPlot<- if(TrendType() == "notrends"){  
        cens <- ifelse(any(DataUse()$Censored==TRUE), TRUE, FALSE)
        waterseries(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, char=DataOpts$Param,
                    censored=cens,
                    years=DataOpts$Years[1]:DataOpts$Years[2],layers=c("points"),
                    assessment=input$SeriesThreshLine, title=Title(),
                    colors=(GoodCol()),assesscolor=ThCol(), 
                    sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth),
                    legend=if(GraphOpts$Legend) "bottom" else "none") +
                    theme(text=element_text(size=GraphOpts$FontSize*10))+
          
          {if(input$ThreshPoint && !is.na(Thresholds()[1])) geom_point(data=DataUse()[DataUse()$Value<Thresholds()[1],], 
                                                                       aes(Date,Value), pch=16,size=GraphOpts$PointSize, color=BadCol()) } +
          
          {if(input$ThreshPoint && !is.na(Thresholds()[2])) geom_point(data=DataUse()[DataUse()$Value>Thresholds()[2],], 
                                                                       aes(Date,Value), pch=16, size=GraphOpts$PointSize, color=BadCol())} 
          
        } else if(TrendType() == "wcosinor"){  
         
        waterseries(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, char=DataOpts$Param, 
                    #years=DataOpts$Years[1]:DataOpts$Years[2],
                    layers=c("points"),
                    assessment=input$SeriesThreshLine, title=Title(),
                    colors=(GoodCol()),assesscolor=ThCol(), 
                    sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth),
                    legend=if(GraphOpts$Legend) "bottom" else "none") +
                    theme(text=element_text(size=GraphOpts$FontSize*10))+
        
            {if(input$Outliers && exists("TrendsOut")) geom_point(data=TrendsOut()[["Outliers"]], aes(Date,Value),pch=1,
                  size=GraphOpts$PointSize+2,color=OutCol(),stroke=1.5)} +
        
            {if(input$ThreshPoint && !is.na(Thresholds()[1])) geom_point(data=DataUse()[DataUse()$Value<Thresholds()[1],], 
                   aes(Date,Value), pch=16,size=GraphOpts$PointSize, color=BadCol()) } +
        
            {if(input$ThreshPoint && !is.na(Thresholds()[2])) geom_point(data=DataUse()[DataUse()$Value>Thresholds()[2],], 
                  aes(Date,Value), pch=16, size=GraphOpts$PointSize, color=BadCol())} +
      
            {if(input$Trends && exists("TrendsOut") && class(TrendsOut()$Analysis)=="lm") geom_line(data=data.frame(
            Value=TrendsOut()$Analysis$fitted.values,Date=TrendsOut()$CDates), aes(Date,Value), color=TrCol(), 
            lwd=GraphOpts$LineWidth) } +
        
            {if(input$Trends && exists("TrendsOut") && class(TrendsOut()$Analysis)=="Cosinor") geom_line(data=data.frame(
            Value=TrendsOut()$PredLine$Preds,Date=TrendsOut()$PredLine$PreDates.Date),  aes(Date,Value), col=TrCol(), 
            lwd=GraphOpts$LineWidth)}
        
        } else if(TrendType() == "nonparCens"){ 
          
            df <- DataUse() %>% group_by(Category, Characteristic, Site, Park, month) %>% 
              mutate(num_meas=sum(!is.na(ValueCen)), 
                     pct_true= sum(ifelse(Censored==FALSE, 1, 0))/num_meas,
                     adjValueCen = ifelse(Censored==TRUE, max(ValueCen), Value)) %>% 
              filter(num_meas >=6) %>% 
              ungroup() %>% droplevels()
            
            df2 <- merge(df, TrendsOut()[,c('month','intercept','slope', 'message')], 
                         by = 'month', all.x = T) %>%
              mutate(pred_y = intercept + slope * year.dec,
                     sign=as.factor(ifelse(message=="no trend", 0, 1)),
                     month = as.factor(lubridate::month(Date, label = TRUE, abbr = FALSE))) %>% 
              arrange(month) 
            
            ylabel = paste0(getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site,
                                               charname = DataOpts$Param, info = "CategoryDisplay"), " (",
                            getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site,
                                        charname = DataOpts$Param, info = "Units"), ")")
          
            waterseries(df2, parkcode=DataOpts$Park, sitecode=DataOpts$Site, 
                        char=DataOpts$Param, censored = TRUE, deseason = TRUE,
                        #years=DataOpts$Years[1]:DataOpts$Years[2],
                        layers=c("points"),
                        assessment=input$SeriesThreshLine, 
                        title=Title(),
                        #colors=(GoodCol()),
                        assesscolor=ThCol(),
                        sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth),
                        legend=if(GraphOpts$Legend) "bottom" else "none") +
                        labs(y = ylabel)+
                        theme(text=element_text(size=GraphOpts$FontSize*10)) +
                        geom_smooth(data=df2, method = 'lm', se = FALSE,
                        aes(x = Date, y = pred_y, linetype = sign), formula = y~x, color='black')+
                        scale_linetype_manual(values = c('dashed', 'solid'), guide = 'none')+
                        

            {if(input$ThreshPoint && !is.na(Thresholds()[1]))
              geom_point(data=df2[df2$AdjValueCen<Thresholds()[1],],
              aes(Date,AdjValueCen), pch=16,size=GraphOpts$PointSize, color=BadCol())} +

            {if(input$ThreshPoint && !is.na(Thresholds()[2]))
              geom_point(data=df2[df2$AdjValueCen>Thresholds()[2],],
              aes(Date,AdjValueCen), pch=16, size=GraphOpts$PointSize, color=BadCol())}
        

        } else if(TrendType() == "nonpar"){ 

             df <- merge(DataUse(), TrendsOut()[,c('month','intercept','slope', 'message')], by = 'month', all.x = T) %>%
               mutate(pred_y = intercept + slope * year.dec,
                      sign=as.factor(ifelse(message=="no trend", 0, 1)),
                      month = as.factor(lubridate::month(Date, label = TRUE, abbr = FALSE))) %>% 
               filter(message != "Too few data points.") %>% arrange(month) %>% droplevels()
             
             if(nrow(df)==0){ #catches case when mann-kendall bootstrap fails due to too little data and/or too many ties
               waterseries(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, char=DataOpts$Param, 
                           layers=c("points"),
                           #years=DataOpts$Years[1]:DataOpts$Years[2],
                           assessment=input$SeriesThreshLine, title=Title(),
                           colors=(GoodCol()),
                           assesscolor=ThCol(), 
                           sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth),
                           legend=if(GraphOpts$Legend) "bottom" else "none") +
                           theme(text=element_text(size=GraphOpts$FontSize*10))+
                 
                 {if(input$ThreshPoint && !is.na(Thresholds()[1])) geom_point(data=DataUse()[DataUse()$Value<Thresholds()[1],], 
                                                                              aes(Date,Value), pch=16,size=GraphOpts$PointSize, color=BadCol()) } +
                 
                 {if(input$ThreshPoint && !is.na(Thresholds()[2])) geom_point(data=DataUse()[DataUse()$Value>Thresholds()[2],], 
                                                                              aes(Date,Value), pch=16, size=GraphOpts$PointSize, color=BadCol())} 
             } else{
             
             ylabel = paste0(getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site,
                                         charname = DataOpts$Param, info = "CategoryDisplay"), " (",
                             getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site,
                                         charname = DataOpts$Param, info = "Units"), ")") 
             
              pointlab = paste0(getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site,
                                            charname = DataOpts$Param, info = "DisplayName"))
             
             waterseries(df, parkcode=DataOpts$Park, sitecode=DataOpts$Site, 
                         char=DataOpts$Param, censored = FALSE, deseason = TRUE,
                         #years=DataOpts$Years[1]:DataOpts$Years[2],
                         layers=c("points"), 
                         assessment=input$SeriesThreshLine, title=Title(),
                         colors=(GoodCol()),
                         assesscolor=ThCol(), 
                         sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth),
                         legend=if(GraphOpts$Legend) "bottom" else "none") +
                         labs(y = ylabel, color = pointlab)+
                         theme(text=element_text(size=GraphOpts$FontSize*10)) +
                         geom_smooth(data=df, method = 'lm', se = FALSE, 
                           aes(x = Date, y = pred_y, linetype = sign), formula = y~x, color='black')+
                         scale_linetype_manual(values = c('dashed', 'solid'), guide = 'none')+ 
                         
               
               {if(input$ThreshPoint && !is.na(Thresholds()[1])) 
                 geom_point(data=df[df$Value<Thresholds()[1],],
                            aes(Date,Value), pch=16,size=GraphOpts$PointSize, color=BadCol())} +
               
               {if(input$ThreshPoint && !is.na(Thresholds()[2])) 
                 geom_point(data=df[df$Value>Thresholds()[2],],
                            aes(Date,Value), pch=16, size=GraphOpts$PointSize, color=BadCol())}
             
           }
        }
      
      
      SeriesPlot  #forces ggplot to draw graph after all the conditionals
    
        })
  
  output$TimeSeries<-renderPlot({
    WaterSeriesOut()
  })

  
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
  BoxYears<-callModule(yearChooser, id="BoxYears", data=DataUse, chosen=reactive(DataOpts$Years) )
  
  
  observeEvent(BoxPark(), DataOpts$Park<-BoxPark() )
  observeEvent(BoxSite(), DataOpts$Site<-BoxSite() )
  observeEvent(BoxParam(), DataOpts$Param<-BoxParam() )
  observeEvent(BoxYears(), DataOpts$Years<-BoxYears() )
  
#### Box Plot ####
  
  BoxPlotOut<-reactive({
    req(DataOpts$Park, DataOpts$Site, DataOpts$Param)
    waterbox(object=WaterData, parkcode=DataOpts$Park, sitecode=if(input$BoxBy !="site") DataOpts$Site else NA, 
             charname = DataOpts$Param, by=input$BoxBy, title=Title(),
             years=DataOpts$Years[1]:DataOpts$Years[2], assessment=input$BoxThreshLine, assesscolor=ThCol(), outliercolor = BadCol(),
             sizes=c(GraphOpts$PointSize, GraphOpts$LineWidth, GraphOpts$LineWidth),
             labels=if(input$BoxBy=="site") getSiteInfo(WaterData, parkcode= DataOpts$Park, info="SiteName") else NA) +
              theme(text=element_text(size=GraphOpts$FontSize*10))
  })
   
  output$BoxPlot<-renderPlot({   BoxPlotOut() })
  
  #### BoxThreshold Summary ####
  
  BoxThresholdSummary<-reactive({    
    req(input$BoxThreshLine)
    paste(h4("Threshold:"),"\n",
          c(getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="LowerDescription"),
            getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, 
                        info="UpperDescription"))[!is.na(Thresholds())])
  })
  
  output$BoxThresholdSummary<-renderUI( HTML(BoxThresholdSummary()) )
  
  
  BoxRefSummary<-reactive({
    req(input$BoxThreshLine) 
    paste(h4("Threshold Reference:"),"\n",
          getCharInfo(WaterData,parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param, info="AssessmentDetails")) 
  })      
  
  output$BoxRefSummary<-renderUI(HTML(BoxRefSummary()))

  
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

#### Exceedances Controls ####
  
  ExceedancesPark<-shiny::callModule(parkChooser, id="DataParkExceedances", data=WaterData, chosen=reactive(DataOpts$Park))
  ExceedancesSite<-shiny::callModule(siteChooser, id="DataSiteExceedances", data=WaterData, park=reactive(DataOpts$Park), chosen=reactive(DataOpts$Site))
  ExceedancesParam<-shiny::callModule(paramChooser, id="DataParamExceedances",data=WaterData, park=reactive(DataOpts$Park), site=reactive(DataOpts$Site), 
                       chosen=reactive(DataOpts$Param))
  
  shiny::observeEvent(ExceedancesPark(), DataOpts$Park<-ExceedancesPark() )
  shiny::observeEvent(ExceedancesSite(), DataOpts$Site<-ExceedancesSite() )
  shiny::observeEvent(ExceedancesParam(), DataOpts$Param<-ExceedancesParam() )
  
### Exceedances Prep ###
  
  ExceedancesPrep<- function(Park, Site, Param, WaterData){
    # A data prep function that returns data.frame and character outputs about threshold exceedances at user selected inputs (park, site, & param)
    # Args:
    #   Park, chr, required. The character string provided by ExceedancesPark() and parkChooser() in global.R
    #   Site, chr, required. The character string provided by ExceedancesSite() and siteChooser() in global.R
    #   Param, chr, required. The character string provided by ExceedancesParam() and paramChooser() in global.R
    #   WaterData, list, required. The list of NCRN water quality data assembled from /wqp_ncrnwater_metadata.csv and /wqp.csv
    # Returns:
    #   tmp, list. Temporary package containing:
    #     exdf, data.frame. Water quality site observations exceeding water quality thresholds and threshold information, containing:
    #       OrganizationFormalName, chr, NPS program name
    #       ActivityMediaSubdivisionName, chr, unique site and date code for an observation event
    #       Date, Date, date of observation
    #       Characteristic, chr, water quality parameter display name
    #       Value, num, numeric value of a water quality parameter observation
    #       ResultMeasure.MeasureUnitCode, chr, unit of a water quality parameter value
    #       LowerThreshold, chr, description of the lower water quality threshold, if applicable
    #       UpperThreshold, chr, description of the upper water quality threshold, if applicable
    #     df, data.frame. All water quality site observations at user selected inputs, containing:
    #       OrganizationFormalName, chr, NPS program name
    #       ActivityMediaSubdivisionName, chr, unique site and date code for an observation event
    #       Date, Date, date of observation
    #       Characteristic, chr, water quality parameter display name
    #       Value, num, numeric value of a water quality parameter observation
    #       ResultMeasure.MeasureUnitCode, chr, unit of a water quality parameter value
    #     lowerthreshold, chr, description of the lower water quality threshold, if applicable
    #     upperthreshold, chr, description of the upper water quality threshold, if applicable
    #     lowerpoint, chr, value of the lower water quality threshold, if applicable
    #     upperpoint, chr, value of the upper water quality threshold, if applicable
    #     unit, chr, unit of a water quality parameter value
    #     sitename, chr, site display name
    #     characteristic, chr, water quality parameter display name
    # Example:
    #   DataOpts$Park<- "GWMP"
    #   DataOpts$Site<- "NCRN_GWMP_TURU"
    #   DataOpts$Param<- "TotalP"
    #   WaterData<- NCRN::getWData(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param)
    #   tmp <- ExceedancesPrep(DataOpts$Park, DataOpts$Site, DataOpts$Param, WaterData)
    #   exdf<- tmp$exdf
    df2 <- getWData(WaterData, parkcode=Park, sitecode=Site, charname=Param)
    df1 <- suppressWarnings(df2 %>% 
                              dplyr::mutate(year.dec = julian(Date)/365, month = as.factor(months(Date))) %>% 
                              dplyr::group_by(month) %>%
                              dplyr::mutate(num_meas = sum(!is.na(Value))) %>% 
                              dplyr::ungroup()) %>% dplyr::mutate(num_mos = length(unique(month)))
    df <- df1[, c("OrganizationFormalName", "ActivityMediaSubdivisionName", "Date", "Characteristic", "Value", "ResultMeasure.MeasureUnitCode")]
    df <- subset(df, !is.na(Value))
    
    LowerThreshold<- NCRNWater::getCharInfo(WaterData, parkcode = Park, sitecode = Site, charname = Param, info="LowerDescription")
    UpperThreshold<- NCRNWater::getCharInfo(WaterData, parkcode = Park, sitecode = Site, charname = Param, info="UpperDescription")
    LowerPoint<- NCRNWater::getCharInfo(WaterData, parkcode = Park, sitecode = Site, charname = Param, info="LowerPoint")
    UpperPoint<- NCRNWater::getCharInfo(WaterData, parkcode = Park, sitecode = Site, charname = Param, info="UpperPoint")
    Unit<- NCRNWater::getCharInfo(WaterData, parkcode = Park, sitecode = Site, charname = Param, info="Units")
    Sitename<- NCRNWater::getCharInfo(WaterData, parkcode = Park, sitecode = Site, charname = Param, info = "SiteName")
    Characteristic<- NCRNWater::getCharInfo(WaterData, parkcode = Park, sitecode = Site, charname = Param, info = "DisplayName")
    
    if(any(df$Value <= LowerPoint, na.rm = TRUE)) {
      lowerdf<- df[df$Value < LowerPoint, ]
      lowerdf$LowerThreshold <- LowerThreshold
    } else{
      lowerdf<- df[0, ]
    }
    
    if(any(df$Value >= UpperPoint, na.rm = TRUE)) {
      upperdf<- df[df$Value > UpperPoint, ]
      upperdf$UpperThreshold <- UpperThreshold
    } else {
      upperdf<- df[0, ]
    }
    
    exdf<- dplyr::bind_rows(lowerdf, upperdf)
    
    tmp <- list(
      'exdf'=exdf
      ,'df'=df
      ,'lowerthreshold'=LowerThreshold
      ,'upperthreshold'=UpperThreshold
      ,'lowerpoint'=LowerPoint
      ,'upperpoint'=UpperPoint
      ,'unit'=Unit
      ,'characteristic'=Characteristic
      ,'sitename'=Sitename
      )
    return(tmp)
  }
    
### Exceedances Data Use Function ###
  
  ExceedancesDataUse<- shiny::reactive({
    # A reactive function that returns a data.frame output to the Exceedances tab and that establishes the conditions to display warning messages
    # Args:
    #   DataOpts$Park, chr, required. The character string provided by parkChooser() in global.R
    #   DataOpts$Site, chr, required. The character string provided by siteChooser() in global.R
    #   DataOpts$Site, chr, required. The character string provided by paramChooser() in global.R
    #   WaterData, list, required. The list of NCRN water quality data assembled from /wqp_ncrnwater_metadata.csv and /wqp.csv
    # Returns:
    #   exdf, data.frame. Water quality site observations exceeding water quality thresholds and threshold information in reverse chronological order, containing:
    #     OrganizationFormalName, chr, NPS program name
    #     ActivityMediaSubdivisionName, chr, unique site and date code for an observation event
    #     Date, Date, date of observation
    #     Characteristic, chr, water quality parameter display name
    #     Value, num, numeric value of a water quality parameter observation
    #     ResultMeasure.MeasureUnitCode, chr, unit of a water quality parameter value
    #     LowerThreshold, chr, description of the lower water quality threshold, if applicable
    #     UpperThreshold, chr, description of the upper water quality threshold, if applicable
    # Example:
    #   DataOpts$Park<- "GWMP"
    #   DataOpts$Site<- "NCRN_GWMP_TURU"
    #   DataOpts$Param<- "TotalP"
    #   WaterData<- NCRN::getWData(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param)
    #   output$ExceedancesTable <-DT::renderDataTable(
    #     expr=datatable(ExceedancesDataUse(), extensions=c("Buttons","KeyTable"),caption=htmltools::tags$caption(htmltools::h3(Title())),
    #                   class="stripe hover order-column cell-border",filter="top",
    #                   rownames=F, options=list(autoWidth=TRUE, dom="Bltirp", buttons=c("copy","csv","excel","pdf","print"), keys=TRUE)
    #     ),server=F)  
    shiny::validate(
      need(DataOpts$Park, message="Choose a Park"),
      need(DataOpts$Site, message="Choose a Site"),
      need(DataOpts$Param, message="Choose a Water Quality Parameter"))  
    
    tmp <- ExceedancesPrep(DataOpts$Park, DataOpts$Site, DataOpts$Param, WaterData)
    exdf <- tmp$exdf
    df <- tmp$df
    LowerThreshold <- tmp$lowerthreshold
    UpperThreshold <- tmp$upperthreshold
    LowerPoint <- tmp$lowerpoint
    UpperPoint <- tmp$upperpoint
    Unit <- tmp$unit
    Characteristic <- tmp$characteristic
    Sitename <- tmp$sitename

    exdf<- exdf %>%
      dplyr::arrange(desc(Date))

      if(!is.na(LowerPoint) & all(df$Value > LowerPoint)) {
        shiny::showNotification(
          paste0("No measurements of ", Characteristic, " at ", Sitename, " fall below the water quality threshold of ", LowerPoint, " ", Unit)
          ,type = "error"
          ,duration = 10
          ,id = "n1")
      }

      if(!is.na(UpperPoint) & all(df$Value < UpperPoint)) {
        shiny::showNotification(
          paste0("No measurements of ", Characteristic, " at ", Sitename, " exceed the water quality threshold of ", UpperPoint, " ", Unit)
          ,type = "error"
          ,duration = 10
          ,id = "n2")
      }

      if(is.na(LowerPoint) & is.na(UpperPoint)) {
        shiny::showNotification(
          paste0("There is no recorded water quality threshold for ", Characteristic, " at ", Sitename, ".")
          ,type = "error"
          ,duration = 10
          ,id = "n3")
      }
    
    return(exdf)
    
  })
  
### Exceedances Data Table Output ###
  
  output$ExceedancesTable <-DT::renderDataTable(
    expr=datatable(ExceedancesDataUse(), extensions=c("Buttons","KeyTable"),caption=htmltools::tags$caption(htmltools::h3(Title())),
                   class="stripe hover order-column cell-border",filter="top",
                   rownames=F, options=list(autoWidth=TRUE, dom="Bltirp", buttons=c("copy","csv","excel","pdf","print"), keys=TRUE)
    ),server=F
  )  
  
### SummarizeExceedances() Function ###
  
  SummarizeExceedances<- function(df, exdf){
    # A function that returns a data.frame containing counts and percentages of observations and exceedances per year of observation
    # Args:
    #   df, data.frame, required. All water quality site observations at user selected inputs
    #   exdf, data.frame, required. Water quality site observations exceeding water quality thresholds and threshold information
    # Returns:
    #   histdata, dataframe. Counts and percents of observations and exceedances per year of water quality observation
    # Example:
    #   DataOpts$Park<- "GWMP"
    #   DataOpts$Site<- "NCRN_GWMP_TURU"
    #   DataOpts$Param<- "TotalP"
    #   WaterData<- NCRN::getWData(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param)
    #   tmp<- ExceedancesPrep(DataOpts$Park, DataOpts$Site, DataOpts$Param, WaterData)
    #   exdf<- tmp$exdf
    #   df<- tmp$df
    #   histdata<- SummarizeExceedances(df, exdf)
    histyears<- data.frame(Year = lubridate::year(df$Date))
    totcount<- histyears %>%
      dplyr::count(Year) %>%
      dplyr::rename("ntot" = "n")
    totcount <- subset(totcount, !is.na(Year))
    
    excount <- exdf %>%
      dplyr::mutate(Year = lubridate::year(Date)) %>%
      dplyr::count(Year) %>%
      dplyr::rename("nex" = "n")
    excount <- subset(excount, !is.na(Year))
    
    histdata<- dplyr::left_join(totcount, excount, by = "Year")
    histdata[is.na(histdata)] <- 0
    histdata<- histdata %>%
      dplyr::mutate(percent_ex = (nex / ntot) * 100) %>%
      dplyr::mutate(formatted_percent_ex = scales::percent(percent_ex / 100, accuracy = 0.01))
    
    return(histdata)
  }
  
### Exceedances Text Function ###
  
  exceedances_text<- shiny::reactive({
    # A reactive function that outputs dynamic text summarizing threshold exceedances for a water quality parameter at a site
    # Args:
    #   DataOpts$Park, chr, required. The character string provided by parkChooser() in global.R
    #   DataOpts$Site, chr, required. The character string provided by siteChooser() in global.R
    #   DataOpts$Site, chr, required. The character string provided by paramChooser() in global.R
    #   WaterData, list, required. The list of NCRN water quality data assembled from /wqp_ncrnwater_metadata.csv and /wqp.csv
    # Returns:
    #   chr. A character string containing reactive formatted HTML text
    # Example:
    #   DataOpts$Park<- "GWMP"  
    #   DataOpts$Site<- "NCRN_GWMP_TURU"
    #   DataOpts$Param<- "TotalP"
    #   WaterData<- NCRN::getWData(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param)
    #   output$exceedances_summary<- shiny::renderText({
    #     exceedances_text()
    #   })
    shiny::validate(
      need(DataOpts$Park, message=""),
      need(DataOpts$Site, message=""),
      need(DataOpts$Param, message=""))
    
    tmp <- ExceedancesPrep(DataOpts$Park, DataOpts$Site, DataOpts$Param, WaterData)
    exdf <- tmp$exdf
    df <- tmp$df
    LowerThreshold <- tmp$lowerthreshold
    UpperThreshold <- tmp$upperthreshold
    LowerPoint <- tmp$lowerpoint
    UpperPoint <- tmp$upperpoint
    Unit <- tmp$unit
    Characteristic <- tmp$characteristic
    Sitename <- tmp$sitename
    
  histdata<- SummarizeExceedances(df, exdf)

  # Text prep
  recent_year<- max(histdata$Year)
  oldest_year<- min(histdata$Year)
  nex<- histdata[histdata$Year == recent_year, "nex"]
  ntot<- histdata[histdata$Year == recent_year, "ntot"]
  recent_freq<- sprintf("%.2f%%", (nex/ntot)*100)
  sum_nex<- sum(histdata$nex)
  sum_ntot<- sum(histdata$ntot)
  sum_freq<- sprintf("%.2f%%", (sum_nex/sum_ntot)*100)
  freq_comp<- ifelse((nex/ntot) > (sum_nex/sum_ntot), "greater than",
                       ifelse((nex/ntot) == (sum_nex/sum_ntot), "equal to", "less than"))
  grammar1<- if(nex==1) {
    paste("There was", nex, "exceedance of the ")
  } else {
    paste("There were", nex, "exceedances of the ")
  }
  grammar2<- if(sum_nex==1) {
    paste("There has been", sum_nex, "exceedance of the ")
  } else {
    paste("There have been", sum_nex, "exceedances of the ")
  }
    
  # Writing text
  summary<- c(
    paste0(grammar1, Characteristic, " water quality threshold among ", ntot, " observations at ", Sitename, " in ", recent_year, ".")
    ,paste0(grammar2, Characteristic, " water quality threshold among ", sum_ntot, " observations at ", Sitename, " since monitoring began in ", oldest_year, ".")
    ,paste0("<u>", recent_freq, "</u>", " of observations exceeded the water quality threshold in ", recent_year, ", ", freq_comp, " the overall exceedance percentage of ", "<u>", sum_freq, "</u>", ".")
  )
  summary_bullets<- paste0("<li>", summary, "</li>", collapse = "")
   
  HTML(paste0(
    "<p><b><span style='font-size: 18px;'>Exceedances Summary:</b></p>",
    "<ul>", summary_bullets, "</ul>"
  ))
   
  })
  
  ### HTML Text Tutput ###
  
  output$exceedances_summary<- shiny::renderText({
    exceedances_text()
  })
  
  ### Exceedances Histogram ###
  
  exceedances_hist_plot<- shiny::reactive({
    # A reactive function that outputs an interactive histogram displaying the percentage of observations per year which a parameter exceeds a threshold at a site
    # Args:
    #   DataOpts$Park, chr, required. The character string provided by parkChooser() in global.R
    #   DataOpts$Site, chr, required. The character string provided by siteChooser() in global.R
    #   DataOpts$Site, chr, required. The character string provided by paramChooser() in global.R
    #   WaterData, list, required. The list of NCRN water quality data assembled from /wqp_ncrnwater_metadata.csv and /wqp.csv
    # Returns:
    #   list. A plotly histogram displaying percentages of observations per year exceeding a threshold at a site, containing:
    #     x, list. Containing the data and formatting layout of the histogram
    #     width, int, optional. An optional plotly parameter
    #     height, int, optional. An optional plotly parameter
    #     sizingPolicy, list. Containing default sizing and formatting information
    #     dependencies, list. Containing html information
    #     elementId, int, optional. An optional plotly parameter
    #     preRenderHook, function. Containing functions to build plotly output
    #     jsHooks, list, optional. An optional plotly feature
    # Example:
    #   DataOpts$Park<- "GWMP"  
    #   DataOpts$Site<- "NCRN_GWMP_TURU"
    #   DataOpts$Param<- "TotalP"
    #   WaterData<- NCRN::getWData(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param)
    #   output$hist<- renderPlotly({
    #     exceedances_hist_plot()
    #   })
    shiny::validate(
      need(DataOpts$Park, message=""),
      need(DataOpts$Site, message=""),
      need(DataOpts$Param, message=""))
    
    tmp <- ExceedancesPrep(DataOpts$Park, DataOpts$Site, DataOpts$Param, WaterData)
    exdf <- tmp$exdf
    df <- tmp$df
    LowerThreshold <- tmp$lowerthreshold
    UpperThreshold <- tmp$upperthreshold
    LowerPoint <- tmp$lowerpoint
    UpperPoint <- tmp$upperpoint
    Unit <- tmp$unit
    Characteristic <- tmp$characteristic
    Sitename <- tmp$sitename
    
  histdata<- SummarizeExceedances(df, exdf)
    
  # Plot prep
    ExPoint<- dplyr::case_when(
      is.na(UpperPoint) == TRUE & !is.na(LowerPoint) == TRUE ~ LowerPoint
      ,!is.na(UpperPoint) == TRUE & is.na(LowerPoint) == TRUE ~ UpperPoint
      ,!is.na(UpperPoint) == TRUE & !is.na(LowerPoint) == TRUE & any(exdf$Value <= LowerPoint, na.rm=TRUE) & all(exdf$Value < UpperPoint, na.rm=TRUE) ~ LowerPoint
      ,!is.na(UpperPoint) == TRUE & !is.na(LowerPoint) == TRUE & any(exdf$Value >= UpperPoint, na.rm=TRUE) & all(exdf$Value > LowerPoint, na.rm=TRUE) ~ UpperPoint
      # ,!is.na(UpperPoint) == TRUE & !is.na(LowerPoint) == TRUE & any(exdf$Value >= UpperPoint, na.rm=TRUE) & any(exdf$Value <= LowerPoint, na.rm=TRUE) ~ paste0(str(LowerPoint), " and ", str(UpperPoint))
    )
    
  # Plotting
    p<- ggplot2::ggplot(histdata, aes(x = Year, y = percent_ex,
                              text = paste0(Year, ", ", Characteristic, "\n",
                                            ntot, " total observation(s)", "\n",
                                            formatted_percent_ex, " of observations exceeding ", ExPoint, " ", Unit))) +
        geom_bar(stat = "identity", fill = "lightgray") +
        ylim(0, 100) +
        labs(
          title = paste0("Percent of ", Characteristic, " observations exceeding the water quality threshold at ", Sitename),
          x = "Year",
          y = "% observations") +
        theme_minimal() +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
    
    plotly::ggplotly(p, tooltip = "text")
  
})
  
  ### Plot Output###
  
  show_plot<- shiny::reactiveVal(FALSE)
  
  output$exceedances_hist<- shiny::renderUI({
    if (show_plot()) {
      plotlyOutput("hist")
    }
  })
  
  output$hist<- renderPlotly({
    exceedances_hist_plot()
  })
  
  shiny::observeEvent(input$hist_button, {
    show_plot(!show_plot())
    new_label<- ifelse(show_plot(), "Hide Histogram", "Show Histogram")
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
  NPSGeoData<-data.frame(ParkCode=getSiteInfo(WaterData, info="ParkCode"), SiteCode=getSiteInfo(WaterData, info="SiteCode"), SiteName=getSiteInfo(WaterData, info= "SiteName"), 
                         latitude=getSiteInfo(WaterData, info="lat"), longitude=getSiteInfo(WaterData, info="long"), stringsAsFactors = F)
  
  #CharIndex is a true/false of characters that have thresholds
  CharIndex<-{getCharInfo(WaterData,info="LowerPoint") %>% is.na %>% not} | {getCharInfo(WaterData,info="UpperPoint") %>% is.na %>% not} 
  NPSchars<-getCharInfo(WaterData, info="CharName")[CharIndex] %>% unique
  names(NPSchars)<-getCharInfo(WaterData, info="DisplayName")[CharIndex] %>% unique
  output$MapChars<-renderUI( selectizeInput(inputId="MapChar",label="Charactersitic to Map", choices=NPSchars[order(names(NPSchars))] ))
  
  #coloring
  MapColors<-colorNumeric(palette="viridis", domain=c(0,1)) # NPS % meets threshol
  MapColors2<-colorFactor(palette="viridis", domain=c("<5th percentile","5th - 25th percentile", 
          "25th - 50th percentile", "50th - 75th percentile", "75th - 95th percentile", "> 95th percentile" ), ordered = T )  # USGS percentile category for discharge
  
  ExceedData<-reactive({
    req(input$MapChar)
    exceed(WaterData, charname=input$MapChar)
  })

  #### the Map ####
  output$WaterMap<-renderLeaflet({ 
    netlat<-dplyr::case_when(Network == "NCRN" ~ 39.25, 
                             Network == "NETN" ~ 42.5)
    netlon<-dplyr::case_when(Network == "NCRN" ~ -77,
                             Network == "NETN" ~ -71.6)
    
    netzoom<-dplyr::case_when(Network == "NCRN" ~ 9,
                              Network == "NETN" ~ 7)
    
    leaflet() %>% 
    setView(lng = netlon, lat = netlat, zoom = netzoom) %>% 
      
    addTiles() # temporary workaround to provide a basemap
    # broken map tiles:
    # addTiles(group="Map", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.397cfb9a,nps.3cf3d4ab,nps.b0add3e6/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",attribution=NPSAttrib, options=tileOptions(minZoom=netzoom)) %>% 
    # addTiles(group="Imagery", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.2c589204,nps.25abf75b,nps.7531d30a/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",attribution=NPSAttrib, options=tileOptions(minZoom=netzoom)) %>% 
    # addTiles(group="Slate", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.9e521899,nps.17f575d9,nps.e091bdaf/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q", attribution=NPSAttrib, options=tileOptions(minZoom=netzoom) ) %>% 
    # addLayersControl(map=., baseGroups=c("Map","Imagery","Slate"), options=layersControlOptions(collapsed=T))
  })

  NPSAttrib<-HTML("<a href='https://www.nps.gov/npmap/disclaimer/'>Disclaimer</a> | 
      &copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a>
      &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors |
      <a class='improve-park-tiles' 
      href='http://insidemaps.nps.gov/places/editor/#background=mapbox-satellite&map=4/-95.97656/39.02772&overlays=park-tiles-overlay'
      target='_blank'>Improve Park Tiles</a>")
  
  observe({
    if(input$MapNPS){
      leafletProxy("WaterMap") %>% 
        clearGroup("NPS") %>% 
        addCircleMarkers(data=NPSGeoData, group="NPS", 
                         layerId=NPSGeoData$SiteCode, 
                         fillColor=MapColors(ExceedData()$Acceptable/ExceedData()$Total),
                         fillOpacity=.8, stroke=FALSE) %>% 
        
        addLegend(position="topright", pal=MapColors, values=c(0,1), opacity=1,
                    layerId="npsLegend",title=paste0("<svg height='15' width='20'>
                    <circle cx='10' cy='10' r='5', stroke='black' fill='black'/></svg> NPS: % of Acceptable <br>Measurements"),
                  labFormat=labelFormat(suffix="%", transform= function(x) 100*x))
        } else {leafletProxy("WaterMap") %>% clearGroup("NPS") %>% removeControl(layerId="npsLegend")}
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
  
  

}) #End of Shiny Server function
    