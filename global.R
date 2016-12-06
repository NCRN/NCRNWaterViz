GraphColors<-read.csv("colors.csv", header=T, as.is=T)


#### Park Module ####

parkChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("ParkIn"),label="1. Park:" , choices=NULL)
}

parkChooser<-function(input,output,session,data){
  observe({updateSelectizeInput(session, "ParkIn",
    choices=c("Choose a Park"="", c(`names<-`(getParkInfo(data, info="ParkCode"), getParkInfo(data, info="ParkShortName"))))
  )})
  return(reactive(input$ParkIn))
}


#### Site Module ####

siteChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId = ns("SiteIn"), label="2. Stream:", choices=NULL)
}

siteChooser<-function(input, output, session, data, park){
  observe({
    updateSelectizeInput(session, inputId = "SiteIn",  
      choices=c("Choose a Site"="",
      c(`names<-`(getSiteInfo(data, parkcode=park(), info="SiteCode"), 
        getSiteInfo(data, parkcode=park(), info="SiteName")  )))
    )
  })
  return(reactive(input$SiteIn))
}

### Parameter Module ####

paramChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("ParamIn"), label="3. Water Parameter:", choices=NULL)
}


paramChooser<-function(input, output, session, data, park, site){
  PChoices<-reactive({
    req(park(), site())
    Choice<-getCharInfo(data, parkcode=park(), sitecode=site(), info="CharName")
    ChoiceName<-paste(getCharInfo(data, parkcode=park(), sitecode=site(), info="DisplayName"),
                       getCharInfo(data, parkcode=park(), sitecode=site(), info="Units") %>% iconv("","UTF-8"))
    if(isTruthy(Choice) & isTruthy(ChoiceName)) { names(Choice)<-ChoiceName }
    return(Choice)
   })
  
  observe(
    updateSelectizeInput(session, inputId="ParamIn",choices=c("Choose a Parameter"="",as.list(PChoices())))
  )
  
  return(reactive(input$ParamIn))
}

#### Years Module ####
yearChooserUI<-function(id){
  ns<-NS(id)
  sliderInput(inputId=ns("YearsShow"), label= "4. Years to Display:", min=1900, max=2100, step=1, value=c(1900,2100),sep="",ticks=F)
}


yearChooser<-function(input,output,session,data)  {
  
observe({
  #req( data() )
  YrMax<-reactive(max(year(data()$Date), na.rm=T))
  YrMin<-reactive(min(year(data()$Date), na.rm=T))
  updateSliderInput(session, inputId="YearsShow", min=YrMin(),max=YrMax(), value=c(YrMin(), YrMax() ) )
})
  
return(reactive(input$YearsShow))
}
  
  