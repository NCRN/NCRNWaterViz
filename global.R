#-------------------------
# Specify the network 
Network <- "NETN" 
Network_long <- "Northeast Temperate Network" # for navbar title
Viz_name <- "Lake and Stream Water Quality"
#Network <- "NCRN" # for leaflet map center 
#Network_long <- "National Capital Region Network" # for navbar title
#Viz_name <- "Stream Water Quality"
GraphColors<-read.csv("colors.csv", header=T, as.is=T)

#### Park Module ####

parkChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("ParkIn"),label="1. Park:" , choices=NULL)
}

parkChooser<-function(input,output,session,data, chosen){
  observe({updateSelectizeInput(session, "ParkIn", selected=chosen(),
    choices=c("Choose a Park"="", c(`names<-`(getParkInfo(data, info="ParkCode"), getParkInfo(data, info="ParkShortName"))))
  )})
  return(reactive(input$ParkIn))
}


#### Site Module ####

siteChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId = ns("SiteIn"), label="2. Site:", choices=NULL)
}

siteChooser<-function(input, output, session, data, park, chosen){
   observe({
     updateSelectizeInput(session, inputId = "SiteIn", selected=chosen(), 
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


paramChooser<-function(input, output, session, data, park, site, chosen){
  PChoices<-reactive({
    req(park(), site())
    Choice<-getCharInfo(data, parkcode=park(), sitecode=site(), info="CharName")
    ChoiceName<-paste0(getCharInfo(data, parkcode=park(), sitecode=site(), info="DisplayName"), " (",
                       getCharInfo(data, parkcode=park(), sitecode=site(), info="Units") %>% 
                         iconv("","UTF-8"), ")")#%>% iconv("","UTF-8"))
    if(isTruthy(Choice) & isTruthy(ChoiceName)) { names(Choice)<-ChoiceName }
    return(Choice)
   })
  
  observe(
    updateSelectizeInput(session, inputId="ParamIn",selected=chosen(), 
                         choices=c("Choose a Parameter"="",as.list(sort(PChoices()))))
  )
  
  return(reactive(input$ParamIn))
}

#### Years Module ####
yearChooserUI<-function(id){
  ns<-NS(id)
  sliderInput(inputId=ns("YearsShow"), label= "4. Years to Display:", min=1900, max=2100, step=1, value=c(1900,2100),sep="",ticks=F)
}


yearChooser<-function(input,output,session,data,chosen)  {
  
observe({
  req( data() )
  if(class(data()$Date)=="Date"){
    YrMax<-reactive(max(year(data()$Date), na.rm=T))
    YrMin<-reactive(min(year(data()$Date), na.rm=T))
    updateSliderInput(session, inputId="YearsShow", min=YrMin(),max=YrMax(),val=c(YrMin(),YrMax()) )
                      #value=chosen())
  }
})
  
return(reactive(input$YearsShow))
}
  
  