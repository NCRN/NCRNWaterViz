library(NCRNWater)
## --- Load config (pure) ------------------------------------------------------
cfg <- NCRNWater::load_app_config(profile = "NCRN")

Network_long     <- cfg$app$network_long
Network          <- cfg$app$network_code
Viz_name         <- cfg$app$app_name
figure_defaults  <- cfg$app$figure_defaults

# Colors CSV: robust read with clear error
GraphColors <- tryCatch({
  utils::read.csv(cfg$files$colors_csv, header = TRUE, stringsAsFactors = FALSE)
}, error = function(e) {
  stop("Failed to read colors CSV at '", cfg$files$colors_csv, "': ", conditionMessage(e))
})

## --- Pre-flight: existence checks for data paths -----------------------------
net_dir    <- file.path(cfg$files$datadir, cfg$app$network_code)
data_fp    <- file.path(net_dir, cfg$files$dataname)
meta_fp    <- file.path(net_dir, cfg$files$metadataname)
photos_dir <- file.path(net_dir, cfg$files$imagesdir)

if (!file.exists(data_fp)) {
  stop("Data file not found: ", data_fp, 
       ". Check cfg$files$datadir and cfg$files$dataname.")
}
if (!file.exists(meta_fp)) {
  stop("Metadata file not found: ", meta_fp, 
       ". Check cfg$files$datadir and cfg$files$metadataname.")
}
# Don't fail if photos missing; handle gently later
photos_available <- dir.exists(photos_dir)

## --- Hydrate NCRNWater object -----------------------------------------------
dh <- NCRNWater::hydrate_network(
  cfg$app$network_code,
  datadir      = cfg$files$datadir,
  dataname     = cfg$files$dataname,
  metadataname = cfg$files$metadataname,
  wqx          = cfg$wqx$enabled
)
WaterData        <- dh$wd
metadata_active  <- dh$metadata_active

## --- Late resolution of time bounds ('auto' -> numeric) ----------------------
if (identical(cfg$time$min_year, "auto") || identical(cfg$time$max_year, "auto")) {
  yrs <- NCRNWater:::resolve_year_bounds(cfg, wd = WaterData)
  cfg$time$min_year <- yrs$min_year
  cfg$time$max_year <- yrs$max_year
  if (identical(yrs$source, "fallback")) {
    message(sprintf("time.auto: using fallback bounds %s–%s.", yrs$min_year, yrs$max_year))
  }
}
MINYR <- cfg$time$min_year
MAXYR <- cfg$time$max_year


LOADING_TEXT <- c(
  "Loading application..."
  ,"Loading application..."
  ,"Loading application..."
)

LOADING_IMAGES <- list(
  list(src= "dwq_NCRN_ANTI_SHCK_2024-06-04_20240604-131442.jpg", location = "Antietam National Battlefield, Sharpsburg Creek", date = "June 4, 2024")
  ,list(src= "dwq_NCRN_MONO_BUCK_2024-06-04_20240604-084406.jpg", location = "Monocacy National Battlefield, Bush Creek", date = "June 4, 2024")
  ,list(src= "dwq_NCRN_PRWI_BONE_2024-06-11_20240611-130821.jpg", location = "Prince William Forest Park, Boneyard Run", date = "June 11, 2024")
)

#### Years Module ####
yearChooserUI<-function(id){
  ns<-NS(id)
  sliderInput(inputId=ns("YearsShow"), label= "Years", min=MINYR, max=MAXYR, step=1, value=c(MINYR,MAXYR),sep="",ticks=F)
}


yearChooser<-function(input,output,session,data,chosen)  {
  
  observe({
    req( data() )
    if(class(data()$Date)=="Date"){
      YrMax<-reactive(max(year(data()$Date), na.rm=T))
      YrMin<-reactive(min(year(data()$Date), na.rm=T))
      
      # debounce slows down the app to prevent infinite loops caused by the user changing
      # variables faster than the app can respond
      YrMax_debounce <- debounce(YrMax, 1000)
      YrMin_debounce <- debounce(YrMin, 1000)
      
      updateSliderInput(session, inputId="YearsShow", min=YrMin_debounce(),max=YrMax_debounce(),val=chosen())
    }
  })
  
  return(reactive(input$YearsShow))
}

yearChooserUI2<-function(id){
  ns<-NS(id)
  sliderInput(inputId=ns("YearsShow2"), label= "Years", min=MINYR, max=MAXYR, step=1, value=c(MINYR,MAXYR),sep="",ticks=F)
}


yearChooser2<-function(input,output,session,data,chosen)  {
  
  observe({
    req( data() )
    if(class(data()$Date)=="Date"){
      YrMax<-reactive(max(year(data()$Date), na.rm=T))
      YrMin<-reactive(min(year(data()$Date), na.rm=T))
      
      # debounce slows down the app to prevent infinite loops caused by the user changing
      # variables faster than the app can respond
      YrMax_debounce <- debounce(YrMax, 1000)
      YrMin_debounce <- debounce(YrMin, 1000)
      
      updateSliderInput(session, inputId="YearsShow2", min=YrMin_debounce(),max=YrMax_debounce(),val=chosen())
    }
  })
  
  return(reactive(input$YearsShow2))
}

#### Date Module ####
# daterangeChooserUI <-function(id){
#   ns<-NS(id)
#   dateRangeInput(inputId=ns("DateRangeIn"), label="4. Date Range (optional)", start = 2005-01-01, end = NULL)
# }
# 
# daterangeChooser<-function(input,output,session,data, chosen)  {
#   observe({
#     req(data())
#     MinDate<-min(data()$Date, na.rm=T)
#     MaxDate<-max(data()$Date, na.rm=T)
#     updateDateRangeInput(session, "DateRangeIn", start = MinDate, end = MaxDate)
#     })
# 
#   return(reactive(input$DateRangeIn))
# }

#### Park Module ####

parkChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("ParkIn"),label="Park" , choices=NULL)
}

parkChooser<-function(input,output,session, data, chosen){
  observe({updateSelectizeInput(session, "ParkIn", selected=chosen(),
                                choices=c("Choose a Park"="", c(`names<-`(getParkInfo(data, info="ParkCode"), getParkInfo(data, info="ParkShortName"))))
  )})
  return(reactive(input$ParkIn))
}

parkChooserUI2<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("ParkIn2"),label="Park" , choices=NULL)
}

parkChooser2<-function(input,output,session, data, chosen){
  observe({updateSelectizeInput(session, "ParkIn2", selected=chosen(),
                                choices=c("Choose a Park"="", c(`names<-`(getParkInfo(data, info="ParkCode"), getParkInfo(data, info="ParkShortName"))))
  )})
  return(reactive(input$ParkIn2))
}


#### Site Module ####

siteChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId = ns("SiteIn"),
                 label="Site",
                 choices=NULL,
                 multiple = TRUE,
                 options = list(plugins = list("remove_button"))
  )
}

siteChooser<-function(input, output, session, data, park, chosen){
  # debounce slows down the app to prevent infinite loops caused by the user changing
  # variables faster than the app can respond
  debouncedSiteIn <- shiny::debounce(reactive(input$SiteIn), 1000)
  
  observe({
    updateSelectizeInput(session, inputId = "SiteIn", selected=chosen(), 
                         choices=c("Choose a Site"="",
                                   c("Select All" = "ALL", setNames(getSiteInfo(data, parkcode=park(), info="SiteCode"), 
                                                                    getSiteInfo(data, parkcode=park(), info="SiteName") )))
    )
  })
  return(reactive({
    if ("ALL" %in% debouncedSiteIn()){
      getSiteInfo(data, parkcode = park(), info = "SiteCode")
    } else {
      debouncedSiteIn()
    }
  }))
}

siteChooserUI2<-function(id){
  ns<-NS(id)
  selectizeInput(inputId = ns("SiteIn2"),
                 label="Site",
                 choices=NULL,
                 multiple = TRUE,
                 options = list(plugins = list("remove_button"))
  )
}

siteChooser2<-function(input, output, session, data, park, chosen){
  # debounce slows down the app to prevent infinite loops caused by the user changing
  # variables faster than the app can respond
  debouncedSiteIn2 <- shiny::debounce(reactive(input$SiteIn2), 1000)
  
  observe({
    updateSelectizeInput(session, inputId = "SiteIn2", selected=chosen(), 
                         choices=c("Choose a Site"="",
                                   c("Select All" = "ALL", setNames(getSiteInfo(data, parkcode=park(), info="SiteCode"), 
                                                                    getSiteInfo(data, parkcode=park(), info="SiteName") )))
    )
  })
  return(reactive({
    if ("ALL" %in% debouncedSiteIn2()){
      getSiteInfo(data, parkcode = park(), info = "SiteCode")
    } else {
      debouncedSiteIn2()
    }
  }))
}

### Parameter Module ####

paramChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("ParamIn"), label="Parameter", choices=NULL)
}


paramChooser<-function(input, output, session, data, park, chosen){
  PChoices<-reactive({
    req(park())
    Choice<-getCharInfo(data, parkcode=park(), info="CharName")
    ChoiceName<-paste0(getCharInfo(data, parkcode=park(), info="DisplayName"), " (",
                       getCharInfo(data, parkcode=park(), info="Units") %>% 
                         iconv("","UTF-8"), ")")#%>% iconv("","UTF-8"))
    if(isTruthy(Choice) & isTruthy(ChoiceName)) { names(Choice)<-ChoiceName }
    return(Choice)
  })
  
  observe(
    updateSelectizeInput(session, inputId="ParamIn",selected=chosen(), 
                         choices=c("Choose a Parameter"="", as.list(sort(PChoices()))))
  )
  
  return(reactive(input$ParamIn))
}

paramChooserUI2<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("ParamIn2"), label="Second Parameter", choices=NULL)
}


paramChooser2<-function(input, output, session, data, park, chosen){
  PChoices<-reactive({
    req(park())
    Choice<-getCharInfo(data, parkcode=park(), info="CharName")
    ChoiceName<-paste0(getCharInfo(data, parkcode=park(), info="DisplayName"), " (",
                       getCharInfo(data, parkcode=park(), info="Units") %>% 
                         iconv("","UTF-8"), ")")#%>% iconv("","UTF-8"))
    if(isTruthy(Choice) & isTruthy(ChoiceName)) { names(Choice)<-ChoiceName }
    return(Choice)
  })
  
  observe(
    updateSelectizeInput(session, inputId="ParamIn2",selected=chosen(), 
                         choices=c("Choose a Parameter"="", as.list(sort(PChoices()))))
  )
  
  return(reactive(input$ParamIn2))
}

### Site Visit Module ####

siteVisitChooserUI<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("siteVisitIn"), label="Site visit", choices=NULL)
}


siteVisitChooser<-function(input, output, session, data, park, site, years, imgs, chosen){
  PChoices<-reactive({
    req(park(), site(), years())
    park <- park()
    sites <- site()
    years <- years()
    imgs <- imgs()
    
    sitevisits <- c()
    if (park %in% names(imgs)){
      for (site in sites){
        if (site %in% names(imgs[[park]])){
          for (yr in names(imgs[[park]][[site]])){
            if (as.numeric(yr) >= as.numeric(years[1]) & as.numeric(yr) <= as.numeric(years[2])){
              for (sitevisit in names(imgs[[park]][[site]][[yr]])){
                sitevisits <- c(sitevisits, sitevisit)
              }
            }
          }
        }
      }
    }
    
    return(sitevisits)
  })
  
  observe(
    updateSelectizeInput(session, inputId="siteVisitIn",selected=chosen(), 
                         choices=c("Choose a site visit"="", as.list(sort(PChoices(), decreasing=T))))
  )
  
  return(reactive(input$siteVisitIn))
}

siteVisitChooserUI2<-function(id){
  ns<-NS(id)
  selectizeInput(inputId=ns("siteVisitIn2"), label="Site visit", choices=NULL)
}


siteVisitChooser2<-function(input, output, session, data, park, site, years, imgs, chosen){
  PChoices<-reactive({
    req(park(), site(), years())
    park <- park()
    sites <- site()
    years <- years()
    imgs <- imgs()
    
    sitevisits <- c()
    if (park %in% names(imgs)){
      for (site in sites){
        if (site %in% names(imgs[[park]])){
          for (yr in names(imgs[[park]][[site]])){
            if (as.numeric(yr) >= as.numeric(years[1]) & as.numeric(yr) <= as.numeric(years[2])){
              for (sitevisit in names(imgs[[park]][[site]][[yr]])){
                sitevisits <- c(sitevisits, sitevisit)
              }
            }
          }
        }
      }
    }
    
    return(sitevisits)
  })
  
  observe(
    updateSelectizeInput(session, inputId="siteVisitIn2",selected=chosen(), 
                         choices=c("Choose a site visit"="", as.list(sort(PChoices(), decreasing=T))))
  )
  
  return(reactive(input$siteVisitIn2))
}

#### Photo module
photoChooserUI<-function(id){
  ns<-NS(id)
  sliderInput(inputId = ns("PhotoPhoto"), label = "Photo", min = 1, max = 8, value = 1, ticks=F)
}

photoChooser<-function(input, output, session, data, park, site, years, imgs, sitevisit){
  PhotoChoices<-reactive({
    req(park(), site(), years(), imgs(), sitevisit())
    park <- park()
    sites <- site()
    years <- years()
    imgs <- imgs()
    sitevisit <- sitevisit()
    
    filenames <- c()
    if (park %in% names(imgs)){
      for (site in sites){
        if (site %in% names(imgs[[park]])){
          for (yr in names(imgs[[park]][[site]])){
            if (as.numeric(yr) >= as.numeric(years[1]) & as.numeric(yr) <= as.numeric(years[2])){
              for (visit in names(imgs[[park]][[site]][[yr]])){
                if (visit == sitevisit){
                  for (f in names(imgs[[park]][[site]][[yr]][[sitevisit]])){
                    fname <- imgs[[park]][[site]][[yr]][[sitevisit]][[f]]$rel_fpath
                    filenames <- c(filenames, fname)
                  }
                }
              }
            }
          }
        }
      }
    }
    
    return(filenames)
  })
  
  observe(
    updateSliderInput(session, inputId="PhotoPhoto", min=1,max=length(PhotoChoices()))
  )
  
  return(reactive(input$PhotoPhoto))
}

photoChooserUI2<-function(id){
  ns<-NS(id)
  sliderInput(inputId = ns("PhotoPhoto2"), label = "Photo", min = 1, max = 8, value = 1, ticks=F)
}

photoChooser2<-function(input, output, session, data, park, site, years, imgs, sitevisit){
  PhotoChoices<-reactive({
    req(park(), site(), years(), imgs(), sitevisit())
    park <- park()
    sites <- site()
    years <- years()
    imgs <- imgs()
    sitevisit <- sitevisit()
    
    filenames <- c()
    if (park %in% names(imgs)){
      for (site in sites){
        if (site %in% names(imgs[[park]])){
          for (yr in names(imgs[[park]][[site]])){
            if (as.numeric(yr) >= as.numeric(years[1]) & as.numeric(yr) <= as.numeric(years[2])){
              for (visit in names(imgs[[park]][[site]][[yr]])){
                if (visit == sitevisit){
                  for (f in names(imgs[[park]][[site]][[yr]][[sitevisit]])){
                    fname <- imgs[[park]][[site]][[yr]][[sitevisit]][[f]]$rel_fpath
                    filenames <- c(filenames, fname)
                  }
                }
              }
            }
          }
        }
      }
    }
    
    return(filenames)
  })
  
  observe(
    updateSliderInput(session, inputId="PhotoPhoto2", min=1,max=length(PhotoChoices()))
  )
  
  return(reactive(input$PhotoPhoto2))
}

#-------------------------
# Facts and images for loading screen
#-------------------------

LOADING_TEXT <- c(
  "Loading application..."
  ,"Loading application..."
  ,"Loading application..."
)

LOADING_IMAGES <- list(
  list(src= "dwq_NCRN_ANTI_SHCK_2024-06-04_20240604-131442.jpg", location = "Antietam National Battlefield, Sharpsburg Creek", date = "June 4, 2024")
  ,list(src= "dwq_NCRN_MONO_BUCK_2024-06-04_20240604-084406.jpg", location = "Monocacy National Battlefield, Bush Creek", date = "June 4, 2024")
  ,list(src= "dwq_NCRN_PRWI_BONE_2024-06-11_20240611-130821.jpg", location = "Prince William Forest Park, Boneyard Run", date = "June 11, 2024")
)


#### Map Module ####
# 
# mapChooserUI<-function(id){
#   ns<-NS(id)
#   selectizeInput(inputId=ns("MapIn"),label="Select Parks:" , choices=NULL, multiple = TRUE, selected = NULL)
# }
# 
# mapChooser<-function(input,output,session, data, park, chosen){
#   observe({updateSelectizeInput(session, "MapIn", selected=chosen(),
#                                 choices=c("Choose a Park"="", c(`names<-`(getParkInfo(data, info="ParkCode"), getParkInfo(data, info="ParkShortName"))))
#   )})
#   return(reactive(input$MapIn))
# }


# mapChooserUI<-function(id){
#   ns<-NS(id)
#   checkboxGroupInput(inputId=ns("MapIn"),label="Select Parks:" , choices=NULL, inline = TRUE)
# }
# 
# mapChooser<-function(input,output,session, data, park, chosen){
#   observe({updateCheckboxGroupInput(session, "MapIn",
#                                 choices=c("Choose a Park"="", c(`names<-`(getParkInfo(data, info="ParkCode"), getParkInfo(data, info="ParkShortName"))))
#   )})
#   return(reactive(input$MapIn))
# }
# 
