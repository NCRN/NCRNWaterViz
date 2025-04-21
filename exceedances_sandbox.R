# create table from one site, parameter

library(NCRNWater)
WaterData<-NCRNWater::importNCRNWater("Data/NCRN", Data="wqp.csv", MetaData = "wqp_ncrnwater_metadata.csv", wqx=T)
mydata <- NCRNWater::getWData(WaterData, parkcode="GWMP", sitecode="NCRN_GWMP_TURU", charname="TotalP")
filtereddata <- mydata[, c("OrganizationFormalName", "ActivityMediaSubdivisionName", "Date", "Characteristic", "Value", "ResultMeasure.MeasureUnitCode")]
filtereddata <- subset(filtereddata, !is.na(Value))

LowerThreshold<- NCRNWater::getCharInfo(WaterData, parkcode = "GWMP", sitecode = "NCRN_GWMP_TURU", charname = "TotalP", info="LowerDescription")
UpperThreshold<- NCRNWater::getCharInfo(WaterData, parkcode = "GWMP", sitecode = "NCRN_GWMP_TURU", charname = "TotalP", info="UpperDescription")
LowerPoint<- NCRNWater::getCharInfo(WaterData, parkcode = "GWMP", sitecode = "NCRN_GWMP_TURU", charname = "TotalP", info="LowerPoint")
UpperPoint<- NCRNWater::getCharInfo(WaterData, parkcode = "GWMP", sitecode = "NCRN_GWMP_TURU", charname = "TotalP", info="UpperPoint")
Unit<- NCRNWater::getCharInfo(WaterData, parkcode = "GWMP", sitecode = "NCRN_GWMP_TURU", charname = "TotalP", info="Units")
sitename<- NCRNWater::getCharInfo(WaterData, parkcode = "GWMP", sitecode = "NCRN_GWMP_TURU", charname = "TotalP", info = "SiteName")
characteristic<- NCRNWater::getCharInfo(WaterData, parkcode = "GWMP", sitecode = "NCRN_GWMP_TURU", charname = "TotalP", info = "DisplayName")



#if(!is.na(LowerPoint)) {
#  if(any(filtereddata$Value <= LowerPoint, na.rm = TRUE)) {
#    lowerdata <- filtereddata[filtereddata$Value < LowerPoint, ]
#    lowerdata$LowerThreshold <- LowerThreshold
#  }
#  else{
#    lowerdata<- filtereddata[0, ]
#  }
#  print("lower ran")
#}

# no if lower 
if(any(filtereddata$Value <= LowerPoint, na.rm = TRUE)) {
  lowerdata <- filtereddata[filtereddata$Value < LowerPoint, ]
  lowerdata$LowerThreshold <- LowerThreshold
} else{
  lowerdata<- filtereddata[0, ]
}
 
#if(!is.na(UpperPoint)) {
#  if(any(filtereddata$Value >= UpperPoint, na.rm = TRUE)) {
#    upperdata <- filtereddata[filtereddata$Value > UpperPoint, ]
#    upperdata$UpperThreshold <- UpperThreshold
#  } 
#  else {
#    upperdata<- filtereddata[0, ]
#  }
#  print("upper ran")
#}

# no if upper
if(any(filtereddata$Value >= UpperPoint, na.rm = TRUE)) {
  upperdata <- filtereddata[filtereddata$Value > UpperPoint, ]
  upperdata$UpperThreshold <- UpperThreshold
} else {
  upperdata<- filtereddata[0, ]
}


#if(is.na(LowerPoint) & is.na(UpperPoint)) {
#  nadata<- filtereddata[0, ]
#}

exdata<- dplyr::bind_rows(lowerdata, upperdata)
exdata<- exdata %>%
  arrange(desc(Date))

library(gt)
sitename<- NCRNWater::getCharInfo(WaterData, parkcode = "GWMP", sitecode = "NCRN_GWMP_TURU", charname = "TotalP", info = "SiteName")
characteristic<- NCRNWater::getCharInfo(WaterData, parkcode = "GWMP", sitecode = "NCRN_GWMP_TURU", charname = "TotalP", info = "DisplayName")
gt_table <- filtereddata %>%
  gt() %>%
  tab_header(paste0(sitename, " ", characteristic, " Exceedances"))
gt_table

# refactor DataUse

ExceedancesDataUse<-reactive({ 
  shiny::validate(
    need(DataOpts$Park, message="Choose a Park"),
    need(DataOpts$Site, message="Choose a Site"),
    need(DataOpts$Param, message="Choose a Water Quality Parameter")
  )  
  df2 <- getWData(WaterData, parkcode=DataOpts$Park, sitecode=DataOpts$Site, charname=DataOpts$Param)
  df1 <- suppressWarnings(df2 %>% mutate(year.dec = julian(Date)/365, month = as.factor(months(Date))) %>% 
                           group_by(month) %>% mutate(num_meas = sum(!is.na(Value))) %>% 
                           ungroup()) %>% mutate(num_mos = length(unique(month)))
  df <- df1[, c("OrganizationFormalName", "ActivityMediaSubdivisionName", "Date", "Characteristic", "Value", "ResultMeasure.MeasureUnitCode")]
  
  LowerThreshold<- NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site, charname = DataOpts$Param, info="LowerDescription")
  UpperThreshold<- NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site, charname = DataOpts$Param, info="UpperDescription")
  LowerPoint<- NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site, charname = DataOpts$Param, info="LowerPoint")
  UpperPoint<- NCRNWater::getCharInfo(WaterData, parkcode = DataOpts$Park, sitecode = DataOpts$Site, charname = DataOpts$Param, info="UpperPoint")
  
  if(!is.na(LowerPoint)) {
    df <- df[df$Value < LowerPoint, ]
    df$LowerThreshold <- LowerThreshold
  }
  
  if(!is.na(UpperPoint)) {
    df <- df[df$Value > UpperPoint, ]
    df$UpperThreshold <- UpperThreshold
  }
  
  if(is.na(LowerPoint) & is.na(UpperPoint)) {
    df$Threshold <- "There is no recorded water quality threshold for this parameter and site"
  }

  
  return(df)
  
})


### histogram and text data wrangling ###
library(dplyr)
library(lubridate)
histyears<- data.frame(Year = lubridate::year(filtereddata$Date))
totcount<- histyears %>%
  dplyr::count(Year) %>%
  dplyr::rename("ntot" = "n")
totcount <- subset(totcount, !is.na(Year))

excount <- exdata %>%
  mutate(Year = lubridate::year(Date)) %>%
  count(Year) %>%
  dplyr::rename("nex" = "n")
excount <- subset(excount, !is.na(Year))

histdata<- dplyr::left_join(totcount, excount, by = "Year")
histdata[is.na(histdata)] <- 0
histdata<- histdata %>%
  mutate(percent_ex = (nex / ntot) * 100) %>%
  mutate(formatted_percent_ex = scales::percent(percent_ex / 100, accuracy = 0.01))

ExPoint<- dplyr::case_when(
  is.na(UpperPoint) == TRUE & !is.na(LowerPoint) == TRUE ~ LowerPoint
  ,!is.na(UpperPoint) == TRUE & is.na(LowerPoint) == TRUE ~ UpperPoint
  ,!is.na(UpperPoint) == TRUE & !is.na(LowerPoint) == TRUE & any(exdata$Value <= LowerPoint, na.rm=TRUE) & all(exdata$Value < UpperPoint, na.rm=TRUE) ~ LowerPoint
  ,!is.na(UpperPoint) == TRUE & !is.na(LowerPoint) == TRUE & any(exdata$Value >= UpperPoint, na.rm=TRUE) & all(exdata$Value > LowerPoint, na.rm=TRUE) ~ UpperPoint
  # ,!is.na(UpperPoint) == TRUE & !is.na(LowerPoint) == TRUE & any(exdata$Value >= UpperPoint, na.rm=TRUE) & any(exdata$Value <= LowerPoint, na.rm=TRUE) ~ paste0(LowerPoint, " and ", UpperPoint)
)



### histogram plotting ###

library(ggplot2)
library(plotly)
p<- ggplot(histdata, aes(x = Year, y = percent_ex,
                         text = paste0(Year, ", ", characteristic, "\n", ntot, " total observation(s)", "\n", formatted_percent_ex, " of observations exceeding ", ExPoint, " ", Unit))) +
    geom_bar(stat = "identity", fill = "lightgray") +
    ylim(0, 100) +
    labs(
      title = paste0("Frequency of exceedance per observation of ", characteristic, " at ", sitename),
      x = "Year",
      y = "% exceedance") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())

p_text<- ggplotly(p, tooltip = "text")
p_text


# reactive text

exceedances_text<- reactive({
  
# There were [nex] exceedances of the [param] water quality threshold among [ntot] observations at [site] in [current year].
# There have been [sum nex] exceedances of the [param] threshold among [sum ntot] observations at [site] from 2005 to [current year].
# The frequency of exceedances per observation in [current year] of [nex/ntot %]is [higher than/lower than/the same as] the frequency of [nex/ntot %] since monitoring began in 2005.
  
  recent_year<- max(histdata$Year)
  oldest_year<- min(histdata$Year)
  nex<- histdata[histdata$Year == recent_year, "nex"]
  ntot<- histdata[histdata$Year == recent_year, "ntot"]
  recent_freq<- sprintf("%.2f%%", (nex/ntot)*100)
  sum_nex<- sum(histdata$nex)
  sum_ntot<- sum(histdata$ntot)
  sum_freq<- sprintf("%.2f%%", (sum_nex/sum_ntot)*100)
  freq_comp<- ifelse(recent_freq > sum_freq, "greater than",
                     ifelse(recent_freq == sum_freq, "equal to", "less than"))
  
  
  HTML(paste0(
    "<p><b>Exceedances Summary:</b></p>",
    "<p>There were ", nex, " exceedances of the ", characteristic, " water quality threshold among ", ntot, " observations at ", sitename, " in ", recent_year, ".<p>",
    "<p>There have been ", sum_nex, " exceedances of the ", characteristic, " water quality threshold among ", sum_ntot, " observations at ", sitename, " from ", oldest_year, " to ", recent_year, ".<p>",
    "<p>The frequency of exceedances per observation in ", current_year, " of ", recent_freq, " is ", freq_comp, " the exceedance frequency of ", sum_freq, "since monitoring began in", oldest_year, ".<p>"))
  
})

output$exceedances_summary<- renderText({
  exceedances_text()
})



# HTML bullets

# # Writing
HTML(paste0(
  "<p><b>Exceedances Summary:</b></p>",
  "<p>", grammar1, Characteristic, " water quality threshold among ", ntot, " observations at ", Sitename, " in ", recent_year, ".<p>",
  "<p>", grammar2, Characteristic, " water quality threshold among ", sum_ntot, " observations at ", Sitename, " since monitoring began in ", oldest_year, ".<p>",
  "<p><u>", recent_freq, "</u>", " of observations exceeded the water quality threshold in ", recent_year, ", ", freq_comp, " the overall exceedance percentage of ", "<u>", sum_freq, "</u>", ".<p>"))

bullets<- c(
  paste0(grammar1, Characteristic, " water quality threshold among ", ntot, " observations at ", Sitename, " in ", recent_year, ".")
  ,paste0(grammar2, Characteristic, " water quality threshold among ", sum_ntot, " observations at ", Sitename, " since monitoring began in ", oldest_year, ".")
  ,paste0("<u>", recent_freq, "</u>", " of observations exceeded the water quality threshold in ", recent_year, ", ", freq_comp, " the overall exceedance percentage of ", "<u>", sum_freq, "</u>", ".")
)

summary_bullets<- paste0("<ul>", paste0("<li>", bullets, "<li>"), collapse = "")
summary_bullets<- paste0(summary_bullets, "</ul>")

HTML(paste0(
  "<p><b>Exceedances Summary:</b></p>",
  summary_bullets
))

## define function SummarizeExceedances() ##

SummarizeExceedances<- function(df, exdf){
  histyears<- data.frame(Year = lubridate::year(df$Date))
  totcount<- histyears %>%
    dplyr::count(Year) %>%
    dplyr::rename("ntot" = "n")
  totcount <- subset(totcount, !is.na(Year))
  
  excount <- exdf %>%
    mutate(Year = lubridate::year(Date)) %>%
    count(Year) %>%
    dplyr::rename("nex" = "n")
  excount <- subset(excount, !is.na(Year))
  
  histdata<- dplyr::left_join(totcount, excount, by = "Year")
  histdata[is.na(histdata)] <- 0
  histdata<- histdata %>%
    dplyr::mutate(percent_ex = (nex / ntot) * 100) %>%
    dplyr::mutate(formatted_percent_ex = scales::percent(percent_ex / 100, accuracy = 0.01))
  
  return(histdata)
}


# check hist output class




