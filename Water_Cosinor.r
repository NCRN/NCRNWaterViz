###Function shoud take in dataframe, date, measurement, formula and output a regression/cosinor

library(season)
library(mgcv)

W.Cosinor<-function(Df.In, DateVar, Measure, Formula) {       #This function takes in a datset, the name of the column with the date and the measurements, and the formula for the cosinor analysis  
  
   Cos.Out<-vector(mode="list", length=4)
   names(Cos.Out)<-c("Analysis","CDates","PredLine","Outliers")            #Cos.Out is the output of the function - a list
   #Df.In[,DateVar]<-as.Date(strftime(Df.In[,DateVar],"%Y/%m/%d"))
   
 if(sum(!is.na(Df.In[eval(Measure)]))<24)  #this checks to make sure there is enough data - 24 measurememnts
     
      return(NA)                #returns NA if there is not enough data

  else { 
    #Reg1<-cosinor(formula=Formula, date=Df.In[,DateVar], data=Df.In,type="daily") #does first test to see if consinor or lm is a better fit
    Reg1<-cosinor(formula=Formula, date=DateVar, data=Df.In,type="daily")
    
    if (summary(Reg1)$significant == TRUE ) {           #if this staatement is true, then consinor is a better fit - seasonal data
      Frac<-yrfraction(Df.In[,DateVar],type="daily")    #for cosw and sinw  to determine where in the year you are
      Df.In$cosw<-cos(Frac*2*pi)                         #for cosinor glm
      Df.In$sinw<-sin(Frac*2*pi)                          # for cosinor glm
      TempCos<-glm(formula=update.formula(Formula, ~.+sinw+cosw), data=Df.In, na.action=na.exclude)  #re-does the cosinor, outside of cosinor function
      Rstud<-rstudent(TempCos)                   #studentized residuals
      Df.In$NewMeas<-Df.In[,eval(Measure)]            #copy result values   to new column for redoing analysis minus outliers
      Df.In$Outie<-FALSE                       #Will mark if there are any Outliers
     
      if(max(abs(Rstud), na.rm=TRUE)>=4){      #check to see if any studentized residual < 4,
        Df.In$NewMeas[which(abs(Rstud)>=4)]<-NA  # replaces outliers with NA 
        Df.In$Outie[which(abs(Rstud)>=4)]<-TRUE   #Marks Outleirs
      } 
    
    }
    
    
    else{
      Reg1<-lm(formula=Formula, data=Df.In, na.action=na.exclude)     #if there is no seasonal pattern, then do a lm
      Rstud<-rstudent(Reg1)                   #studentized residuals
      Df.In$NewMeas<-Df.In[,eval(Measure)]           #copy result values
      Df.In$Outie<-FALSE                       #Will mark if there are any Outliers
         
      if(max(abs(Rstud), na.rm=TRUE)>=4){      #check to see if any studentizedresidual < 4,
        Df.In$NewMeas[which(abs(Rstud)>=4)]<-NA #put in NA for the big outliers identified above
        Df.In$Outie[which(abs(Rstud)>=4)]<-TRUE #marks outliers
       }  
    
    }
  
  #redo analayis and feed to output
    Formula2<-update.formula(Formula, eval(NewMeas)~.)                               #now do analysis with updated data
  #  Reg2<-cosinor(formula=Formula2, date=Df.In[,DateVar], data=Df.In, type="daily") #does second test to see if consinor or lm is a better fit
  Reg2<-cosinor(formula=Formula2, date=DateVar, data=Df.In, type="daily")
  if (summary(Reg2)$significant == TRUE ) {              #if cosinor is the better choice do it.
        Frac<-yrfraction(Df.In[,DateVar],type="daily")    #for cosw and sinw
        Df.In$cosw<-cos(Frac*2*pi)                         #for cosinor glm
        Df.In$sinw<-sin(Frac*2*pi)
        TempCos2<-glm(formula=update.formula(Formula2, ~.+sinw+cosw), data=Df.In, na.action=na.exclude)    #now do cosinor via glm
        
        #Make predicted values for plotting
        PreLen<-as.numeric(length(seq(from=min(Df.In[,DateVar],na.rm=TRUE),to=max(Df.In[,DateVar],na.rm=TRUE),by=1)))       #length of the data to be used for predicitons
        PreDates<-data.frame(matrix(ncol=4,nrow=PreLen))                                                                    #data from for predicitons
        colnames(PreDates)<-c(eval(DateVar),"Frac","sinw","cosw")
        PreDates[,DateVar]<-seq(from=min(Df.In[,DateVar],na.rm=TRUE), to=max(Df.In[,DateVar],na.rm=TRUE), by=1)          #add in dates with data
        PreDates$Frac<-yrfraction(PreDates[,DateVar],type="daily")
        PreDates$sinw<-sin(PreDates$Frac*2*pi)
        PreDates$cosw<-cos(PreDates$Frac*2*pi)
        Preds<-predict(TempCos2,newdata=PreDates,na.action=na.exclude)                                                 #make predicitons
        
        Cos.Out$Analysis<-Reg2
        Cos.Out$Cdates<-Df.In[!is.na(Df.In$NewMeas),DateVar]
        Cos.Out$PredLine<-data.frame(PreDates=PreDates,Preds=Preds)
        Cos.Out$Outliers<-Df.In[Df.In$Outie==TRUE,c(DateVar,eval(Measure))]
        return(Cos.Out)
     }
    
    else{
      Reg3<-lm(formula=Formula2, data=Df.In, na.action=na.exclude)
      Cos.Out$Analysis<-Reg3
      Cos.Out$CDates<-Df.In[!is.na(Df.In$NewMeas),DateVar]
      Cos.Out$Outliers<-Df.In[Df.In$Outie==TRUE,c(DateVar,eval(Measure))]#[,c(eval(Measure),DateVar)]
      return(Cos.Out)
    }
  
  }
}
