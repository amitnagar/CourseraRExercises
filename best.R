best <- function(state,outcome) {
        ##read outcomedata
        outcomeList<-read.csv("c:\\RData\\outcome-of-care-measures.csv", as.is=TRUE)
        names(outcomeList)
        ##Subset based on state
        ##sub<-subset(outcomeList,outcomeList$State == state, select = 'Hospital.Name','Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack')
        
        if (outcome == "heart attack")
        {
                mortalities<-subset(outcomeList,outcomeList$State == state, select = c(1, 2,11))
                
                if ( nrow(mortalities) > 0 ){
                  ##change column names
                  colnames(mortalities)<-c('provider','hospital', 'mortality')                
                }
                  else {
                          c<-stop("invalid state")
                  }
                        
        }
        
        if (outcome == "heart failure")
        {
                mortalities<-subset(outcomeList,outcomeList$State == state, select = c(1,2,17))
                if ( nrow(mortalities) > 0 ){
                  ##change column names
                  colnames(mortalities)<-c('provider','hospital', 'mortality')                
                }
                else {
                        c<-stop("invalid state")
                }
                
        }
        if (outcome == "pneumonia")
        {
                mortalities<-subset(outcomeList,outcomeList$State == state, select = c(1,2,23))
                if ( nrow(mortalities) > 0 ){
                ##change column names
                colnames(mortalities)<-c('provider','hospital', 'mortality')                
                }
                else {
                        c<-stop("invalid state")
                }
        }

        ppp<-subset(mortalities, mortality != "Not Available")
        
        mortal<- subset(ppp, select = mortality)
        ##minData<-lapply(mortal,min)
        head(sort(as.numeric(mortal$mortality), decreasing = FALSE),1)
        
        #result <- subset(ppp, ppp$mortality==minData$mortality)
        result <- subset(ppp, ppp$mortality==head(sort(as.numeric(mortal$mortality), decreasing = FALSE),1))
        head(result,1)$hospital
        
}