rankhospital <- function(state,outcome, rank="best") {
        
        
        ##read outcomedata
        outcomeList<-read.csv("c:\\RData\\outcome-of-care-measures.csv", as.is=TRUE)
        
        ##if (outcome!=c("heart attack, heart failure, pneumonia"))
          ##      result<-stop(c("invalid outcome"))
        
        ##Check the state
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


if ( rank <= nrow(mortalities) | rank == "worst"){
         
        ppp<-subset(mortalities, mortality != "Not Available")
        
        ##get mortality data
        mortal<- subset(ppp, select = mortality)
        
        ##Return hospital name in the state with the given rank 30-day death rate
        if ( rank == "worst"){rank = nrow(mortal)}        
        
        list<-head(sort(as.numeric(mortal$mortality), decreasing = FALSE),rank)
        
        
        ##result <- subset(ppp, ppp$mortality==minData$mortality)
        
        ##head(result,1)$hospital
        index<-as.character(list[length(list)])
        inum<-as.numeric(index)
        data<-format(round(inum), nsmall = 1)
        result <- subset(ppp, ppp$mortality==data)$hospital
        result
}else{
        result<-c("NA")
        result
        
}        
}