rankall <- function(outcome, num="best") {
        
        
        ##read outcomedata
        outcomeList<-read.csv("c:\\RData\\outcome-of-care-measures.csv", as.is=TRUE)
        
        
        ##Check the state
        if (outcome == "heart attack")
        {
                mortalities<-subset(outcomeList, select = c(7, 2,11))
                
                if ( nrow(mortalities) > 0 ){
                        ##change column names
                        colnames(mortalities)<-c('state','hospital', 'mortality')                
                }
                
                else {
                        c<-stop("invalid state")
                }
                
                states<-unique(mortalities$state)
                state<-c()
                hospital<-c()
                for (i in states){
                    mortalities.in.state<-subset(mortalities,mortalities$state == i, select = c(1, 2,3))  
                    mortalities.in.state.no.NA<-subset(mortalities.in.state, mortality != "Not Available")  
                    mortalities.in.state.num<- subset(mortalities.in.state.no.NA, select = mortality)
                    list<-head(sort(as.numeric(mortalities.in.state.num$mortality), decreasing = FALSE),num)
                    index<-as.character(list[length(list)])
                    inum<-as.numeric(index)
                    data<-format(inum, nsmall = 1)
                    
                    num.hospitals = subset(mortalities.in.state.no.NA, mortalities.in.state.no.NA$mortality==data)
                    sorted.num.hospitals<-num.hospitals[with(num.hospitals, order(hospital)), ]
                    hospital <- append(hospital,sorted.num.hospitals$hospital)
                    for (j in 1:nrow(num.hospitals)){
                        state<-append(state,i)
                    }
                }
                my.result <- data.frame(hospital = hospital, state = state)
                
        }
        
        if (outcome == "heart failure")
        {
                mortalities<-subset(outcomeList, select = c(7,2,17))
                if ( nrow(mortalities) > 0 ){
                        ##change column names
                        colnames(mortalities)<-c('state','hospital', 'mortality')                
                }
                else {
                        c<-stop("invalid state")
                }
                states<-unique(mortalities$state)
                state<-c()
                hospital<-c()
                for (i in states){
                        mortalities.in.state<-subset(mortalities,mortalities$state == i, select = c(1, 2,3))  
                        mortalities.in.state.no.NA<-subset(mortalities.in.state, mortality != "Not Available")  
                        mortalities.in.state.num<- subset(mortalities.in.state.no.NA, select = mortality)
                        list<-head(sort(as.numeric(mortalities.in.state.num$mortality), decreasing = FALSE),num)
                        index<-as.character(list[length(list)])
                        inum<-as.numeric(index)
                        data<-format(inum, nsmall = 1)
                        
                        num.hospitals = subset(mortalities.in.state.no.NA, mortalities.in.state.no.NA$mortality==data)
                        sorted.num.hospitals<-num.hospitals[with(num.hospitals, order(hospital)), ]
                        hospital <- append(hospital,sorted.num.hospitals$hospital)
                        for (j in 1:nrow(num.hospitals)){
                                state<-append(state,i)
                        }
                }
                my.result <- data.frame(hospital = hospital, state = state)
                
                
        }
        if (outcome == "pneumonia")
        {
                mortalities<-subset(outcomeList, select = c(7, 2,23))
                if ( nrow(mortalities) > 0 ){
                        ##change column names
                        colnames(mortalities)<-c('state','hospital', 'mortality')                
                }
                else {
                        c<-stop("invalid state")
                }
                
                states<-unique(mortalities$state)
                state<-c()
                hospital<-c()
                for (i in states){
                        mortalities.in.state<-subset(mortalities,mortalities$state == i, select = c(1, 2,3))  
                        mortalities.in.state.no.NA<-subset(mortalities.in.state, mortality != "Not Available")  
                        mortalities.in.state.num<- subset(mortalities.in.state.no.NA, select = mortality)
                        
                        list<-head(sort(as.numeric(mortalities.in.state.num$mortality), decreasing = FALSE),num)
                        index<-as.character(list[length(list)])
                        inum<-as.numeric(index)
                        data<-format(inum, nsmall = 1)
                        if ( num == "worst") {num = dim(mortalities.in.state.no.NA)[1]}
                        num.hospitals = subset(mortalities.in.state.no.NA, mortalities.in.state.no.NA$mortality==data)
                        sorted.num.hospitals<-num.hospitals[with(num.hospitals, order(hospital)), ]
                        hospital <- append(hospital,sorted.num.hospitals$hospital)     
                        
                        for (j in 1:nrow(num.hospitals)){
                                state<-append(state,i)
                        }
                }
                my.result <- data.frame(hospital = hospital, state = state)
                
        }

        my.result
}