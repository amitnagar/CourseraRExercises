pollutantmean <- function(directory, pollutant, id) {
        data<-list.files("C:\\RData\\wk2\\specdata")
        
        ##smeans<-c(id)
        ##nmeans<-c(id)
        
        v<-as.vector(id)
        means<-vector(length=length(v))
        
        for(i in 1:length(v)){
                
                flocation<-paste("C:\\RData\\wk2\\specdata", data[id[i]],sep="\\")
                csvdata<-read.csv(flocation,header=TRUE)
                csvdataNoNA<-na.omit(csvdata)
                if (pollutant == "sulfate"){
                        means[i]<-mean(csvdataNoNA$sulfate,na.rm=TRUE)
                }else{
                        means[i]<-mean(csvdataNoNA$nitrate,na.rm=TRUE)
                }
        }
        
        if (pollutant == "sulfate"){
                mean<-mean(means,na.rm=TRUE)
        }else{
                mean<-mean(means,na.rm=TRUE)
        }
        
        mean
        
}
