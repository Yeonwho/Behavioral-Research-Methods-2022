
library(foreach)
library(doParallel)
n_core=detectCores()
cl = makeCluster(n_core-1)
registerDoParallel(cl)

data_clean<-read.csv("Eventdata.csv")
Estudents<-unique(data_clean$ID)

p<-3963
Lmax<-7
action_2<-read.csv("action2_forcheck.csv",na.strings = c(""," ", NA))
number_of_actions<-nrow(action_2)
View(action_2)

#########################################################################
#### This part is from Juanita Hicks In NCME 2022 (Training Session).####
#### It is to exclude duplication that may occur ########################
library(data.table)

table<-action_2[,c(2:8)]

table<-as.data.table(table)
table[, table(duplicated(table))]
table[, isDupe := duplicated(table)]
table[, table(isDupe)]

action_2<- table[isDupe == FALSE, -'isDupe']
#####
########################################################################
####### Let action_2 have Action number & Description ################## 
for (i in 1:number_of_actions){
  
  action_2[i,"ID"] <- paste0("ACTION",i)
  action_2[i,"Description"] <- ""
  for (j in 1:Lmax) {
    action_2[i,"Length"] = Lmax
    if ( is.na(action_2[i,paste0("A",j)]) ) {
      action_2[i,"Length"] = (j-1)
      break
    }
    if ( action_2[i,paste0("A",j)] == "" ) {
      action_2[i,"Length"] = (j-1)
      break
    }
    action_2[i,"Description"] <- paste(action_2[i,"Description"], action_2[i,paste0("A",j)])
  }
}




#rm(ResultDf_Number)
ResultDf_Number     <- data.frame("ACTION1" = rep(0,p))

######  ResultDf_Number #####
for (i in 1:number_of_actions){
  ResultDf_Number[paste0("ACTION",i)] <- rep(0,p)
}


result <- foreach (i=1:p, .combine=rbind) %dopar% {
  ResultDf_Number_i   <- ResultDf_Number[i,]
  ResultDf_Number_i["Student"] <- i
  temporary<-data_clean[which(data_clean$ID==Estudents[i]),]
  n<-length(temporary$event_value)
  for (k in 1:number_of_actions) {
    for (j in 1:n){
      tempcount <- 0
      for (x in 1:action_2[k,"Length"]) {
        if (j+(x-1)<=n) {
          if (temporary$event_value[j+(x-1)]==action_2[k,paste0("A",x)]) {
            tempcount <- tempcount + 1
          }
        }
      }
      if (tempcount == action_2[k,"Length"] ) {
        ResultDf_Number_i[paste0("ACTION",k)] <- ResultDf_Number_i[paste0("ACTION",k)]+1
      }
    }
    
  }
  return(ResultDf_Number_i)

}

ResultDf_Number <- result

#stopCluster(cl)