best<-function(state,outcome){
  out<-read.csv("/home/robla/Desktop/Coursera/DataScience/ProgrammingAssignment3/outcome-of-care-measures.csv")
  if(outcome=="heart attack")
    sub<-out[out$State==state,c(2,7,11)]
  if(outcome=="heart failure")
    sub<-out[out$State==state,c(2,7,17)]
  if(outcome=="pneumonia")
    sub<-out[out$State==state,c(2,7,23)]
  sub[,3]<-as.numeric(sub[,3])
  colnames(sub)[3]="Mortality"
  low<-min(sub[,3],na.rm=TRUE)
  hosp<-sub$Hospital.Name[sub$Mortality==low]
  print(hosp)
}

best("AK", "pneumonia")
