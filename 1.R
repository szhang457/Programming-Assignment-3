outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

best <- function(state, outcome) {
## Read outcome data
data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
if(outcome=="heart attack") {
	column<-11}
else if(outcome=="heart failure"){
	column<-17}
else if(outcome=="pneumonia"){
	column<-23}
v1<-data$State==state
data1<-data[,c(2,column)][v1,]
data1[,2]<-as.numeric(data1[,2])
## Check that state and outcome are valid

## Return hospital name in that state with lowest 30-day death rate
data2<-data1[order(data1[,2],data1[,1]),]
data2[1,1]
}

rankhospital <- function(state, outcome, num = "best") {
## Read outcome data
## Read outcome data
data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
if(outcome=="heart attack") {
	column<-11}
else if(outcome=="heart failure"){
	column<-17}
else if(outcome=="pneumonia"){
	column<-23}
v1<-data$State==state
data1<-data[,c(2,column)][v1,]
data1[,2]<-as.numeric(data1[,2])

## Check that state and outcome are valid
## Return hospital name in that state with the given rank 30-day death rate
if(num == "worst"){
data2<-data1[order(-data1[,2],data1[,1]),]
data2[1,1]}
else if(num== "best"){
data2<-data1[order(data1[,2],data1[,1]),]
data2[1,1]}
else {
data2<-data1[order(data1[,2],data1[,1]),]
data2[num,1]
}
}


rankall <- function(outcome, num = "best") {
	result<-matrix(,ncol=2)
	data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
	if(outcome=="heart attack") {
		column<-11}
	else if(outcome=="heart failure"){
		column<-17}
	else if(outcome=="pneumonia"){
		column<-23}
	for(i in unique(data$State)){
		v1<-data$State==i
		data1<-data[,c(7,2,column)][v1,]
		data1[,3]<-as.numeric(data1[,3])
		if(num == "worst"){
			data2<-data1[order(-data1[,3]),]
			temp<-as.vector(data2[1,2],i)}
		else if(num== "best"){
			data2<-data1[order(data1[,3]),]
			temp<-as.vector(data2[1,2],i)}
		else {
			data2<-data1[order(data1[,3]),]
			temp<-as.vector(data2[num,2],i)}
		result<-rbind(result,as.matrix(temp))}
	result[order(result[,1]),]
}
