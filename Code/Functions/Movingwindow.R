########## Aggregate Function ###########

### Creating some fake data
julianday<- seq(120, 155, by = 1)
speed<- runif(36, 0.1, 15)
dataset<- data.frame(julianday, speed)


## Setting up moving window function -- the data set supplied must have a column labeled "julianday". The function can deal with NA values in covariate column, this is the column with speed, height or whatever column you want to calculate the mean. 

moving.window<- function(dataset, covariate.column, window.size) {
	unique.days<- unique(dataset$julianday)
	n<- window.size
	result<- vector(length = length(unique.days))
for (i in 1:length(unique.days)) {
	sv<- unique.days[i]
	max.date<- sv + n
	min.date<- sv - n
		if(min.date < (min(dataset$julianday, na.rm = TRUE))){
			data.window<- data[c(which(dataset$julianday == sv):which(dataset$julianday == max.date)),]
			result[i]<- mean(data.window[,covariate.column], na.rm = TRUE)
		} else {
			if(max.date >= max(dataset$julianday, na.rm = TRUE)) {
				data.window<- data[c(which(dataset$julianday == min.date):which(dataset$julianday == max(dataset$julianday, na.rm = TRUE))),]
				result[i]<- mean(data.window[,covariate.column], na.rm = TRUE)
			} else {
			data.window<- data[c(which(dataset$julianday == min.date):which(dataset$julianday == max.date)),]
			result[i]<- mean(data.window[,covariate.column], na.rm = TRUE)
			}
		}
	}
	result
}

a<- moving.window(dataset = dataset, covariate.column = 2, window.size = 3)