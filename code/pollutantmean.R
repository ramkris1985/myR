pollutantmean <- function(directory, pollutant, id=1:332) {

	fpath <- paste(getwd(),"/",directory,"/", sep="")
	file_lst <- as.vector(dir(fpath))

	pm_data <- read.csv(paste(fpath,file_lst[id[1]],sep=""), header=TRUE, col.names=c("Date","sulfate","nitrate","ID"))

	if(length(id) > 1) {
		for ( n in id[2]:id[length(id)] ) {
		pm_data <- rbind(pm_data,read.csv(paste(fpath,file_lst[n],sep=""), header=TRUE, col.names=c("Date","sulfate","nitrate","ID")))
		}
	}	

	mean(pm_data[,pollutant],na.rm=TRUE)
}
