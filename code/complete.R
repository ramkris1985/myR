complete <- function(directory, id = 1:332) {

	fpath <- paste(getwd(),"/",directory,"/", sep="")
	file_lst <- as.vector(dir(fpath))
	id_v <- numeric()
	cc_v <- numeric()

 	for ( n in id) {
                 
 		pm_data <- read.csv(paste(fpath,file_lst[n],sep=""), header=TRUE, col.names=c("Date","sulfate","nitrate","ID"))
 		cc <- sum(complete.cases(pm_data))
 		id_v <- c(id_v,n)
 		cc_v <- c(cc_v,cc)
 	}
 
 	df_result <- data.frame(id=id_v, nobs=cc_v)
 	df_result
}
