corr <- function(directory, threshold=0) {

	fpath <- paste(getwd(),"/",directory,"/", sep="")
	file_lst <- as.vector(dir(fpath))
	corr_v <- numeric()

	for ( n in 1:length(file_lst) ) {
		pm_data <- read.csv(paste(fpath,file_lst[n],sep=""), header=TRUE, col.names=c("Date","sulfate","nitrate","ID"))
		cc <- complete.cases(pm_data)
                ccs <- sum(cc)
		if(ccs >= threshold) {
			co <- cor(pm_data[cc,][,"sulfate"],pm_data[cc,][,"nitrate"])
			corr_v <- c(corr_v, co)
		}
	}	

        corr_v
}

