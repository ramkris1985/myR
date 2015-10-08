rankall <- function(outcome, num = "best") {

	## Read outcome data
	ho <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check state and outcome are valid
	unique_state <- sort(unique(ho$State))
	valid_outcome <- c("heart attack", "heart failure", "pneumonia")


	if( sum( sapply( valid_outcome, function(x) { identical(x,outcome) } ) ) < 1 ) {
		stop("Invalid Outcome")
	}

	if(num  != "best" & num!= "worst" & is.numeric(num)==FALSE) 	{
		stop("Invalid Num")
	}

	## Return hospital name in that state with given rank 30-day death rate

	if( outcome == valid_outcome[1] )	{
		ha_valid_data   <- complete.cases(ho[ ,11])
		valid_hospi_nm  <- ho[ha_valid_data,][,2]
		valid_state     <- ho[ha_valid_data,][,7]
		valid_hospi_val <- ho[ha_valid_data,][,11]
	}

	else if( outcome == valid_outcome[2] )	{
		ha_valid_data <- complete.cases(ho[ ,17])
		valid_hospi_nm  <- ho[ha_valid_data,][,2]
		valid_state     <- ho[ha_valid_data,][,7]
		valid_hospi_val <- ho[ha_valid_data,][,17]
	}
	else if( outcome == valid_outcome[3] )	{
		ha_valid_data <- complete.cases(ho[ ,23])
		valid_state     <- ho[ha_valid_data,][,7]
		valid_hospi_nm  <- ho[ha_valid_data,][,2]
		valid_hospi_val <- ho[ha_valid_data,][,23]
	}
	else stop("Invalid Outcome")
	
	tmp_df <- data.frame(hospital=valid_hospi_nm, state = valid_state, oname=as.numeric(valid_hospi_val))


        rank_all <- data.frame(tmp_df[order(tmp_df$state, tmp_df$oname, tmp_df$hospital,na.last=NA),][,])
	new <- data.frame(hospital="", state = "")

	if(num == "best") {
		for (i in 1:length(unique_state)) {
			new <- rbind( new, rank_all[rank_all$state==unique_state[i],][1,c(1,2)])
		}
	} else if(num == "worst") {
		for (i in 1:length(unique_state)) {
			new <- rbind( new, rank_all[rank_all$state==unique_state[i],][-1,c(1,2)])
		}
	} else if(is.numeric(num)==TRUE) {
		for (i in 1:length(unique_state)) {
			new <- rbind( new, rank_all[rank_all$state==unique_state[i],][num,c(1,2)])
		}
	} else new <- NA
 	
new
}


