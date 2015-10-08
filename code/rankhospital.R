rankhospital <- function(state, outcome, num = "best") {

	## Read outcome data
	ho <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check state and outcome are valid
	valid_state <- unique(ho$State)
	valid_outcome <- c("heart attack", "heart failure", "pneumonia")


	if( sum( sapply( valid_state, function(x) { identical(x,state) } ) ) < 1 ) {
		stop("Invalid State")
	}
	
	if( sum( sapply( valid_outcome, function(x) { identical(x,outcome) } ) ) < 1 ) {
		stop("Invalid Outcome")
	}

	if(num  != "best" & num!= "worst" & is.numeric(num)==FALSE) 	{
		stop("Invalid Num")
	}

	## Return hospital name in that state with given rank 30-day death rate

	if( outcome == valid_outcome[1] )	{
		ha_valid_data   <- complete.cases(ho[ ,11])
		valid_hospi_nm  <- ho[ho$State == state & ha_valid_data,][,2]
		valid_hospi_val <- ho[ho$State == state & ha_valid_data,][,11]
	}

	else if( outcome == valid_outcome[2] )	{
		ha_valid_data <- complete.cases(ho[ ,17])
		valid_hospi_nm  <- ho[ho$State == state & ha_valid_data,][,2]
		valid_hospi_val <- ho[ho$State == state & ha_valid_data,][,17]
	}
	else if( outcome == valid_outcome[3] )	{
		ha_valid_data <- complete.cases(ho[ ,23])
		valid_hospi_nm  <- ho[ho$State == state & ha_valid_data,][,2]
		valid_hospi_val <- ho[ho$State == state & ha_valid_data,][,23]
	}
	else stop("Invalid Outcome")

	tmp_df <- data.frame(hname=valid_hospi_nm, oname=as.numeric(valid_hospi_val))
	if(num == "best") {
		out <- as.character(tmp_df[order(tmp_df$oname, tmp_df$hname,na.last=NA),][1,1])
	} else if(num == "worst") {
		row <- nrow(tmp_df[order(tmp_df$oname, tmp_df$hname,na.last=NA),][,])
		out <- as.character(tmp_df[order(tmp_df$oname, tmp_df$hname,na.last=NA),][row,1])
	} else if(is.numeric(num)==TRUE) {
		out <- as.character(tmp_df[order(tmp_df$oname, tmp_df$hname,na.last=NA),][num,1])
	} else  out <- NA
	
	out
}


