best <- function(state, outcome) {

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

	## Return hospital name in that state with lowest 30-day death rate

	if( outcome == valid_outcome[1] )	{
		#ha_valid_data   <- complete.cases(as.numeric(ho[ ,11]))
		ha_valid_data   <- complete.cases(ho[ ,11])
		valid_hospi_nm  <- ho[ho$State == state & ha_valid_data,][,2]
		valid_hospi_val <- ho[ho$State == state & ha_valid_data,][,11]
	}

	else if( outcome == valid_outcome[2] )	{
#		ha_valid_data <- complete.cases(as.numeric(ho[ ,17]))
		ha_valid_data <- complete.cases(ho[ ,17])
		valid_hospi_nm  <- ho[ho$State == state & ha_valid_data,][,2]
		valid_hospi_val <- ho[ho$State == state & ha_valid_data,][,17]
	}
	else if( outcome == valid_outcome[3] )	{
#		ha_valid_data <- complete.cases(as.numeric(ho[ ,23]))
		ha_valid_data <- complete.cases(ho[ ,23])
		valid_hospi_nm  <- ho[ho$State == state & ha_valid_data,][,2]
		valid_hospi_val <- ho[ho$State == state & ha_valid_data,][,23]
	}
	else stop("Invalid Outcome")

	tmp_df <- data.frame(hname=valid_hospi_nm, oname=as.numeric(valid_hospi_val))
	hosp <- as.character(tmp_df[which.min(tmp_df$oname), ][,"hname"])
	hosp
	
}


