CircularVariance <-
function(azimuths){
		n_elements=length(azimuths);
		module=MeanModule(azimuths);
		variance=1-module;
		return(round(variance,4));
	}

