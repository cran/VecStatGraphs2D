CircularStandardDeviation <-
function(azimuths){
		n_elements=length(azimuths);
		variance=CircularVariance(azimuths);
		deviation=(-2*log(1-variance))^0.5;
		return(round(deviation,4));
	}

