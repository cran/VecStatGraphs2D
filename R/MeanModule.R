MeanModule <-
function(azimuths){
		n_elements=length(azimuths);
		sin_=AddSinVectors(azimuths);
		cos_=AddCosVectors(azimuths);
		module=sqrt((sin_*sin_)+(cos_*cos_));
		mean_module=module/(n_elements);
		return(round(mean_module,4));
	}

