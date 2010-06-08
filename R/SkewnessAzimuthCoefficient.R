SkewnessAzimuthCoefficient <-
function(azimuths){	
		n_elements=length(azimuths);
		sin_=AddSinVectors(azimuths);
		cos_=AddCosVectors(azimuths);
		CSub2=(1/n_elements)*sum(cos(2*ToRadians(azimuths)));
		SSub2=(1/n_elements)*sum(sin(2*ToRadians(azimuths)));
		RSub2=((CSub2^2+SSub2^2)^0.5);
		R=MeanModule(azimuths)
		result=(RSub2*(sin(atan(SSub2/CSub2)-2*(atan(sin_/cos_)))))/((1-R)^1.5);;
		return(round(result,4));
	}

