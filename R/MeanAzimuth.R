MeanAzimuth <-
function(azimuths){
		
		sin_=AddSinVectors(azimuths);
		cos_=AddCosVectors(azimuths);
		azimuth=atan(sin_/cos_);
		azimuth=ToSexagesimal(azimuth);
		
		#Azimuth calculated is a value between 0º-90º, but is posible that is other quadrant.
		if((sin_>0)&&(cos_>0)){
			}
		
		if((sin_>0)&&(cos_<0)){
			azimuth=azimuth+pi;
			}
		
		if((sin_<0)&&(cos_>0)){
			azimuth=azimuth+2*pi;
			}
		
		if((sin_<0)&&(cos_<0)){
			azimuth=azimuth+pi;
			}
		
		return(round(azimuth,4));
	}

