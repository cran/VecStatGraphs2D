AddSinVectors <-
function(vectors){
		sin_sum=0;	
		h=1;
		radians=ToRadians(vectors);#radians is the input vectors in radians system.		
		radians_sin=sin(radians);
		sin_sum=sum(radians_sin);#sin_sum is the sine amount
		return(sin_sum);
	}

