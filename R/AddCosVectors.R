AddCosVectors <-
function(vectors){
		cos_sum=0;
		radians=ToRadians(vectors);#radians is the input vectors in radians system.
		radians_cos=cos(radians);
		cos_sum=sum(radians_cos);#cos_sum is the cosine amount
		return(cos_sum);
	}

