RayleighTest <-
function(azimuths){
		n=length(azimuths);
		m_module=MeanModule(azimuths);
		z=n*(m_module^2);
		#p=exp(-z);
		if(n>=50){
  		p=exp(-z);
			print(paste("Rayleigh Test - Probability of Uniformity = ",round(p,3),"%, (",p,")"));
		}
		else{
		  p=exp(-z)*(1+(2*z-z^2)/(4*n) - ((24*z-132*z^2 + 76*z^3 - 9*z^4)/288*n^2));
			print(paste("Rayleigh Test - Probability of Uniformity = ",0));
		}
	}

