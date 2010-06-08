RayleighTest <-
function(azimuths){
		n_elements=length(azimuths);
		m_module=MeanModule(azimuths);
		z=n_elements*(m_module^2);
		p=exp(-z);
		if(n_elements>=50){
			print(paste("Rayleigh Test - Probability of Uniformity = ",round(p,3),"%, (",p,")"));
		}
		else{
			print(paste("Rayleigh Test - Probability of Uniformity = ",0));
		}
	}

