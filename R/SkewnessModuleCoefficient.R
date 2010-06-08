SkewnessModuleCoefficient <-
function(modules){
	  	m_arit=ArithmeticMean(modules);
	    n=NumberOfElements(modules);
	    s=ModuleStandardDeviation(modules);
		return(round((n/((n-1)*(n-2)))*(sum(((modules-m_arit)/s)^3)),4));
	}

