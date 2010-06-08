KurtosisModuleCoefficient <-
function(modules){
		m_arit=ArithmeticMean(modules);
	    n=NumberOfElements(modules);
	    s=ModuleStandardDeviation(modules);
    
	    a=(n*(n+1))/((n-1)*(n-2)*(n-3));
	    b=sum(((modules-m_arit)/s)^4);
	    cc=(3*(n-1)^2)/((n-2)*(n-3));
		return(round((((n*(n+1))/((n-1)*(n-2)*(n-3)))*(sum(((modules-m_arit)/s)^4)))-((3*(n-1)^2)/((n-2)*(n-3))),4))
	}

