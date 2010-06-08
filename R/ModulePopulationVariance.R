ModulePopulationVariance <-
function(modules){
		m_arit=ArithmeticMean(modules);
		n=NumberOfElements(modules);
		return(round((sum((modules-m_arit)^2))/(n),4));
    }

