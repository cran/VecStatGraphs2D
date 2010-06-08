StandardError <-
function(modules){
		m_arit=ArithmeticMean(modules);
		n=NumberOfElements(modules);
		return(round(sqrt((sum((modules-m_arit)^2))/(n*(n-1))),4));
    }

