AllModuleStatistics <-
function(modules){
		n_elemetns=NumberOfElements(modules);
		max_value=MaxValue(modules);
		min_value=MinValue(modules);
		range_value=Range(modules);
		module_sum=ModuleSum(modules);
		m_arithmetic= ArithmeticMean(modules);
		s_error=StandardError(modules);
		s_d_module=ModuleStandardDeviation(modules);
		v_module=ModuleVariance(modules);
		s_d_module_p=ModulePopulationStandardDeviation(modules);
		v_module_p=ModulePopulationVariance(modules);
		cs=SkewnessModuleCoefficient(modules);
		ca=KurtosisModuleCoefficient(modules);	
		
		print("************************************************************");
		print("****                                                    ****");
		print("****            CIRCULAR STATISTICS - MODULE            ****");
		print("****                                                    ****");
		print("************************************************************");
	
		print(paste("NUMBER OF ELEMENTS = ",n_elemetns));   
   		print(paste("MAX VALUE = ",max_value));   
   		print(paste("MIN VALUE = ",min_value));
		print(paste("RANGE = ",range_value)); 
   		print(paste("MODULES SUM = ",module_sum));
		print(paste("ARITHMETIC MEAN = ",m_arithmetic));
		print(paste("STANDARD ERROR = ",s_error));
		print(paste("MODULE STANDARD DEVIATION = ",s_d_module));
		print(paste("MODULE VARIANCE = ",v_module));
        print(paste("MODULE POPULATION VARIANCE = ",v_module_p));
        print(paste("MODULE POPULATION STANDARD DEVIATION = ",s_d_module_p));
        print(paste("SKEWNESS COEFFICIENT = ",cs));
        print(paste("KURTOSIS COEFFICIENT = ",ca));
	}

