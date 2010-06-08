AllAzimuthStatistics <-
function(azimuths){
		n_elements=length(azimuths)
		m_azimuth=MeanAzimuth(azimuths);
		m_module=MeanModule(azimuths);
		c_variance=CircularVariance(azimuths);
		s_deviation=CircularStandardDeviation(azimuths);
		vm_parameter=VonMisesParameter(azimuths);
		c_dispersal=CircularDispersal(azimuths);
		s_azimuth=SkewnessAzimuthCoefficient(azimuths);
		k_azimuth=KurtosisAzimuthCoefficient(azimuths);
	
		print("*************************************************************");
		print("****                                                     ****");
		print("****            CIRCULAR STATISTICS - AZIMUTH            ****");
		print("****                                                     ****");
		print("*************************************************************");
	
   		print(paste("MEAN AZIMUTH = ",m_azimuth));
		print(paste("MEAN MODULE = ",m_module));
   		print(paste("CIRCULAR STANDARD DEVIATION = " ,s_deviation));
		print(paste("CIRCULAR VARIANCE = ",c_variance));
        print(paste("VON MISES PARAMETER = ",vm_parameter));
        print(paste("CIRCULAR DISPERSION = ",c_dispersal));
        print(paste("SKEWNESS COEFFICIENT OF AZIMUTH = ",s_azimuth));
		print(paste("KURTOSIS COEFFICIENT OF AZIMUTH = ",k_azimuth));

	}

