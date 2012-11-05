LoadData <-
function(FileName, Type = 2, Direction = 2){
	data_=ReadFromFile(FileName);
    if(length(data_) > 1){
      if(CorrectType(Type,data_) == FALSE){
        switch (Type,print("Error, the file is not of (X origin, Y origin) - (X end, Y end) type"),
			print("Error, the file is not of Rectangular type"),
            	print("Error, the file is not of Polar coordinates type"),
                );
      }    
      else {  
        switch(Direction,dir_<-1,dir_<-2);        
        if(Type==1){
          error=ToCalculateError(data_);
          polar_vectors=VectorsToPolar(error,dir_);
          rectangular_vectors=error;
        }
	  if(Type==2){
          polar_vectors=VectorsToPolar(data_,dir_);	
          rectangular_vectors=data_;
		}
        if(Type==3){
          rectangular_vectors=VectorsToRectangular(data_,dir_);
          polar_vectors=data_;
		}
        
        
        num_data=dim(data_);
        res=matrix(nrow=num_data[1],ncol=9); 
        res[,1]=polar_vectors[,1];
        res[,2]=polar_vectors[,2];
        res[,3]=rectangular_vectors[,1];
        res[,4]=rectangular_vectors[,2];
        res[1,5]=Direction;
        res[2,5]=Type;
        res[3,5]=1111;
        if(Type==1){
          res[3,5]=9999;
          res[,6]=data_[,1];
          res[,7]=data_[,2];
          res[,8]=data_[,3];
          res[,9]=data_[,4];
        }
         return(res);
      }
	}    
  }

