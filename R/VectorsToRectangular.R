VectorsToRectangular <-
function(vectors,init){

		rectangular_vectors=vectors;
		grades<-vectors[,2];
		module<-vectors[,1];
		
		radians<-ToRadians(grades);
		
		y1=sin(radians)*module;
		x1=cos(radians)*module;

		if(init==1){
			x=y1;
			y=x1;
		}
		if(init==2){
			x=x1;
			y=y1;
		}
		rectangular_vectors[,1]<-x;
		rectangular_vectors[,2]<-y;
		return(round(rectangular_vectors,4));
	}

