DrawVectors <-
function(measured_data_,real_data_){ #PODER CAMBIAR TAMAÑO DE LAS FLECHAS
	
	plot.new();
	par(fg="white");
	
	length_x_max=max(c(real_data_[,1],measured_data_[,1]));
	length_y_max=max(c(real_data_[,2],measured_data_[,2]));
    length_x_min=min(c(real_data_[,1],measured_data_[,1]));
    length_y_min=min(c(real_data_[,2],measured_data_[,2]));
	center_x=0;
	center_y=0;
	
    plot(range(length_x_min,length_x_max),range(length_y_min,length_y_max),type="n",xlab="", ylab="", bty="n",axes=FALSE);
	#title(main="VECTORS",sub="Vectors are represented by arrows. \n The arrow defines the direction, and the length of the arrow \n defines the module of the vector.",cex.sub = 0.75, font.sub = 3, col.sub = "black");
	
	box(lty = 1, col = 'black')
	grid(lwd=1);

	axis(side=1,pos=0,col="black",at=round(seq(0,length_x_max, len=5),2),cex.axis=0.7,font.axis=3);
	axis(side=1,pos=0,col="black",at=round(seq(length_x_min, 0, len=5),2),cex.axis=0.7,font.axis=3);
	axis(side=2,pos=0,col="black",at=round(seq(0,length_y_max, len=5),2),las=1,cex.axis=0.7,font.axis=3);
	axis(side=2,pos=0,col="black",at=round(seq(length_y_min, 0, len=5),2),las=1,cex.axis=0.7,font.axis=3);
	
    number_elements=length(real_data_)/2;
	for(i in 1:number_elements){                                     #si añado un paremetro más da error en el number_elements
		x=measured_data_[i,1]-real_data_[i,1];
		y=measured_data_[i,2]-real_data_[i,2];

		if((x!=0)&&(y!=0)){
			arrows(measured_data_[i,1], measured_data_[i,2],real_data_[i,1],real_data_[i,2], length = 0,07, code = 2, col="blue");
		}
	}
}

