DrawModuleAndAzimuthDistribution <-
function(data_x,data_y,Direction = 2){
	
	if(Direction==1){
			type_=0;
	}
	if(Direction==2){
			type_=1;
	}
	rectangular_vectors<-array(c(data_x,data_y),dim=c(length(data_x),2))
	polar_data<-VectorsToPolar(rectangular_vectors,type_)
	
	plot.new();
	par(fg="white");
	
	max_=max(polar_data[,1])+1;
	d1=round(max_);
	d2=round(d1-(max_/4));
	d3=round(d2-(max_/4));
	d4=round(d3-(max_/4));
	length_=d1+10;
	center_x=0;
	center_y=0;
	plot(range(length_,-length_),range(length_,-length_), type="n",xlab="", ylab="", bty="n",col.axis="white");
	#title(main="MODULE AND AZIMUTHS DISTRIBUTION",sub="Every line corresponds to a single \n observation (module and azimuth)",cex.sub = 0.75, font.sub = 3, col.sub = "black");
	
	box(lty = 1, col = 'black')
	grid();
	
	DrawCircle(0,0,d1,border="black",lty=1,lwd=1);
	DrawCircle(0,0,d2,border="black",lty=3,lwd=1);
	DrawCircle(0,0,d3,border="black",lty=3,lwd=1);
	DrawCircle(0,0,d4,border="black",lty=3,lwd=1);
	
	if(type_==0){
		text(center_x,center_y+d1, " 90 ",adj = c(0,0), cex=0.8,pos=3, col="black")
		text(center_x-d1,center_y, " 180 ",adj = c(0,0), cex=0.8, pos=2,col="black")
		text(center_x,center_y-d1, " 270 ",adj = c(0,0), cex=0.8,pos=1, col="black")
		text(center_x+d1,center_y, " 0 ",adj = c(0,0), cex=0.8, pos=4, col="black")
		}
	
	if(type_==1){
		text(center_x,center_y+d1, " 0 ",adj = c(0,0), cex=0.8,pos=3, col="black")
		text(center_x-d1,center_y, " 270 ",adj = c(0,0), cex=0.8, pos=2, col="black")
		text(center_x,center_y-d1, " 180 ",adj = c(0,0), cex=0.8,pos=1, col="black")
		text(center_x+d1,center_y, " 90 ",adj = c(0,0), cex=0.8, pos=4, col="black")
		}
		
	segments(center_x,center_y-d1,center_x,center_y+d1,col="black",lwd=2);
	segments(center_x+d1,center_y,center_x-d1,center_y,col="black",lwd=2);
	
	text(center_x,center_y+d1,d1,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x,center_y+d2,d2,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x,center_y+d3,d3,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x,center_y+d4,d4,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	
	text(center_x,center_y-d1,d1,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x,center_y-d2,d2,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x,center_y-d3,d3,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x,center_y-d4,d4,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	
	text(center_x+d1,center_y,d1,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x+d2,center_y,d2,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x+d3,center_y,d3,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x+d4,center_y,d4,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	
	text(center_x-d1,center_y,d1,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x-d2,center_y,d2,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x-d3,center_y,d3,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x-d4,center_y,d4,adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	
	for(i in 1:(length(rectangular_vectors)/2)){
		arrows(center_x, center_y,rectangular_vectors[i,1],rectangular_vectors[i,2],length = 0.06, code = 2, col = "blue",lwd=0.1);
	}
	
	azimuth=MeanAzimuth(polar_data[,2]);# The mean azimuth is calculated.
	if(type_==0){
		radian=ToRadians(azimuth);
	}
	if(type_==1){
		radian=ToRadians(90-azimuth);
	}
	x=cos(radian)*ArithmeticMean(polar_data[,1]);
	y=sin(radian)*ArithmeticMean(polar_data[,1]);
	arrows(0,0,x,y,length=0.15,code=2,col="red",lty=1,lwd=2);	
}

