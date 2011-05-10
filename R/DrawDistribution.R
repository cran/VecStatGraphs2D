DrawDistribution <-
function(azimuths,Direction = 2){

	if(Direction==1){
			type=0;
	}
	if(Direction==2){
			type=1;
	}
	
	plot.new();
	par(fg="white");
	
	his=Histogram(azimuths,1);#The histogram is calculated. The number of vectors with the same azimuth is calculated.

	max_=max(his[,1]);
	d1=round(max_)+1;
	length_=d1+1.5;
	center_x=0;
	center_y=0;
	
	plot(range(length_,-length_),range(length_,-length_), type="n",xlab="", ylab="", bty="n",col.axis="white");
	title(main="RAW DATA DISTRIBUTION",sub="A symbol (circle) is plotted \n for each observation.",cex.sub = 0.75, font.sub = 3, col.sub = "black");
	
	box(lty = 1, col = 'black')
	grid();
	
	
	DrawCircle(center_x,center_y,d1,border="black",lty=1,lwd=1);
	
	if(type==0){
		text(center_x,center_y+d1, " 90 ",adj = c(0,0), cex=(1-(length_/90)),pos=3, col="black")
		text(center_x-d1,center_y, " 180 ",adj = c(0,0), cex=(1-(length_/90)),pos=2, col="black")
		text(center_x,center_y-d1, " 270 ",adj = c(0,0), cex=(1-(length_/90)),pos=1, col="black")
		text(center_x+d1,center_y, " 0 ",adj = c(0,0), cex=(1-(length_/90)), pos=4, col="black")
		}
	
	if(type==1){
		text(center_x,center_y+d1, " 0 ",adj = c(0,0), cex=(1-(length_/90)),pos=3, col="black")
		text(center_x-d1,center_y, " 270 ",adj = c(0,0), cex=(1-(length_/90)), pos=2, col="black")
		text(center_x,center_y-d1, " 180 ",adj = c(0,0), cex=(1-(length_/90)),pos=1, col="black")
		text(center_x+d1,center_y, " 90 ",adj = c(0,0), cex=(1-(length_/90)), pos=4, col="black")
		}

	#Draw the segments near to 0,90,180 and 270
	segments(center_x,center_y-d1,center_x,center_y+d1,col="black",lwd=2);
	segments(center_x+d1,center_y,center_x-d1,center_y,col="black",lwd=2);

	for(i in 0:359){
		h=his[i+1,1];
		if(h>0){
			for(g in 1:h-1){# The points in the same direction are drawn.
				if(type==0){
					radian=ToRadians(i);
				}
				else{
					radian=ToRadians(90-i);
				}
				x=cos(radian)*(d1-((d1*0.025)*g));
				y=sin(radian)*(d1-((d1*0.025)*g));
				points(x,y,cex=0.5,pch=16,col="blue");
			}
		}
	}
	
	azimuth=MeanAzimuth(azimuths);# The mean azimuth is calculated.
	
	if(type==0){
		radian=ToRadians(azimuth);
	}
	else{
		radian=ToRadians(90-azimuth);
	}
	x=cos(radian)*(d1+(d1*0.1));
	y=sin(radian)*(d1+(d1*0.1));
	arrows(center_x,center_y,x,y,length=0.2,code=2,col="red",lty = par("lty"),lwd=3);
		
	n=NumberOfElements(azimuths);
	module=MeanModule(azimuths);
	vm=VonMisesParameter(azimuths);
	ci=ConfidenceInterval(n,azimuth,module,vm); 
	
	if(type==0){
		xmin=cos(ToRadians(ci[1]))*(d1+(d1*0.1));
		xmax=cos(ToRadians(ci[2]))*(d1+(d1*0.1));
		ymin=sin(ToRadians(ci[1]))*(d1+(d1*0.1));
		ymax=sin(ToRadians(ci[2]))*(d1+(d1*0.1));
		DrawArc(center_x,center_y,radius=d1+(d1*0.1),angle1=ToRadians(ci[1]),angle2 = ToRadians(ci[2]),n=35,col="red",lwd=2);     
	}
	else{
		xmin=cos(ToRadians(90-ci[1]))*(d1+(d1*0.1));
		xmax=cos(ToRadians(90-ci[2]))*(d1+(d1*0.1));
		ymin=sin(ToRadians(90-ci[1]))*(d1+(d1*0.1));
		ymax=sin(ToRadians(90-ci[2]))*(d1+(d1*0.1));
		DrawArc(center_x,center_y,radius=d1+(d1*0.1),angle1=ToRadians(90-ci[1]),angle2 = ToRadians(90-ci[2]),n=35,col="red",lwd=2);
	}
	points(xmin,ymin,col="red",pch=19)
	points(xmax,ymax,col="red",pch=19)
}

