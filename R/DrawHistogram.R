DrawHistogram <-
function(azimuths, TamClasses = 15, Direction = 2){
	
	if(Direction==1){
			type=0;
	}
	if(Direction==2){
			type=1;
	}
	
	histogram<-Histogram(azimuths,TamClasses);
	
	plot.new();
	par(fg="white");
	
	max_=max(histogram[,2])*100+1;
	d1=round(max_);
	d2=round(d1-(max_/4));
	d3=round(d2-(max_/4));
	d4=round(d3-(max_/4));
	length_=d1+4;
	center_x=0;
	center_y=0;
	plot(range(length_,-length_),range(length_,-length_), type="n",xlab="", ylab="", bty="n",col.axis="white");
	title(main="HISTOGRAM",sub="A histogram is wrapped around the circle. \n Each section of the histogram depicts the number\n of observations falling within that portion of the range",cex.sub = 0.75, font.sub = 3, col.sub = "black");
	
	
	box(lty = 1, col = 'black')
	grid();
	
	

	DrawCircle(0,0,d1,border="black",lty=1,lwd=1);
	DrawCircle(0,0,d2,border="black",lty=3,lwd=1);
	DrawCircle(0,0,d3,border="black",lty=3,lwd=1);
	DrawCircle(0,0,d4,border="black",lty=3,lwd=1);
	
	if(type==0){
		text(center_x,center_y+d1+2, " 90 ",adj = c(0,0), cex=(1-(length_/90)),pos=3, col="black")
		text(center_x-d1-3.5,center_y-0.15, " 180 ",adj = c(0,0), cex=(1-(length_/90)), col="black")
		text(center_x,center_y-d1-3, " 270 ",adj = c(0,0), cex=(1-(length_/90)),pos=3, col="black")
		text(center_x+d1+1.5,center_y-0.15, " 0 ",adj = c(0,0), cex=(1-(length_/90)), col="black")
		}
	
	if(type==1){
		text(center_x,center_y+d1+2, " 0 ",adj = c(0,0), cex=(1-(length_/90)),pos=3, col="black")
		text(center_x-d1-3.5,center_y-0.15, " 270 ",adj = c(0,0), cex=(1-(length_/90)), col="black")
		text(center_x,center_y-d1-3, " 180 ",adj = c(0,0), cex=(1-(length_/90)),pos=3, col="black")
		text(center_x+d1+1.5,center_y-0.15, " 90 ",adj = c(0,0), cex=(1-(length_/90)), col="black")
		}

	#Draw the segments near to 0,90,180 and 270
	segments(center_x,center_y-d1,center_x,center_y+d1,col="black",lwd=2);
	segments(center_x+d1,center_y,center_x-d1,center_y,col="black",lwd=2);
	
	#Draw all portion.
	angle=360/(length(histogram)/2);
	for(i in 1:(length(histogram)/2)){
		if(type==0){
			DrawPortion((i*angle)-angle,i*angle,histogram[i,2]*100);
			}
		if(type==1){
			DrawPortion(90-((i*angle)-angle),90-(i*angle),histogram[i,2]*100);
			}
	}
	text(center_x,center_y+d1,paste(d1,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x,center_y+d2,paste(d2,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x,center_y+d3,paste(d3,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x,center_y+d4,paste(d4,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	
	text(center_x,center_y-d1,paste(d1,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x,center_y-d2,paste(d2,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x,center_y-d3,paste(d3,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x,center_y-d4,paste(d4,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	
	text(center_x+d1,center_y,paste(d1,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x+d2,center_y,paste(d2,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x+d3,center_y,paste(d3,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x+d4,center_y,paste(d4,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	
	text(center_x-d1,center_y,paste(d1,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x-d2,center_y,paste(d2,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x-d3,center_y,paste(d3,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
	text(center_x-d4,center_y,paste(d4,"%"),adj = c(0,0), cex=0.6, col="black",font=1,offset=0.1,pos=3);
}

