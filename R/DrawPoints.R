DrawPoints <-
function(data_x,data_y,PercentajeOutliers = 5, HarmonicMean = FALSE){

	data_=array(c(data_x,data_y),dim=c(length(data_x),2))
	if(HarmonicMean == FALSE){
		Vec1=VectorsToPolar(data_,1);
		Vec=Vec1[,1];
	}
	else{
		Vec=AllHarmonicMean(data_);
	}
		
	plot.new();
	par(fg="white");
	
	length_x_max=max(data_[,1]);
	length_y_max=max(data_[,2]);
    length_x_min=min(data_[,1]);
    length_y_min=min(data_[,2]);
	center_x=0;
	center_y=0;
	
	plot(range(length_x_min,length_x_max),range(length_y_min,length_y_max),type="p",xlab="", ylab="", bty="n",axes=FALSE);
	title(main="POINTS",sub=paste("End nodes of the error vectors form a cloud of points. \n Outliers are plotted in red.",PercentajeOutliers,"% points with value." ),cex.sub = 0.75, font.sub = 3, col.sub = "black");
	
	box(lty = 1, col = 'black')
	grid(NA,NULL,lwd=1);
	axis(side=1,pos=0,col="grey",at=seq(length=length_x_max, from=0, by=10),cex.axis=0.55,font.axis=3);
	axis(side=1,pos=0,col="grey",at=seq(length=abs(length_x_min), from=0, by=-10),cex.axis=0.55,font.axis=3);
	axis(side=2,pos=0,col="grey",at=seq(length=length_y_max, from=0, by=10),las=1,cex.axis=0.55,font.axis=3);
	axis(side=2,pos=0,col="grey",at=seq(length=abs(length_y_min), from=0, by=-10),las=1,cex.axis=0.55,font.axis=3);
	
    cant=as.integer(length(data_[,1])*(PercentajeOutliers/100));
	data_=data_[ order(Vec,decreasing=TRUE), ]
	

	for(i in 1:length(data_[,1])){
	 	if(i>cant){
          	points(data_[i,1],data_[i,2],cex=0.5,col="Black",pch=19);
		}
		else{
		   	points(data_[i,1],data_[i,2],cex=0.5,col="Red",pch=19);
		}
	}
}

