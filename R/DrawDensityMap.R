DrawDensityMap <-
function(data_x,data_y,PercentajeOutliers = 5, PaintPoint = FALSE, Div = 250, HarmonicMean = FALSE, PaintAxis = FALSE){
	require(MASS)
	
	data_=array(c(data_x,data_y),dim=c(length(data_x),2))
	
	plot.new();
	
	f1 <- kde2d(data_[,1], data_[,2], n = Div, lims =c(min(data_[,1]),max(data_[,1]),min(data_[,2]),max(data_[,2])))
	
	ramp <- colorRamp(c("white","yellow","red"))
	colo = rgb( ramp(seq(0, 1, length = 13)), max = 255)
	image(f1,col=colo)
	title(main="DENSITY MAP",sub="The density map is calculated by \n means of describers of Kernels.",cex.sub = 0.75, font.sub = 3, col.sub = "black");
	box(lty = 1, col = 'black')
	
	if(PaintAxis==TRUE){
		axis(side=1,pos=0,col="grey",cex.axis=0.7,font.axis=3);
		axis(side=2,pos=0,col="grey",cex.axis=0.7,font.axis=3);
	}
	
	if(PaintPoint==TRUE){
		if(HarmonicMean == FALSE){
			Vec1=VectorsToPolar(data_,1);
			Vec=Vec1[,1];
		}
		else{
			Vec=AllHarmonicMean(data_);
		}
		cant=as.integer(length(data_[,1])*(PercentajeOutliers/100));
		data_=data_[ order(Vec,decreasing=TRUE), ]
	
		for(i in 1:length(data_[,1])){
		 	if(i>cant){
		      	points(data_[i,1],data_[i,2],cex=0.5,col="Black",pch=19);
			}
			else{
			   	points(data_[i,1],data_[i,2],cex=0.5,col="Green",pch=19);
			}
		}
	}
}

