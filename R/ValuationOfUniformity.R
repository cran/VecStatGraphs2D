ValuationOfUniformity <-
function(azimuths){

		percent<-(length(azimuths)*20)/100;
	
		sort_vectors=ToRadians(sort(azimuths));# Sort and transform to radians.
		v=(sort_vectors/(2*pi));# All value will be between 0-1
	
		x<-array(NA,c(2,length(azimuths)));
		x[2,]<-v;
		x[1,]<-(seq(1,length(azimuths))/(length(azimuths)+1))
	
		y<-array(NA,c(2,2*percent));
		y[2,1:percent]<-v[1:percent]+1;
		y[2,(percent+1):(2*percent)]<-v[(length(azimuths)-(percent-1)):(length(azimuths))]-1;
		y[1,1:percent]<-x[1,1:percent]+1;
		y[1,(percent+1):(2*percent)]<-x[1,(length(azimuths)-(percent-1)):(length(azimuths))]-1;
	
		z<-array(NA,c(2,2*percent+length(azimuths)));
		z[,]<-c(x[,],y[,]);

		plot.new();
		par(fg="white");
		plot(z[1,],z[2,],type="p",col="Blue",ylab="",pch=16, xlab="",col.axis="black");#All points are drawn.
		#title(main="GRAPHICAL ASSESSMENT OF UNIFORMITY",sub="The points should lie roughly along 45 grades line \n passing through (0,0) if the uniform model is correct.",cex.sub = 0.75, font.sub = 3, col.sub = "black");
	
		box(lty = 1, col = 'black'); 
		grid();
		abline(a=0,b=1,col="black");
	}

