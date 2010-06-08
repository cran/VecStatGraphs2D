DrawPortion <-
function(init,finish,length_){
 	polygon(c(0,length_*cos(seq(ToRadians(finish),ToRadians(init),length.out=50))),c(0,length_*sin(seq(ToRadians(finish),ToRadians(init),length.out=50))),border="black",col="blue"); 
  	}

