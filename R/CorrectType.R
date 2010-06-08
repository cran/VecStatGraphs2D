CorrectType <-
function(type,data_){
    if((min(data_) >= 0) && length(data_) == 2 && type==2)
      return (TRUE);
    if(length(data_) == 2 && type==1)
      return (TRUE);
    if(length(data_) == 4 && type==3)
      return (TRUE);
    return (FALSE);
  }

