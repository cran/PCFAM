rowcol <-
function(I,J,elements){
 elements[elements>(I*J)]=NA
 whichcol=ceiling(elements/I)
 whichrow=elements-(I*(whichcol-1))
 #for (i in 1:length(elements)){print(x[whichrow[i],whichcol[i]])}
 return(list(whichrow=whichrow,whichcol=whichcol))}
