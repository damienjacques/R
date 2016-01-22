library(raster)

linstretch<-function(img,minmax){
  temp<-calc(img,fun=function(x) (255*(x-minmax[1]))/(minmax[2]-minmax[1]))
  #set all values above or below minmax to 0 or 255
  temp[temp<0]<-0;temp[temp>255]<-255;
  return(temp)
}

linstrechMultiband<-function(r, bands, minmax.b1, minmax.b2, minmax.b3){
  b1<-r[[bands[1]]]
  b2<-r[[bands[2]]]
  b3<-r[[bands[3]]]
  b1.str<-linstretch(b1, minmax=minmax.b1)
  b2.str<-linstretch(b2, minmax=minmax.b2)
  b3.str<-linstretch(b3, minmax=minmax.b3)
  out<-stack(b1.str, b2.str, b3.str)
  return(out)
}