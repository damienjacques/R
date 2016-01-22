
list.diff<-function(dir, pattern){
  filename.all<-list.files(dir)
  filename.diff<-list.files(dir, pattern)
  filename<-setdiff(filename.all,filename.diff)
  return(filename)
}

