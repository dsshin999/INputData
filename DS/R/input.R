
#'@export


inputdata<-function(){
  library(dplyr)
  chart<-read.csv(file.choose(),header=T)
  print("put name"); name<-scan(,what="",n=1)
  print("put bf.eq5d ");bf.eq5d<-scan(,what=0,n=1)
  print("put bf.gh "); bf.gh<-scan(,what=0,n=1)
  print("put bf.pr "); bf.pr<-scan(,what=0,n=1)
  print("put bf.mr "); bf.mr<-scan(,what=0,n=1)
  print("putbf.vt "); bf.vt<-scan(,what=0,n=1)
  print("put age "); age<-scan(,what=0,n=1)
  print("put bf.strides ");bf.strides<-scan(,what=0,n=1)
  print("put af.eq5d "); af.eq5d<-scan(,what=0,n=1)
  print("put af.gh "); af.gh<-scan(,what=0,n=1)
  print("put af.mr "); af.mr<-scan(,what=0,n=1)
  print("put af.vt "); af.vt<-scan(,what=0,n=1)
  print("put af.strides"); af.strides<-scan(,what=0,n=1)
  print("put char:insert experimental or comparison"); char<-scan(,what="",n=1)
  print("put illness "); illness<-scan(,what="",n=1)
  dataadd<-data.frame(cbind(name,bf.eq5d,bf.gh,bf.pr,bf.mr,bf.vt,age,bf.strides,af.eq5d,af.gh,af.mr,af.vt,af.strides,char,illness))
  chart<-rbind(chart,dataadd)
  write.table(chart,file.choose(), row.names=FALSE,sep=",",quote=FALSE,append=FALSE)
  print("if you want to continue 1, or exit program put 2");
  
  select<-scan(,,n=1)
  if (select=="1")
  {
    return(inputdata())
  }else
  {
    q()
  }
  
}