# INputData
inputting data to excel.csv file

#'Inputting data to excel
#'
#'inputs what is asked
#'@param name,bf.eq5d,bf.gh,bf.pr,bf.mr,bf.vt,age,bf.strides,af.eq5d,af.gh,af.mr,af.vt,af.strides,char,illness
#'@return saving data to excel-csv file
#'@export 
do<-function()
{
  chart<-read.csv(file.choose(),header=T)
  print("이름 입력"); name<-scan(,what="",n=1)
  print("bf.eq5d 입력");bf.eq5d<-scan(,what=0,n=1)
  print("bf.gh 입력"); bf.gh<-scan(,what=0,n=1)
  print("bf.pr 입력"); bf.pr<-scan(,what=0,n=1)
  print("bf.mr 입력"); bf.mr<-scan(,what=0,n=1)
  print("bf.vt 입력"); bf.vt<-scan(,what=0,n=1)
  print("age 입력"); age<-scan(,what=0,n=1)
  print("bf.strides 입력");bf.strides<-scan(,what=0,n=1)
  print("af.eq5d 입력"); af.eq5d<-scan(,what=0,n=1)
  print("af.gh 입력"); af.gh<-scan(,what=0,n=1)
  print("af.mr 입력"); af.mr<-scan(,what=0,n=1)
  print("af.vt 입력"); af.vt<-scan(,what=0,n=1)
  print("af.strides 입력"); af.strides<-scan(,what=0,n=1)
  print("char 입력"); char<-scan(,what=0,n=1)
  print("illness 입력"); illness<-scan(,what="",n=1)
  dataadd<-data.frame(cbind(name,bf.eq5d,bf.gh,bf.pr,bf.mr,bf.vt,age,bf.strides,af.eq5d,af.gh,af.mr,af.vt,af.strides,char,illness))
  chart<-rbind(chart,dataadd)
  write.table(chart,"C:/Users/hawayi/Desktop/r/InputData/chart.csv", row.names=FALSE,sep=",",quote=FALSE,append=FALSE)
  print("계속 입력을 진행할 경우 1, 프로그램을 끝낼시 2를 입력");
  #데이터 입력할지 중단할지 결정//
  select<-scan(,,n=1)
  if (select=="1")
  {
    return(do())
  }else
  {
    q()
  }
  
}
