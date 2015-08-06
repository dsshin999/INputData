library(shiny)
library(dplyr)
library(ggplot2)
library(datasets)

shinyServer(function(input, output){
  output$plot <- renderPlot({

    if(input$x == "age")
    {
      experimental<-filter(dat, char=="experimental")
      experimental.50<-filter(experimental, age<60)
      experimental.60<-filter(experimental, age>=60, age<70)
      experimental.70<-filter(experimental, age>=70, age<80)
      experimental.over<-filter(experimental, age>=80)
      comparison<-filter(dat, char=="comparison")
      comparison.50<-filter(comparison, age<60)
      comparison.60<-filter(comparison, age>=60, age<70)
      comparison.70<-filter(comparison, age>=70, age<80)
      comparison.over<-filter(comparison, age>=80)


      aver.ex.50<-summarise(experimental.50, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
      aver.ex.60<-summarise(experimental.60, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
      aver.ex.70<-summarise(experimental.70, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
      aver.ex.over<-summarise(experimental.over, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))


      aver.cp.50<-summarise(comparison.50, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
      aver.cp.60<-summarise(comparison.60, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
      aver.cp.70<-summarise(comparison.70, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
      aver.cp.over<-summarise(comparison.over, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))

      age<-c(50, 60, 70, 80)
      aver.cp<-rbind(aver.cp.50,aver.cp.60,aver.cp.70,aver.cp.over)
      aver.cp$age<-age

      aver.ex<-rbind(aver.ex.50,aver.ex.60,aver.ex.70,aver.ex.over)
      aver.ex$age<-age

      char<-c("comparison", "comparison", "comparison", "comparison")
      aver.cp$char<-char
      char<-c("experimental", "experimental","experimental","experimental")
      aver.ex$char<-char
      total<-rbind(aver.cp, aver.ex)
      diff.st<-total$aver_af.st-total$aver_bf.st
      diff.eq5d<-total$aver_af.eq-total$aver_bf.eq
      diff.mr<-total$aver_af.mr-total$aver_bf.mr

      total$diff.st<-diff.st
      total$diff.eq<-diff.eq5d
      total$diff.mr<-diff.mr

      p<- ggplot(total, aes_string(input$x, input$y)) + geom_bar(width=3, stat="identity", position="dodge")
      #p<-qplot(total$input$x, total$input$y, position="dodge", geom="bar", stat="identity")
      if(input$color != "None")
      {p<-p+aes_string(fill=input$color)}

      print(p)
    }
    else{
      experimental<-filter(dat, char=="experimental")
      experimental.a<-filter(experimental, illness=="A") #질병1
      experimental.b<-filter(experimental, illness=="B") #질병2
      experimental.c<-filter(experimental, illness=="C") #질병3
      comparison<-filter(dat, char=="comparison")
      comparison.a<-filter(comparison, illness=="A") #질병1
      comparison.b<-filter(comparison, illness=="B") #질병2
      comparison.c<-filter(comparison, illness=="C") #질병3

      aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), 		aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
      aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
      aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))

      aver.cp.a<-summarise(comparison.a, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
      aver.cp.b<-summarise(comparison.b, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
      aver.cp.c<-summarise(comparison.c, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))


      aver.cp<-rbind(aver.cp.a,aver.cp.b,aver.cp.c)
      aver.cp$illness<-c("A","B","C")
      aver.cp
      aver.ex<-rbind(aver.ex.a,aver.ex.b,aver.ex.c)
      aver.ex$illness<-c("A","B","C")

      char<-c("comparison", "comparison", "comparison")
      aver.cp$char<-char
      char<-c("experimental", "experimental","experimental")
      aver.ex$char<-char
      total<-rbind(aver.cp, aver.ex)

      diff.st<-total$aver_af.st-total$aver_bf.st
      diff.eq5d<-total$aver_af.eq-total$aver_bf.eq
      diff.mr<-total$aver_af.mr-total$aver_bf.mr
      total$diff.st<-diff.st
      total$diff.eq<-diff.eq5d
      total$diff.mr<-diff.mr

      p<- ggplot(total, aes_string(x=input$x, y=input$y)) + geom_bar(width=.3, stat="identity", position="dodge")
      if(input$color != "None")
      {p<-p+aes_string(fill=input$color)}

      print(p)
    }

  })
}
)
