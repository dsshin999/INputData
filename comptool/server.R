library(shiny)
library(datasets)
library(dplyr)
shinyServer(function(input, output) {
  
  
  output$total <- renderPlot({
   bartype<-input$axis 
   if(bartype=="Age"){
     barplot(total[,input$colnames], 
            main=input$colnames,
            ylab="indexes",
            xlab="age")
   }
   else{
     experimental<-filter(dat, char=="experimental")
     experimental.a<-filter(experimental, illness==a[1]) #ì§ˆë³‘1
     experimental.b<-filter(experimental, illness==a[2]) #ì§ˆë³‘2
     experimental.c<-filter(experimental, illness==a[3]) #ì§ˆë³‘3
     comparison<-filter(dat, char=="comparison")
     comparison.a<-filter(comparison, illness==a[1]) #ì§ˆë³‘1
     comparison.b<-filter(comparison, illness==a[2]) #ì§ˆë³‘2
     comparison.c<-filter(comparison, illness==a[3]) #ì§ˆë³‘3
     #?‹¤?—˜êµ? ?°?´?„°?“¤?„ ?‚˜?´??€ë³„ë¡œ ?‰ê· ë‚´?–´ ?…Œ?´ë¸”ë¡œ ??€?ž¥
     aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
     aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
     aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
     
     #??€ì¡°êµ° ?°?´?„°ë¥? ?‚˜?´??€ë³„ë¡œ ?‰ê· ë‚´?–´ ?…Œ?´ë¸”ë¡œ ??€?ž¥
     aver.cp.a<-summarise(comparison.a, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
     aver.cp.b<-summarise(comparison.b, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
     aver.cp.c<-summarise(comparison.c, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
     
     
     aver.cp<-rbind(aver.cp.a,aver.cp.b,aver.cp.c) 
     aver.cp$illness<-a
     aver.cp
     aver.ex<-rbind(aver.ex.a,aver.ex.b,aver.ex.c)
     aver.ex$illness<-a
     
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
     barplot(total[,input$colnames], 
             main=input$colnames,
             ylab="indexes",
             xlab="illness")
   }
  })
})

