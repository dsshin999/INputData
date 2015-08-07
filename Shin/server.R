library(shiny)
library(datasets)
library(dplyr)

shinyServer(
                       function(input, output) {

                         output$total <- renderPlot({
                           bartype<-input$axis

                                                       if(bartype=="Age"){
                             barplot(total[,input$colnames],
                                     main=c("실험군, 대조군",input$colnames),
                                     names.arg=c("50s","60s","70s","80s","50s","60s","70s","80s"),
                                     ylab="indexes",
                                     xlab="연령대별 비교")
                           }
                           else{
                             experimental<-filter(dat, char=="experimental")
                             experimental.a<-filter(experimental, illness==a[1]) #질병1
                             experimental.b<-filter(experimental, illness==a[2]) #질병2
                             experimental.c<-filter(experimental, illness==a[3]) #질병3
                             comparison<-filter(dat, char=="comparison")
                             comparison.a<-filter(comparison, illness==a[1]) #질병1
                             comparison.b<-filter(comparison, illness==a[2]) #질병2
                             comparison.c<-filter(comparison, illness==a[3]) #질병3
                             #????????? ??????????????? ?????????별로 ???균내??? ??????블로 ??????
                             aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
                             aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
                             aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))

                             #???조군 ???????????? ?????????별로 ???균내??? ??????블로 ??????
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
                                     main=c("실험군, 대조군",input$colnames),
                                     names.arg=c("질병A","질병B","질병C","질병A","질병B","질병C"),
                                     ylab="indexes",
                                     xlab="질병별 비교"
                                     )}})

                         output$tota <- renderPlot({

                           barplot(c(monthly[,input$month1],monthly[,input$month2]),
                                   names.arg=c(input$month1,input$month2),
                                   main=("기간 별 평균걸음수 비교"),
                                   width=0.3,
                                   space=0.5,
                                   beside=TRUE,
                                   ylab="걸음수",
                                   xlab="#개월 경과 후 걸음수 비교")
                         })
                         output$tot <- renderPlot({
                           people<-filter(comparison,name=="input$names")
                           barplot(c(persons[,input$month3],persons[,input$month4]),
                                   names.arg=c(input$month3,input$month4),
                                   main=(input$names),
                                   width=0.3,
                                   space=0.5,
                                   beside=TRUE,
                                   ylab="걸음수",
                                   xlab="#개월 경과 후 걸음수 비교")
                         })
                         })

