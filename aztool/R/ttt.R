#' @return
#' @export
library(shiny)
library(datasets)
library(ggplot2)
library(dplyr)

ds<-function(){
  dat<-read.csv(file.choose(),header=T,stringsAsFactors = F)
  experimental<-filter(dat, char=="experimental")
  experimental.50<-filter(experimental, age<60)
  experimental.60<-filter(experimental, age>=60, age<70)
  experimental.70<-filter(experimental, age>=70, age<80)
  experimental.over<-filter(experimental, age>=80)
  comparison<-filter(dat, char=="comparison")
  monthly<-summarise(comparison, month1=mean(m1),month2=mean(m2),month3=mean(m3),month4=mean(m4),month5=mean(m5),month6=mean(m6),month7=mean(m7),month8=mean(m8),month9=mean(m9),month10=mean(m10),month11=mean(m11),month12=mean(m12))
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





  server<-function(input, output) {



    output$total <- renderPlot({
      bartype<-input$axis

      if(bartype=="Age"){
        barplot(total[,input$colnames],
                main=c("Experimental, Control group",input$colnames),
                names.arg=c("50s","60s","70s","80s","50s","60s","70s","80s"),
                ylab="indexes",
                xlab="comparison with age")
      }
      else{
        a<-c("A","B","C")
        experimental<-filter(dat, char=="experimental")
        experimental.a<-filter(experimental, illness==a[1]) #질병1
        experimental.b<-filter(experimental, illness==a[2]) #질병2
        experimental.c<-filter(experimental, illness==a[3]) #질병3
        comparison<-filter(dat, char=="comparison")
        comparison.a<-filter(comparison, illness==a[1]) #질병1
        comparison.b<-filter(comparison, illness==a[2]) #질병2
        comparison.c<-filter(comparison, illness==a[3]) #질병3

        aver.ex.a<-summarise(experimental.a, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
        aver.ex.b<-summarise(experimental.b, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))
        aver.ex.c<-summarise(experimental.c, aver_bf.eq=mean(bf.eq5d), aver_bf.gh=mean(bf.gh), aver_bf.mr=mean(bf.mr), aver_bf.vt=mean(bf.vt), aver_bf.st=mean(bf.strides), aver_af.eq=mean(af.eq5d), aver_af.gh=mean(af.gh), aver_af.mr=mean(af.mr), aver_af.vt=mean(af.vt), aver_af.st=mean(af.strides))


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
                main=c("Experimental, Control group",input$colnames),
                names.arg=c("질병A","질병B","질병C","질병A","질병B","질병C"),
                ylab="indexes",
                xlab="comparison with age"
        )}})

    output$tota <- renderPlot({

      barplot(c(monthly[,input$month1],monthly[,input$month2]),
              names.arg=c(input$month1,input$month2),
              main=("Comparing strides within terms"),
              width=0.3,
              space=0.5,
              beside=TRUE,
              ylab="strides",
              xlab="#strides after # of month")
    })
    output$tot <- renderPlot({
      patients<-input$names
      people2<-filter(comparison,name=="input$names")
      persons<-summarise(people2, month1=mean(m1),month2=mean(m2),month3=mean(m3),month4=mean(m4),month5=mean(m5),month6=mean(m6),month7=mean(m7),month8=mean(m8),month9=mean(m9),month10=mean(m10),month11=mean(m11),month12=mean(m12))
      if(patients=="Name"){

      }
      else{
        barplot(c(persons[,input$month3],persons[,input$month4]),
                names.arg=c(input$month3,input$month4),
                main=(input$names),
                width=0.3,
                space=0.5,
                beside=TRUE,
                ylab="strides",
                xlab="strides after # of month")
      }

    }
    )
  }
  ui<-navbarPage("Analyzing Tool",
                 tabPanel("Overall Index",
                          # Use a fluid Bootstrap layoutfluidPage(

                          # Give the page a title
                          titlePanel("Comparing experimental-control group"),
                          # Generate a row with a sidebar
                          sidebarLayout(
                            # Define the sidebar with one input
                            sidebarPanel(
                              selectInput("axis", "Please Select x-axis", choices=c("Age","Illness")),
                              selectInput("colnames", "indexes", choices=colnames(total)),




                              hr(),
                              helpText("total")
                            ),

                            # Create a spot for the barplot
                            mainPanel(
                              plotOutput("total")
                            ))),

                 tabPanel("Average strides",fluidPage(

                   # Give the page a title
                   titlePanel("Comparing strides within terms"),
                   # Generate a row with a sidebar
                   sidebarLayout(
                     # Define the sidebar with one input
                     sidebarPanel(


                       selectInput("month1", "select month", choices=colnames(monthly)),
                       selectInput("month2", "select month2", choices=colnames(monthly)),

                       hr(),
                       helpText("total")
                     ),

                     # Create a spot for the barplot
                     mainPanel(
                       plotOutput("tota")
                     )))
                 ),
                 tabPanel("Patients Strides",fluidPage(

                   # Give the page a title
                   titlePanel("Comparing patient's strides within terms"),
                   # Generate a row with a sidebar
                   sidebarLayout(
                     # Define the sidebar with one input
                     sidebarPanel(
                       textInput("names","Name:","Name"),


                       selectInput("month3", "select month", choices=colnames(monthly)),
                       selectInput("month4", "select month2", choices=colnames(monthly)),

                       hr(),
                       helpText("total")
                     ),

                     # Create a spot for the barplot
                     mainPanel(
                       plotOutput("tot")
                     )))
                 ))
  shinyApp(ui=ui,server=server)
}
