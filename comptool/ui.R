library(shiny)
library(ggplot2)
library(dplyr)
library(datasets)

	dat <- read.csv("/Users/gimyejin/Desktop/chart.csv", header=T,stringsAsFactors = F)

	shinyUI(pageWithSidebar(

	  headerPanel("비교군, 대조군 비교"),

	  sidebarPanel(

	    selectInput('x', 'X', c("age", "illness")),
	    selectInput('y', 'Y', names(total)),
	    selectInput('color', 'Seperation', c("None","char")),
	    textInput("names", "환자이름:", value="Enter the name"),

	    selectInput("month1","select month", choices=c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12")),
	    selectInput("month2", "select month2", choices=c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12"))
	  ),

	  mainPanel(
	    plotOutput('plot'))
	))
