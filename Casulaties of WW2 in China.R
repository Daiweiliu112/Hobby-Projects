library(shiny)
library(ggplot2)
getwd()
data<-read.csv("D:/shiny/chinese_theater.csv")
head(data)

data_clean <- na.omit(data)
data_clean

total <- function(vec) {
  total = 0
  for (x in vec) {
    total = total + x
  }
  return (total)
}

total2 <- function(vec, num){
  total = 0
  count = 0
  for(x in vec) {
    total = total + x
    count = count + 1
    if (count == num) {
      break
    }
  }
}



CCCR_clean <- na.omit(data$CCCR)
CCCR_total<- total(CCCR_clean)
CCCR_total

CCJR_clean <- na.omit(data$CCJR)
CCJR_total <- total(CCJR_clean)
CCJR_total

JCCR_clean <- na.omit(data$JCCR)
JCCR_total <- total(JCCR_clean)
JCCR_total

JCJR_clean <-na.omit(data$JCJR)
JCJR_total <- total(JCJR_clean)
JCJR_total

year_total <- function(year, col) {
  x = 1
  cas = 0
  while(x <= nrow(data_clean)) {
    if (data_clean[x,1] == year) {
      cas = cas + data_clean[x,col]
      x = x + 1
    }
    else {
      x = x + 1
    }
  }
  return (cas)
}

year_total2 <- function(year_start,year_end, col) {
  x = 1
  cas = 0
  while(x <= nrow(data_clean)) {
    if (data_clean[x,1] == year_start || data_clean[x,1] <= year_end) {
      cas = cas + data_clean[x,col]
      x = x + 1
    }
    else {
      break
    }
  }
  print(x)
  return (cas)
}

CCCR_37 <- year_total(1937,2)
CCJR_37 <- year_total(1937,3)
JCCR_37 <- year_total(1937,6)
JCJR_37 <- year_total(1937,7)

CCCR_38 <- year_total(1938, 2)
CCJR_38 <- year_total(1938, 3)
JCCR_38 <- year_total(1938, 6)
JCJR_38 <- year_total(1938, 7)

CCCR_39 <- year_total(1939, 2)
CCJR_39 <- year_total(1939, 3)
JCCR_39 <- year_total(1939, 6)
JCJR_39 <- year_total(1939, 7)

CCCR_40 <- year_total(1940, 2)
CCJR_40 <- year_total(1940, 3)
JCCR_40 <- year_total(1940, 6)
JCJR_40 <- year_total(1940, 7)

CCCR_41 <- year_total(1941, 2)
CCJR_41 <- year_total(1941, 3)
JCCR_41 <- year_total(1941, 6)
JCJR_41 <- year_total(1941, 7)

CCCR_42 <- year_total(1942, 2)
CCJR_42 <- year_total(1942, 3)
JCCR_42 <- year_total(1942, 6)
JCJR_42 <- year_total(1942, 7)

CCCR_43 <- year_total(1943, 2)
CCJR_43 <- year_total(1943, 3)
JCCR_43 <- year_total(1943, 6)
JCJR_43 <- year_total(1943, 7)

CCCR_44 <- year_total(1944, 2)
CCJR_44 <- year_total(1944, 3)
JCCR_44 <- year_total(1944, 6)
JCJR_44 <- year_total(1944, 7)

CCCR_45 <- year_total(1945, 2)
CCJR_45 <- year_total(1945, 3)
JCCR_45 <- year_total(1945, 6)
JCJR_45 <- year_total(1945, 7)

country <- c("China", "China", "Japan", "Japan")
cas <- c("CR", "JR", "CR", "JR" )

value_cumulative <- c(CCCR_total, CCJR_total, JCCR_total, JCJR_total)
data_cumulative <- data.frame(country,cas,value_cumulative)

value_37 <- c(CCCR_37, CCJR_37, JCCR_37, JCJR_37)
data_37 <- data.frame(country, cas, value_37)

value_38 <- c(CCCR_38, CCJR_38, JCCR_38, JCJR_38)
data_38 <- data.frame(country, cas, value_38)

value_39 <- c(CCCR_39, CCJR_39, JCCR_39, JCJR_39)
data_39 <- data.frame(country, cas, value_39)

value_40 <- c(CCCR_40, CCJR_40, JCCR_40, JCJR_40)
data_40 <- data.frame(country, cas, value_40)

value_41 <- c(CCCR_41, CCJR_41, JCCR_41, JCJR_41)
data_41 <- data.frame(country, cas, value_41)

value_42 <- c(CCCR_42, CCJR_42, JCCR_42, JCJR_42)
data_42 <- data.frame(country, cas, value_42)

value_43 <- c(CCCR_43, CCJR_43, JCCR_43, JCJR_43)
data_43 <- data.frame(country, cas, value_43)

value_44 <- c(CCCR_44, CCJR_44, JCCR_44, JCJR_44)
data_44 <- data.frame(country, cas, value_44)

value_45 <- c(CCCR_45, CCJR_45, JCCR_45, JCJR_45)
data_45 <- data.frame(country, cas, value_45)


line_CR<- data.frame(record = rep(c("Chinese Casualties", "Japanese Casualties"),each = 9),
  years_cumulative = rep(c("1937", "1938", "1939", "1940", "1941", "1942", "1943", "1944", "1945"), 2),
                   cas_cumulative = c(CCCR_37, CCCR_38, CCCR_39, CCCR_40, CCCR_41, CCCR_42, CCCR_43, CCCR_44, CCCR_45,
                           JCCR_37, JCCR_38, JCCR_39,JCCR_40,JCCR_41,JCCR_42,JCCR_43,JCCR_44,JCCR_45))


line_total <- data.frame(record = rep(c("Chinese Casualties", "Japanese Casualties"),each = 9),
                         years_cumulative = rep(c("1937", "1938", "1939", "1940", "1941", "1942", "1943", "1944", "1945"), 2),
                         deaths_total = c(year_total2(1937, 1937, 2),year_total2(1937, 1938, 2), year_total2(1937, 1939, 2),
                                          year_total2(1937, 1940, 2), year_total2(1937, 1941, 2), year_total2(1937, 1942,2),
                                          year_total2(1937, 1943, 2), year_total2(1937, 1944, 2), year_total2(1937, 1945,2),
                                          year_total2(1937, 1937, 6),year_total2(1937, 1938, 6), year_total2(1937, 1939, 6),
                                          year_total2(1937, 1940, 6), year_total2(1937, 1941, 6), year_total2(1937, 1942,6),
                                          year_total2(1937, 1943, 6), year_total2(1937, 1944, 6), year_total2(1937, 1945,6)))


ui<-fluidPage(
  sliderInput(inputId="years",
              label = "Choose a year",
              value = 1937, min = 1937, max = 1945),
  actionButton(inputId = "line", label="Cumulative Trend"),
  actionButton(inputId = "bar_year", label = "Year Specific"),
  actionButton(inputId = "total", label = "Total Deaths"),

  plotOutput("bar")
)

server<-function(input, output) {
  ## if want the data to change then it must be in the reactive wrapper
  ## conditional on the data, only one function for the actual plot
  ## this "data" is a function, a reactive function. thus it needs to be data() instead of just data
  
  data <- reactive({
    if (input$years == 1937) {
      return(data_37)
    }
    else if (input$years == 1938) {
      return(data_38)
    }
    else if (input$years ==1939) {
      return(data_39)
    }
    else if(input$years == 1940) {
      return(data_40)
    }
    else if(input$years == 1941) {
      return(data_41)
    }
    else if(input$years == 1942) {
      return(data_42)
    }
    else if(input$years == 1943) {
      return(data_43)
    }
    else if(input$years == 1944) {
      return(data_44)
    }
    else{
      return(data_45)
    }
  })
  

  
  

  
  ## in ggplot y = ... actually takes the data, thus it keeps overwritting the previous conditionals
  output$bar <- renderPlot({
      ggplot(data(), aes(fill = cas, y = data()[,3], x = country)) +
        geom_bar(position = "dodge", stat="identity") +
        ggtitle("Chinese reported casualties Vs. Japanese reported Casualties")+
      ylim(0, 700000)

    })
  
  observeEvent(input$line, 
               output$bar <- renderPlot({ggplot(data = line_CR, aes(x=years_cumulative, y =cas_cumulative, group = record)) +
                 geom_line(aes(color=record))+
                 geom_point(aes(color=record))}))
  
  observeEvent(input$total,
               output$bar <- renderPlot({ggplot(data = line_total, aes(x=years_cumulative, y =deaths_total, group = record)) +
                   geom_line(aes(color=record))+
                   geom_point(aes(color=record))}))
  
  observeEvent(input$bar_year,
               output$bar <- renderPlot({
                 ggplot(data(), aes(fill = cas, y = data()[,3], x = country)) +
                   geom_bar(position = "dodge", stat="identity") +
                   ggtitle("Chinese reported casualties Vs. Japanese reported Casualties")+
                   ylim(0, 700000)
                 
               }))

}

shinyApp(ui=ui, server=server)


