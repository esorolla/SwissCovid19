#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(chron)
library(tidyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(magrittr) ## In order to be able to use piping ( %>% )
library(readxl)
library(httr)

url1 <- "https://github.com/daenuprobst/covid19-cases-switzerland/raw/master/covid_19_data_switzerland-phase2.xlsx"
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
df1 <- read_excel(tf, 1) #infected
df2 <- read_excel(tf, 2) #fatalities

df1 <- data.frame(df1[,1:27])
df2 <- data.frame(df2[,1:27])

## We impute the missing values to zero:
for (j in 1:length(df1)){df1[is.na(df1[,j]),j] <- 0}
for (j in 1:length(df2)){df2[is.na(df2[,j]),j] <- 0}

vecNames <- colnames(df1)

df1New <- data.frame(var = cumsum(df1[,2]))

for (i in 3:length(df1)){
    Aux <- data.frame(var = cumsum(df1[,i]))
    df1New <- cbind(df1New, Aux)
}

df1New <- cbind(df1$Date,df1New) #We add the date
colnames(df1New) <- vecNames     #We name the variables
df1New$Date <- as.Date(df1New$Date, format = "%Y-%m-%d") #We coerce the date into Date format

df2New <- data.frame(var = cumsum(df2[,2]))

for (i in 3:length(df2)){
    Aux <- data.frame(var = cumsum(df2[,i]))
    df2New <- cbind(df2New, Aux)
}

df2New <- cbind(df2$Date,df2New) #We add the date
colnames(df2New) <- vecNames     #We name the variables
df2New$Date <- as.Date(df2New$Date, format = "%Y-%m-%d") #We coerce the date into Date format

## We get the data tidy and calculate the accumulated number of infections for each region:
df1New <- melt(df1New, id.vars = c("Date"), measure.vars = colnames(df1)[2:27])
df2New <- melt(df2New, id.vars = c("Date"), measure.vars = colnames(df1)[2:27])

#df1New <- df1New %>%
#    mutate(elapsed.days = as.numeric(difftime(df1New$Date, df1New$Date[1], units = "days")))

colnames(df1New)[2] <- "Region"
colnames(df1New)[3] <- "Cumulative.infected"

colnames(df2New)[2] <- "Region"
colnames(df2New)[3] <- "Cumulative.fatalities"

dfNew <- data.frame(df1New$Date,df1New$Region,df1New$Cumulative.infected,df2New$Cumulative.fatalities)
colnames(dfNew)[1] <- "Date"
colnames(dfNew)[2] <- "Region"
colnames(dfNew)[3] <- "Cumulative.infected"
colnames(dfNew)[4] <- "Cumulative.fatalities"

fluidPage(
    titlePanel("Covid-19 evolution in Switzerland"),
    
    tabsetPanel(
        tabPanel("Documentation", includeHTML("include.html")),
        tabPanel("Plot",
    
            sidebarLayout(
                sidebarPanel(
                    sliderInput("n", "Number of days since 31-5-2020:", min = 0, max = 120, value = 0,
                        step = 1),
                    selectInput(inputId = "Region",
                        label = "Choose a region:",
                        choices = unique(dfNew$Region)),
                    submitButton("Submit")
                ),

            mainPanel(htmlOutput("motionchart2"), verbatimTextOutput("text")
            )
            )

        )
    )
)