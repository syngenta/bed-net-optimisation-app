
library(DT)
library(future.callr)
library(magrittr)
library(promises)
library(shiny)
library(viridis)

for (i in dir('r', full.names=TRUE)) source(i)

plan(callr)

shinyApp(ui, server)
