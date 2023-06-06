library(pdftools)
library(glue)
library(tidyverse)
urllist<-read.csv("url.csv")
url<-urllist$name # don't forget to run this
url
#creating pdf names
#month <- c("January", "February", "March", "April", "May", "June", "July",
          # "August", "September", "October", "November", "December")
#year <- c("200X")
#pdf_names <- 
  #tidyr::expand_grid(month, year) %>%
  #glue_data("data-{month}-{year}.pdf")
pdf_names<-urllist$fname
pdf_names
class(pdf_names)
safe_download <- safely(~ download.file(.x , .y, mode = "wb"))
walk2(url, pdf_names,safe_download)

