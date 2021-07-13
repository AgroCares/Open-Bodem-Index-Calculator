library(pdftools)
library(data.table)
library(stringr)

pdf_t <- pdf_text('DEV/schema/test_nem_nl.pdf')
pdf_d <- pdf_data('DEV/schema/test_nem_nl.pdf')
pdf_i <- pdf_info('DEV/schema/test_nem_nl.pdf')


dt <- as.data.table(pdf_d)

test <- which(str_detect(pdf_t, 'Heterodera'))
test <- strsplit(pdf_t, '\n')

# Tutorial poging
l_text <- pdf_text('DEV/schema/test_nem_nl.pdf')
l_text <- strsplit(l_text, '\n')

aardappelrow <- grep("^.*Aardappel", l_text)
klik <- grep('Klik', l_text)
fundrow <- grep('Best4Soil ', l_text)
test <- l_text[-fundrow]
