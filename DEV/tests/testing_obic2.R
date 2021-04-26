# Testing whether the package as a whole is useable using realistic data
library(data.table)
library(OBIC)

obiin <- readRDS('..//OBIC functies nematode/obiin_pdf.rds')

res1 <- OBIC::obic(obiin, add_relative_score = FALSE, add_recommendations = FALSE)

x <- 1
x <- NA
x <- data.table(species = 'aal', count = 2)
x <- data.table(species = 2, dooku = 'count')
!is.na(x)
if(!any(is.na(x))) {
  checkmate::assert_data_table(x)
  checkmate::assert_subset(
    colnames(x),
    empty.ok = FALSE,
    choices = c('species', 'count')
  )
}

z <- data.table(species = 'aal', count = 2)
res2 <- OBIC::obic(obiin, add_relative_score = FALSE, add_recommendations = FALSE, dt_nema = z)

dumaal <- as.data.table(read.csv('DEV/dummy_nema.csv', sep = ';'))
res2 <- OBIC::obic(obiin, add_relative_score = FALSE, add_recommendations = FALSE, dt_nema = dumaal)

# Check how obic responds to adding relative score
res1.relative <-  OBIC::obic(obiin, add_relative_score = TRUE, add_recommendations = FALSE)
res2.relative <- OBIC::obic(obiin, add_relative_score = TRUE, add_recommendations = FALSE, dt_nema = dumaal)

# Check how obic responds to adding recommendations
res1.recom <-  OBIC::obic(obiin, add_relative_score = FALSE, add_recommendations = TRUE)
res2.recom <- OBIC::obic(obiin, add_relative_score = FALSE, add_recommendations = TRUE, dt_nema = dumaal)
