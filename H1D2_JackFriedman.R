# Complete all of the items below
# Use comments where you're having trouble or questions

setwd("~/Desktop/PSYC798W/Data")

library(dplyr)

# 1. Read your data set into R

  vdem.full <- read.csv("~/Desktop/PSYC798W/Data/V-Dem-CY-Core-v8.csv")

# 2. Peek at the top few rows

  head(vdem.full)

# 3. Peek at the top few rows for only a few columns

  head(vdem.full[,1:10])

# 4. How many rows does your data have?

  nrow(vdem.full) # = 26,537 rows
  length(vdem.full) # to get columns

# 5. Get a summary for every column

  summary(vdem.full)

# 6. Get a summary for one column

  summary(vdem.full$v2x_polyarchy)

# 7. Are any of the columns giving you unexpected values?


# 8. Select a few key columns, make a vector of the column names

  colnames(vdem.full)
  
  col.names <- c(vdem.full$country_name, vdem.full$country_text_id, vdem.full$country_id, vdem.full$year, vdem.full$v2x_polyarchy, 
                 vdem.full$v2x_libdem, vdem.full$vdem.full$v2x_jucon, vdem.full$v2xlg_legcon, vdem.full$v2pscomprg)

# 9. Create a new data.frame with just that subset of columns

  vdem.sub1 <- vdem.full %>%
    select(., country_name, country_text_id, country_id, year, v2x_polyarchy, v2x_libdem, v2x_jucon, v2xlg_legcon, 
           v2pscomprg)

# 10. Create a new data.frame that is just the first 10 rows
#     and the last 10 rows of the data from the previous step

  vdem.sub2 <- vdem.sub1[c(1:10, 26528:26537),]

# 11. Create a new data.frame that is a random sample of half of the rows.
# HINT: ?sample

  vdem.random <- sample_n(vdem, 13269)

# 12. Find a comparison in your data that is interesting to make
#     (comparing two sets of numbers)
#     - run a t.test() (or cor.test()) for that comparison
#     - decide whether you need a non-default test
#       (e.g., Student's, paired)
#     - run the test with BOTH the formula and "vector"
#       formats, if possible
#     - if one is NOT possible, say why you can't do it


    # For easier analysis, subsetting data to include observations from 20th century
    vdem.sub3 <- subset(vdem.sub1, year > 1899 & year < 2001)
    
    # Testing correlation (vector approach) of electoral democracy scores with judicial constraints on the executive scores
    cor1 <- cor.test(vdem.sub3$v2x_polyarchy, vdem.sub3$v2x_jucon, na.rm = TRUE) 
    
    # Testing correlation (formula approach) of electoral democracy scores with judicial constraints on the executive scores
    cor1.1 <- cor.test(~ vdem.sub3$v2x_polyarchy + vdem.sub3$v2x_jucon, na.rm = TRUE)


# 13. Repeat #12 for TWO more comparisons
#     - Tip: it's okay if the comparisons are kind of nonsensical, this is 
#       just a programming exercise


    ## Comparing legislative constraints on the executive in democracies vs. nondemocracies
    
      # First creating dummy variable for democracy/nondemocracy
    vdem.sub3$dem <- 0
    vdem.sub3$dem[vdem.sub3$v2x_polyarchy > .5] <- 1
    
      # Running t-test
    ttest_1 <- t.test(v2xlg_legcon ~ dem, vdem.sub3)
    
    
    ## Comparing electoral democracy scores in the first half vs. the second half of the 20th century
    
      # First creating a dummy variable: period = 1 for first half of century, period = 2 for second half of century
    vdem.sub3$period <- 1
    vdem.sub3$period[vdem.sub3$year > 1949] <- 2
    
      # Running t-test
    ttest_2 <- t.test(v2x_polyarchy ~ period, vdem.sub3)

# 14. Save all results from #12 and #13 in an .RData file
    
    save(cor1, cor1.1, ttest_1, ttest_2, file = "H1_Q12-13.RData")

# 15. Email me your version of this script, PLUS the .RData
#     file from #14
#     - ALTERNATIVELY, push your version of this script and your .RData results
#       to a repo on GitHub, and send me the link


