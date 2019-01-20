




vdem8.full <- read.csv("~/Desktop/data_2018F/V-Dem-CY-Core-v8.csv")


# Subsetting V-dem data
vdem <- subset(vdem8.full, year > 1959 & year < 2000, 
               select = c(country_name, country_text_id, country_id, year, v2x_polyarchy))


# Change in v2x_polyarchy from one year to the next
vdem <- vdem %>% 
  group_by(country_id) %>%
  mutate(diff = v2x_polyarchy - lag(v2x_polyarchy))

# New diff column for more decimals
vdem$diff2 <- 0
vdem[,"diff2"] <- round(vdem[,'diff'],10)


# New variable for basic 0.01 threshold
#  vdem <- vdem %>% 
#    group_by(country_id) %>%
#    mutate(decline2 = case_when(diff <= -0.01 ~ 1,
#                               TRUE ~ 0))

# New variable: 1 = decline greater/equal to .01, 2 = incline greater/equal to .02, 0 = all else
vdem <- vdem %>% 
  group_by(country_id) %>%
  mutate(decline = case_when(diff <= -0.01 ~ 1,
                             diff >= 0.02 ~ 2,
                             TRUE ~ 0))
         
# Not quite right
vdem$episode <- vdem$decline
for (i in 2:nrow(vdem)) {
  next.three <- vdem$episode[i:(i + 3)]
  if (vdem$episode[i] == 0) {
    if (vdem$episode[i - 1] == 1 & any(next.three == 1)) {
      vdem$episode[i] <- 1
      if (vdem$episode[i - 1] == 1 & any(next.three == 1)) {
        vdem$episode[i] <- 1
        if ((vdem$decline[i] == 0) & (vdem$decline[i + 1] == 2)) {
          vdem$episode[i] <- 0
        }
      }
    }
  }
}

# Think this is right, but need to check
vdem$episode2 <- vdem$decline
for (i in 2:nrow(vdem)) {
  next.three <- vdem$decline[i:(i + 3)]
  if (vdem$episode2[i] == 0) {
    if (vdem$episode2[i - 1] == 1 & any(next.three == 1)) {
      vdem$episode2[i] <- 1
      if (vdem$episode2[i - 1] == 1 & any(next.three == 1)) {
        vdem$episode2[i] <- 1
        if ((vdem$decline[i] == 0) & ((vdem$decline[i + 1] == 2 | vdem$decline[i + 2] == 2))) {
          vdem$episode2[i] <- 0
        }
      }
    }
  }
}

previous.three <- vdem$decline[i:(i - 3)]
if (any(previous.three == 0) & vdem$episode2[i] == 2) {
  vdem$episode2[i] <- 0
}

vdem$v8episode <- vdem8$ep3

table(vdem$episode2[vdem$episode2 == 1])
table(vdem8$ep3[vdem8$ep3 == 1])

matches <- vdem[which(vdem$episode == 1), "country_name" ]
matches1 <- vdem[which(vdem$episode == 1), "year" ]
matches <- cbind(matches, matches1)

matches.x <- vdem8[which(vdem8$ep3 == 1), "country_name" ]
matches.xx <- vdem8[which(vdem8$ep3 == 1), "year" ]
matches.x <- cbind(matches.x, matches.xx)

setdiff(matches.x, matches)


vdem$episode <- vdem$decline
for (i in 2:nrow(vdem)) {
  next.four <- vdem$decline[i:(i + 4)]
  if (vdem$decline[i] == 0) {
    if (vdem$decline[i - 1] == 1 & any(lag(vdem$cnext.four == 1)) {
      vdem$episode[i] <- 1
      if (vdem$decline[i - 1] == 2) {
        vdem$episode[i] <- 0
      } 
    }
  }
}

if (vdem$decline[i] != 2) {
  vdem$episode[i] <- 1
  previous.year <- vdem$decline[i:(i - 1)]
  
vdem$episode[i] <- 1
vdem$episode[i + 1] <- 1
vdem$episode[i + 2] <- 1
vdem$episode[i + 3] <- 1
if (any(next.four == 2)) {
  vdem$episode[i] <- 0
}

# trying to figure out +.02
vdem$episode <- vdem$decline
for (i in 2:nrow(vdem)) {
  if (vdem$decline[i] == 0) {
    if (vdem$decline[i - 1] == 1) {
       next.three <- vdem$decline[i:(i +3)]
       if (any(next.three == 1)) {
         vdem$episode[i] <- 1
         if (vdem$decline[i] == 2 & vdem$decline[i - 1] == 0) {
           vdem$episode[i - 1] <- 0
         }
      } 
    }
  }
}

# episode2
vdem$episode2 <- vdem$decline
for (i in 2:nrow(vdem)) {
  if (vdem$decline[i] == 0) {
    if (vdem$decline[i - 1] == 1) {
      next.three <- vdem$decline[i:(i +3)]
      if (any(next.three == 1)) {
        vdem$episode2[i] <- 1
        if (any(next.three == 2)) {
          vdem$episode2[i] <- 0
        }
      } 
    }
  }
}


table(vdem$episode2[vdem$episode2 == 1])
table(vdem8$ep3[vdem8$ep3 == 1])

table(vdem$episode2[vdem$episode2 == 1 & vdem$diff > -.01])
print(vdem[which(vdem$episode2 == 1 & vdem$diff > -.01), "year" ])



# This worked!!!!
for (i in 2:nrow(vdem)) {
  if (vdem$decline[i] == 0) {
    if (vdem$decline[i - 1] == 1) {
      next.three <- vdem$decline[i:(i +3)]
      if (any(next.three == 1)) {
        vdem$episode[i] <- 1
      } 
    }
  }
}



for (i in 2:nrow(vdem)) {
  if (vdem$decline[i] == 1) {
    print("hello")
      } 
    }

  for (i in 1:dim(vdem)[1]) {
    if (vdem$decline[i] == 1) {
      vdem$example[i] <- vdem$example[i]*5
    } 
  }
  
vdem$example <- vdem$diff
for (i in 1:dim(vdem)[1]) {
  if (vdem$decline[i] == 1) {
    vdem$example[i] <- vdem$example[i]*5
  } 
}


# any() looks at vector of trues and falses and returns any if they're true



















