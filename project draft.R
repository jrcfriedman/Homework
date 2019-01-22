


rm(list=ls())

library


vdem8.full <- read.csv("~/Desktop/data_2018F/V-Dem-CY-Core-v8.csv")


# Subsetting V-dem data
vdem8 <- subset(vdem8.full, year > 1959 & year < 2000, 
                select = c(country_name, country_text_id, country_id, year, v2x_polyarchy))


for (counter in vdem8$v2x_polyarchy) {
  vdem8 <- vdem8 %>% group_by(country_id) %>% mutate(decline.01 = case_when(counter - lag(counter)) <= -0.01 ~ 1, 
                                                     TRUE ~ 0)
}
vdem$decline <- c(NA, diff(vdem$v2x_polyarchy))
vdem$decline.01 <- as.numeric(vdem$decline <= -.01)



for (counter in 2:nrow(vdem)) {
  if (counter <= -0.01) {
    vdem$decline.01 <- 1
  } else { 
    vdem$decline.01 <- 0
  }
}

matches <- list(c(2,1),c(5,2),c(6,3))

decline.01 <- vector("double", length = nrow(vdem))
for (match in matches){
  total_goals <- c(total_goals, sum(match))
}



for (counter in 2:nrow(vdem)) {
  if (vdem$decline.01[counter] == 0)
    if (vdem$decline.01[lag(counter)] == 1) {
      if # counter + 1 : counter + 3
} }
}

# any() looks at vector of trues and falses and returns any if they're true





# Creating new variable if decline from year to year was greater than -.01
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(dec01 = v2x_polyarchy - lag(v2x_polyarchy) <= -0.01)
vdem8$dec01 <- as.numeric(vdem8$dec01)

# Creating new variable of v2x_polyarchy (EDI) difference from year to year
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(diff = v2x_polyarchy - lag(v2x_polyarchy))

# Creating new diff column with more decimals for easier viewing
vdem8$diff2 <- 0
vdem8[,"diff2"] <- round(vdem8[,'diff'],10)

# Coding observations as 1's for declines of 0.01, including 4-year stagnation period
# If no EDI 0.01 declines over 4 years, or if EDI increases by 0.02, then observation is coded as 0

# Step 1
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(ep1 = case_when( (dec01 == 0 & lag(dec01) == 1 & lead(dec01 ) == 1) & (diff < .02) ~ 1,
                          dec01 == 1 ~ 1,
                          TRUE ~ 0))
# Step 2
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(ep2 = case_when( (dec01 == 0 & lag(dec01) == 1 & lead(dec01) == 1) & (diff < .02) ~ 1,
                          (dec01 == 0 & lag(dec01,2) == 1 & (lead(dec01) == 1 | lead(dec01,2) == 1)) & (diff < .02 & lag(diff) < .02) ~ 1,
                          (dec01 == 0 & lag(dec01) == 1 & lead(dec01,2) == 1) & (diff < .02) ~ 1,
                          dec01 == 1 ~ 1,
                          TRUE ~ 0))
# Step 3
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(ep3 = case_when( (ep2 == 0 & lag(ep2) == 1 & lead(ep2) == 1) & (diff < .02) ~ 1,
                          ep2 == 1 ~ 1,
                          TRUE ~ 0))


# The following lines of code are a roundabout way of (1) identifying distinct backsliding episodes 
# and (2) taking the difference of the EDI score between the end and start of the epidodes

# Creating dummy variable episode start year
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(start_year = case_when(ep3 == 1 & lag(ep3) != 1 ~ 1,
                                TRUE ~ 0))

# Creating dummy variable for episode end year
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(end_year = case_when(ep3 == 1 & lead(ep3) != 1 ~ 1,
                              TRUE ~ 0))

# Replacing dummies with actual years
vdem8$start_year <- ifelse(vdem8$ep3 == 1 & lag(vdem8$ep3) == 0, vdem8$year, 0)
vdem8$end_year <- ifelse(vdem8$ep3 == 1 & lead(vdem8$ep3) == 0, vdem8$year, 0)

# Putting an x marker in the year before the start year
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(dum = case_when((lead(end_year) != 0 & lead(start_year != 0)) & (end_year == 0 & start_year == 0) ~ "x", 
                         (lead(start_year) != 0 & lead(end_year == 0)) & (end_year == 0 & start_year == 0) ~ "x",
                         TRUE ~ "0"))

# Subsetting so only observations with ep3 1s or x's remain
vdem8a <- subset(vdem8, ep3 == 1 | dum == "x")

# Subsetting so only observations with end years or x's remain 
vdem8a <- subset(vdem8a, (end_year != 0) | dum == "x")

# Generating ep.change variable that calculates overal decline in episode
vdem8a <- vdem8a %>% 
  group_by(country_id) %>%
  mutate(ep.change = v2x_polyarchy - lag(v2x_polyarchy))

# Removing difference calculations between non-episodes
vdem8a <- subset(vdem8a, end_year != 0)

# Removing difference calculations that are less than 0.1
vdem8a <- subset(vdem8a, ep.change <= -.1)


























rm(list=ls())

library(dplyr)

vdem.full <- read.csv("~/Desktop/data_2018F/V-Dem-CY-Core-v8.csv")

## For comparison

vdem8 <- subset(vdem.full, year > 1959 & year < 2000, 
                select = c(country_name, country_text_id, country_id, year, v2x_polyarchy))

# Creating new variable if decline from year to year was greater than -.01
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(dec01 = v2x_polyarchy - lag(v2x_polyarchy) <= -0.01)
vdem8$dec01 <- as.numeric(vdem8$dec01)

# Creating new variable of v2x_polyarchy (EDI) difference from year to year
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(diff = v2x_polyarchy - lag(v2x_polyarchy))

# Creating new diff column with more decimals for easier viewing
vdem8$diff2 <- 0
vdem8[,"diff2"] <- round(vdem8[,'diff'],10)

# Coding observations as 1's for declines of 0.01, including 4-year stagnation period
# If no EDI 0.01 declines over 4 years, or if EDI increases by 0.02, then observation is coded as 0

# Step 1
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(ep1 = case_when( (dec01 == 0 & lag(dec01) == 1 & lead(dec01) == 1) & (diff < .02) ~ 1,
                          dec01 == 1 ~ 1,
                          TRUE ~ 0))
# Step 2
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(ep2 = case_when( (dec01 == 0 & lag(dec01) == 1 & lead(dec01) == 1) & (diff < .02) ~ 1,
                          (dec01 == 0 & lag(dec01,2) == 1 & (lead(dec01) == 1 | lead(dec01,2) == 1)) & (diff < .02 & lag(diff) < .02) ~ 1,
                          (dec01 == 0 & lag(dec01) == 1 & lead(dec01,2) == 1) & (diff < .02) ~ 1,
                          dec01 == 1 ~ 1,
                          TRUE ~ 0))
# Step 3
vdem8 <- vdem8 %>% 
  group_by(country_id) %>%
  mutate(ep3 = case_when( (ep2 == 0 & lag(ep2) == 1 & lead(ep2) == 1) & (diff < .02) ~ 1,
                          ep2 == 1 ~ 1,
                          TRUE ~ 0))





# Subsetting V-dem data
vdem <- subset(vdem.full, year > 1959 & year < 2000, 
               select = c(country_name, country_text_id, country_id, year, v2x_polyarchy))


# Change in v2x_polyarchy from one year to the next
vdem <- vdem %>% 
  group_by(country_id) %>%
  mutate(diff = v2x_polyarchy - lag(v2x_polyarchy))

# New diff column for more decimals
vdem$diff2 <- 0
vdem[,"diff2"] <- round(vdem[,'diff'],10)

vdem$diff3 <- 0
vdem$diff3[is.na(vdem$diff)] <- 99

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



# Think this is right, but need to check
vdem$episode2 <- vdem$decline
for (i in 2:nrow(vdem)) {
  next.three <- vdem$decline[i:(i + 3)]
  if (vdem$episode2[i] == 0) {
    if (vdem$episode2[i - 1] == 1 & any(next.three == 1)) {
      vdem$episode2[i] <- 1
      if (vdem$episode2[i - 1] == 1 & any(next.three == 1)) {
        vdem$episode2[i] <- 1
        if ((vdem$decline[i] == 0) & (((vdem$decline[i + 1] == 2 | vdem$decline[i + 2] == 2) & (vdem$decline[i + 1] != 1) ) ) ) {
          vdem$episode2[i] <- 0
        }
      }
    }
  }
}

# Replaces 1's with 0's for first year per country
vdem$episode2[is.na(vdem$diff)] <- 0

# Replace 2's with 0's
vdem$episode2[vdem$episode2 == 2] <- 0


for (i in 2:nrow(vdem)) {
  if (vdem$episode2[i] == 0) {
    if (vdem$episode2[i - 1] == 1 & any(next.three == 1)) {
      vdem$episode2[i] <- 1
      if (vdem$episode2[i - 1] == 1 & any(next.three == 1)) {
        vdem$episode2[i] <- 1
        if ((vdem$decline[i] == 0) & (((vdem$decline[i + 1] == 2 | vdem$decline[i + 2] == 2) & (vdem$decline[i + 1] != 1) ) ) ) {
          vdem$episode2[i] <- 0
        }
      }
    }
  }
}


###### Creating function

backslide(vdem, v2x_polyarchy, new.column = "Episodes")

backslide <- function(vdemdata, x, group, new.column) {
  
  vdem <- vdem %>% 
    group_by(country_id) %>%
    mutate(diff = x - lag(x))
  
  
  
}

# Change in v2x_polyarchy from one year to the next
vdem <- vdem %>% 
  group_by(country_id) %>%
  mutate(diff = v2x_polyarchy - lag(v2x_polyarchy))

# New diff column for more decimals
vdem$diff2 <- 0
vdem[,"diff2"] <- round(vdem[,'diff'],10)

vdem$diff3 <- 0
vdem$diff3[is.na(vdem$diff)] <- 99

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
        if ((vdem$decline[i] == 0) & (((vdem$decline[i + 1] == 2 | vdem$decline[i + 2] == 2) & (vdem$decline[i + 1] != 1) ) ) ) {
          vdem$episode2[i] <- 0
        }
      }
    }
  }
}

# Replaces 1's with 0's for first year per country
vdem$episode2[is.na(vdem$diff)] <- 0



# adding ep3 variable 
# vdem$v8episode <- vdem8$ep3










table(vdem$episode2[vdem$episode2 == 1])
table(vdem8$ep3[vdem8$ep3 == 1])
table(vdem$v8episode[vdem$v8episode == 1])

matches <- vdem[which(vdem$episode2 == 1), "country_name" ]
matches1 <- vdem[which(vdem$episode2 == 1), "year" ]
matches <- cbind(matches, matches1)

matches.x <- vdem8[which(vdem8$ep3 == 1), "country_name" ]
matches.xx <- vdem8[which(vdem8$ep3 == 1), "year" ]
matches.x <- cbind(matches.x, matches.xx)

setdiff(matches.x, matches)
setdiff(matches, matches.x)






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
















