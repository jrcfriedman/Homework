

rm(list=ls())

library(dplyr)

# Reading in df
vdem.full <- read.csv("~/Desktop/PSYC798W/Data/V-Dem-CY-Core-v8.csv")
vdem <- read.csv("~/Desktop/PSYC798W/Homework/vdem.csv")

vdem <- subset(vdem.full, year > 1959 & year < 2000, 
                select = c(country_name, country_text_id, country_id, year, v2x_polyarchy))

vdem2 <- subset(vdem.full, year > 1959 & year < 2000, 
               select = c(country_name, country_text_id, country_id, year, v2x_polyarchy))

vdemx <- vdem

#################   
#  Step 1
################# 

# Creating new column (diff) that calculates the change in v2x_polyarchy from one year to the next
vdem <- vdem %>% 
  group_by(country_id) %>%
  mutate(diff = v2x_polyarchy - lag(v2x_polyarchy))

# Creating new column coded as follows: 1 = decline greater/equal to .01, 2 = incline greater/equal to .02, 0 = all else
vdem <- vdem %>% 
  group_by(country_id) %>%
  mutate(decline = case_when(diff <= -0.01 ~ 1,
                             diff >= 0.02 ~ 2,
                             TRUE ~ 0))

#################   
#  Step 2
#################  

# Loop for creating episodes
vdem$episode <- vdem$decline
for (i in 2:nrow(vdem)) {
  next.three <- vdem$decline[i:(i + 3)]
  if (vdem$episode[i] == 0) {
    if (vdem$episode[i - 1] == 1 & any(next.three == 1)) {
      vdem$episode[i] <- 1
        if ((vdem$decline[i] == 0) & (((vdem$decline[i + 1] == 2 | vdem$decline[i + 2] == 2) & (vdem$decline[i + 1] != 1) ) ) ) {
          vdem$episode[i] <- 0
      }
    }
  }
}

# Replacing 1's with random number (99) for first year per country
# Doing this because a few countries had 1's at the start of the time span
# Using 99 instead of 0 so as not to confuse no decline with the start of country df
vdem$episode[is.na(vdem$diff)] <- 99

# Replace 2's with 0's (because 2's are no-decline observations, essentially)
vdem$episode[vdem$episode == 2] <- 0


#################   
#  Step 3
################# 

## Calculating magnitude of episode decline (difference between v2x_polyarchy at the start and end of backsliding episode)
  

## Episode difference lag 1
vdem$ep.diff <- 0
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 0 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 1]
    vdem$ep.diff[i] <- dd
  }
}

## Episode difference lag 2
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 2]
    vdem$ep.diff[i] <- dd
  }
}

## Episode difference lag 3
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 3]
    vdem$ep.diff[i] <- dd
  }
}

## Episode difference lag 4
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 4]
    vdem$ep.diff[i] <- dd
  }
}

## Episode difference lag 5
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 && vdem$episode[i - 4] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 5]
    vdem$ep.diff[i] <- dd
  }
}

## Episode difference lag 6
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 && vdem$episode[i - 4] == 1 && vdem$episode[i - 5] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 6]
    vdem$ep.diff[i] <- dd
  }
}

## Episode difference lag 7
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 && vdem$episode[i - 4] == 1 && vdem$episode[i - 5] == 1 && vdem$episode[i - 6] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 7]
    vdem$ep.diff[i] <- dd
  }
}

## Episode difference lag 8
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 && vdem$episode[i - 4] == 1 && vdem$episode[i - 5] == 1 && vdem$episode[i - 6] == 1 && vdem$episode[i - 7] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 8]
    vdem$ep.diff[i] <- dd
  }
}

## Episode difference lag 9
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 && vdem$episode[i - 4] == 1 && vdem$episode[i - 5] == 1 && vdem$episode[i - 6] == 1 && vdem$episode[i - 7] == 1 && vdem$episode[i - 8] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 9]
    vdem$ep.diff[i] <- dd
  }
}

## Episode difference lag 10
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 && vdem$episode[i - 4] == 1 && vdem$episode[i - 5] == 1 && vdem$episode[i - 6] == 1 && vdem$episode[i - 7] == 1 && vdem$episode[i - 8] == 1 && vdem$episode[i - 9] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 10]
    vdem$ep.diff[i] <- dd
  }
}



#################   
#  Step 4
################# 


###### Creating function

# Attempt 1
backslide <- function(df, x, new.column) {
  df <- as.df.frame(df)
  x <- df$x
  new.column <- length(nrow(df))
  df <- df %>% group_by(country_id) %>% mutate(new.column = x - lag(x))
}

backslide(df = vdem, x = v2x_polyarchy, new.column = "diff")


# Attempt 2: Trying again, without "new.column" argument
backslide <- function(df, x) {
  df <- as.df.frame(df)
  x <- df$x
  df <- df %>% group_by(country_id) %>% mutate(diff = x - lag(x))
}

backslide(df = vdem, x = "v2x_polyarchy")





# This worked
backslide <- function(df, x, group_var) {
  group_var <- enquo(group_var)
  print(group_var)
  
  x <- enquo(x)
  print(x)
  
  df <- df %>%
    group_by(!!group_var) %>%
    mutate(diff = !!x - lag(!!x))
  
  
  
  return(df)
}

vdem3 <- backslide(vdem2, v2x_polyarchy, country_id)

 
# Adding decline variable  *worked :-)
backslide <- function(df, x, group_var, neg.threshold, pos.threshold) {
  group_var <- enquo(group_var)
  print(group_var)
  
  x <- enquo(x)
  print(x)
  
  df <- df %>%
    group_by(!!group_var) %>%
    mutate(diff = !!x - lag(!!x))
  
  df2 <- df %>% 
    group_by(!!group_var) %>%
    mutate(decline = case_when(diff <= -neg.threshold ~ 1,
                               diff >= pos.threshold ~ 2,
                               TRUE ~ 0))
  
  return(df2)
}

vdem4 <- backslide(vdem2, v2x_polyarchy, country_id, 0.02, 0.03)

table(vdem4$decline)


# Adding loop *worked :-)
backslide <- function(df, x, group_var, neg.threshold, pos.threshold, stagnation) {
  group_var <- enquo(group_var)
  print(group_var)
  
  x <- enquo(x)
  print(x)
  
  df <- df %>%
    group_by(!!group_var) %>%
    mutate(diff = !!x - lag(!!x))
  
  df <- df %>% 
    group_by(!!group_var) %>%
    mutate(decline = case_when(diff <= -neg.threshold ~ 1,
                               diff >= pos.threshold ~ 2,
                               TRUE ~ 0))
  
  df$episode <- df$decline
  for (i in 2:nrow(df)) {
    next.three <- df$decline[i:(i + stagnation)]
    if (df$episode[i] == 0) {
      if (df$episode[i - 1] == 1 & any(next.three == 1)) {
        df$episode[i] <- 1
          if ((df$decline[i] == 0) & (((df$decline[i + 1] == 2 | df$decline[i + 2] == 2) & (df$decline[i + 1] != 1) ) ) ) {
            df$episode[i] <- 0
        }
      }
    }
  }
  
  return(df)
}

vdem5 <- backslide(vdem2, v2x_polyarchy, country_id, 0.01, 0.02, 3)

table(vdem5$episode)
table(vdem$episode)


# Adding code for 99s and replacing 2's with 0's *worked :-)
backslide <- function(df, x, group_var, neg.threshold, pos.threshold, stagnation) {
  group_var <- enquo(group_var)
  print(group_var)
  
  x <- enquo(x)
  print(x)
  
  df <- df %>%
    group_by(!!group_var) %>%
    mutate(diff = !!x - lag(!!x))
  
  df <- df %>% 
    group_by(!!group_var) %>%
    mutate(decline = case_when(diff <= -neg.threshold ~ 1,
                               diff >= pos.threshold ~ 2,
                               TRUE ~ 0))
  
  df$episode <- df$decline
  for (i in 2:nrow(df)) {
    next.three <- df$decline[i:(i + stagnation)]
    if (df$episode[i] == 0) {
      if (df$episode[i - 1] == 1 & any(next.three == 1)) {
        df$episode[i] <- 1
        if ((df$decline[i] == 0) & (((df$decline[i + 1] == 2 | df$decline[i + 2] == 2) & (df$decline[i + 1] != 1) ) ) ) {
          df$episode[i] <- 0
        }
      }
    }
  }
  
  df$episode[is.na(df$diff)] <- 99
  
  df$episode[df$episode == 2] <- 0
  
  return(df)
}

vdem6 <- backslide(vdem2, v2x_polyarchy, country_id, 0.01, 0.02, 3)

table(vdem6$episode)
table(vdem$episode)


rm(backslide)

# Adding calculation of episode magnitudes  
debug(backslide)
backslide <- function(df, x, group_var, neg.threshold, pos.threshold, stagnation) {
  group_var <- enquo(group_var)
  print(group_var)
  
  xx <- enquo(x)
  print(xx)
  
  df <- df %>%
    group_by(!!group_var) %>%
    mutate(diff = !!xx - lag(!!xx))
  
  df <- df %>% 
    group_by(!!group_var) %>%
    mutate(decline = case_when(diff <= -neg.threshold ~ 1,
                               diff >= pos.threshold ~ 2,
                               TRUE ~ 0))
  
  df$episode <- df$decline
  for (i in 2:nrow(df)) {
    next.three <- df$decline[i:(i + stagnation)]
    if (df$episode[i] == 0) {
      if (df$episode[i - 1] == 1 & any(next.three == 1)) {
        df$episode[i] <- 1
        if ((df$decline[i] == 0) & (((df$decline[i + 1] == 2 | df$decline[i + 2] == 2) & (df$decline[i + 1] != 1) ) ) ) {
          df$episode[i] <- 0
        }
      }
    }
  }
  
   df$episode[is.na(df$diff)] <- 99
  
   df$episode[df$episode == 2] <- 0
  
  df$episodesize <- 0 # creating new empty column
  
  # running loop for episodes lasting 1 year (not working)
  df <- df %>% 
    group_by(!!group_var) %>%
    mutate(episode.size = case_when(episode == 1 ~ x - ,
                               diff >= pos.threshold ~ 2,
                               TRUE ~ 0))
  
  for (j in 2:nrow(df)) {
    if (df$episode[j] == 1 & df$episode[j + 1] != 1 & df$episode[j - 1] == 0) {
      dd <- df$x[j] - df$x[j-1]
      df$episodesize[j] <- dd
    }
  }
  
  # running nested loop for episodes
  for (m in 1:100) {
    for (i in 2:nrow(vdem)) {
      leads <- vdem$episode[i:(i + m)]
      if (vdem$episode[i] == 1 & vdem$episode[i - 1] != 1 && all(leads == 1))  {
        dd <- vdem$v2x_polyarchy[i + m] - vdem$v2x_polyarchy[i - 1]
        vdem$ep.diff2[i + m] <- dd
      }
    }
  }

  return(df)
}

vdem7 <- backslide(vdem2, v2x_polyarchy, country_id, 0.01, 0.02, 3)


