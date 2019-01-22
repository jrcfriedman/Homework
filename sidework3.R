

df <- vdem

#################   
#  Step 1
################# 

# Creating new column (diff) that calculates the change in v2x_polyarchy from one year to the next
df <- df %>% 
  group_by(country_id) %>%
  mutate(diff = v2x_polyarchy - lag(v2x_polyarchy))

# Creating new column coded as follows: 1 = decline greater/equal to .01, 2 = incline greater/equal to .02, 0 = all else
df <- df %>% 
  group_by(country_id) %>%
  mutate(decline = case_when(diff <= -0.01 ~ 1,
                             diff >= 0.02 ~ 2,
                             TRUE ~ 0))

#################   
#  Step 2
#################  

# Loop for creating episodes
df$episode <- df$decline
for (i in 2:nrow(df)) {
  next.three <- df$decline[i:(i + 3)]
  if (df$episode[i] == 0) {
    if (df$episode[i - 1] == 1 & any(next.three == 1)) {
      df$episode[i] <- 1
      if ((df$decline[i] == 0) & (((df$decline[i + 1] == 2 | df$decline[i + 2] == 2) & (df$decline[i + 1] != 1) ) ) ) {
        df$episode[i] <- 0
      }
    }
  }
}

# Replacing 1's with random number (99) for first year per country
# Doing this because a few countries had 1's at the start of the time span
# Using 99 instead of 0 so as not to confuse no decline with the start of country df
df$episode[is.na(df$diff)] <- 99

# Replace 2's with 0's (because 2's are no-decline observations, essentially)
df$episode[df$episode == 2] <- 0


#################   
#  Step 3
################# 

## Calculating magnitude of episode decline (difference between v2x_polyarchy at the start and end of backsliding episode)


## Episode difference lag 1
df$episode.size <- 0
for (i in 2:nrow(df)) {
  if (df$episode[i] == 1 & df$episode[i - 1] == 0 & df$episode[i + 1] != 1)  {
    dd <- df$v2x_polyarchy[i] - df$v2x_polyarchy[i - 1]
    df$episode.size[i] <- dd
  }
}

## Episode difference lag 2
for (i in 2:nrow(df)) {
  if (df$episode[i] == 1 & df$episode[i - 1] == 1 & df$episode[i + 1] != 1)  {
    dd <- df$v2x_polyarchy[i] - df$v2x_polyarchy[i - 2]
    df$episode.size[i] <- dd
  }
}

## Episode difference lag 3
for (i in 2:nrow(df)) {
  if (df$episode[i] == 1 & df$episode[i - 1] == 1 && df$episode[i - 2] == 1 & df$episode[i + 1] != 1)  {
    dd <- df$v2x_polyarchy[i] - df$v2x_polyarchy[i - 3]
    df$episode.size[i] <- dd
  }
}

## Episode difference lag 4
for (i in 2:nrow(df)) {
  if (df$episode[i] == 1 & df$episode[i - 1] == 1 && df$episode[i - 2] == 1 && df$episode[i - 3] == 1 & df$episode[i + 1] != 1)  {
    dd <- df$v2x_polyarchy[i] - df$v2x_polyarchy[i - 4]
    df$episode.size[i] <- dd
  }
}

## Episode difference lag 5
for (i in 2:nrow(df)) {
  if (df$episode[i] == 1 & df$episode[i - 1] == 1 && df$episode[i - 2] == 1 && df$episode[i - 3] == 1 && df$episode[i - 4] == 1 & df$episode[i + 1] != 1)  {
    dd <- df$v2x_polyarchy[i] - df$v2x_polyarchy[i - 5]
    df$episode.size[i] <- dd
  }
}

## Episode difference lag 6
for (i in 2:nrow(df)) {
  if (df$episode[i] == 1 & df$episode[i - 1] == 1 && df$episode[i - 2] == 1 && df$episode[i - 3] == 1 && df$episode[i - 4] == 1 && df$episode[i - 5] == 1 & df$episode[i + 1] != 1)  {
    dd <- df$v2x_polyarchy[i] - df$v2x_polyarchy[i - 6]
    df$episode.size[i] <- dd
  }
}

## Episode difference lag 7
for (i in 2:nrow(df)) {
  if (df$episode[i] == 1 & df$episode[i - 1] == 1 && df$episode[i - 2] == 1 && df$episode[i - 3] == 1 && df$episode[i - 4] == 1 && df$episode[i - 5] == 1 && df$episode[i - 6] == 1 & df$episode[i + 1] != 1)  {
    dd <- df$v2x_polyarchy[i] - df$v2x_polyarchy[i - 7]
    df$episode.size[i] <- dd
  }
}

## Episode difference lag 8
for (i in 2:nrow(df)) {
  if (df$episode[i] == 1 & df$episode[i - 1] == 1 && df$episode[i - 2] == 1 && df$episode[i - 3] == 1 && df$episode[i - 4] == 1 && df$episode[i - 5] == 1 && df$episode[i - 6] == 1 && df$episode[i - 7] == 1 & df$episode[i + 1] != 1)  {
    dd <- df$v2x_polyarchy[i] - df$v2x_polyarchy[i - 8]
    df$episode.size[i] <- dd
  }
}

## Episode difference lag 9
for (i in 2:nrow(df)) {
  if (df$episode[i] == 1 & df$episode[i - 1] == 1 && df$episode[i - 2] == 1 && df$episode[i - 3] == 1 && df$episode[i - 4] == 1 && df$episode[i - 5] == 1 && df$episode[i - 6] == 1 && df$episode[i - 7] == 1 && df$episode[i - 8] == 1 & df$episode[i + 1] != 1)  {
    dd <- df$v2x_polyarchy[i] - df$v2x_polyarchy[i - 9]
    df$episode.size[i] <- dd
  }
}

## Episode difference lag 10
for (i in 2:nrow(df)) {
  if (df$episode[i] == 1 & df$episode[i - 1] == 1 && df$episode[i - 2] == 1 && df$episode[i - 3] == 1 && df$episode[i - 4] == 1 && df$episode[i - 5] == 1 && df$episode[i - 6] == 1 && df$episode[i - 7] == 1 && df$episode[i - 8] == 1 && df$episode[i - 9] == 1 & df$episode[i + 1] != 1)  {
    dd <- df$v2x_polyarchy[i] - df$v2x_polyarchy[i - 10]
    df$episode.size[i] <- dd
  }
}

