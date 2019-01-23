

vdem.sub <- subset(vdem.full, year > 1899, 
                select = c(country_name, country_text_id, country_id, year, v2x_polyarchy))


#################   
#  Step 1
################# 

# Creating new column (diff) that calculates the change in v2x_polyarchy from one year to the next
vdem.sub <- vdem.sub %>% 
  group_by(country_id) %>%
  mutate(diff = v2x_polyarchy - lag(v2x_polyarchy))

# Creating new column coded as follows: 1 = decline greater/equal to .01, 2 = incline greater/equal to .02, 0 = all else
vdem.sub <- vdem.sub %>% 
  group_by(country_id) %>%
  mutate(decline = case_when(diff <= -0.01 ~ 1,
                             diff >= 0.02 ~ 2,
                             TRUE ~ 0))

#################   
#  Step 2
#################  

# Loop for creating episodes
vdem.sub$episode <- vdem.sub$decline
for (i in 2:nrow(vdem.sub)) {
  next.three <- vdem.sub$decline[i:(i + 3)]
  if (vdem.sub$episode[i] == 0) {
    if (vdem.sub$episode[i - 1] == 1 & any(next.three == 1)) {
      vdem.sub$episode[i] <- 1
      if ((vdem.sub$decline[i] == 0) & (((vdem.sub$decline[i + 1] == 2 | vdem.sub$decline[i + 2] == 2) & (vdem.sub$decline[i + 1] != 1) ) ) ) {
        vdem.sub$episode[i] <- 0
      }
    }
  }
}

# Replacing 1's with random number (99) for first year per country
# Doing this because a few countries had 1's at the start of the time span
# Using 99 instead of 0 so as not to confuse no decline with the start of country df
vdem.sub$episode[is.na(vdem.sub$diff)] <- 99

# Replace 2's with 0's (because 2's are no-decline observations, essentially)
vdem.sub$episode[vdem.sub$episode == 2] <- 0








xx <- vdem.sub$episode
yy <- rle(xx)
table(yy$values)
zz <- yy$values
vv <- yy$lengths
yyd <- data.frame("episode" = zz, "count" = vv)
table(yyd$episode)
max(yyd$count[yyd$episode == 1])









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

return(df)
}












  


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



## Trying condensed way
vdem$ep.diff2 <- 0
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 && vdem$episode[i + 1] != 1 && all(vdem$episode[i:(i - 9)] == 1))  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 10]
    vdem$ep.diff2[i] <- dd
  }
}

vdem$ep.diff2 <- 0
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 & vdem$episode[i + 1] != 1 && (vdem$episode[i - (2:9)] == 1))  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 10]
    vdem$ep.diff[i] <- dd
  }
}

## Trying condensed way
vdem$ep.diff2 <- 0
for (i in 2:nrow(vdem)) {
  prev.four <- vdem$episode[i:(i + 4)]
  if (vdem$episode[i] == 1 & vdem$episode[i + 1] != 1 & all(prev.four == 1))  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 5]
    vdem$ep.diff2[i] <- dd
  }
}




vdem$ep.diff2 <- 0
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 & vdem$episode[i + 1] != 1 && vdem$episode[i - 3] == 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 6]
    vdem$ep.diff2[i] <- dd
  }
}

class(vdem$ep.diff)




# this
vdem$ep.diff2 <- 0
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 0 & vdem$episode[i + 1] != 1)  {
    vdem$ep.diff2[i] <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 1]
  }
}



for (m in 1:100) {
  for (i in 2:nrow(vdem)) {
    leads <- vdem$episode[i:(i + m)]
    if (vdem$episode[i] == 1 & vdem$episode[i - 1] != 1 && all(leads == 1))  {
      dd <- vdem$v2x_polyarchy[i + m] - vdem$v2x_polyarchy[i - 1]
      vdem$ep.diff2[i + m] <- dd
    }
  }
}

for (m in 1:20) {
  for (i in 2:nrow(vdem)) {
    leads <- vdem$episode[i:(i + m)]
    if (vdem$episode[i] == 1 & vdem$episode[i - 1] != 1 && all(leads == 1))  {
      dd <- vdem$v2x_polyarchy[i - 1] - vdem$v2x_polyarchy[i + m]
      vdem$ep.diff2[i + m] <- dd
    }
  }
}


  
  vdem <- vdem %>% 
    group_by(country_id) %>%
    mutate(episode.size = case_when(episode == 1 & lead(episode) == 0 & lag(episode) == 0 ~ v2x_polyarchy - lag(v2x_polyarchy),
                                    TRUE ~ 0))
  

vdem <- vdem %>% 
  group_by(country_id) %>%
  mutate(episode.size = case_when(episode == 1 & lead(episode) == 0 & lag(episode) == 0 ~ v2x_polyarchy - lag(v2x_polyarchy),
                                  TRUE ~ 0))




> vdem$ep.diff2 <- 0
> for (m in 1:20) {
  +   for (i in 2:nrow(vdem)) {
    +     leads <- vdem$episode[i + m]
    +     if (vdem$episode[i] == 1 & vdem$episode[i - 1] != 1 && all(leads == 1))  {
      +       dd <- vdem$v2x_polyarchy[i - 1] - vdem$v2x_polyarchy[i + m]
      +       vdem$ep.diff2[i] <- dd
      +     }
    +   }
  + }



for (i in 2:nrow(vdem)) {
  if (!is.na(vdem$episode[i])) {
    if (vdem$episode[i] == 1 & vdem$episode[i + 1] != 1 && vdem$episode[i - 3] == 1)  {
      dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 4]
      vdem$ep.diff2[i] <- dd
    }
  }
}  


if (!is.na(vdem$episode[i])) 
vdem.sub$decline[i:(i + 3)]

&& vdem$episode[i - m] == 1


