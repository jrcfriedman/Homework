



f <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
f()
rm(f)






#################   
#  Step 1
################# 

# Creating new column (diff) that calculates the change in v2x_polyarchy from one year to the next
vdemx <- vdemx %>% 
  group_by(country_id) %>%
  mutate(diff = v2x_polyarchy - lag(v2x_polyarchy))

# Creating new column coded as follows: 1 = decline greater/equal to .01, 2 = incline greater/equal to .02, 0 = all else
vdemx <- vdemx %>% 
  group_by(country_id) %>%
  mutate(decline = case_when(diff <= -0.01 ~ 1,
                             diff >= 0.02 ~ 2,
                             TRUE ~ 0))

#################   
#  Step 2
#################  
# Loop for creating episodes
vdemx$episode <- vdemx$decline
for (i in 2:nrow(vdemx)) {
  next.three <- vdemx$decline[i:(i + 3)]
  if (vdemx$episode[i] == 0) {
    if (vdemx$episode[i - 1] == 1 & any(next.three == 1)) {
      vdemx$episode[i] <- 1
        if ((vdemx$decline[i] == 0) & (((vdemx$decline[i + 1] == 2 | vdemx$decline[i + 2] == 2) & (vdemx$decline[i + 1] != 1) ) ) ) {
          vdemx$episode[i] <- 0
      }
    }
  }
}







table(vdem$episode)
table(vdemx$episode)





## Episode difference lag 1
vdem$episode.size <- 0
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 0 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 1]
    vdem$episode.size[i] <- dd
  }
}

## Episode difference lag 2
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 2]
    vdem$episode.size[i] <- dd
  }
}

## Episode difference lag 3
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 3]
    vdem$episode.size[i] <- dd
  }
}

## Episode difference lag 4
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 4]
    vdem$episode.size[i] <- dd
  }
}

## Episode difference lag 5
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 && vdem$episode[i - 4] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 5]
    vdem$episode.size[i] <- dd
  }
}

## Episode difference lag 6
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 && vdem$episode[i - 4] == 1 && vdem$episode[i - 5] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 6]
    vdem$episode.size[i] <- dd
  }
}

## Episode difference lag 7
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 && vdem$episode[i - 4] == 1 && vdem$episode[i - 5] == 1 && vdem$episode[i - 6] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 7]
    vdem$episode.size[i] <- dd
  }
}

## Episode difference lag 8
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 && vdem$episode[i - 4] == 1 && vdem$episode[i - 5] == 1 && vdem$episode[i - 6] == 1 && vdem$episode[i - 7] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 8]
    vdem$episode.size[i] <- dd
  }
}

## Episode difference lag 9
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 && vdem$episode[i - 4] == 1 && vdem$episode[i - 5] == 1 && vdem$episode[i - 6] == 1 && vdem$episode[i - 7] == 1 && vdem$episode[i - 8] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 9]
    vdem$episode.size[i] <- dd
  }
}

## Episode difference lag 10
for (i in 2:nrow(vdem)) {
  if (vdem$episode[i] == 1 & vdem$episode[i - 1] == 1 && vdem$episode[i - 2] == 1 && vdem$episode[i - 3] == 1 && vdem$episode[i - 4] == 1 && vdem$episode[i - 5] == 1 && vdem$episode[i - 6] == 1 && vdem$episode[i - 7] == 1 && vdem$episode[i - 8] == 1 && vdem$episode[i - 9] == 1 & vdem$episode[i + 1] != 1)  {
    dd <- vdem$v2x_polyarchy[i] - vdem$v2x_polyarchy[i - 10]
    vdem$episode.size[i] <- dd
  }
}









# Preserving old loop that had extra if statement -- see *
# vdem$episode <- vdem$decline
# for (i in 2:nrow(vdem)) {
#   next.three <- vdem$decline[i:(i + 3)]
#   if (vdem$episode[i] == 0) {
#     if (vdem$episode[i - 1] == 1 & any(next.three == 1)) {
#       vdem$episode[i] <- 1
#     * if (vdem$episode[i - 1] == 1 & any(next.three == 1)) {
#         vdem$episode[i] <- 1
#         if ((vdem$decline[i] == 0) & (((vdem$decline[i + 1] == 2 | vdem$decline[i + 2] == 2) & (vdem$decline[i + 1] != 1) ) ) ) {
#           vdem$episode[i] <- 0
#         }
#       }
#     }
#   }
# }