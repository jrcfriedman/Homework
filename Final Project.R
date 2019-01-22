

rm(list=ls())

library(dplyr)

# Reading in data
vdem.full <- read.csv("~/Desktop/data_2018F/V-Dem-CY-Core-v8.csv")

reb<-read.dta("http://willreed.org/concessions_elections.dta")

# Subsetting V-dem data
vdem <- subset(vdem.full, year > 1959 & year < 2000, 
               select = c(country_name, country_text_id, country_id, year, v2x_polyarchy))


# Change in v2x_polyarchy from one year to the next
vdem <- vdem %>% 
  group_by(country_id) %>%
  mutate(diff = v2x_polyarchy - lag(v2x_polyarchy))

# New variable: 1 = decline greater/equal to .01, 2 = incline greater/equal to .02, 0 = all else
vdem <- vdem %>% 
  group_by(country_id) %>%
  mutate(decline = case_when(diff <= -0.01 ~ 1,
                             diff >= 0.02 ~ 2,
                             TRUE ~ 0))

# Loop for creating episodes
vdem$episode <- vdem$decline
for (i in 2:nrow(vdem)) {
  next.three <- vdem$decline[i:(i + 3)]
  if (vdem$episode[i] == 0) {
    if (vdem$episode[i - 1] == 1 & any(next.three == 1)) {
      vdem$episode[i] <- 1
      if (vdem$episode[i - 1] == 1 & any(next.three == 1)) {
        vdem$episode[i] <- 1
        if ((vdem$decline[i] == 0) & (((vdem$decline[i + 1] == 2 | vdem$decline[i + 2] == 2) & (vdem$decline[i + 1] != 1) ) ) ) {
          vdem$episode[i] <- 0
        }
      }
    }
  }
}

# Replaces 1's with 0's for first year per country
vdem$episode[is.na(vdem$diff)] <- 99

# Replace 2's with 0's
vdem$episode[vdem$episode == 2] <- 0



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







###### Creating function

backslide(vdemdata = vdem, x = v2x_polyarchy, group = country_id, new.column = "Episodes")

backslide <- function(data, x, new.column) {
  data <- as.data.frame(data)
  x <- data$x
  new.column <- length(nrow(data))
  data <- data %>% group_by(country_id) %>% mutate(new.column = x - lag(x))
}

backslide(data = vdem, x = v2x_polyarchy, new.column = "diff")


# Trying again, without "new.column" argument
backslide <- function(data, x) {
  data <- as.data.frame(data)
  x <- data$x
  data <- data %>% group_by(country_id) %>% mutate(diff = x - lag(x))
}

backslide(data = vdem, x = "v2x_polyarchy")
