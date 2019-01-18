

vdem8.full <- read.csv("~/Desktop/data_2018F/V-Dem-CY-Core-v8.csv")


# Subsetting V-dem data
vdem8 <- subset(vdem8.full, year > 1959 & year < 2000, 
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







