index_table <- function(year) { # REQUIRES THE VARS DATAFRAME: vars <- read.csv("GI_Values_Tidy.csv")
  vars.year <- vars %>% filter(YEAR == year) %>% # Widening the df using GI_VAR 
    pivot_wider(names_from = GI_VAR, values_from = VALUE)
  
  GI_df <- data.frame(vars.year[1,]) # Creates a dataframe to populate with CCA data
  for(row in seq(2, dim(vars.year)[1])) { # first row is Chicago Total
    for (col in seq(4, dim(vars.year)[2])) { # first 3 columns are CCA, Year, Population
      GI_df[row, col] <- ifelse(vars.year[row, col] > vars.year[1, col], -1, 1) # -1 is below the city average, 1 is above
    }
  }
  
  for(int in c(4, 9, 10, 11, 12, 14, 16)) {
    GI_df[2:78, int] <- GI_df[2:78, int] * -1 # converts these index value columns to their correct values according to the methodology
  }
  
  
  for(int in seq(1,3)){ # Adds information for columns 1:3 from the table
    GI_df[,int] <- vars.year[,int]
  }
  
  GI_df <- GI_df %>% mutate("Index Total" = rowSums(.[4:16])) # Sums the GI value for each CCA
  names(GI_df)[names(GI_df) == "Population"] <- paste("Population ", year)
  names(GI_df)[names(GI_df) == "Index Total"] <- paste("Index Total ", year) # Assign a year to the Index Total field
  GI_df$COMMUNITY.AREA <- paste0(factor(GI_df$COMMUNITY.AREA)) # De-factor field
  GI_df$CCA <- toupper(str_extract(GI_df$COMMUNITY.AREA, "[a-zA-z].*")) # Split name from CCA num
  GI_df$CCA.NUM <- str_extract(GI_df$COMMUNITY.AREA, "[:digit:]*") # Split num from CCA name
  GI_df <- GI_df %>% select(COMMUNITY.AREA, CCA, CCA.NUM, everything()) # reorder for readability
}

set_typology <- function(change, value){
  ifelse(change > 4 | change < -4, case_when( # If there is change
    change > 4 & change <= 7 ~ "5 - Not Gentrification", 
    change > 7 ~ "6 - Gentrification",
    change < -4 & change >= -7 ~ "7 - Mild Decline",
    change < -7 & change > -10 ~ "8 - Moderate Decline",
    change <= -10 ~ "9 - Serious Decline",
  ), case_when( # If there isn't change
    value > 7 ~ "1 - Upper Class",
    value > 0 & value <= 7 ~ "2 - Middle Class",
    value < 0 & value >= -7 ~ "3 - Poverty",
    value < -7 ~ "4 - Extreme Poverty",
  )
  )
}

change_table <- function(from, to) { # Uses all functions above and requires the vars df to be imported (GI_Values_Tidy.csv)
  ind.from <- index_table(from)
  
  ind.to <- index_table(to)
  
  change.index <- ind.from %>% select(COMMUNITY.AREA:CCA.NUM, contains("Population"), -(Pct.White:Pct.Private.School.Attendance), contains("Index Total")) # Creates change.index df
  change.index <- ind.to %>% select(COMMUNITY.AREA,contains("Population"), contains("Index Total")) %>% left_join(change.index) # Appends ind.to$Index Total to the previous df
  change.index <- change.index %>% select(COMMUNITY.AREA, CCA, CCA.NUM, contains(paste(from)), contains(paste(to))) # Arranges df
  
  change.index$change <- change.index[,7] - change.index[,5] # Creates change value
  
  change.index <- change.index[2:78,] # removes city of Chicago observation
  
  change <- change.index$change # Sets variables into vector objects
  value <- change.index[,7]
  change.index <- change.index %>% mutate(type = as.factor(set_typology(change, value))) # determines typology and appends to end of change.index df
}

var_over_time <- function(cca, from, to, var) { # requires the vars df to be imported (GI_Values_Tidy.csv)
  vars.year <- vars  %>% filter(COMMUNITY.AREA == cca) %>%# Widen the df using GI_VAR and filter the cca
    pivot_wider(names_from = GI_VAR, values_from = VALUE) %>% mutate_if(is.numeric, round, 1)
  
  # clean up year column for plotting
  
  vars.year$YEAR <- acsDateFix(vars.year$YEAR)
  to <- acsDateFix(to)
  
  # plot
  
  from <- as.Date(paste0(from, "-01-01"))
  to <- as.Date(paste0(to, "-01-01"))
  gi_var <- var 
  
  vars.year %>% filter(vars.year$YEAR >= from & vars.year$YEAR <= to) %>% # filter for the time period selected
    ggplot() + 
    geom_line(aes(x = YEAR, y = !!sym(gi_var))) + 
    labs(title = vars.year$COMMUNITY.AREA)
}

acsDateFix <- function(str) { # Functionalizing turning the year data into something usable with Lubridate
  str <- case_when(
    str == "2008 - 2012" ~ 2012,
    str == "2013 - 2017" ~ 2017,
    TRUE ~ as.numeric(paste(str))
  )
  
  str <- as.Date(paste0(str, "-01-01"))
}
