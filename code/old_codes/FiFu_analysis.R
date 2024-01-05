
####LIBRARIES####
library(tidyverse) # you need to load the packages each time you use R

####DATA####
pa_data <- read_csv("OL_PA_all.csv")

####CLASS CHECK####
pa_data$Shape_Length <- as.numeric(pa_data$Shape_Length)
pa_data$Shape_Area <- as.numeric(pa_data$Shape_Area)
pa_data$ID <- as.factor(pa_data$ID)
pa_data$Plot_ID <- as.factor(pa_data$Plot_ID)
pa_data$Source <- as.factor(pa_data$Source)
pa_data$Fate <- as.factor(pa_data$Fate)
pa_data$Recruit[is.na(pa_data$Recruit)] <- "N" #fill the data
pa_data$Recruit <- as.factor(pa_data$Recruit)
summary(pa_data)
head(pa_data)


####Classify juvenile and adult####
pa_data$size_class[pa_data$Shape_Area<=0.002827433 ] <- 'J' #size smaller than 6 cm is juvenile
pa_data$size_class[pa_data$Shape_Area > 0.002827433] <- 'A'
pa_data$size_class <- as.factor(pa_data$size_class)

####Calculate pa_ratio####
pa_data$pa_ratio <- pa_data$Shape_Length / pa_data$Shape_Area

####Plot pa_ratio with size####
data_2020 = filter(pa_data, Year=='2020')
data_2020
plot(log10(data_2020$Shape_Area), log10(data_2020$pa_ratio),
     pch = 10,
     col = factor(data_2020$size_class))
abline(v=log10(pi*0.03^2))
abline(h=log10(40)) #check the standard of compact adult

####Classify irregular and compact adult####
levels(pa_data$size_class) <- c(levels(pa_data$size_class), "IA", "CA")
pa_data$size_class[pa_data$pa_ratio<40 & pa_data$size_class=='A'] <- 'IA'
pa_data$size_class[pa_data$pa_ratio>=40 & pa_data$size_class=='A'] <- 'CA'

####Plot pa_ratio with size base on morphological stages####
data_2022 = filter(pa_data, Year=='2022')
data_2022
plot(log10(data_2022$Shape_Area), log10(data_2022$pa_ratio),
     pch = 10,
     col = factor(data_2022$size_class))
abline(v=log10(pi*0.03^2))
abline(h=log10(40)) #check the standard of compact adult

####Plot pa_ratio and mortality####
data_2020 = filter(pa_data, Year=='2020')
data_2020$mortality <- ifelse(data_2020$Fate == "D", 0, ifelse(is.na(data_2020$Fate), 1, 1))


###Seperate data


####Add Fate (G,S) and their sa_change####
pa_data_GS <- data.frame()
# Subset the data.frame to only include observations from the two dates of interest
for (plot_id in c("M1", "M2", "TY3")) { # loop in sites
  #print(plot_id)
  for (t in 1:2) { # loop in survey times
    #print(paste0("t=",t))
    pa_data_subset <- pa_data[pa_data$Plot_ID == plot_id,]
    pa_data_subset <- pa_data_subset[pa_data_subset$Survey_times == t | pa_data_subset$Survey_times == t+1, ]
    pa_data_subset <- pa_data_subset[!(pa_data_subset$Fate %in% c("D","Fu","Fi") & pa_data_subset$Survey_times == t), ]
    pa_data_subset <- pa_data_subset[!(pa_data_subset$Recruit == "Y"& pa_data_subset$Survey_times == t+1),] #don't calculate new colonies
    pa_data_subset <- pa_data_subset[!(!is.na(pa_data_subset$Source) & pa_data_subset$Survey_times == t+1),]
    
    # Create a new data.frame to store the area change for each coral colony
    area_change_df <- data.frame(ID = unique(pa_data_subset$ID), sa_change = NA)
    
    # Loop through each ID and calculate the area change between the two dates
    for (i in 1:length(unique(pa_data_subset$ID))) {
      #print(paste0("i=",i))
      current_id <- unique(pa_data_subset$ID)[i]
      current_area <- pa_data_subset[pa_data_subset$ID == current_id & pa_data_subset$Survey_times == t, "Shape_Area"]
      future_area <- pa_data_subset[pa_data_subset$ID == current_id & pa_data_subset$Survey_times == t+1, "Shape_Area"]
      # Check if current_area and future_area are not NA
      if (nrow(current_area) > 0 & nrow(future_area) > 0) {
        area_change <- future_area - current_area
        area_change_df[i, "sa_change"] <- area_change
      }
    }
    pa_data_subset <- pa_data_subset[pa_data_subset$Survey_times == t,] #only left t to merge
    # Merge the area change data.frame back into the original data.frame using the ID variable
    pa_data_subset$sa_change <- area_change_df$sa_change[match(pa_data_subset$ID, area_change_df$ID)]
    pa_data_GS <- rbind(pa_data_GS, pa_data_subset)
    }
}

#Calculate for growth and shrink
pa_data_GS$Fate <- ifelse(pa_data_GS$sa_change >= 0, "G", "S")

#pa_data <- pa_data %>%
 # left_join(pa_data_rbind %>% select(ID, Date, Time, sa_change, Fate),
  #          by = c("ID", "Date", "Time"))

####Add sa_change of D and N####
pa_data_DN <- data.frame()

# Subset the data.frame to only include D and N colonies
plot_id <- "M1"
t <- 1

pa_data_subset <- pa_data[pa_data$Plot_ID == plot_id,]
pa_data_subset <- pa_data_subset[pa_data_subset$Survey_times == t | pa_data_subset$Survey_times == t+1, ]
pa_data_subset <- pa_data_subset[(pa_data_subset$Fate == "D" & pa_data_subset$Survey_times == t) | (pa_data_subset$Recruit == "Y"& pa_data_subset$Survey_times == t+1), ]

pa_data_subset <- pa_data_subset[!(!is.na(pa_data_subset$Source) & pa_data_subset$Survey_times == t+1),]



# Create a new data.frame to store the area change for each coral colony
area_change_df <- data.frame(ID = unique(pa_data_subset$ID), sa_change = NA)


# Loop through each ID and calculate the area change between the two dates
for (i in 1:length(unique(pa_data_subset$ID))) {
  #print(paste0("i=",i))
  current_id <- unique(pa_data_subset$ID)[i]
  current_area <- pa_data_subset[pa_data_subset$ID == current_id & pa_data_subset$Survey_times == t, "Shape_Area"]
  future_area <- pa_data_subset[pa_data_subset$ID == current_id & pa_data_subset$Survey_times == t+1, "Shape_Area"]
  # Check if current_area and future_area are not NA
  if (nrow(current_area) > 0 & nrow(future_area) > 0) {
    area_change <- future_area - current_area
    area_change_df[i, "sa_change"] <- area_change
  }
}

####Calculate Fate####

