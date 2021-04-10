## ---------------------------------- ##
##                                    ##
## Advanced R: Web Scraping Project   ##
##                                    ##
## Author: Matthew Lopez              ##
##                                    ##  
## Date Created: 2020-03-19           ##
##                                    ##
##                                    ##
## -----------------------------------##

# 1. Importing Packages --------------------------------------------
library(data.table)
library(dplyr) # for data manipulation
library(ggplot2) # for creating plots
library(hrbrthemes) # theme for ggplot2
library(plyr)
library(readr)
library(reldist) # for caclulating gini coefficient and other measures of inequality
library(rvest) #package for webscarping
library(stringr) # for parsing text 
#library(viridis)

# Set working directory to location of R file and related files
setwd("C:\\Users\\matth\\Documents\\College Classes\\Spring 2021")


# 2. Variables for URL's--------------------------------------------
# Webscraping requires gathering data for multiple teams over many years.
# To complete webscraping, will need variables that contain the different teams and 
# years for gathering data.

# Creating variable to hold all the team names as they appear in the url. 
team_url <- c("golden_state_warriors", "brooklyn_nets", "philadelphia_76ers",
              "los_angeles_clippers", "los_angeles_lakers", "milwaukee_bucks",
              "orlando_magic", "new_orleans_pelicans", "utah_jazz",
              "washington_wizards", "san_antonio_spurs", "memphis_grizzlies",
              "portland_trail_blazers", "houston_rockets", "cleveland_cavaliers",
              "minnesota_timberwolves", "dallas_mavericks", "denver_nuggets",
              "phoenix_suns", "indiana_pacers", "toronto_raptors",
              "miami_heat", "chicago_bulls", "boston_celtics",
              "atlanta_hawks", "detroit_pistons", "sacramento_kings",
              "charlotte_hornets", "oklahoma_city_thunder", "new_york_knicks")

# Creating variable which contains the different years as they appear in the url.
years <- c("1990-1991", "1991-1992", "1992-1993",
           "1993-1994", "1994-1995", "1995-1996",
           "1996-1997", "1997-1998", "1998-1999",
           "1999-2000", "2000-2001", "2001-2002",
           "2002-2003", "2003-2004", "2004-2005",
           "2005-2006", "2006-2007", "2007-2008",
           "2008-2009", "2009-2010", "2010-2011",
           "2011-2012", "2012-2013", "2013-2014",
           "2014-2015", "2015-2016", "2016-2017",
           "2017-2018", "2018-2019", "2019-2020",
           "2020-2021")



# 3. Web Scraping Loop for Team Salaries----------------------------------------------------------------------------------------------------------------------

# Create container for total team salaries to be gathered in loop
all_salaries <- data.frame(Team= character(), 
                           Salary = numeric(), Adj_salary = numeric(),
                           Year= character()
                          )

# Loop for total team salaries
for(i in 1:length(years)){
  
  if(years[i] == "2020-2021" ){ # this condition is used to test if the year being scraped is the current year. The current year has a different url than other years.
    theurl <- paste0("https://hoopshype.com/salaries/") # this variable stores the url for the current year
    
    #Download/Read the contents of the html file
    html<- read_html(theurl) 
    # Extract the table information by reading all tables on website and selecting appropriate one
    table_teams <-
      html %>%
      html_table(fill = TRUE)
    
    # Create a dataframe from the html information stored in the table_teams variable
    Team_Salaries <- data.frame(table_teams)[,-c(4:8)] # dataframe is created excluding non relevant columns
    Team_Salaries$Adj_Salary <- Team_Salaries$X3 # Creating Adjusted Salary variable, same as the "X3" Salary Variable
    Team_Salaries <- dplyr::rename(Team_Salaries,Team=X2, Nominal_Salary=X3) # Rename variables to appropriate name after importing
    Team_Salaries$Year <-  years[i] # Store the value of the year gathered in loop 
    
    # Store all information gathered in loop to the dataframe created before the loop
    all_salaries <- rbind(all_salaries,Team_Salaries[,-1])
    
    # Message is printed in order to know how far along the loop is
    print(paste0("Finished Gathering Salaries for Year: ", years[i]))

  } else{ # this condition is used for all years other than the current year
  # Creating Variable for url template
  theurl <- paste0("https://hoopshype.com/salaries/",years[i],"/")

  #Download/Read the html
  html<- read_html(theurl)
  
  # Find the table information stored in the html
  table_teams <-
    html %>%
    html_table(fill = TRUE)
  
  # Convert data to a dataframe
  Team_Salaries <- data.frame(table_teams)
  # Check to make sure all columns of data was imported properly, 1996 does not have data available as of 3/10/21
  if(length(Team_Salaries) == 4){
    
  # Rename columns so new dataframe can be combined with empty template created before loop
  Team_Salaries <- dplyr::rename(Team_Salaries,Team=X2, Nominal_Salary=X3, Adj_Salary = X4)
  # Create a year column in order to find appropriately identify data later
  Team_Salaries$Year <-  years[i]
  # Add the data found for each year to the original empty dataset created
  all_salaries <- rbind(all_salaries,Team_Salaries[,-1])
  # Print message in order to visually see where we are in process
  print(paste0("Finished Gathering Salaries for Year: ", years[i]))
  } else{
    print(paste0("Data not appropriately scraped, check url for Year:", years[i]))
    }
  }
}

# 4. Cleaning Team Salary Data----------------------------------------------------------------------------------------------------------------------
Total_Team_Salary <- all_salaries[all_salaries$Team != "Team",] # creates a dataset that excludes rows that imported the "Team" header from the website table

# Convert Salary variables to numeric variables. They were originally imported as character variables through webscraping.
Total_Team_Salary$Nominal_Salary <- as.numeric(gsub('[$,]', '', Total_Team_Salary$Nominal_Salary))
Total_Team_Salary$Adj_Salary <- as.numeric(gsub('[$,]', '', Total_Team_Salary$Adj_Salary))

# Basic Summary Stats
table(Total_Team_Salary$Team)
summary(Total_Team_Salary$Nominal_Salary)
summary(Total_Team_Salary$Adj_Salary)


# 5. Web Scraping Loop for Player Salaries----------------------------------------------------------------------------------------------------------------------

# Before scraping player salaries, create container for storing data scraped
Player_Salaries <- data.frame(Player= character(), 
                              Salary = numeric(), Adj_Salary = numeric())

# Data is scraped from year 1990 and after. Two teams did not exist until 1995, will create a list of these teams to reference in the loop. 
teams_1995 <- c("memphis_grizzlies","toronto_raptors")

# Also, the Charlotte Hornets did not play for two years.
hornets_off <- c(13, 14) # variable accounting for years hornets did not play

# The pelicans are the newest team. They will be accounted for in the loop

# Loop to collect player salaries for each team over the years
for(i in 1:length(team_url)){
  if (team_url[i] == "new_orleans_pelicans"){ #first condition to check for in loop
    
    for(j in 13:length(years)){ #loop will scrape data only for the time period the pelicans have been a team
      if (years[j] == "2020-2021"){ #check to see if the year being scraped is the current year.
      theurl <- paste0("https://hoopshype.com/salaries/",team_url[i],"/")
      
      #Download/Read the html
      html<- read_html(theurl)
    
      table_player_salary <-
      html %>%
      html_table(fill = TRUE)
      
      # Modify data thtat was scraped for ease later
      Year_Salaries <-  table_player_salary[[1]]
      Year_Salaries <- Year_Salaries[,1:2]
      Year_Salaries$Adj_Salary <- Year_Salaries$`Player Salaries.1`
      Year_Salaries$Team <- team_url[i]
      Year_Salaries$Year <- years[j]
      colnames(Year_Salaries)[2:3] <- c("Salary", "Adj_Salary")
      
      # Store scraped data
      Player_Salaries <- rbind(Player_Salaries, Year_Salaries)
      
      # Print message showing status
      print(paste0("Finished Year: ", years[j] , " for: " , team_url[i] ))
      } else{ #condition for scraping pelican data before current year
        theurl <- paste0("https://hoopshype.com/salaries/",team_url[i],"/",years[j],"/")
        
        #Download/Read the html
        html<- read_html(theurl)
        
        table_player_salary <-
          html %>%
          html_table(fill = TRUE)
        
        # Storing and modifying data
        Year_Salaries <-  table_player_salary[[1]]
        Year_Salaries$Team <- team_url[i]
        Year_Salaries$Year <- years[j]
        colnames(Year_Salaries)[2:3] <- c("Salary", "Adj_Salary")
        
        # Preserve data before next step
        Player_Salaries <- rbind(Player_Salaries, Year_Salaries)
        
        # Print message showing status
        print(paste0("Finished Year: ", years[j] , " for: " , team_url[i] ))
        
      }
    }
  } else if (team_url[i] %in% teams_1995){ #condition checking for teams not created until 1995
    for(j in 6:length(years)){
      if (years[j] == "2020-2021"){
      theurl <- paste0("https://hoopshype.com/salaries/",team_url[i],"/")
      
      #Download/Read the html
      html<- read_html(theurl)
      
      table_player_salary <-
        html %>%
        html_table(fill = TRUE)
      
      # Storing and modifying data
      Year_Salaries <-  table_player_salary[[1]]
      Year_Salaries <- Year_Salaries[,1:2]
      Year_Salaries$Adj_Salary <- Year_Salaries$`Player Salaries.1`
      Year_Salaries$Team <- team_url[i]
      Year_Salaries$Year <- years[j]
      colnames(Year_Salaries)[2:3] <- c("Salary", "Adj_Salary")
      
      # Preserve data before next step
      Player_Salaries <- rbind(Player_Salaries, Year_Salaries)
      
      # Print message showing status
      print(paste0("Finished Year: ", years[j] , " for: " , team_url[i] ))
      } else{
        theurl <- paste0("https://hoopshype.com/salaries/",team_url[i],"/",years[j],"/")
        
        #Download/Read the html
        html<- read_html(theurl)
        
        table_player_salary <-
          html %>%
          html_table(fill = TRUE)
        
        # Storing and modifying data
        Year_Salaries <-  table_player_salary[[1]]
        Year_Salaries$Team <- team_url[i]
        Year_Salaries$Year <- years[j]
        colnames(Year_Salaries)[2:3] <- c("Salary", "Adj_Salary")
        
        # Preserve data before next step
        Player_Salaries <- rbind(Player_Salaries, Year_Salaries)
        
        # Print message showing status
        print(paste0("Finished Year: ", years[j] , " for: " , team_url[i] ))
      }
    }
  } else if(team_url[i] == "charlotte_hornets"){ #last condition to check for special teams. Hornets didn't play for two seasons.
    for(j in (1:30)[-hornets_off]){
      if(years[j] == "2020-2021"){
      theurl <- paste0("https://hoopshype.com/salaries/",team_url[i],"/")
      
      #Download/Read the html
      html<- read_html(theurl)
      
      table_player_salary <-
        html %>%
        html_table(fill = TRUE)
      
      Year_Salaries <-  table_player_salary[[1]]
      Year_Salaries <- Year_Salaries[,1:2]
      Year_Salaries$Adj_Salary <- Year_Salaries$`Player Salaries.1`
      Year_Salaries$Team <- team_url[i]
      Year_Salaries$Year <- years[j]
      colnames(Year_Salaries)[2:3] <- c("Salary", "Adj_Salary")
      
      Player_Salaries <- rbind(Player_Salaries, Year_Salaries)
      
      # Print message showing status
      print(paste0("Finished Year: ", years[j] , " for: " , team_url[i] ))
      } else{ #condition for Hornets during years other than current year
        theurl <- paste0("https://hoopshype.com/salaries/",team_url[i],"/",years[j],"/")
        
        #Download/Read the html
        html<- read_html(theurl)
        
        table_player_salary <-
          html %>%
          html_table(fill = TRUE)
        
        Year_Salaries <-  table_player_salary[[1]]
        Year_Salaries$Team <- team_url[i]
        Year_Salaries$Year <- years[j]
        colnames(Year_Salaries)[2:3] <- c("Salary", "Adj_Salary")
        
        Player_Salaries <- rbind(Player_Salaries, Year_Salaries)
        
        # Print message showing status
        print(paste0("Finished Year: ", years[j] , " for: " , team_url[i] ))
      }
  }  
    
  } else{ #Final condition for scraping all teams that do not have special circumstances
      for(j in 1:length(years)){
        if(years[j] == "2020-2021"){ #current year check
        theurl <- paste0("https://hoopshype.com/salaries/",team_url[i],"/")
        
        #Download/Read the html
        html<- read_html(theurl)
        
        table_player_salary <-
          html %>%
          html_table(fill = TRUE)
        
        Year_Salaries <-  table_player_salary[[1]]
        Year_Salaries <- Year_Salaries[,1:2]
        Year_Salaries$Adj_Salary <- Year_Salaries$`Player Salaries.1`
        Year_Salaries$Team <- team_url[i]
        Year_Salaries$Year <- years[j]
        colnames(Year_Salaries)[2:3] <- c("Salary", "Adj_Salary")
        
        Player_Salaries <- rbind(Player_Salaries, Year_Salaries)
        
        # Print message showing status
        print(paste0("Finished Year: ", years[j] , " for: " , team_url[i] ))
        }else{ #all years except current year
          theurl <- paste0("https://hoopshype.com/salaries/",team_url[i],"/",years[j],"/")
          
          #Download/Read the html
          html<- read_html(theurl)
          
          table_player_salary <-
            html %>%
            html_table(fill = TRUE)
          
          Year_Salaries <-  table_player_salary[[1]]
          Year_Salaries$Team <- team_url[i]
          Year_Salaries$Year <- years[j]
          colnames(Year_Salaries)[2:3] <- c("Salary", "Adj_Salary")
          
          Player_Salaries <- rbind(Player_Salaries, Year_Salaries)
          
          # Print message showing status
          print(paste0("Finished Year: ", years[j] , " for: " , team_url[i]))
        }
        
      }
    } 

  }

# 6. Cleaning the Player Salary Data----------------------------------------------------------------------------------------------------------------------

# Create copy of original data to preserve original
Player_Salaries_2 <- Player_Salaries

# Rename Columns of dataset to simpler names
colnames(Player_Salaries_2) <- c("Player","Nominal_Salary",
                                 "Real_Salary", "Team","Season")

# Eliminate observations that were imported from header portion of tables as rows from website
Player_Salaries_2 <- Player_Salaries_2[Player_Salaries_2$Player != "Player",]
Player_Salaries_2 <- Player_Salaries_2[Player_Salaries_2$Player != "Totals",]

# Convert dollar amounts originally stored as characters to numeric 
Player_Salaries_2$Nominal_Salary <- as.numeric(gsub('[$,]', '', Player_Salaries_2$Nominal_Salary))
Player_Salaries_2$Real_Salary <- as.numeric(gsub('[$,]', '', Player_Salaries_2$Real_Salary))

# Extract Team Name from full team variable , create new variable containing only team name, not location
Player_Salaries_2$Team_Name <- str_extract(Player_Salaries_2$Team, pattern = "[^_]*$")

# Check to see if the variables appear to be created properly. 
table(Player_Salaries_2$Team_Name)

# Extract Location (city) from Team name and create new variable. This is necessary in order to merge datasets later.
Player_Salaries_2$Team_City <- str_extract(Player_Salaries_2$Team, pattern = "(.*)\\_") #using regular expression to extract the first portion of the team variable
Player_Salaries_2$Team_City <- gsub("_"," ", Player_Salaries_2$Team_City, fixed = TRUE)  
Player_Salaries_2$Team_City <- str_trim(Player_Salaries_2$Team_City) 
Player_Salaries_2$Team_City <- str_to_title(Player_Salaries_2$Team_City) 
Player_Salaries_2$Team_Name <- str_to_title(Player_Salaries_2$Team_Name) 

# Small issue with Los Angelese Teams. Because there are two team in the same city, original team was identied as "LA ......" in the website data
# Format of the value for "Team City" should be: "LA Team" in order to match the "Total_Team_Salary" dataset when being merged.
Player_Salaries_2$Team_City[Player_Salaries_2$Team_City == "Los Angeles" & 
                              Player_Salaries_2$Team == "los_angeles_clippers"] <- "LA Clippers"

Player_Salaries_2$Team_City[Player_Salaries_2$Team_City == "Los Angeles" & 
                              Player_Salaries_2$Team == "los_angeles_lakers"] <- "LA Lakers"

# Fix issue with how Portland Trail Blazers was read by stringr
Player_Salaries_2$Team_City[Player_Salaries_2$Team_City == "Portland Trail"] <- "Portland"


# 7. Final Modifications and Merging of Player and Team Salary Datasets------------------------------------------------------------------------------------------------------

# Extract first year from the Season and Year variables 
Player_Salaries_2$Season <- str_extract(Player_Salaries_2$Season, pattern = "^[^-]*")
Total_Team_Salary$Year <- str_extract(Total_Team_Salary$Year, pattern = "^[^-]*")

# Rename columns to use in merging using dplyr
Player_Salaries_Merge <-  dplyr::rename(Player_Salaries_2,Team_Merge = Team_City)

Total_Salary_Merge <-  dplyr::rename(Total_Team_Salary,
                                     Team_Merge = Team,
                                     Season = Year,
                                     Nominal_Team_Salary = Nominal_Salary,
                                     Real_Team_Salary = Adj_Salary)


# Add categorical variables grouping teams by division and conference. This can be useful for comparing 2 conferences or 6 divisions instead of 30 different teams 

# Conference Variable for "Gini_Salary_Data" as well as "Total_Salary_Merge" data.
east_conference <- c("Philadelphia", "Brooklyn", "Milwaukee",
                      "Boston", "New York", "Miami", 
                      "Charlotte", "Toronto", "Chicago", 
                      "Indiana", "Atlanta", "Washington",
                      "Cleveland", "Orlando", "Detroit")

west_conference <- c("utah", "phoenix", "LA Lakers",
                      "LA Clippers", "Portland", "Denver",
                      "San Antonio", "Dallas", "Golden State",
                      "Memphis", "New Orleans", "Oklahoma City",
                      "Sacramento", "Houston", "Minnesota")

# Create a Conference categorical variable in both Gini Salary and Total Salary datasets. 
Total_Salary_Merge$Conference <- ifelse(Total_Salary_Merge$Team_Merge %in% east_conference, "Eastern", "Western")


# Will also create categories by division for the teams. Creating 2 version of each division in order to 
Atlantic <- c("Philadelphia", "Brooklyn","Boston",
               "New York", "Toronto")

Central <- c("Milwaukee","Chicago", "Indiana",
              "Cleveland", "Detroit")

Southeast <- c( "Miami", "Charlotte", "Atlanta",
                 "Washington", "Orlando")

Northwest <- c("Utah", "Portland", "Denver",
                "Oklahoma City", "Minnesota")

Pacific <- c("Phoenix", "LA Lakers","LA Clippers",
              "Golden State", "Sacramento")

Southwest <- c("San Antonio", "Dallas", "Memphis",
                "New Orleans", "Houston")

# Loop for assigning division
for(i in 1:nrow(Total_Salary_Merge)){
  if(Total_Salary_Merge$Team_Merge[i] %in% Atlantic){
    Total_Salary_Merge$Division[i] <- "Atlantic"
    
  } else if(Total_Salary_Merge$Team_Merge[i] %in% Central){
    Total_Salary_Merge$Division[i] <- "Central"
    
  } else if(Total_Salary_Merge$Team_Merge[i] %in% Southeast){
    Total_Salary_Merge$Division[i] <- "Southeast"
    
  } else if(Total_Salary_Merge$Team_Merge[i] %in% Northwest){
    Total_Salary_Merge$Division[i] <- "Northwest"
    
  }else if(Total_Salary_Merge$Team_Merge[i] %in% Pacific){
    Total_Salary_Merge$Division[i] <- "Pacific"
  } else(
    Total_Salary_Merge$Division[i] <- "Southwest"
  )
}

# Look at the top of the each dataset
head(Player_Salaries_Merge)
head(Total_Salary_Merge)

# Compare the columns that will be used to merge the team and player salary datasets
# Team variables
table(Player_Salaries_2$Team_City)
table(Total_Team_Salary$Team)
# Year variable
table(Player_Salaries_2$Season)
table(Total_Team_Salary$Year)

# Merge the two datasets containing Player salaries and team salaries
test_merge <- full_join(Player_Salaries_Merge, Total_Salary_Merge, by = c("Team_Merge","Season")) # Merged using the "Team Merge" and "Season" variables
# Look at observations from data that has NA's. Can determine if these observations can be removed.
missing_merge <- test_merge[is.na(test_merge$Nominal_Salary),]

# Create a dataset excluding any NA variables. This variable will be used to calculate level of inequality(ex: Gini Coefficient)
Gini_Salary_Data <- na.omit(test_merge)

# Check if there are any Salaries for players missing
missing_salary <- Gini_Salary_Data[Gini_Salary_Data$Real_Salary == 0,]
# Some salaries are missing. These are from players who were injured and waved or waved during a season. 
# They ended up not technicallly recieving a salary from their team

# Subsetting by deleting these 3 observations
Gini_Salary_Data <- Gini_Salary_Data[Gini_Salary_Data$Real_Salary != 0,]

# Save file to pc
write.csv(Gini_Salary_Data, 
          "C:\\Users\\matth\\Documents\\College Classes\\Spring 2021\\Player_Salary.csv",
          row.names = TRUE)

# 8. Caclulating measure of Inequality in NBA---------------------------

# Storing copy of original merged data
Gini_Copy_Data <- Gini_Salary_Data

# Calculate gini for individual team for each season
gini_values <- aggregate(Gini_Copy_Data$Real_Salary , list(Gini_Copy_Data$Team, Gini_Copy_Data$Season),FUN= gini) # gini function included in reldist package

# Calculate gini for entire division for each season. Alternatively, will find median value by division
gini_division <- aggregate(Gini_Copy_Data$Real_Salary , list(Gini_Copy_Data$Season, Gini_Copy_Data$Division),FUN= gini)
gini_division$x <- round(gini_division$x,2) # round gini value to 2 decimal places for simplicity
gini_division <- dplyr::rename(gini_division, Season = Group.1, Division = Group.2, Gini = x)

# Calculate gini for entire conference 
gini_conference <- aggregate(Gini_Copy_Data$Real_Salary , list(Gini_Copy_Data$Season, Gini_Copy_Data$Conference),FUN= gini)
gini_conference$x <- round(gini_conference$x,2)
gini_conference <- dplyr::rename(gini_conference, Season = Group.1, Conference = Group.2, Gini = x)

#Renaming column "x" from caclulating gini
gini_values <- dplyr::rename(gini_values,
                             Team = Group.1,
                             Season= Group.2,
                             team_gini = x)

# Median Gini across NBA
median_gini <- aggregate(gini_values$team_gini , list(gini_values$Season), FUN= median)
median_gini <- dplyr::rename(median_gini, Season = Group.1, Gini = x)
median_gini$Season <- as.numeric(median_gini$Season)
median_gini$Gini <- round(median_gini$Gini, 2)

# Merge gini calculation and full data to calculate median gini from precalculated division and conference data
Team_Gini <- merge(Gini_Copy_Data,gini_values, by = c("Team", "Season"))

# Calculating Median Division Gini
Median_Div_Gini <- aggregate(Team_Gini$team_gini, list(Team_Gini$Season, Team_Gini$Division),FUN= median)
Median_Div_Gini <- dplyr::rename(Median_Div_Gini, Season = Group.1, Division = Group.2, Median_Gini = x)
Median_Div_Gini$Median_Gini <- round(Median_Div_Gini$Median_Gini ,2)

# Calculating Median Conference Gini
Median_Conference_Gini <- aggregate(Team_Gini$team_gini, list(Team_Gini$Season, Team_Gini$Conference),FUN= median)
Median_Conference_Gini <- dplyr::rename(Median_Conference_Gini, Season = Group.1, Conference = Group.2, Median_Gini = x)
Median_Conference_Gini$Median_Gini <- round(Median_Conference_Gini$Median_Gini ,2)

# Combining the median and total gini values of the conference and division datasets
Division_Ginis <- merge(Median_Div_Gini, gini_division, by = c("Season","Division"))
Conference_Ginis <- merge(Median_Conference_Gini, gini_conference, by = c("Season","Conference"))

#9. Reading in Country Inequality Data----------------------------------------

# Reading Country inequality data for visualization
Gini_Countries <- read_csv("All_Country_Gini_Data.csv", 
                           col_names = TRUE, skip = 3)

# Eliminating unnecessary columns
Gini_Countries <- Gini_Countries[,-c(2,3,4,64:66)]

# Storing select countries in variable
countries <- c("United States", "United Kingdom",
               "Germany", "France",
               "Canada", "China",
               "South Aftica", "Norway")

# Subset data to selected countries
Sample_Countries <- Gini_Countries[Gini_Countries$`Country Name` %in% countries,]
Sample_Countries <- Sample_Countries[,c(1,32:ncol(Sample_Countries))]
Sample_Countries <- as.data.frame(Sample_Countries)

# Get an idea of the data
str(Sample_Countries)
colnames(Sample_Countries)

# Convert Data from wide to long format for plotting using ggplot2
Countries_Long <- reshape(Sample_Countries,
                          direction = "long",
                          varying = list(names(Sample_Countries)[2:30]),
                          v.names = "Gini",
                          idvar =  "Country Name",
                          timevar = "Year",
                          times = 1990:2018)
colnames(Countries_Long)[1] <- "Country"  # Rename first column
Countries_Long$Gini <- Countries_Long$Gini / 100 # Convert Gini to ratio

#Plotting country inequality since 1990 - Omitting NA values for simplicity
World_Inequality <- 
  na.omit(Countries_Long) %>%
  ggplot(aes(x= Year, y = Gini, colour = Country)) +
  geom_point() + 
  geom_line() +
  labs(title = "Inequality Level of Selected Countries") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n=5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_classic()

# Printing Graph
World_Inequality

# 10. Plotting Summary Data of NBA Salaries ------------------------------------------------------------------------------------------------------

# Plotting Total Team Salary over time
ggplot(data= Total_Salary_Merge,
       mapping = aes(x=Season, y= Real_Team_Salary)) +
  geom_point() +
  scale_x_discrete(breaks = scales::pretty_breaks(n=5)) + 
  scale_y_continuous(name="Team Salary ($)", labels = scales::comma) +
  labs(title = "Total Team Salaries Since 1990")

# Plotting total salary for by color and storing in variable
Team_Salary_Graph <- 
  ggplot(data= Gini_Salary_Data,
         mapping = aes(x=Season, y= Real_Team_Salary, colour = Team_Merge)) +
  geom_point(size=2.5, alpha=0.8) +
  scale_x_discrete(breaks = scales::pretty_breaks(n=5)) + 
  scale_y_continuous(name="Team Salary ($)", labels = scales::comma) +
  labs(title = "Total Team Salaries Since 1990", colour = "Team") + 
  theme_classic()

# Print graph
Team_Salary_Graph

# Comparing East and Western Conference Income
ggplot(data= Total_Salary_Merge,
       mapping = aes(x=Season, y= Real_Team_Salary, shape=Conference, color= Conference)) +
  geom_point(size=2) +
  scale_x_discrete(breaks = scales::pretty_breaks(n=5)) + 
  scale_y_continuous(name="Team Salary ($)", labels = scales::comma) +
  labs(title = "Total Team Salaries by Conference") + 
  theme_ft_rc()

# Plotting player income 2019 with histogram
ggplot(Gini_Salary_Data[Gini_Salary_Data$Season == 2019,], aes(x = Real_Salary)) +
  geom_histogram(bins = 30) + 
  scale_x_continuous(name="Real Player Salary ($)", labels = scales::comma)

# Summarizing Player data to use in graphs 
Player_Salary_Summary <- 
Gini_Salary_Data %>%
  group_by(Season) %>%
  dplyr::summarise(Mean_Salary = mean(Real_Salary),
                   Median_Salary = median(Real_Salary),
                   Min_Salary = min(Real_Salary),
                   Max_Salary = max(Real_Salary))


# Plot Mean and Median Salaries
m <- ggplot(Player_Salary_Summary, aes(x = Season, group=1))

Mean_Median_Salary_Graph <- # Saving Graph to variable
  m + 
  geom_line(aes(y = Mean_Salary, colour= "Mean Salary"), size = .75) + 
  geom_line(aes(y = Median_Salary, colour= "Median Salary"), size = .75) +
  scale_x_discrete(breaks = scales::pretty_breaks(n=5)) +
  scale_y_continuous(name="Salary ($)", labels = scales::comma) + 
  labs(title = "Mean and Median Player Salaries") + 
  theme_classic()

# Print graph  
Mean_Median_Salary_Graph  

# Min and Max Salaries
e <- ggplot(Player_Salary_Summary, aes(x = Season, group=1))
e +
  geom_line(aes(y = Min_Salary, colour= "Min_Salary")) + 
  geom_line(aes(y = Max_Salary, colour= "Max_Salary")) +
  scale_y_continuous(name="Player Salary ($)", labels = scales::comma) + 
  labs(title = "Gap Between Minimum and Maximum Salaries")

# Minimum Salary
ggplot(Player_Salary_Summary, aes(x = Season, y = Min_Salary, group=1)) + 
  geom_line() + 
  scale_y_continuous(name="Player Salary ($)", labels = scales::comma)

# Maximum Salary
Maximum_Player_Salary <- 
  ggplot(Player_Salary_Summary, aes(x = Season, y = Max_Salary, group=1)) + 
  geom_line(size=.75, color = "black") + 
  scale_y_continuous(name="Player Salary ($)", labels = scales::comma) +
  scale_x_discrete(breaks = scales::pretty_breaks(n=5)) + 
  labs(title = "Maximum Player Salary (in real dollars)") + 
  theme_classic()
Maximum_Player_Salary

# 11. Plotting Inequality of NBA-----------------------------------------------------------------

# Plotting gini coefficient for a single team
a <- ggplot(Team_Gini[Team_Gini$Team == "golden_state_warriors",],aes(x= Season, y = team_gini))
a + geom_blank()
a + geom_path(lineend = "butt",linejoin = "round", linemitre = 10)

# Basic plot of Median Gini
a <- ggplot(median_gini,aes(x= Season, y = Gini))
a + geom_blank()
a + geom_path(lineend = "butt",linejoin = "round", linemitre = 10)

# Subsetting US Gini data for comparison with NBA
US_Gini <- Countries_Long[Countries_Long$Country == "United States",]
US_Gini$Gini <- round(US_Gini$Gini,2)
US_Gini$Season <- US_Gini$Year
US_NBA_Gini <- merge(US_Gini,median_gini, by= "Season")

# Better plot of Median Gini
NBA_Gini <- 
a + geom_point() + 
  geom_line(color = "firebrick1")+
  ylab("Gini Coefficient") +
  theme_classic() +
  scale_x_continuous(breaks = scales::pretty_breaks(n=5)) +
  labs(title = "Level of NBA League Inequality")

# Print Graph
NBA_Gini

# Comparing US Inequality to NBA Inequality
US_NBA_Graph <- 
  ggplot()+
  geom_point(data=median_gini,aes(x= Season, y = Gini)) + 
  geom_line(data= median_gini, size=1,aes(x= Season, y = Gini, colour= "NBA"))+ 
  geom_point(data=na.omit(US_Gini), aes(x= Season, y = Gini)) +
  geom_line(data= na.omit(US_Gini), aes(x = Year, y= Gini, colour= "USA"), size=1) + 
  scale_color_grey() +
  ylab("Gini Coefficient") +
  theme_classic() +
  scale_x_continuous(breaks = scales::pretty_breaks(n=5)) +
  labs(title = "Comparing NBA and US Inequality") +
  scale_colour_manual(name="Line Color", values = c(NBA = "firebrick1", USA = "black"))

# Print Graph
US_NBA_Graph

# Total Gini by NBA Division
d <- ggplot(Division_Ginis, aes(x=Season, y= Gini, group = Division, color = Division))
d +
  scale_x_discrete(breaks = scales::pretty_breaks(n=5)) +
  geom_line(size=1) +
  scale_colour_brewer(type = "qual",palette = 2) +
  labs(title = "Inequality by NBA Division", y = "Gini Coefficient") + 
  theme_classic()

# Median Gini by NBA Division
d <- ggplot(Division_Ginis, aes(x=Season, y= Median_Gini, Gini, group = Division, color = Division))

Division_Gini_Graph <- 
  d +
  scale_x_discrete(breaks = scales::pretty_breaks(n=5)) +
  geom_line(size=1) +
  scale_colour_brewer(type = "qual",palette = 2) +
  labs(title = "Median Gini by NBA Division",
       y = "Gini Coefficient") + 
  theme_classic()

# Printing Graph
Division_Gini_Graph

# Total Gini by Conference
c <- ggplot(Conference_Ginis, aes(x=Season, y= Gini, group = Conference, color = Conference))
c +
  scale_x_discrete(breaks = scales::pretty_breaks(n=5)) +
  geom_line(size=1) +
  scale_colour_brewer(type = "qual",palette = 2) +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Gini by NBA Conference",
       y = "Gini Coefficient")

# Median Gini by Conference
c <- ggplot(Conference_Ginis, aes(x=Season, y= Median_Gini, group = Conference, color = Conference))

Conference_Gini_Graph <- 
c +
  scale_x_discrete(breaks = scales::pretty_breaks(n=5)) +
  geom_line(size=1) +
  scale_colour_brewer(type = "qual",palette = 2) +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Median Gini by NBA Conference",
       y = "Gini Coefficient")

# Printing Graph
Conference_Gini_Graph

# 12. Reading in team win records-------------------------------------

# Importing team records and gini values for US and developed world.
Team_record <- read.csv("Win History for NBA Teams.csv")
Team_record
str(Team_record)

# Creating column containing abbreviations
Team_record <- Team_record[,-c(1,3)]
Team_abbr <- as.data.frame(colnames(Team_record))
Team_abbr <- as.data.frame(Team_abbr[-1,])
colnames(Team_abbr) <- "Abbreviation"
Team_abbr[] <- lapply(Team_abbr, as.character)

# Preparing to reshape data to long format
n <- Team_record$Season
Team_transpose <- transpose(Team_record[,-1])
colnames(Team_transpose) <- n
Team_transpose <- Team_transpose[,ncol(Team_transpose):1]
Win_record <- cbind(Team_abbr,Team_transpose)

Win_Reshape <- reshape(Win_record,
                       direction = "long",
                       varying = list(names(Win_record)[2:32]),
                       v.names = "Wins",
                       idvar =  "Abbreviation",
                       timevar = "Season",
                       times = 1990:2020)
  
# Read in text containing NBA Abbreviations
NBA_Team_Abbreviations <- read_delim("NBA Team Abbreviations.txt", 
                                     "-", escape_double = FALSE, col_names = FALSE, 
                                     trim_ws = TRUE)
# Rename columns
colnames(NBA_Team_Abbreviations) <- c("Abbr", "Team")

# Missing San Antonio Spurs, add team in new row:
Spurs <- data.frame(Abbr = "SAS", 
                       Team = "San Antonio Spurs")

# Insert the team that was missing using rbind
NBA_Team_Abbreviations <- rbind(NBA_Team_Abbreviations,Spurs)

#Some abbreviations imported were old or wrong. Change them to the correct abbreviation
NBA_Team_Abbreviations$Abbr[NBA_Team_Abbreviations$Abbr == "CHA"] <- "CHO"
NBA_Team_Abbreviations$Abbr[NBA_Team_Abbreviations$Abbr == "NOH"] <- "NOP"
NBA_Team_Abbreviations$Abbr[NBA_Team_Abbreviations$Abbr == "BKN"] <- "BRK"
NBA_Team_Abbreviations$Abbr[NBA_Team_Abbreviations$Abbr == "UTH"] <- "UTA"

NBA_Team_Abbreviations <- NBA_Team_Abbreviations[order(NBA_Team_Abbreviations$Abbr),]

# Combine new NBA_Team_Abbreviations with the Win_Reshape data. This is done to have a variable to merge with the Gini Salary Data
Wins_Team <- merge(Win_Reshape, NBA_Team_Abbreviations,
                   by.x = "Abbreviation",
                   by.y = "Abbr")

# Finally, Combine Wins with the Gini Values Data
gini_values$Team <- gsub("_"," ", gini_values$Team, fixed = TRUE)
gini_values$Team <- str_to_title(gini_values$Team)
Gini_and_Wins <- merge(gini_values,Wins_Team, by = c("Team", "Season"))

# Comparing gini and # of wins. All Years
Gini_Win_Pairwise_Graph <- 
  ggplot(data = Gini_and_Wins, aes(x = team_gini, y = Wins)) + 
  geom_point(color = "#ef8a62") + 
  facet_wrap(~Season) + 
  xlab("Gini")

# Comparing gini and # of wins. Certain Years
ggplot(data = Gini_and_Wins[Gini_and_Wins$Season %in% c(1990:2000),], aes(x = team_gini, y = Wins)) + 
  geom_point(color = "#ef8a62", size= 1.5) + 
  facet_wrap(~Season) + 
  xlab("Gini")

# Smoothing the pairwise comparisons
ggplot(data = Gini_and_Wins, aes(x = team_gini, y = Wins)) + 
  geom_point(color = "#ef8a62") + 
  facet_wrap(~Season) + 
  geom_smooth(method = "lm")+
  xlab("Gini")



# Output graphs for presentation using Cairo --------------------------------------

# Inequality of different countries
ggsave(World_Inequality, filename = "World Inequality.png", dpi = 300, type = "cairo",
       width = 6, height = 3, units = "in")

# Total Team Salry Since 1990
ggsave(Team_Salary_Graph, filename = "Team Salary Graph.png", dpi = 300, type = "cairo",
       width = 15, height = 7, units = "in")

# Maximum Player Salary
ggsave(Maximum_Player_Salary, filename = "Maximum Player Salary Graph.png", dpi = 300, type = "cairo",
       width = 6, height = 3, units = "in")

# Mean/Median Player Salary
ggsave(Mean_Median_Salary_Graph, filename = "Mean-Median Salary Graph.png", dpi = 300, type = "cairo",
       width = 6, height = 3, units = "in")

# NBA Gini
ggsave(NBA_Gini, filename = "NBA Gini Graph.png", dpi = 300, type = "cairo",
       width = 6, height = 3, units = "in")

# US/NBA Inequality
ggsave(US_NBA_Graph, filename = "US-NBA Inequality Graph.png", dpi = 300, type = "cairo",
       width = 6, height = 3, units = "in")

# Gini Division
ggsave(Division_Gini_Graph, filename = "Gini Division Graph.png", dpi = 300, type = "cairo",
       width = 6, height = 3, units = "in")

# Gini Conference
ggsave(Conference_Gini_Graph, filename = "Gini Conference Graph.png", dpi = 300, type = "cairo",
       width = 6, height = 3, units = "in")

# Gini and Win Pairwise comparison
ggsave(Gini_Win_Pairwise_Graph, filename = "Gini and Win Pairwise Graph.png", dpi = 300, type = "cairo",
       width = 15, height = 7, units = "in")




# After submitting project---------------------------------
# Correlation / Linear regression
head(Gini_Salary_Data)
head(Gini_and_Wins)

ols <- lm(Wins ~ team_gini, data= Gini_and_Wins)
summary(ols)

ols_2 <- lm(Wins ~ log(team_gini), data= Gini_and_Wins)
summary(ols_2)

ols_3 <- lm(Wins ~ team_gini + factor(Team) - 1, data= Gini_and_Wins)
summary(ols_3)

cor(Gini_and_Wins[,c(3,5)])

library(plm)
plm_model <- plm(Wins ~ team_gini, data= Gini_and_Wins,
                 index = c("Team", "Season"), model="within")
summary(plm_model)
