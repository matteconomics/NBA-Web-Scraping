# Scraping NBA Data --------------------------------------------------



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
#setwd("C:\\Users\\matth\\Documents\\College Classes\\Spring 2021")


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

# Extracting first portion of the year
Total_Team_Salary$Year <- str_extract(Total_Team_Salary$Year, pattern = "^[^-]*")

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

# Extracting first portion of the season.
Player_Salaries_2$Season <- str_extract(Player_Salaries_2$Season, pattern = "^[^-]*")

#7. Saving Scraped Datasets ----------------------------------------------

# View each of the scraped data after cleaning
head(Player_Salaries_2)
head(Total_Team_Salary)

# Save files
write.csv(Player_Salaries_2,
          "data\\Player_Salaries_2.csv",
          row.names = TRUE)

write.csv(Total_Team_Salary,
          "data\\Total_Team_Salary.csv",
          row.names = TRUE)


