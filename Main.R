library(dplyr)
library(tidyr)
library(stringr)
library(rvest)
library(RSelenium)
library(SuppDists)

electoral_college <- read_html(
  "https://ballotpedia.org/Electoral_College_in_the_2024_presidential_election"
)

table_nodes <- html_nodes(electoral_college, "table")

table_df <- html_table(table_nodes[[2]], fill = TRUE)

table_df <- table_df[-1, ] # remove first row 
table_df <- table_df[, -c(3, 4 ,5)] # 3-5 col

# write.csv(table_df, "simpleEC.csv")

names(table_df)[1] <- "State"
names(table_df)[2] <- "EV"

# Vector of state names and abbreviations
state_names <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California",
  "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
  "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
  "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
  "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
  "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
  "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
  "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
  "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
)

state_abbr <- c(
  "AL", "AK", "AZ", "AR", "CA",
  "CO", "CT", "DE", "FL", "GA",
  "HI", "ID", "IL", "IN", "IA",
  "KS", "KY", "LA", "ME", "MD",
  "MA", "MI", "MN", "MS", "MO",
  "MT", "NE", "NV", "NH", "NJ",
  "NM", "NY", "NC", "ND", "OH",
  "OK", "OR", "PA", "RI", "SC",
  "SD", "TN", "TX", "UT", "VT",
  "VA", "WA", "WV", "WI", "WY"
)

# Convert to lowercase and format as links
state_links <- tolower(state_names)
state_links <- gsub(" ", "-", state_links, fixed = TRUE)

# Construct URLs
base_url <- "https://www.realclearpolling.com/polls/president/general/2024/"
urls <- paste0(base_url, state_links, "/trump-vs-harris")

# Create data frame
state_data <- data.frame(
  Abbrev = state_abbr,
  State = state_names,
  URL = urls,
  stringsAsFactors = FALSE
)

# Print the data frame
print(state_data)


#initialize state polls dataframe 
state_polls_24 <- as.data.frame(matrix(NA, nrow = 0,ncol = 7))
names(state_polls_24) <- c("Poll", "Date", "Sample", "Harris (D)", "Trump (R)", "State", "Rank")

driver <- rsDriver(browser="firefox", port=4555L, verbose=F, chromever = NULL)

remote <- driver[["client"]]

for(i in 1:nrow(state_data)){
  URL <- state_data$URL[i]
  state <- state_data$State[i]
  print(state)
  
  remote$navigate(URL)
  Sys.sleep(5)
  
  page_source <- remote$getPageSource()[[1]]
  page <- read_html(page_source)
  tables <- page %>% html_nodes("table")
  
  
  # Check if tables were found
  if (length(tables) > 0) {
    # Assuming the last table is the one you want (you may need to adjust this)
    state_polls <- tables[[length(tables)]] %>%
      html_table(fill = TRUE) %>%
      as.data.frame()
    
    # Select relevant columns (adjust column names as needed)
    state_polls <- state_polls %>%
      select(pollster, date, sample, `Harris (D)`, `Trump (R)`)
    
    # Add state and rank columns
    state_polls$State <- state
    state_polls$Rank <- 1:nrow(state_polls)
    
    # Combine with existing state_polls_24 dataframe
    state_polls_24 <- rbind(state_polls_24, state_polls)
  } else {
    print("no tables found on page ")
  }
  
}

# Close RSelenium connection
remote$close()
driver$server$stop()

# Clean up column names and calculate Spread
state_polls_24 <- state_polls_24 %>%
  rename("Harris" = `Harris (D)`, "Trump" = `Trump (R)`) %>%
  mutate(Spread = Trump - Harris)

# Print the resulting data frame
print(state_polls_24)

rcp_averages_24 <-  state_polls_24 %>% 
  filter(pollster == 'RCP Average')

state_polls_24 <- state_polls_24 %>% 
  filter(pollster != 'RCP Average')

# Calculate average of last 5 state polls 
state_summary_24 <- state_polls_24 %>% 
  filter(Rank <= 5) %>% 
  group_by(State) %>% 
  summarize(
    spread_24 = mean(Spread)
  )

# Calculate SD for all state polls
SD_24 <- state_polls_24 %>% 
  group_by(State) %>% 
  summarize(
    Stdev_24 = sd(Spread)
  )


# Combine averages and sds
state_summary_24 <- state_summary_24 %>% 
  left_join(SD_24)

rm(SD_24, state_polls, state_polls_24, state_data)

# NATIONAL DATA 
driver <- rsDriver(browser="firefox", port=4555L, verbose=F, chromever = NULL)
remote <- driver[["client"]]

url <- "https://www.realclearpolling.com/polls/president/general/2024/trump-vs-harris"

remote$navigate(url)
Sys.sleep(10)

page_source <- remote$getPageSource()[[1]]
page <- read_html(page_source)
tables <- page %>% html_nodes("table")

national_24 <- tables[[1]] %>% html_table()


remote$close()
driver$server$stop()

national_24 <- national_24 %>% 
  rename("Harris" = 'Harris (D)', "Trump" = 'Trump (R)') %>% 
  mutate(Spread = Trump - Harris)

national_spread_24_current <- as.numeric(national_24[1,5] - national_24[1,6])
national_sd_24 <- sd(national_24$Spread[-1])

rm(national_24)

# Building the model

forecast_data <- table_df %>% 
  left_join(state_summary_20) %>% 
  left_join(state_summary_24)

forecast_data <- forecast_data %>%  mutate (
  national_adj = national_spread_24_current - national_spread_20_current,
  national_sd = national_sd_24,
  Spread = case_when(
    !is.na(spread_24) & is.na(Spread_2020) ~ spread_24,
    !is.na(spread_24) & !is.na(Spread_2020) ~ .7*spread_24 + .3*(Spread_2020+national_adj),
    is.na(spread_24) & !is.na(Spread_2020) ~ Spread_2020 + national_adj,
    is.na(spread_24) & is.na(Spread_2020) ~ national_adj,
    
  ),
  Sd = case_when(
    is.na(Stdev_20) & (is.na(Stdev_24)|is.nan(Stdev_24)|Stdev_24 == 0) ~ national_sd,
    !is.na(Stdev_20) & (is.na(Stdev_24) | is.nan(Stdev_24)) ~ national_sd,
    !is.na(Stdev_20) & !(is.na(Stdev_24)|is.nan(Stdev_24)) ~ Stdev_24,
    is.na(Stdev_20) & !(is.na(Stdev_24)|is.nan(Stdev_24)) ~ Stdev_24
  )
  
)

#num trials
n = 1500

results_matrix <- matrix(0, nrow = 51, ncol = n)

Dist <- list(
  gamma = 0,
  delta = .5,
  xi = .01,
  lambda = 1,
  type = "SN"
)

dist_multiplier <- rJohnson(n, Dist)


for (i in 1:51) {
  for (j in 1:ncol(results_matrix)) {
    value <- forecast_data[i, 9] + dist_multiplier[j] * forecast_data[i, 10]
    results_matrix[i,j] <- unlist(value)
  }
  
}

trump_wins <- ifelse(results_matrix > 0, 1, 0)
trump_state_probs <- apply(trump_wins, 1, sum)/n
forecast_data$Trump_Prob <- trump_state_probs

harris_wins <- ifelse(results_matrix < 0, 1, 0)
harris_state_probs <- apply(harris_wins, 1, sum)/n
forecast_data$Harris_Prob <- harris_state_probs

votes <- as.numeric(table_df$EV, ncol = 1)

for(i in 1:n){
  trump_wins[ ,i] <- trump_wins[ ,i] * votes
  harris_wins[ ,i] <- harris_wins[ ,i] * votes
}

trump_votes <- apply(trump_wins, 2, sum)
harris_votes <- apply(harris_wins, 2, sum)

results <- data.frame(trump_votes, harris_votes)

results <- results %>% mutate(
  winner = ifelse(trump_votes >= harris_votes, "Trump", "Harris")
)

win_table <- results %>% 
  group_by(winner) %>% 
  tally() %>% 
  rename(Percent = n) %>% 
  mutate(Percent = Percent/n)

results %>% 
  group_by(winner) %>% 
  tally() %>% 
  rename(Percent = n) %>%   
  mutate(Percent = scales::percent(Percent/n, accuracy = .01))

# Projected EV counts 
trump_median <- median(results$trump_votes)
paste0("Trump Median: ", trump_median)

harris_median <- median(results$harris_votes)
paste0("Harris Median: ", harris_median)