
# 2020 RESULTS 


electoral_college_2020 <- read_html(
  "https://ballotpedia.org/Electoral_College_in_the_2020_presidential_election"
)

table_nodes <- html_nodes(electoral_college_2020, "table")

table2020_df <- html_table(table_nodes[[2]], fill = TRUE)

table2020_df <- table2020_df[-1, ] # remove first row 
table2020_df <- table2020_df[, -c(3, 4 ,5)] # 3-5 col

# Construct URLs
base_url <- "https://www.realclearpolling.com/polls/president/general/2020/"
urls <- paste0(base_url, state_links, "/trump-vs-biden")

# Create data frame
state_data <- data.frame(
  Abbrev = state_abbr,
  State = state_names,
  URL = urls,
  stringsAsFactors = FALSE
)


#initialize state polls dataframe 
state_polls_20 <- as.data.frame(matrix(NA, nrow = 0,ncol = 7))
names(state_polls_20) <- c("Poll", "Date", "Sample", "Biden (D)", "Trump (R)", "State", "Rank")

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
    # Assuming the last table is the one you want
    state_polls <- tables[[length(tables)]] %>%
      html_table(fill = TRUE) %>%
      as.data.frame()
    
    # Select relevant columns (adjust column names as needed)
    state_polls <- state_polls %>%
      select(pollster, date, sample, `Biden (D)`, `Trump (R)`)
    
    # Add state and rank columns
    state_polls$State <- state
    state_polls$Rank <- 1:nrow(state_polls)
    
    # Combine with existing state_polls_24 dataframe
    state_polls_20 <- rbind(state_polls_20, state_polls)
  } else {
    print("No tables found on page")
  }
  
}

# Close RSelenium connection
remote$close()
driver$server$stop()

# Clean up column names and calculate Spread
state_polls_20 <- state_polls_20 %>%
  rename("Biden" = `Biden (D)`, "Trump" = `Trump (R)`) %>%
  mutate(Spread = Trump - Biden)

# Print the resulting data frame
print(state_polls_20)

# filter out final resutls
rcp_final_20 <- state_polls_20 %>% 
  filter(pollster == "Final Results")

state_polls_20 <- state_polls_20 %>% 
  filter(pollster != "Final Results", pollster != 'RCP Average')

# calculate SD for all state polls 
SD_20 <- state_polls_20 %>% 
  group_by(State) %>% 
  summarize(
    Stdev_20 = sd(Spread)
  )

# Combine averages and SDs
state_summary_20 <- rcp_final_20 %>% 
  left_join(SD_20) %>% 
  select(State, Spread_2020 = Spread, Stdev_20)

#### 2020 NATIONAL DATA
nationalURL <- "https://www.realclearpolling.com/polls/president/general/2020/trump-vs-biden"

driver <- rsDriver(browser="firefox", port=4555L, verbose=F, chromever = NULL)
remote <- driver[["client"]]

remote$navigate(nationalURL)

Sys.sleep(5)

page_source <- remote$getPageSource()[[1]]
page <- read_html(page_source)
tables <- page %>% html_nodes("table")

national_20 <- tables[[2]] %>% html_table()

print(national_20)

remote$close()
driver$server$stop()

national_20 <- national_20 %>% 
  rename("Biden" = 'Biden (D)', "Trump" = 'Trump (R)') %>% 
  mutate(Spread = Trump - Biden)

national_spread_20_current <- as.numeric(national_20[1,5] - national_20[1,6])
national_sd_20 <- sd(national_20$Spread[-1])

print(national_spread_20_current)
print(national_sd_20)

rm(national_20)
