library(tidyverse)
library(magrittr)
library(httr)
# Thank you Bob Rudis for creating curlconverter.
library(curlconverter)
library(jsonlite)


get_comparison_congress <- function(congress){
  # curl command for pulling from ProPublica Congress API.
  # This command gets the names of all the congresspeople.
  cmd <- paste0('curl "https://api.propublica.org/congress/v1/',congress,'/house/members.json"
  -H "X-API-Key: cBkzyrgs6k7uCIXKZM5Jv9FXRQO2AjHUKk4KMvP2"')
  
  # Make the request here.
  members_request <- straighten(cmd) %>% make_req()
  
  # These three lines clean the response into a dataframe that can be used for analysis.
  members_rec <- toJSON(content(members_request[[1]](), as = 'parsed'), pretty = T)
  members_json <- fromJSON(members_rec[[1]])
  members <- members_json[["results"]][["members"]][[1]] %>% 
    select(-ideal_point, -contact_form, -dw_nominate, -office, -phone, -fax, -leadership_role) %>% 
    as_tibble()
  
  # Here we are filtering for just KY members
  ky_members <- members %>%  
    filter(state == 'KY')
  
  # Here, we are getting Congressman Yarmuth's id.
  y <- ky_members %>% 
    filter(last_name == 'Yarmuth') %>% 
    select(id) %>% 
    unlist()
  
  # Here, we are getting every other KY resprentative's ID
  others <- ky_members %>% 
    filter(last_name != 'Yarmuth') %>% 
    select(id, first_name, last_name, party, district) %>% 
    mutate(id = as.character(id), first_name = as.character(first_name), last_name = as.character(last_name),
           party = as.character(party), district = as.character(district))
  
  # Make request of the API for comparisons between "other" congresspeople and Congressman Yarmuth.
  others %<>% 
    mutate(comparisons_txt = paste0('curl "https://api.propublica.org/congress/v1/members/',y,'/votes/',id,'/',congress,'/house.json" 
                                    -H "X-API-Key: cBkzyrgs6k7uCIXKZM5Jv9FXRQO2AjHUKk4KMvP2"')) %>%
    mutate(comparisons_request = straighten(comparisons_txt) %>% make_req())
  
  # Convert all the responses into data that we can use
  # I hate doing this in a loop, so I called it 'trash'
  trash <- vector(length = nrow(others))
  for(i in 1:length(trash)) trash[i] <- toJSON(content(others$comparisons_request[[i]](), as = 'parsed'), pretty = T)
  
  # Cleaning the 'trash' into a returnable data frame.
  trash %>% 
    map(fromJSON) %>% 
    map('results') %>% 
    map_df(bind_rows) %>% 
    mutate(first_member_id = as.character(first_member_id), second_member_id = as.character(second_member_id),
           congress = as.character(congress), common_votes = as.numeric(common_votes), disagree_votes = as.numeric(disagree_votes)) %>% 
    select(first_member_id, second_member_id, congress, common_votes, disagree_votes) %>% 
    left_join(others, by = c('second_member_id' = 'id')) %>% 
    select(-comparisons_txt, -comparisons_request)
}

# Running the function over all the Congresses that Rep. Yarmuth has served in.
congress_115 <- get_comparison_congress('115')
congress_114 <- get_comparison_congress('114')
congress_113 <- get_comparison_congress('113')
congress_112 <- get_comparison_congress('112')
congress_111 <- get_comparison_congress('111')
congress_110 <- get_comparison_congress('110')

comparison <- bind_rows(congress_115, congress_114, congress_113, congress_112, congress_111, congress_110)

# Save for use in Rmd.
save(comparison, file = 'y_comparison.rda')

## The scores are in this data frame.
summary <- comparison %>% 
  group_by(second_member_id, last_name, first_name) %>% 
  summarize(tot_votes = sum(common_votes),
            disagree = sum(disagree_votes)) %>% 
  mutate(y_score = (tot_votes - disagree) / tot_votes)
