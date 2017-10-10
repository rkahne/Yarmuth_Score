library(tidyverse)
library(magrittr)
library(httr)
library(curlconverter)
library(jsonlite)


get_comparison_congress <- function(congress){
  cmd <- paste0('curl "https://api.propublica.org/congress/v1/',congress,'/house/members.json"
  -H "X-API-Key: "')
  
  
  members_request <- straighten(cmd) %>% make_req()
  
  members_rec <- toJSON(content(members_request[[1]](), as = 'parsed'), pretty = T)
  members_json <- fromJSON(members_rec[[1]])
  members <- members_json[["results"]][["members"]][[1]] %>% 
    select(-ideal_point, -contact_form, -dw_nominate, -office, -phone, -fax, -leadership_role) %>% 
    as_tibble()
  
  
  ky_members <- members %>%  
    filter(state == 'KY')
  
  y <- ky_members %>% 
    filter(last_name == 'Yarmuth') %>% 
    select(id) %>% 
    unlist()
  
  others <- ky_members %>% 
    filter(last_name != 'Yarmuth') %>% 
    select(id, first_name, last_name, party, district) %>% 
    mutate(id = as.character(id), first_name = as.character(first_name), last_name = as.character(last_name),
           party = as.character(party), district = as.character(district))
  
  others %<>% 
    mutate(comparisons_txt = paste0('curl "https://api.propublica.org/congress/v1/members/',y,'/votes/',id,'/',congress,'/house.json" 
                                    -H "X-API-Key: "')) %>%
    mutate(comparisons_request = straighten(comparisons_txt) %>% make_req())
  
  trash <- vector(length = nrow(others))
  for(i in 1:length(trash)) trash[i] <- toJSON(content(others$comparisons_request[[i]](), as = 'parsed'), pretty = T)
  
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

congress_115 <- get_comparison_congress('115')
congress_114 <- get_comparison_congress('114')
congress_113 <- get_comparison_congress('113')
congress_112 <- get_comparison_congress('112')
congress_111 <- get_comparison_congress('111')
congress_110 <- get_comparison_congress('110')

comparison <- bind_rows(congress_115, congress_114, congress_113, congress_112, congress_111, congress_110)

save(comparison, file = 'y_comparison.rda')

summary <- comparison %>% 
  group_by(second_member_id, last_name, first_name) %>% 
  summarize(tot_votes = sum(common_votes),
            disagree = sum(disagree_votes)) %>% 
  mutate(y_score = (tot_votes - disagree) / tot_votes)

summary_district <- comparison %>% 
  group_by(district) %>% 
  summarize(tot_votes = sum(common_votes),
            disagree = sum(disagree_votes)) %>% 
  mutate(y_score = (tot_votes - disagree) / tot_votes)

comparison %>% 
  mutate(disagree_pct = disagree_votes / common_votes) %>% 
  ggplot(aes(x = congress, y = disagree_pct, fill = party, label = paste0(round(disagree_pct, 2) * 100, '%'))) +
  scale_fill_manual(values = c('D' = 'blue', 'R' = 'red')) +
  geom_bar(stat = 'identity') +
  geom_label(size = 3) +
  facet_wrap(~district + last_name) +
  theme_minimal() + 
  theme(legend.position = 'none')


comparison %>% 
  mutate(disagree_pct = disagree_votes / common_votes) %>% 
  ggplot(aes(x = congress, y = disagree_pct, group = last_name, color = district)) +
  geom_line() +
  theme_minimal()

comparison %>% 
  mutate(disagree_pct = disagree_votes / common_votes) %>% 
  filter(district == 5) %>% 
  ggplot(aes(x = congress, y = disagree_pct, fill = last_name, label = paste0(round(disagree_pct, 2) * 100, '%'))) +
  geom_bar(stat = 'identity') +
  geom_label(show.legend = F) +
  scale_fill_manual(values = 'red') +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = 'Fifth District Yarmuth Score',
       x = 'Congress', y = 'Yarmuth Score') +
  theme_minimal() + 
  theme(legend.title = element_blank())
