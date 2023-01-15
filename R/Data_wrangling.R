#' ***** required Libraries, mentioned in details section in the documentation of each function, are loaded in the script sourcing this file (dashboard-atp-tour.Rmd) to avoid multiple loads time, installing if required is taken into consideration
#' ***** This script includes extensive as well as simple functions to minimize the code in the dashboard script 
#' ***** At this stage, automatic Checks of the entered parameters is not taken into consideration in the functions 
#' ***** Manual Checks on functions were done through entering multiple possible values for the parameters 
#' ***** output checks are available in the file test.R existing in the same directory


#' ********************************************************************************
#' @title 	            nb_match_surface.R                                       
#' @author              Khaoula Aroui                                            
#' @description 	      Provides the count of matchs by surface type for a given
#'                      data and tourney
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          table to treat (mandatory)                               
#' @param tournoi       Tourney of interest(mandatory)                           
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns             a tibble of existing surfaces along with their count of 
#'                      appearance (number of matchs) in the data
#' @details             required packages are "dplyr"              
#' @examples                                                                       
#' nb_match_surface(atp, "Wimbledon")                                           
#' ********************************************************************************
nb_match_surface<-function(data, tournoi){
  data<-  data %>%filter(tourney_name==tournoi)# filter data according to the selected tourney
  data%>%filter(!is.na(surface))%>%
    group_by(surface)%>%
    summarise(nb_match=n())%>%
    arrange(desc(nb_match))->surf_uniq
  return(surf_uniq)
}


#' ********************************************************************************
#' @title 	            nb_match_func.R                                        
#' @author              Khaoula Aroui                                            
#' @description 	      Provides the count of played matchs for a given data
#'                      and tourney
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          table to treat (mandatory)                               
#' @param tournoi       Tourney of interest(mandatory)                           
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns            a tibble of one column containing the count of played matchs
#' @details            required packages are "dplyr"                  
#' @examples                                                                       
#' nb_match_func(atp, "Wimbledon")                                           
#' ********************************************************************************
nb_match_func<-function(data, tournoi){
  data<-  data %>%filter(tourney_name==tournoi)# filter data according to the selected tourney
  data %>%
    summarise(nb_match=n())->nb_match_df
  return(nb_match_df)
}


#' ********************************************************************************
#' @title 	            players_final_ages.R                                        
#' @author              Khaoula Aroui                                            
#' @description 	      Provides the players appearing in finals along with 
#'                      their age for a given data and tourney
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          table to treat (mandatory)                               
#' @param tournoi       Tourney of interest(mandatory)                           
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns             a tibble of two columns: winner name and winner age 
#'                      existing in the input table
#' @details             required packages are "dplyr"                
#' @examples                                                                       
#' players_final_ages(atp, "Wimbledon")                                           
#' ********************************************************************************
players_final_ages<-function(data, tournoi){
  data<-  data %>%filter(tourney_name==tournoi)# filter data according to the selected tourney
  data %>%
      filter(round == "F")%>%
      select(winner_name, winner_age)->age
  return(age)
}

#' ********************************************************************************************
#' @title 	            player_info.R                                         
#' @author              Khaoula Aroui                                            
#' @description 	      Provides the count of contributions and the win rate 
#'                      for the players for a given data
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          table to treat (mandatory)                               
#' @param tournoi       Tourney of interest(mandatory)                                             
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns             a data.frame of 3 columns : player, Contributed (integer), and won (rate) 
#' @details             required packages are "dplyr"              
#' @examples                                                                       
#' player_info(atp, "wimbledon")                                           
#' *********************************************************************************************
player_info <- function(data, tournoi){
  data<-  data %>%filter(tourney_name==tournoi)# filter data according to the selected tourney
  names <- c("Player", "Contributed","Won")
  result_tab <- setNames(data.frame(matrix(ncol = length(names), nrow = 0)),names)
  u_winner = unique(data$winner_name)
  u_loser = unique(data$loser_name)
  for(v in unique(data$winner_name)){
    nb_matchs <- count(data %>% filter(winner_name==v | loser_name==v))
    nb_win <- count((data %>% filter(winner_name==v)))
    percent_w <- paste(round(nb_win/nb_matchs*100,1), "%", sep="")
    
    dt_res <- data.frame("Player"=v,"Contributed"=nb_matchs,"Won"=percent_w)
    result_tab <- rbind(result_tab, dt_res)
  }
  
  colnames(result_tab)<-c("j","nb","g")
  result_tab <- result_tab %>% filter(!is.nan(nb) & !is.nan(g)) %>% arrange(desc(nb))
  colnames(result_tab)<-names
  return(result_tab)
}


#' ********************************************************************************
#' @title 	            Top_10.R                                         
#' @author              Khaoula Aroui                                            
#' @description 	      For a given tourney name, provides an ordered data.frame for top 10 players 
#'                      according to the count of a specific variable 
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          table to treat (mandatory)   
#' @param tournoi       Tourney of interest(mandatory)                                             
#' @param variable      The variable for which we want to visualize the top 10
#'                      players(mandatory)
#'                      Can have only two string values: "Victories" or "Aces"                        
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns             a data.frame of 2 columns and 10 rows: player_name and total
#' @details             required packages are "dplyr"             
#' @examples                                                                       
#' Top_10(atp, "wimbledon", "Victories")                                           
#' ********************************************************************************
Top_10 <- function(data, tournoi,variable ){
  data<-  data %>%filter(tourney_name==tournoi)# filter data according to the selected tourney
  
  # according to the parameter variable   
  if(variable== 'Victories'){
    total_wins <- data %>%
      group_by(winner_name) %>%
      summarize(total = n()) %>%
      arrange(desc(total))
    top_10 <- total_wins$winner_name[1:10]
    top_10_df <- total_wins %>% filter(winner_name %in% top_10)%>%rename(player_name=winner_name)
    
  }
  
  else if(variable == 'Aces'){
    aces_win <- data %>%
      group_by(winner_name) %>%
      summarize(w_ace = sum(w_ace))
    aces_win[is.na(aces_win)] <- 0
    aces_lose <- data %>%
      group_by(loser_name) %>%
      summarize(l_ace = sum(l_ace))
    aces_lose[is.na(aces_lose)] <- 0
    
    all_ace <- merge(aces_win, aces_lose, by.x = c('winner_name'), by.y = c('loser_name'))%>%rename(player_name=winner_name)
    all_ace$total = all_ace$w_ace + all_ace$l_ace
    all_ace <- all_ace %>%
      arrange(desc(total))
    top_10_ace <- all_ace$player_name[1:10]
    top_10_df <- all_ace %>% filter(player_name %in% top_10_ace)%>%select(-c('w_ace', 'l_ace'))
    
  }

  return(top_10_df)
}


#' *******************************************************************************************************************
#' @title 	            Atp_ranks.R                                       
#' @author              Khaoula Aroui                                            
#' @description 	      provides a table including ATP ranking several informations for the winners and/ or 
#'                      second placed  (according to the parameter variable) of 
#'                     the finals for a specific input Tourney
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          Table to treat (mandatory)                               
#' @param tournoi       Tourney of interest(mandatory)          
#' @param variable      Default value is "none", else it is the variable for which we want to visualize the 
#'                      top 5 winners (mandatory). user-defined value is used by the function otherwise, the 
#'                      default value is used. Can have only two string values: 'Lowest Ranked' or Highest Ranked',
#'                      else the default value is used.                       
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns             if variable is in ('Lowest Ranked', Highest Ranked' ) returns a tibble including the year, the
#'                      final's winner name, his rank, and his ranking points, else if variable is set to default value 
#'                      then returns a tibble including the year , the final's winner name, the second placed name, their 
#'                      ranks and their ranking points 
#' @details             required packages are "dplyr"              
#' @examples                                                                       
#' Atp_ranks(atp, "Roland Garros", 'Lowest Ranked'  )                                           
#' Atp_ranks(atp, "Roland Garros") #in this case the parameter variable is set to its default value                                           
#' ********************************************************************************************************************
Atp_ranks=function(data, tournoi, variable="none"){
  
  data <- data %>% filter(tourney_name == tournoi, round == "F")
  table = data %>%
    select(
      year,
      winner_rank,
      winner_rank_points,
      winner_name,
      loser_rank,
      loser_rank_points,
      loser_name
    ) %>% filter(!is.na(winner_rank_points) |
                   !is.na(loser_rank_points)) %>% filter(winner_rank_points != 0 |
                                                           loser_rank_points != 0)
  if (variable %in% c('Lowest Ranked', 'Highest Ranked')){
    table=table%>%select(year, winner_rank, winner_rank_points,  winner_name)
    
    if(variable== 'Lowest Ranked')
    {
      table=table%>%arrange(desc(winner_rank))
      table=distinct(table, winner_name, .keep_all=TRUE)
      table <- table[1:5,]
    }
    
    else if(variable == 'Highest Ranked')
    {
      table=table%>%arrange(winner_rank)
      table=distinct(table, winner_name, .keep_all=TRUE)
      table <- table[1:5,]
    }
    return(table)
  }
  
  else {return(table)}
  
}



#' ********************************************************************************
#' @title 	            victories_hand.R                                         
#' @author              Khaoula Aroui                                            
#' @description 	      Provides the count of victories according to
#'                      the winner' s hand for a given data and tourney name
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          table to treat (mandatory)  
#' @param tournoi       Tourney of interest(mandatory)                                             
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns             a data.frame of 2 columns winner_hand and count
#' @details             required libraries are "dplyr"                    
#' @examples                                                                       
#' victories_hand(atp, "wimbledon")                                           
#' ********************************************************************************
victories_hand <- function(data, tournoi){
  data<-  data %>%filter(tourney_name==tournoi)# filter data according to the selected tourney
  winner_hand_table <- data %>% 
    group_by(winner_hand) %>%
    summarize(count = n()) 
  
  winner_hand_table$winner_hand[winner_hand_table$winner_hand=="L"]<-"Left handed"
  winner_hand_table$winner_hand[winner_hand_table$winner_hand=="R"]<-"Right handed"
  winner_hand_table$winner_hand[winner_hand_table$winner_hand=="U"]<-"Ambidextrous"

  return(winner_hand_table)
}


#' ********************************************************************************
#' @title 	            most_final_appearances.R                                         
#' @author              Khaoula Aroui                                            
#' @description 	      Provides a table for the top 10 appearances of countries
#'                      in the finals according to ranking (winner or 2nd place)
#'                      for a specific tourney 
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          table to treat (mandatory)                               
#' @param tournoi       Tourney of interest(mandatory)                                             
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns             a data.frame of 3 columns: Country, Appearances, and group
#' @details             required libraries are "dplyr" and "forcats"             
#' @examples                                                                       
#' most_final_appearances(atp, "wimbledon")                                           
#' ********************************************************************************
most_final_appearances <- function(data, tournoi){
  data<-  data %>%filter(tourney_name==tournoi)# filter data according to the selected tourney
  final=data%>%filter( round == "F")
  
  Winner_Country <- aggregate(data.frame(count = final$winner_nationality), list(value = final$winner_nationality), length)
  Runner_Country <- aggregate(data.frame(count = final$loser_nationality), list(value = final$loser_nationality), length)
  
  i<-1
  n<-nrow(Runner_Country)
  for(v in Winner_Country$value){
    if(!(v %in% Runner_Country$value)){
      Runner_Country[n+i,"value"]<-v
      Runner_Country[n+i,"count"]<-0
      i<-i+1}}
  i<-1
  n<-nrow(Winner_Country)
  for(v in Runner_Country$value){
    if(!(v %in% Winner_Country$value)){
      Winner_Country[n+i,"value"]<-v
      Winner_Country[n+i,"count"]<-0
      i<-i+1}}
  
  Both <- rbind(Winner_Country, Runner_Country)
  Both=Both%>%mutate(group = ifelse(dplyr::row_number()%in% seq(1:nrow(Winner_Country)), "Winner", "Second place"))
  Both2=Both%>%group_by(value)%>%mutate(count_all=sum(count))
  Both2$percent=Both2$count_all/sum(Both2$count_all)*100
  Both2=Both2%>%arrange(desc(percent))
  Both2=distinct(Both2, value,percent)
  top10 <- Both2$value[1:10]
  Both2=Both2%>%filter(value %in% top10) 
  Both=merge(Both2%>%select(value, percent), Both, by.x = "value")%>% 
       mutate(Country = fct_reorder(value, desc(percent)), Appearances=count)%>%select(-c(value, count, percent))
  return(Both)
}



#' ********************************************************************************
#' @title 	            Extract duration for each matches of a specified tournament                                         
#' @author              Antoine Quevillart                                            
#' @description 	      This function returns the name of the two players 
#'                      who have met the most in a tournament.
#'                      
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          The tibble to be filtered, as formated by Jeff Sackman.
#' @param tournoi       Name of the tournament from which we want to extract information.                              
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns A tibble containing the name of the two opponents with the most confrontations of a tournament.      
#' @details                          
#' @examples                                                                       
#' duration('Roland Garros', data)                                            
#' ********************************************************************************
duration <- function(tournoi, data) {
  data<-  data %>%filter(tourney_name==tournoi)# filter data according to the selected tourney
  data %>%
    mutate(tourney_date = lubridate::ymd(tourney_date),
           year = lubridate::year(tourney_date)) %>%
    filter(tourney_name == tournoi) %>%
    select(year, tourney_name, minutes, winner_name, loser_name, draw_size, score) -> duration
  return(duration)
}


#' ********************************************************************************
#' @title 	            Extract all confrontations.                                        
#' @author              Antoine Quiveillart                                            
#' @description 	      This function returns the name of the two players 
#'                      of each matches. 
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          The tibble to be filtered, as formated by Jeff Sackman.
#' @param tournoi       Name of the tournament from which we want to extract information.                              
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns A tibble containing the name of the opponents of each matches from 'tour'.      
#' @details                          
#' @examples                                                                       
#' get_opponent(data, 'Roland Garros')                                            
#' ********************************************************************************
get_opponent <- function(data, tournoi) {
  data<-  data %>%filter(tourney_name==tournoi)# filter data according to the selected tourney
  data %>%
    mutate(tourney_date = lubridate::ymd(tourney_date),
           year = lubridate::year(tourney_date)) %>%
    select(tourney_name, tourney_date, winner_name, loser_name)
}

#' ********************************************************************************
#' @title 	            Extract the most frequents confrontations                                        
#' @author              Antoine Quiveillart                                            
#' @description 	      This function returns the name of the two players 
#'                      of each matches. 
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          The tibble to be filtered, as formated by Jeff Sackman.
#' @param tournoi       Name of the tournament from which we want to extract information.                              
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns A tibble containing the name of the two opponents with the most confrontations of a tournament.      
#' @details                          
#' @examples                                                                       
#' get_maxopp('Roland Garros', data)                                            
#' ********************************************************************************
get_maxopp = function(tournoi, data) {
  confrontation = get_opponent(data, tournoi) 
  confrontation = confrontation %>%
    mutate(p1_won = paste0(winner_name, " Vs ", loser_name),
           p2_won = paste0(loser_name, " Vs ", winner_name)) %>%
    select(-winner_name, -loser_name) 
  unique_conf = unique(unlist(confrontation))
  count_conf = sapply(confrontation, function(x) table(factor(x, 
                                                              levels = unique_conf, 
                                                              ordered = TRUE)))
  count_conf <- cbind(confront = rownames(count_conf), count_conf)
  rownames(count_conf) <- 1:nrow(count_conf)
  count_conf = data.frame(count_conf)
  count_conf = count_conf %>%
    filter(p1_won != 0 | p2_won != 0) %>%
    mutate(p1_won = as.numeric(p1_won),
           p2_won = as.numeric(p2_won)) %>%
    arrange(desc(p1_won + p2_won)) %>%
    mutate(total_confront = p1_won + p2_won) %>%
    select(-tourney_date, -tourney_name) 
  max_opposition = count_conf[1,]
  return(max_opposition)
}


#' ********************************************************************************
#' @title 	            Restructure_Frequent_oppo.R                                         
#' @author              Khaoula Aroui                                            
#' @description 	      Provides the most frequents rivalry: players' names,
#'                      wons ,total rivalries based on a function documented below
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          table to treat (mandatory)
#' @param tournoi       Tourney of interest(mandatory)                              
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns             a data.frame of 2 columns, second column's name changes 
#'                      according to input parameters 
#' @details             requred libraries are "dplyr", and "janitor"             
#' @examples                                                                       
#' Restructure_Frequent_oppo(atp, "wimbledon")                                            
#' ********************************************************************************
Restructure_Frequent_oppo <- function(data, tournoi){
  most_freq_opposition = get_maxopp(tournoi, data)
  most_freq_opposition=t(most_freq_opposition)
  rownames(most_freq_opposition)=c("Player 1 Vs Player 2", "Player 1 wins", "Player 2 wins", "Total rivalries")
  colnames(most_freq_opposition)=NULL
  most_freq_opposition=as.data.frame((most_freq_opposition))
  most_freq_opposition <- tibble::rownames_to_column(most_freq_opposition, "rn")
  most_freq_opposition=most_freq_opposition %>%row_to_names(row_number = 1)
  
  return(most_freq_opposition)
}


