# Dev records : unit tests

# version : 04.2.2

# date : 2022-11-24

# MAJ : 2023-01-14

# licence : GPL

# Author : Grace MOLINGO

#

#' $Rev:: $: 

#' $Author:: $:Grace MOLINGO

#' $Date:: $:2023-01-14

#-----------------------------------------------------------------------

# TestSuite for DVR: create, execute and print all tests suite

#-----------------------------------------------------------------------


### Importation for library ###

if (!require(testthat)) install.packages('testthat')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(janitor)) install.packages('janitor')
if (!require(readxl)) install.packages('readxl')
library(testthat)
library(tidyverse)
library(janitor)
library(readxl)

### Functions de test and database###

source(file="R/Data_wrangling.R")
source(file="R/Data_importation.R")

# Importation of data
test_surf<-read_excel("Data_test.xlsx", sheet="Surface")
test_match<-read_excel("Data_test.xlsx", sheet="Nb_match")
test_age<-read_excel("Data_test.xlsx", sheet="Winner_age")
test_win<-read_excel("Data_test.xlsx", sheet="Wins")
test_ace<-read_excel("Data_test.xlsx", sheet="Ace")
test_hand<-read_excel("ata_test.xlsx", sheet="Winner_hand")
test_confr<-read_excel("Data_test.xlsx", sheet="Confrontation")
test_dur<-read_excel("Data_test.xlsx", sheet="Duration")
test_app<-read_excel("Data_test.xlsx", sheet="Appearance")
test_rank<-read_excel("Data_test.xlsx", sheet="Ranking_points")
tourney<-unique(atp$tourney_name)


# ------------------------------------------------------------------------------

# Test the function that calculates the number of match by surface type a given
# tourney 

#-------------------------------------------------------------------------------

for(t in tourney){
  test_that("Number of match by surface type a given tourney",{
    expect_equal(nb_match_surface(atp, t)%>%
                   select(surface)%>%
                   pull(),
                 test_surf%>%
                   filter(Tourney==t)%>%
                   select(surface)%>%
                   pull())
    expect_equal(nb_match_surface(atp, t)%>%
                   select(nb_match)%>%
                   pull(),
                 test_surf%>%
                   filter(Tourney==t)%>%
                   select(nb_match)%>%
                   pull())
  })
}


#-------------------------------------------------------------------------------

# Test the function that calculates the number of match by tourney 

#-------------------------------------------------------------------------------

for(t in tourney){
  test_that("Number of match by tourney",{
    expect_equal(nb_match_func(atp, t)%>%
                   select(nb_match)%>%
                   pull(),
                 test_match%>%
                   filter(Tourney==t)%>%
                   select(nb_match)%>%
                   pull())
  })
}


#-------------------------------------------------------------------------------

# Test the function that calculates the winner age by tourney

#-------------------------------------------------------------------------------

for(t in tourney){
  test_that("Winner age by tourney",{
    expect_equal(players_final_ages(atp, t)%>%
                   select(winner_name)%>%
                   pull(),
                 test_age%>%
                   filter(Tourney==t)%>%
                   select(Name)%>%
                   pull())
    expect_equal(players_final_ages(atp, t)%>%
                   select(winner_age)%>%
                   pull(),
                 test_age%>%
                   filter(Tourney==t)%>%
                   select(Age)%>%
                   pull())
  })
}


#-------------------------------------------------------------------------------

# Test the function that calculates the number of wins of a player by tourney

#-------------------------------------------------------------------------------

for(t in tourney){
  test_that("Number ofwins of a player by tourney",{
    expect_equal(Top_10(atp, t, 'Victories')%>%
                   select(player_name)%>%
                   pull(),
                 test_win%>%
                   filter(Tourney==t)%>%
                   select(Name)%>%
                   pull())
    expect_equal(Top_10(atp, t, 'Victories')%>%
                   select(total)%>%
                   pull(),
                 test_win%>%
                   filter(Tourney==t)%>%
                   select(Wins)%>%
                   pull())
  })
}


#-------------------------------------------------------------------------------

# Test the function that calculates the number of ace of a player by tourney

#-------------------------------------------------------------------------------

for(t in tourney){
  test_that("Number of ace of a player by tourney",{
    expect_equal(Top_10(atp, t, 'Ace')%>%
                   select(player_name)%>%
                   pull(),
                 test_ace%>%
                   filter(Tourney==t)%>%
                   select(Name)%>%
                   pull())
    expect_equal(Top_10(atp, t, 'Ace')%>%
                   select(total)%>%
                   pull(),
                 test_ace%>%
                   filter(Tourney==t)%>%
                   select(Ace)%>%
                   pull())
  })
}


#-------------------------------------------------------------------------------

# Test the function that calculates the number of wins according to the winner's
# hand of the players by tourney

#-------------------------------------------------------------------------------

for(t in tourney){
  test_that("Number of wins according to the winner's hand by tourney",{
    expect_equal(victories_hand(atp, t)%>%
                   select(winner_hand)%>%
                   pull(),
                 test_hand%>%
                   filter(Tourney==t)%>%
                   select(Hand)%>%
                   pull())
    expect_equal(victories_hand(atp, t)%>%
                   select(count)%>%
                   pull(),
                 test_hand%>%
                   filter(Tourney==t)%>%
                   select(nb_wins)%>%
                   pull())
  })
}


#-------------------------------------------------------------------------------

# Test the function that calculates the number of times a country appears in a 
# tourney

#-------------------------------------------------------------------------------

for(t in tourney){
  test_that("Number of times a country appears in a tourney",{
    expect_equal(most_final_appearances(atp, t)%>%
                   select(Country)%>%
                   unique()%>%
                   pull(),
                 factor(test_app%>%
                          filter(Tourney==t)%>%
                          select(Country)%>%
                          unique()%>%
                          pull(),
                        levels=levels(most_final_appearances(atp, t)%>%
                                        select(Country)%>%
                                        unique()%>%
                                        pull())))
    expect_equal(most_final_appearances(atp, t)%>%
                   select(Appearances)%>%
                   pull(),
                 test_app%>%
                   filter(Tourney==t)%>%
                   select(nb_appearance)%>%
                   pull())
  })
}


#-------------------------------------------------------------------------------

# Test the function that calculates the duration of the longest match by tourney

#-------------------------------------------------------------------------------

for(t in tourney){
  test_that("The duration of the longest match by tourney",{
    expect_equal(duration(t,atp)%>%
                   filter(minutes==max(minutes, na.rm=TRUE))%>%
                   select(minutes)%>%
                   pull()%>%
                   unique(),
                 test_dur%>%
                   filter(Tourney==t)%>%
                   select(Duration)%>%
                   pull())
  })
}


#-------------------------------------------------------------------------------

# Test the function that calculates the number of confrontation by tourney

#-------------------------------------------------------------------------------

for(t in tourney){
  test_that("The number of confrontation by tourney",{
    expect_equal(get_maxopp(t, atp)%>%
                   select(confront)%>%
                   pull(),
                 test_confr%>%
                   filter(Tourney==t)%>%
                   select(Confrontation)%>%
                   pull())
    expect_equal(get_maxopp(t, atp)%>%
                   select(total_confront)%>%
                   pull(),
                 test_confr%>%
                   filter(Tourney==t)%>%
                   select(nb_confrontation)%>%
                   pull())
  })
}


#-------------------------------------------------------------------------------

# Test the function that provides all the information on ATP ranking of the 
# players by tourney

#-------------------------------------------------------------------------------

for(t in tournoi){
  test_that("More information on ATP ranking of the players",{
    expect_equal(Atp_ranks(atp, t)%>%
                   rename(winner_points = winner_rank_points, winner_points = winner_rank_points),
                 test_rank%>%
                   filter(Tourney==t)%>%
                   select(-Tourney))
  })
}
