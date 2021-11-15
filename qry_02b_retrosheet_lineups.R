library(tidyverse);
setwd("~/Desktop/github/baseball-database")


##breaks down str_extract_all output list and returns a string of all matches
unpack=function(string, look_for){
  
  sapply(str_extract_all(string, look_for), function(x) paste(x, collapse=""))
  
};


##cuts down as.data.frame because i'm lazy
df_=function(df){as.data.frame(df)};


##returns the selected game list from getRetrosheet output
retro_list=function(year, team){
  
  retrosheet::getRetrosheet("play", year, team)
  
};


##declaring team list
teams=retrosheet::getTeamIDs(2003)

##currently set up to pull one team at a time from vector built above
##highlight block below and run 30 times
##write csv, null out values and start process over for new year
##RETURN AND REWRITE FUNCTION TO LOOP THROUGH ALL TEAMS
##LOOP WORKED FINE BUT UNABLE TO SAVE OUTPUT VIA DISPLAYING ON SCREEN
##OR STORING IN ENVIRONMENT 
x=x+1
team_list=retro_list(2003, teams[x]);
##df_lineups=NULL;
for(i in 1:length(team_list)){
  
  if(is.null(team_list[[i]]$sub)) {
    
    df_game_lineup=cbind(df_(team_list[[i]]$id) %>% drop_na(), 
                         df_(team_list[[i]]$start) %>% drop_na()) %>% 
      transmute(game_id=df, lineup="start", retroID, 
                team, batPos, fieldPos) %>% 
      left_join(df_(team_list[[i]]$data) %>% drop_na())
    
  }
  
  else{
    
    df_game_lineup=rbind(cbind(df_(team_list[[i]]$id) %>% drop_na(), 
                               df_(team_list[[i]]$start) %>% drop_na()) %>% 
                           transmute(game_id=df, lineup="start", retroID, 
                                     team, batPos, fieldPos), 
                         cbind(df_(team_list[[i]]$id) %>% drop_na(), 
                               df_(team_list[[i]]$sub) %>% drop_na()) %>% 
                           transmute(game_id=df, lineup="sub", retroID, team, 
                                     batPos, fieldPos)) %>% 
      arrange(team) %>% 
      left_join(df_(team_list[[i]]$data) %>% drop_na())
    
  }
  
  df_lineups=rbind(df_lineups, df_game_lineup)
  rm(i, df_game_lineup)
  
};





write.csv(df_lineups, "lineups_2003.csv", row.names=FALSE)
x=0
df_lineups=NULL;
