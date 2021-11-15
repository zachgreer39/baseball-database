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
x=0
teams=retrosheet::getTeamIDs(2015)



##df_game_info=NULL;

x=x+1
team_list=retro_list(2015, teams[x]);
for(i in 1:length(team_list)) {
  
  single_line=cbind(df_(team_list[[i]]$id) %>% rename(game_id=df) %>% drop_na(),
        df_(team_list[[i]]$info) %>% drop_na() %>% t() %>% df_() %>% 
          janitor::row_to_names(1))
  df_game_info=plyr::rbind.fill(df_game_info, single_line)
  rm(single_line, i)
}



write.csv(df_game_info, "df_games_15_21.csv", row.names=FALSE)
