library(tidyverse);
setwd("~/Desktop/github/baseball-database")

##retrosheet wd: setwd("~/retrosheet/download.folder/zipped")
retro_pull=function(game_num, type) {
  
  df_fun=function(df){as.data.frame(df)}

  section=ifelse(type=="id", 1, 
                 ifelse(type=="version", 2, 
                        ifelse(type=="info", 3, 
                               ifelse(type=="start", 4, 
                                      ifelse(type=="play", 5, 
                                             ifelse(type=="com", 6, 
                                                    ifelse(type=="sub", 7, 
                                                           ifelse(type=="data", 
                                                                  8, NA))))))))
  

    
    retro_df=function(game_num=game_num, section=section){
      df_fun(df_retro[[game_num]][section])
      }
    
    id_bind=function(sub_section){
      cbind(retro_df(game_num, 1) %>% transmute(game_id=id), sub_section)
      }
    
    if(section==1){retro_df(game_num, section) %>% transmute(game_id=id)}
    
    else if(section==2){id_bind(retro_df(game_num, section)) %>% 
        mutate(version=as.numeric(version))}
    
    else if(section==3){id_bind(retro_df(game_num, section)) %>% 
        transmute(game_id, category=info.category, info=info.info)}
    
    else if(section==4){id_bind(retro_df(game_num, section)) %>% 
        transmute(game_id, retroID=start.retroID, team=start.team, 
                  batPos=start.batPos, fieldPos=start.fieldPos)}
    
    else if(section==5){id_bind(retro_df(game_num, section)) %>% 
        transmute(game_id, inning=play.inning, bat_team=play.team, 
                  retroID=play.retroID, count=play.count, 
                  pitch_seq=play.pitches, play=play.play)}
    
    else if(section==6){id_bind(retro_df(game_num, section)) %>% 
        transmute(game_id, comment=com)}
    
    else if(section==7){id_bind(retro_df(game_num, section)) %>% 
        transmute(game_id, retroID=sub.retroID, team=sub.team, 
                  batPos=sub.batPos, fieldPos=sub.fieldPos)}
    
    else if(section==8){id_bind(retro_df(game_num, section)) %>% 
        transmute(game_id, metric=data.projCode, retroID=data.retroID, 
                  ER=as.numeric(data.ER))}
    
    else if(is.na(section)){df_fun(df_retro[[game_num]])}
    
};

