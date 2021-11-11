library(tidyverse);
##packages used but not loaded: lahman, baseballr

career_batting=function(year, regular_season=TRUE) {
  
  group_batting=function(df){
    df %>% replace(is.na(.), 0) %>%
      transmute(playerID, season=yearID, level="MLB", league=lgID, team=teamID, 
                order, g=G, pa=(H+BB+SO+HBP+SH+SF), ab=AB, h=H, db=X2B, tr=X3B, 
                hr=HR, r=R, rbi=RBI, k=SO, bb=BB, bb=IBB, ubb=BB-IBB, 
                hbp=HBP, sh=SH, sf=SF, gidp=GIDP, sb=SB, cs=CS)
  }
  
  if(regular_season==TRUE) {
    group_batting(Lahman::Batting %>% 
                    filter(lgID %in% c("AL", "NL") & yearID>year) %>% 
                    mutate(order=paste0("R", "-", as.character(stint))))
      
  }
  
  else{
    group_batting(Lahman::BattingPost %>% mutate(order=round) %>% 
                    filter(lgID %in% c("AL", "NL") & yearID>year))

  }
  
};
modern_era_batting=function(year=1960){
  
  df1=career_batting(year=year)
  list=unique(df1$playerID)
  df2=career_batting(year=min((Lahman::Batting)$yearID)) %>% 
    filter(playerID %in% list)
  df2=df2 %>% anti_join(df1, 
                        by=c("playerID", "season", "level", 
                             "league", "team", "order"));
  
  
  df3=career_batting(year=year, regular_season=FALSE)
  list=unique(df3$playerID)
  df4=career_batting(year=min((Lahman::BattingPost)$yearID), 
                     regular_season=FALSE) %>% filter(playerID %in% list)
  df4=df4 %>% anti_join(df3, 
                        by=c("playerID", "season", "level", 
                             "league", "team", "order"))
  
  playoff_order=function(df){
    c(sort(unique(df$order)[str_detect(unique(df$order), "R")]),
      sort(unique(df$order)[str_detect(unique(df$order), "WC")]),
      sort(unique(df$order)[str_detect(unique(df$order), "DIV")]),
      sort(unique(df$order)[str_detect(unique(df$order), "DS")]),
      sort(unique(df$order)[str_detect(unique(df$order), "CS")]), "WS")
  }
  
  df_final=rbind(df1, df2, df3, df4) %>% mutate(order=as.character(order))
  
  df_final %>% mutate(order=factor(order)) %>% 
    mutate(order=fct_relevel(order, playoff_order(df_final))) %>% 
    arrange(playerID, season, level, league, order) %>% 
    rename(lahmanID=playerID)
  
};

df_yrly_batting=modern_era_batting();



career_pitching=function(year, regular_season=TRUE) {
  
  group_pitching=function(df){
    df %>% replace(is.na(.), 0) %>%
      transmute(playerID, season=yearID, level="MLB", league=lgID, team=teamID, 
                order, g=G, gs=GS, w=W, l=L, 
                ip=as.numeric(paste0(floor(IPouts/3), ".", IPouts%%3)), 
                r=R, er=ER, h=H, hr=HR, k=SO, bb=BB, ibb=IBB, ubb=BB-IBB, 
                hbp=HBP, wp=WP, bk=BK, sh=SH, sf=SF, gidp=GIDP, bf=BFP, 
                cg=CG, sho=SHO, gf=GF, sv=SV)
  }
  
  if(regular_season==TRUE) {
    group_pitching(Lahman::Pitching %>% 
                     mutate(order=paste0("R", "-", as.character(stint))) %>% 
                     filter(lgID %in% c("AL", "NL") & yearID>year))
  }
  
  else{
    group_pitching(Lahman::PitchingPost %>% mutate(order=round) %>% 
                     filter(lgID %in% c("AL", "NL") & yearID>year))
  }
  
};
modern_era_pitching=function(year=1960){
  
  df1=career_pitching(year=year)
  list=unique(df1$playerID)
  df2=career_pitching(year=min((Lahman::Pitching)$yearID)) %>% 
    filter(playerID %in% list)
  df2=df2 %>% anti_join(df1, 
                        by=c("playerID", "season", "level", 
                             "league", "team", "order"));
  
  df3=career_pitching(year=year, regular_season=FALSE)
  list=unique(df3$playerID)
  df4=career_pitching(year=min((Lahman::PitchingPost)$yearID), 
                     regular_season=FALSE) %>% filter(playerID %in% list)
  df4=df4 %>% anti_join(df3, 
                        by=c("playerID", "season", "level", 
                             "league", "team", "order"))
  
  playoff_order=function(df){
    c(sort(unique(df$order)[str_detect(unique(df$order), "R")]),
      sort(unique(df$order)[str_detect(unique(df$order), "WC")]),
      sort(unique(df$order)[str_detect(unique(df$order), "DIV")]),
      sort(unique(df$order)[str_detect(unique(df$order), "DS")]),
      sort(unique(df$order)[str_detect(unique(df$order), "CS")]), "WS")
  }
  
  df_final=rbind(df1, df2, df3, df4) %>% mutate(order=as.character(order))
  
  df_final %>% mutate(order=factor(order)) %>% 
    mutate(order=fct_relevel(order, playoff_order(df_final))) %>% 
    arrange(playerID, season, level, league, order) %>% 
    rename(lahmanID=playerID)
  
};

df_yrly_pitching=modern_era_pitching();



lahmanID_join=function(df){
  
  Lahman::People %>% rename(lahmanID=playerID) %>% semi_join(df)
  
};
lahmanID_merge=function(){
  
  rbind(lahmanID_join(df_yrly_batting), 
        lahmanID_join(df_yrly_pitching)) %>% distinct() %>%
    transmute(player_name=paste(nameFirst, nameLast), lahmanID, 
              retroID, bbrefID, bats, throws, debutGame=debut, finalGame, 
              birthDate, deathDate, birthCity, birthState, birthCountry)
  
};
chadwick_tbl=function(){
  
  drop_col=function(df){df[,colSums(is.na(df))<nrow(df)]}
  
  baseballr::get_chadwick_lu() %>% 
    transmute(retroID=key_retro, bbrefID=key_bbref, 
              bbrefMinorsID=key_bbref_minors, mlbID=key_mlbam, 
              fangraphsID=key_fangraphs, npbID=key_npb, 
              nflID=key_sr_nfl, nbaID=key_sr_nba, nhlID=key_sr_nhl, 
              col_debut_yr=col_played_first, 
              pro_debut_yr=pro_played_first, 
              mlb_debut_yr=mlb_played_first, 
              col_final_yr=col_played_last, 
              pro_final_yr=pro_played_last, 
              mlb_final_yr=mlb_played_last, 
              col_mng_debut_yr=col_managed_first, 
              pro_mng_debut_yr=pro_managed_first, 
              mlb_mng_debut_yr=mlb_managed_first, 
              col_mng_final_yr=col_managed_last, 
              pro_mng_final_yr=pro_managed_last, 
              mlb_mng_final_yr=mlb_managed_last, 
              pro_ump_debut_yr=pro_umpired_first, 
              mlb_ump_debut_yr=mlb_umpired_first, 
              pro_ump_final_yr=pro_umpired_last, 
              mlb_ump_final_yr=mlb_umpired_last) %>% drop_col()
  
};
player_merge=function(){
  
  lahmanID_merge() %>% 
    left_join(chadwick_tbl())
  
};

df_players=player_merge();
rm(career_batting, career_pitching, chadwick_tbl, lahmanID_join, 
   lahmanID_merge, modern_era_batting, modern_era_pitching, player_merge);
