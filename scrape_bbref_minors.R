library(tidyverse);
library(rvest);
library(xml2);


##Need to clean file & identify why mapping breaks


list_players=read.csv("player_list.csv") %>% 
  rename(playerID=1) %>% 
  pull(playerID) %>% 
  unique()
list_players=c(list_players[1:2241], "lopez-033jos", list_players[2243:5770])

scrape_batting=function(playerID){
  
  header=c("playerID", "Year", "Age", "AgeDif", "Tm", "Lg", 
           "Lev", "Aff", "G", "PA", "AB", "R", "H", "2B", "3B", 
           "HR", "RBI", "SB", "CS", "BB", "SO", "BA", "OBP", 
           "SLG", "OPS", "TB", "GDP", "HBP", "SH", "SF", "IBB")
  
  batting=function(df){
    
    df %>%
      filter(!(G=="" | str_detect(Tm, "season") | str_detect(Tm, "Season") | 
                 str_detect(Tm, "Tm") | str_detect(Tm, "Teams"))) %>%
      rename(db=14, 
             tr=15) %>%
      transmute(playerID, 
                year=Year, 
                team=Tm, 
                org=Aff, 
                league=Lg, 
                level=Lev, 
                age=as.numeric(Age), 
                g=as.numeric(G), 
                pa=as.numeric(PA), 
                ab=as.numeric(AB), 
                ba=as.numeric(BA), 
                obp=as.numeric(OBP), 
                slg=as.numeric(SLG), 
                ops=as.numeric(OPS), 
                h=as.numeric(H), 
                db=as.numeric(db), 
                tr=as.numeric(tr), 
                hr=as.numeric(HR), 
                r=as.numeric(R), 
                rbi=as.numeric(RBI), 
                k=as.numeric(SO), 
                bb=as.numeric(BB), 
                ibb=as.numeric(IBB), 
                sb=as.numeric(SB), 
                cs=as.numeric(CS), 
                tb=as.numeric(TB), 
                gdp=as.numeric(GDP), 
                hbp=as.numeric(HBP), 
                sh=as.numeric(SH), 
                sf=as.numeric(SF)) %>% 
      filter(pa>0)
    
    
  };
  
  df_main=paste0("https://www.baseball-reference.com/register/player.fcgi?id=",
         playerID) %>% 
    read_html() %>% 
    html_element("body") %>% 
    html_table() %>% 
    cbind(playerID, .)
  
  list_main=paste0("https://www.baseball-reference.com/register/player.fcgi?id=",
                   playerID) %>% 
    read_html() %>%
    html_nodes(xpath='//comment()') %>%
    html_text() %>%
    paste(collapse="") %>% 
    read_html() %>% 
    html_table()
  
  extr_names=function(num){
    
    identical(colnames(as.data.frame(cbind(playerID, 
                                           list_main[[num]]))), 
              header)
    
  }
  
  list_names=map_chr(.x=1:length(list_main), .f=extr_names)
  
  index=match(TRUE, list_names)
  
  df_sec=as.data.frame(cbind(playerID, 
                             list_main[[index]]))
  
  if(identical(df_main %>% colnames(), header)){
    
    df_main %>% batting()
    
  } 
  
  else if(identical(df_sec %>% colnames(), header)){
    
    df_sec %>% batting()
    
  }
  
};

##connection cuts out if too many players are run through in sequence
df_batting=map_df(.x=list_players[1:100], .f=scrape_batting)
df_batting=rbind(df_batting, 
                 map_df(.x=list_players[101:250], .f=scrape_batting))
df_batting=rbind(df_batting, 
                 map_df(.x=list_players[251:500], .f=scrape_batting))
##alternating in case of data loss
df_batting2=rbind(df_batting, 
                  map_df(.x=list_players[501:1000], .f=scrape_batting))
df_batting=rbind(df_batting2, 
                  map_df(.x=list_players[1001:1500], .f=scrape_batting))
df_batting2=rbind(df_batting, 
                 map_df(.x=list_players[1501:2000], .f=scrape_batting))
df_batting=rbind(df_batting2, 
                 map_df(.x=list_players[2001:2100], .f=scrape_batting))
df_batting2=rbind(df_batting, 
                 map_df(.x=list_players[2101:2150], .f=scrape_batting))
df_batting=rbind(df_batting2, 
                 map_df(.x=list_players[2151:2200], .f=scrape_batting))
df_batting2=rbind(df_batting, 
                  map_df(.x=list_players[2201:2225], .f=scrape_batting))
##2226:2250
df_batting=rbind(df_batting2, 
                 map_df(.x=list_players[2251:2500], .f=scrape_batting))
df_batting2=rbind(df_batting, 
                  map_df(.x=list_players[2501:3000], .f=scrape_batting))
df_batting=rbind(df_batting2, 
                 map_df(.x=list_players[3001:3500], .f=scrape_batting))
df_batting2=rbind(df_batting, 
                  map_df(.x=list_players[3501:4000], .f=scrape_batting))
df_batting=rbind(df_batting2, 
                 map_df(.x=list_players[4001:4500], .f=scrape_batting))
df_batting2=rbind(df_batting, 
                  map_df(.x=list_players[4501:5000], .f=scrape_batting))
df_batting=df_batting2
df_batting=rbind(df_batting, 
                 map_df(.x=list_players[5001:5250], .f=scrape_batting))
df_batting2=df_batting
df_batting=rbind(df_batting, 
                 map_df(.x=list_players[5251:5500], .f=scrape_batting))
df_batting2=df_batting
df_batting=rbind(df_batting, 
                 map_df(.x=list_players[5501:5770], .f=scrape_batting))
df_batting2=df_batting;
df1=map_df(.x=list_players[2226:2230], .f=scrape_batting)
df2=map_df(.x=list_players[2231:2240], .f=scrape_batting)
df3=map_df(.x=list_players[2245:2250], .f=scrape_batting)
df4=map_df(.x=list_players[2243:2244], .f=scrape_batting)
scrape_batting(list_players[2241])
##wrong ID, changed from lopez-008jos in game to lopez-033jos
scrape_batting("lopez-033jos")
df_batting=rbind(df_batting2, df1, df2, df3, df4, 
                 scrape_batting(list_players[2241]), 
                 scrape_batting("lopez-033jos")) %>% 
  distinct() %>% 
  mutate(level=factor(level,
                      levels=c("Maj", "AAA", "AA", "A+", "A", 
                               "A-", "Rk", "Fgn", "Ind", "FgW"))) %>%
  arrange(year, playerID, level)
write.csv(df_batting, "df_batting.csv", row.names=FALSE, col.names=TRUE)










scrape_pitching=function(playerID){
  
  header=c("playerID", "Year", "Age", "AgeDif", "Tm", "Lg", "Lev", "Aff", 
           "W", "L", "W-L%", "ERA", "RA9", "G", "GS", "GF", "CG", "SHO", 
           "SV", "IP", "H", "R", "ER", "HR", "BB", "IBB", "SO", "HBP", 
           "BK", "WP", "BF", "WHIP", "H9", "HR9", "BB9", "SO9", "SO/W")
  
  pitching=function(df){
    
    df %>%
      filter(!(G=="" | str_detect(Tm, "season") | str_detect(Tm, "Season") | 
                 str_detect(Tm, "Tm") | str_detect(Tm, "Teams"))) %>%
      rename(k_bb=37) %>%
      transmute(playerID, 
                year=Year, 
                team=Tm, 
                org=Aff, 
                league=Lg, 
                level=Lev, 
                age=as.numeric(Age), 
                g=as.numeric(G), 
                gs=as.numeric(GS), 
                ip=as.numeric(IP), 
                w=as.numeric(W), 
                l=as.numeric(L), 
                bf=as.numeric(BF), 
                h=as.numeric(H), 
                r=as.numeric(R), 
                er=as.numeric(ER), 
                hr=as.numeric(HR), 
                k=as.numeric(SO), 
                bb=as.numeric(BB), 
                ibb=as.numeric(IBB),
                hbp=as.numeric(HBP), 
                bk=as.numeric(BK), 
                wp=as.numeric(WP), 
                cg=as.numeric(CG), 
                sho=as.numeric(SHO), 
                gf=as.numeric(GF), 
                sv=as.numeric(SV), 
                era=as.numeric(ERA), 
                ra9=as.numeric(RA9),
                whip=as.numeric(WHIP), 
                h9=as.numeric(H9), 
                hr9=as.numeric(HR9), 
                k9=as.numeric(SO9),
                bb9=as.numeric(BB9), 
                k_bb=as.numeric(k_bb)) ##%>% 
      ##filter(bf>0)
    
  };
  
  df_main=paste0("https://www.baseball-reference.com/register/player.fcgi?id=",
                 playerID) %>% 
    read_html() %>% 
    html_element("body") %>% 
    html_table() %>% 
    cbind(playerID, .)
  
  list_main=paste0("https://www.baseball-reference.com/register/player.fcgi?id=",
                   playerID) %>% 
    read_html() %>%
    html_nodes(xpath='//comment()') %>%
    html_text() %>%
    paste(collapse="") %>% 
    read_html() %>% 
    html_table()
  
  extr_names=function(num){
    
    identical(colnames(as.data.frame(cbind(playerID, 
                                           list_main[[num]]))), 
              header)
    
  }
  
  list_names=map_chr(.x=1:length(list_main), .f=extr_names)
  
  index=match(TRUE, list_names)
  
  df_sec=as.data.frame(cbind(playerID, 
                             list_main[[index]]))
  
  if(identical(df_main %>% colnames(), header)){
    
    df_main %>% pitching()
    
  } 
  
  else if(identical(df_sec %>% colnames(), header)){
    
    df_sec %>% pitching()
    
  }
  
};



df=map_df(.x=list_players[1:10], .f=scrape_pitching)
df1=map_df(.x=list_players[11:500], .f=scrape_pitching)
df2=map_df(.x=list_players[501:1000], .f=scrape_pitching)
df3=map_df(.x=list_players[1001:1500], .f=scrape_pitching)
df4=map_df(.x=list_players[1501:2000], .f=scrape_pitching)
df5=map_df(.x=list_players[2001:2500], .f=scrape_pitching)
df6=map_df(.x=list_players[2501:3000], .f=scrape_pitching)
df7=map_df(.x=list_players[3001:3500], .f=scrape_pitching)
df8=map_df(.x=list_players[3501:4000], .f=scrape_pitching)
df9=map_df(.x=list_players[4001:4500], .f=scrape_pitching)
df10=map_df(.x=list_players[4501:5000], .f=scrape_pitching)
df11=map_df(.x=list_players[5001:5500], .f=scrape_pitching)
df12=map_df(.x=list_players[5501:5770], .f=scrape_pitching)



df5=map_df(.x=list_players[2001:2250], .f=scrape_pitching)
##df6=map_df(.x=list_players[2251:2500], .f=scrape_pitching)
df12=map_df(.x=list_players[2501:2750], .f=scrape_pitching)
##df13=map_df(.x=list_players[2751:3000], .f=scrape_pitching)
##df14=map_df(.x=list_players[5501:5650], .f=scrape_pitching)
df15=map_df(.x=list_players[5651:5770], .f=scrape_pitching)


##df_5=map_df(.x=list_players[2001:2100], .f=scrape_pitching)
df_12=map_df(.x=list_players[2101:2200], .f=scrape_pitching)
##df_15=map_df(.x=list_players[2201:2250], .f=scrape_pitching)
df_16=map_df(.x=list_players[2501:2600], .f=scrape_pitching)
##df_17=map_df(.x=list_players[2601:2700], .f=scrape_pitching)
df_18=map_df(.x=list_players[2701:2750], .f=scrape_pitching)
##df_19=map_df(.x=list_players[5651:5700], .f=scrape_pitching)
df_20=map_df(.x=list_players[5701:5750], .f=scrape_pitching)
##df_21=map_df(.x=list_players[5751:5770], .f=scrape_pitching)


##df_12=map_df(.x=list_players[2101:2125], .f=scrape_pitching)
df_16=map_df(.x=list_players[2126:2150], .f=scrape_pitching)
##df_18=map_df(.x=list_players[2151:2175], .f=scrape_pitching)
##df_20=map_df(.x=list_players[2176:2200], .f=scrape_pitching)
##df_22=map_df(.x=list_players[2501:2525], .f=scrape_pitching)
df_23=map_df(.x=list_players[2526:2550], .f=scrape_pitching)
##df_24=map_df(.x=list_players[2551:2575], .f=scrape_pitching)
##df_25=map_df(.x=list_players[2576:2600], .f=scrape_pitching)
##df_26=map_df(.x=list_players[2701:2725], .f=scrape_pitching)
df_27=map_df(.x=list_players[2726:2750], .f=scrape_pitching)
df_28=map_df(.x=list_players[5701:5725], .f=scrape_pitching)
##df_29=map_df(.x=list_players[5726:5750], .f=scrape_pitching)

pitching_df=rbind(df, df_12, df_15, df_17, df_18, df_19, df_20, 
                  df_21, df_22, df_24, df_25, df_26, df_29, df_5, 
                  df1, df10, df11, df13, df14, df2, df3, df4, 
                  df6, df7, df8, df9)
rm(df, df_12, df_15, df_17, df_18, df_19, df_20, 
   df_21, df_22, df_24, df_25, df_26, df_29, df_5, 
   df1, df10, df11, df13, df14, df2, df3, df4, 
   df6, df7, df8, df9)


##df1=map_df(.x=list_players[2126:2130], .f=scrape_pitching)
##df2=map_df(.x=list_players[2131:2140], .f=scrape_pitching)
df3=map_df(.x=list_players[2141:2150], .f=scrape_pitching)
df4=map_df(.x=list_players[2526:2530], .f=scrape_pitching)
##df5=map_df(.x=list_players[2531:2540], .f=scrape_pitching)
##df6=map_df(.x=list_players[2541:2550], .f=scrape_pitching)
##df7=map_df(.x=list_players[2726:2730], .f=scrape_pitching)
##df8=map_df(.x=list_players[2731:2740], .f=scrape_pitching)
df9=map_df(.x=list_players[2741:2750], .f=scrape_pitching)
##dfa1=map_df(.x=list_players[5701:5710], .f=scrape_pitching)
##dfa2=map_df(.x=list_players[5711:5720], .f=scrape_pitching)
dfa3=map_df(.x=list_players[5721:5725], .f=scrape_pitching)

df3=map_df(.x=list_players[2141:2145], .f=scrape_pitching)
##df4=map_df(.x=list_players[2146:2150], .f=scrape_pitching)
df9=map_df(.x=list_players[2526:2527], .f=scrape_pitching)
##dfa3=map_df(.x=list_players[2528:2530], .f=scrape_pitching)
dfa4=map_df(.x=list_players[2741:2745], .f=scrape_pitching)
##dfa5=map_df(.x=list_players[2746:2750], .f=scrape_pitching)
dfa6=map_df(.x=list_players[5721:5722], .f=scrape_pitching)
##dfa7=map_df(.x=list_players[5723:5725], .f=scrape_pitching)

df3=rbind(
scrape_pitching(list_players[2141]),
scrape_pitching(list_players[2142]),
##scrape_pitching(list_players[2143]),
scrape_pitching(list_players[2144]),
scrape_pitching(list_players[2145]),
##scrape_pitching(list_players[2526]),
scrape_pitching(list_players[2527]),
scrape_pitching(list_players[2741]),
scrape_pitching(list_players[2742]),
##scrape_pitching(list_players[2743]),
scrape_pitching(list_players[2744]),
##scrape_pitching(list_players[2745]),
scrape_pitching(list_players[5721])
##scrape_pitching(list_players[5722]),
)

df9=rbind(
  scrape_pitching(list_players[2143]),
  scrape_pitching(list_players[2526]),
  scrape_pitching(list_players[2743]),
  scrape_pitching(list_players[2745]),
  scrape_pitching(list_players[5722])
)



rbind(pitching_df, df1, df2, df3, df4, df5, df6, df7, df8, 
      df9, dfa1, dfa2, dfa3, dfa5, dfa7)

df_pitching=rbind(pitching_df, df1, df2, df3, df4, df5, df6, df7, df8, 
                  df9, dfa1, dfa2, dfa3, dfa5, dfa7) %>% 
  distinct()

rm(pitching_df, df1, df2, df3, df4, df5, df6, df7, df8, 
   df9, dfa1, dfa2, dfa3, dfa5, dfa7)




df_pitching %>% 
  mutate(level=factor(level,
                      levels=c("Maj", "AAA", "AA", "A+", "A", 
                               "A-", "Rk", "Fgn", "Ind", "FgW"))) %>%
  arrange(year, playerID, level) %>% 
write.csv("df_pitching.csv", row.names=FALSE, col.names=TRUE)



















scrape_fielding=function(playerID){
  
  header=c("playerID", "Year", "Age", "Tm", "Lg", "Lev", "Aff", "Var.8", "G", 
           "GS", "CG", "Inn", "Ch", "PO", "A", "E", "DP", "Fld%", "RF/9", 
           "RF/G", "PB", "WP", "SB", "CS", "CS%", "lgCS%", "PO")
  
  list_main=paste0("https://www.baseball-reference.com/register/player.fcgi?id=",
                   playerID) %>% 
    read_html() %>%
    html_nodes(xpath='//comment()') %>%
    html_text() %>%
    paste(collapse="") %>% 
    read_html() %>% 
    html_table()
  
  extr_names=function(num){
    
    identical(colnames(as.data.frame(cbind(playerID, 
                                           list_main[[num]]))), 
              header)
    
  }
  
  list_names=map_chr(.x=1:length(list_main), .f=extr_names)
  
  index=match(TRUE, list_names)
  
  
  as.data.frame(cbind(playerID, 
                             list_main[[index]])) %>% 
    rename(pk=27, 
           fld=18, 
           rf9=19, 
           rfG=20) %>%
    filter(!(G=="" | str_detect(Tm, "season") | str_detect(Tm, "Season") | 
               str_detect(Tm, "Tm") | str_detect(Tm, "Teams"))) %>%
    transmute(playerID, 
              year=Year, 
              team=Tm, 
              org=Aff, 
              league=Lg, 
              level=Lev, 
              age=as.numeric(Age), 
              pos=Var.8, 
              g=as.numeric(G), 
              gs=as.numeric(GS), 
              inn=as.numeric(Inn), 
              ch=as.numeric(Ch), 
              po=as.numeric(PO), 
              a=as.numeric(A),
              e=as.numeric(E), 
              dp=as.numeric(DP), 
              fld=as.numeric(fld), 
              rf9=as.numeric(rf9), 
              rfG=as.numeric(rfG), 
              pb=as.numeric(PB), 
              wp=as.numeric(WP), 
              sb=as.numeric(SB), 
              cs=as.numeric(CS), 
              pk=as.numeric(pk))
    
};




df1=map_df(.x=list_players[1:50], .f=scrape_fielding)




df2=map_df(.x=list_players[501:1000], .f=scrape_fielding)
df3=map_df(.x=list_players[1001:1500], .f=scrape_fielding)
df4=map_df(.x=list_players[1501:2000], .f=scrape_fielding)
df5=map_df(.x=list_players[2001:2500], .f=scrape_fielding)
df6=map_df(.x=list_players[2501:3000], .f=scrape_fielding)
df7=map_df(.x=list_players[3001:3500], .f=scrape_fielding)
df8=map_df(.x=list_players[3501:4000], .f=scrape_fielding)
df9=map_df(.x=list_players[4001:4500], .f=scrape_fielding)
df10=map_df(.x=list_players[4501:5000], .f=scrape_fielding)
df11=map_df(.x=list_players[5001:5500], .f=scrape_fielding)
df12=map_df(.x=list_players[5501:5770], .f=scrape_fielding)




scrape_teams=function(playerID){
  
  paste0("https://www.baseball-reference.com/register/player.fcgi?id=",
         playerID) %>% 
    read_html() %>%
    html_nodes(xpath='//comment()') %>%
    html_text() %>%
    paste(collapse="") %>% 
    read_html() %>% 
    html_table() %>% 
    .[[2]] %>%
    cbind(playerID, .)
  
};