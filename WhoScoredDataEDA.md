WhoScored Data
================
Eric Morris
12/30/2018

La Liga
-------

All data was pulled manually from WhoScored (scraping was difficult due to UI). Statistics were filtered for players with a WhoScored's automatic minimum appearances filter (players with total appearances greater than the average number of appearances in Primera Division) and then further filtered to only include players with &gt;500 minutes played. Statistics were pulled on 12/29, during La Liga's Winter Break (all teams have played 17/38 matches except Real Madrid with 16).

``` r
ws_liga_stats = 
  read_excel("./data/WhoScoredLaLigaStats.xlsx", col_names = TRUE) %>% 
  janitor::clean_names() %>% 
  separate(player, into = c("name", "team"), sep = ",") %>%
  mutate(goals_per_90 = (goals / mins) * 90,
         assists_per_90 = (assists / mins) * 90,
         gpa_per_90 = goals_per_90 + assists_per_90,
         shots_kp_game = shots_per_game + key_passes_per_g)
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 236 rows [1, 2,
    ## 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
ws_liga_stats %>% 
  group_by(team) %>% 
  summarize()
```

    ## # A tibble: 20 x 1
    ##    team               
    ##    <chr>              
    ##  1 " Athletic Bilbao" 
    ##  2 " Atletico Madrid" 
    ##  3 " Barcelona"       
    ##  4 " Celta Vigo"      
    ##  5 " Deportivo Alaves"
    ##  6 " Eibar"           
    ##  7 " Espanyol"        
    ##  8 " Getafe"          
    ##  9 " Girona"          
    ## 10 " Leganes"         
    ## 11 " Levante"         
    ## 12 " Rayo Vallecano"  
    ## 13 " Real Betis"      
    ## 14 " Real Madrid"     
    ## 15 " Real Sociedad"   
    ## 16 " Real Valladolid" 
    ## 17 " SD Huesca"       
    ## 18 " Sevilla"         
    ## 19 " Valencia"        
    ## 20 " Villarreal"

``` r
quantile(ws_liga_stats$shots_kp_game, c(0.5, 0.75, 0.80, 0.85, 0.90))
```

    ##  50%  75%  80%  85%  90% 
    ## 1.60 2.60 2.70 3.00 3.45

``` r
ws_liga_stats %>% 
  ggplot(aes(x = shots_kp_game, y = gpa_per_90, color = team)) +
  geom_point(alpha = 0.75) +
  geom_text_repel(aes(label = ifelse(gpa_per_90 >= 0.479, name, ifelse(shots_kp_game >= 3.45, name, ''))), angle = 30, size = 2) +
  scale_color_manual(values = c(" Athletic Bilbao" = "red2",
                               " Atletico Madrid" = "red",
                               " Barcelona" = "maroon",
                               " Celta Vigo" = "lightblue",
                               " Deportivo Alaves" = "blue",
                               " Eibar" = "dodgerblue4",
                               " Espanyol" = "mediumblue",
                               " Getafe" = "royalblue2",
                               " Girona" = "firebrick1",
                               " Leganes" = "dodgerblue",
                               " Levante" = "red4",
                               " Rayo Vallecano" = "gold",
                               " Real Betis" = "green3",
                               " Real Madrid" = "purple1",
                               " Real Sociedad" = "royalblue3",
                               " Real Valladolid" = "mediumpurple",
                               " SD Huesca" = "midnightblue",
                               " Sevilla" = "firebrick3",
                               " Valencia"  = "gray0",
                               " Villarreal" = "yellow3")) + 
  labs(title = "La Liga Offensive Leaders at Christmas Break", 
       y = "Goals plus Assists per 90", 
       x = "Shots plus Key Passes per game", 
       color = "Player's Team",
       caption = "Data: WhoScored Graph: @ericdavidmorris
       Marked players are in top 10% of either category") + 
  scale_x_continuous(breaks = seq(0, 8.4, 0.20)) + 
  scale_y_continuous(breaks = seq(0, 1.90, 0.10)) + 
  theme_bw() +
  theme(legend.position = "bottom") 
```

<img src="WhoScoredDataEDA_files/figure-markdown_github/La Liga Chart-1.png" style="display: block; margin: auto;" />

``` r
#Messi filtered out

no_messi = ws_liga_stats %>% 
  filter(name != "Lionel Messi")

quantile(no_messi$gpa_per_90, c(0.5, 0.75, 0.80, 0.85, 0.90))
```

    ##       50%       75%       80%       85%       90% 
    ## 0.1669759 0.3074316 0.3462205 0.4002063 0.4722428

``` r
ws_liga_stats %>% 
  filter(name != "Lionel Messi") %>%
  ggplot(aes(x = shots_kp_game, y = gpa_per_90, color = team)) +
  geom_point(alpha = 0.75) +
  geom_text_repel(aes(label = ifelse(gpa_per_90 >= 0.472, name, ifelse(shots_kp_game >= 3.40, name, ''))), angle = 30, size = 2) +
  scale_color_manual(values = c(" Athletic Bilbao" = "red2",
                               " Atletico Madrid" = "red",
                               " Barcelona" = "maroon",
                               " Celta Vigo" = "lightblue",
                               " Deportivo Alaves" = "blue",
                               " Eibar" = "dodgerblue4",
                               " Espanyol" = "mediumblue",
                               " Getafe" = "royalblue2",
                               " Girona" = "firebrick1",
                               " Leganes" = "dodgerblue",
                               " Levante" = "red4",
                               " Rayo Vallecano" = "gold",
                               " Real Betis" = "green3",
                               " Real Madrid" = "purple1",
                               " Real Sociedad" = "royalblue3",
                               " Real Valladolid" = "mediumpurple",
                               " SD Huesca" = "midnightblue",
                               " Sevilla" = "firebrick3",
                               " Valencia"  = "gray0",
                               " Villarreal" = "yellow3")) + 
  labs(title = "La Liga Offensive Leaders at Christmas Break (Messi Filtered)", 
       y = "Goals plus Assists per 90", 
       x = "Shots plus Key Passes per game", 
       color = "Player's Team",
       caption = "Data: WhoScored Graph: @ericdavidmorris
       Marked players are in top 10% of either category") + 
  scale_x_continuous(breaks = seq(0, 5.1, 0.10)) + 
  scale_y_continuous(breaks = seq(0, 1.2, 0.10)) + 
  theme_bw() +
  theme(legend.position = "bottom") 
```

<img src="WhoScoredDataEDA_files/figure-markdown_github/La Liga Chart-2.png" style="display: block; margin: auto;" />

Serie A
-------

All data was pulled manually from WhoScored (scraping was difficult due to UI). Statistics were filtered for players with a WhoScored's automatic minimum appearances filter (players with total appearances greater than the average number of appearances in Serie A) and then further filtered to only include players with &gt;500 minutes played. Statistics were pulled on 12/30, all teams have played 19 matches.

``` r
ws_seriea_stats = 
  read_excel("./data/SerieAWhoScoredStats.xlsx", col_names = TRUE) %>% 
  janitor::clean_names() %>% 
  separate(player, into = c("name", "team"), sep = ",") %>%
  mutate(goals_per_90 = (goals / mins) * 90,
         assists_per_90 = (assists / mins) * 90,
         gpa_per_90 = goals_per_90 + assists_per_90,
         shots_kp_game = shots_per_game + key_passes_per_g)
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 237 rows [1, 2,
    ## 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
ws_seriea_stats %>% 
  group_by(team) %>% 
  summarize()
```

    ## # A tibble: 20 x 1
    ##    team             
    ##    <chr>            
    ##  1 AC Milan         
    ##  2 Atalanta         
    ##  3 Bologna          
    ##  4 Cagliari         
    ##  5 Chievo           
    ##  6 Empoli           
    ##  7 Fiorentina       
    ##  8 Frosinone        
    ##  9 Genoa            
    ## 10 Inter            
    ## 11 Juventus         
    ## 12 Lazio            
    ## 13 Napoli           
    ## 14 Parma Calcio 1913
    ## 15 Roma             
    ## 16 Sampdoria        
    ## 17 Sassuolo         
    ## 18 SPAL 2013        
    ## 19 Torino           
    ## 20 Udinese

``` r
quantile(ws_seriea_stats$shots_kp_game, c(0.5, 0.75, 0.80, 0.85, 0.90))
```

    ##  50%  75%  80%  85%  90% 
    ## 1.90 2.80 3.10 3.56 4.24

``` r
quantile(ws_seriea_stats$gpa_per_90, c(0.5, 0.75, 0.80, 0.85, 0.90))
```

    ##       50%       75%       80%       85%       90% 
    ## 0.1750973 0.3437739 0.3682088 0.4359258 0.5355495

``` r
ws_seriea_stats %>% 
  ggplot(aes(x = shots_kp_game, y = gpa_per_90, color = team)) +
  geom_point(alpha = 0.75) +
  geom_text_repel(aes(label = ifelse(gpa_per_90 >= 0.535, name, ifelse(shots_kp_game >= 4.2, name, ''))), angle = 30, size = 2) +
  scale_color_manual(values = c("AC Milan" = "firebrick2",
                                "Atalanta" = "mediumblue",
                                "Bologna" = "navy",    
                                "Cagliari" = "midnightblue",    
                                "Chievo" = "gold1",        
                                "Empoli" = "coral1",        
                                "Fiorentina" = "purple4",    
                                "Frosinone" = "yellow3",     
                                "Genoa" = "royalblue4",       
                                "Inter" = "royalblue",
                                "Juventus" = "black",      
                                "Lazio" = "lightblue",         
                                "Napoli" = "dodgerblue",
                                "Parma Calcio 1913" = "gray",         
                                "Roma" = "darkorange",        
                                "Sampdoria" = "royalblue2", 
                                "Sassuolo" = "seagreen",
                                "SPAL 2013" = "steelblue3",   
                                "Torino" = "tomato4",
                                "Udinese" = "gray47")) + 
  labs(title = "Serie A Offensive Leaders after 19 matches", 
       y = "Goals plus Assists per 90", 
       x = "Shots plus Key Passes per game", 
       color = "Player's Team",
       caption = "Data: WhoScored Graph: @ericdavidmorris
       Marked players are in top 10% of either category") + 
  scale_x_continuous(breaks = seq(0, 8.2, 0.20)) + 
  scale_y_continuous(breaks = seq(0, 1.20, 0.10)) + 
  theme_bw() +
  theme(legend.position = "bottom") 
```

<img src="WhoScoredDataEDA_files/figure-markdown_github/Serie A Chart-1.png" style="display: block; margin: auto;" />

La Liga + Serie A
-----------------

``` r
italy_spain_merged = 
  bind_rows(ws_liga_stats, ws_seriea_stats)

quantile(italy_spain_merged$shots_kp_game, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95))
```

    ##  50%  75%  80%  85%  90%  95% 
    ## 1.70 2.60 2.90 3.30 3.78 4.44

``` r
quantile(italy_spain_merged$gpa_per_90, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95))
```

    ##       50%       75%       80%       85%       90%       95% 
    ## 0.1711027 0.3182461 0.3627586 0.4285739 0.5124851 0.7237094

``` r
quantile(italy_spain_merged$ws_rating, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95))
```

    ##   50%   75%   80%   85%   90%   95% 
    ## 6.780 6.940 7.010 7.070 7.130 7.224

``` r
italy_spain_merged %>% 
  ggplot(aes(x = shots_kp_game, y = gpa_per_90, color = league)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(aes(label = ifelse(gpa_per_90 >= 0.512, name, ifelse(shots_kp_game >= 3.78, name, ''))), angle = 30, size = 2) +
  labs(title = "La Liga and Serie A Top Creators as of 12/29", 
       y = "Goals plus Assists per 90", 
       x = "Shots plus Key Passes per game", 
       color = "League",
       caption = "Data: WhoScored Graph: @ericdavidmorris
       Marked players are in top 10% of either category") + 
  scale_x_continuous(breaks = seq(0, 8.4, 0.20)) + 
  scale_y_continuous(breaks = seq(0, 1.9, 0.10)) + 
  theme_bw() +
  scale_color_manual(values = c("La Liga" = "firebrick1",
                                "Serie A" = "green4")) + 
  theme(legend.position = "bottom") 
```

<img src="WhoScoredDataEDA_files/figure-markdown_github/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

``` r
italy_spain_merged %>% 
  ggplot(aes(x = ws_rating, y = shots_kp_game, color = league)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(aes(label = ifelse(shots_kp_game >= 4.44, name, ifelse(ws_rating >= 7.22, name, ''))), angle = 30, size = 2) +
  labs(title = "La Liga and Serie A Top Creators as of 12/29", 
       y = "Shots plus Key Passes per game", 
       x = "WhoScored Rating", 
       color = "League",
       caption = "Data: WhoScored Graph: @ericdavidmorris
       Marked players are in top 5% of either category") + 
  scale_x_continuous(breaks = seq(0, 8.8, 0.20)) + 
  scale_y_continuous(breaks = seq(0, 8.4, 0.20)) + 
  theme_bw() +
  scale_color_manual(values = c("La Liga" = "firebrick1",
                                "Serie A" = "green4")) + 
  theme(legend.position = "bottom") 
```

<img src="WhoScoredDataEDA_files/figure-markdown_github/unnamed-chunk-1-2.png" style="display: block; margin: auto;" />

Bundesliga
----------

All data was pulled manually from WhoScored (scraping was difficult due to UI). Statistics were filtered for players with a WhoScored's automatic minimum appearances filter (players with total appearances greater than the average number of appearances in Serie A) and then further filtered to only include players with &gt;450 minutes played. Statistics were pulled on 12/30, all teams have played 17 matches.

``` r
ws_bundesliga_stats = 
  read_excel("./data/BundesLigaWhoScoredStats.xlsx", col_names = TRUE) %>% 
  janitor::clean_names() %>% 
  separate(player, into = c("name", "team"), sep = ",") %>%
  mutate(goals_per_90 = (goals / mins) * 90,
         assists_per_90 = (assists / mins) * 90,
         gpa_per_90 = goals_per_90 + assists_per_90,
         shots_kp_game = shots_per_game + key_passes_per_g)
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 208 rows [1, 2,
    ## 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
ws_bundesliga_stats %>% 
  group_by(team) %>% 
  summarize()
```

    ## # A tibble: 18 x 1
    ##    team                  
    ##    <chr>                 
    ##  1 Augsburg              
    ##  2 Bayer Leverkusen      
    ##  3 Bayern Munich         
    ##  4 Borussia Dortmund     
    ##  5 Borussia M.Gladbach   
    ##  6 Eintracht Frankfurt   
    ##  7 Fortuna Duesseldorf   
    ##  8 Freiburg              
    ##  9 Hannover 96           
    ## 10 Hertha Berlin         
    ## 11 Hoffenheim            
    ## 12 Mainz 05              
    ## 13 Nuernberg             
    ## 14 RasenBallsport Leipzig
    ## 15 Schalke 04            
    ## 16 VfB Stuttgart         
    ## 17 Werder Bremen         
    ## 18 Wolfsburg

``` r
quantile(ws_bundesliga_stats$shots_kp_game, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95))
```

    ##  50%  75%  80%  85%  90%  95% 
    ## 1.95 2.60 2.86 3.10 3.53 4.00

``` r
quantile(ws_bundesliga_stats$gpa_per_90, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95))
```

    ##       50%       75%       80%       85%       90%       95% 
    ## 0.2057597 0.3761757 0.4187355 0.5184121 0.6152417 0.8390306

``` r
quantile(ws_bundesliga_stats$ws_rating, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95))
```

    ##    50%    75%    80%    85%    90%    95% 
    ## 6.7900 6.9325 7.0020 7.0600 7.1500 7.3500

``` r
ws_bundesliga_stats %>% 
  ggplot(aes(x = shots_kp_game, y = gpa_per_90, color = team)) +
  geom_point(alpha = 0.75) +
  geom_text_repel(aes(label = ifelse(gpa_per_90 >= 0.615, name, ifelse(shots_kp_game >= 3.53, name, ''))), angle = 30, size = 2) +
  scale_color_manual(values = c("Augsburg" = "dimgray",
                                "Bayer Leverkusen" = "firebrick4",
                                "Bayern Munich" = "firebrick2",
                                "Borussia Dortmund" = "gold1",
                                "Borussia M.Gladbach" = "green3",
                                "Eintracht Frankfurt" = "gray0",
                                "Fortuna Duesseldorf" = "tomato3",  
                                "Freiburg" = "gray25",       
                                "Hannover 96" = "chartreuse4",       
                                "Hertha Berlin" = "dodgerblue2",       
                                "Hoffenheim" = "blue1",     
                                "Mainz 05" = "red1",       
                                "Nuernberg" = "tomato4",
                                "RasenBallsport Leipzig" = "navy",
                                "Schalke 04" = "blue3",        
                                "VfB Stuttgart" = "orangered2",    
                                "Werder Bremen" = "springgreen3", 
                                "Wolfsburg" = "limegreen")) + 
  labs(title = "Bundesliga Offensive Leaders at Christmas Break", 
       y = "Goals plus Assists per 90", 
       x = "Shots plus Key Passes per game", 
       color = "Player's Team",
       caption = "Data: WhoScored Graph: @ericdavidmorris
       Marked players are in top 10% of either category") + 
  scale_x_continuous(breaks = seq(0, 6.0, 0.10)) + 
  scale_y_continuous(breaks = seq(0, 2.2, 0.10)) + 
  theme_bw() +
  theme(legend.position = "bottom") 
```

<img src="WhoScoredDataEDA_files/figure-markdown_github/German Chart-1.png" style="display: block; margin: auto;" />

Ligue 1
-------

All data was pulled manually from WhoScored (scraping was difficult due to UI). Statistics were filtered for players with a WhoScored's automatic minimum appearances filter (players with total appearances greater than the average number of appearances in Serie A) and then further filtered to only include players with &gt;450 minutes played. Statistics were pulled on 12/30, during the winter break.

``` r
ws_ligue_stats = 
  read_excel("./data/Ligue1WhoScoredStats.xlsx", col_names = TRUE) %>% 
  janitor::clean_names() %>% 
  separate(player, into = c("name", "team"), sep = ",") %>%
  mutate(goals_per_90 = (goals / mins) * 90,
         assists_per_90 = (assists / mins) * 90,
         gpa_per_90 = goals_per_90 + assists_per_90,
         shots_kp_game = shots_per_game + key_passes_per_g)
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 242 rows [1, 2,
    ## 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
ws_ligue_stats %>% 
  group_by(team) %>% 
  summarize()
```

    ## # A tibble: 20 x 1
    ##    team               
    ##    <chr>              
    ##  1 Amiens             
    ##  2 Angers             
    ##  3 Bordeaux           
    ##  4 Caen               
    ##  5 Dijon              
    ##  6 Guingamp           
    ##  7 Lille              
    ##  8 Lyon               
    ##  9 Marseille          
    ## 10 Monaco             
    ## 11 Montpellier        
    ## 12 Nantes             
    ## 13 Nice               
    ## 14 Nimes              
    ## 15 Paris Saint Germain
    ## 16 Reims              
    ## 17 Rennes             
    ## 18 Saint Etienne      
    ## 19 Strasbourg         
    ## 20 Toulouse

``` r
quantile(ws_ligue_stats$shots_kp_game, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95))
```

    ##   50%   75%   80%   85%   90%   95% 
    ## 1.600 2.475 2.600 3.085 3.890 4.695

``` r
quantile(ws_ligue_stats$gpa_per_90, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95))
```

    ##       50%       75%       80%       85%       90%       95% 
    ## 0.1581766 0.3400049 0.4018173 0.4758145 0.5410642 0.6917545

``` r
quantile(ws_ligue_stats$ws_rating, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95))
```

    ##    50%    75%    80%    85%    90%    95% 
    ## 6.7700 6.9500 7.0180 7.1000 7.1890 7.3195

``` r
ws_ligue_stats %>% 
  ggplot(aes(x = shots_kp_game, y = gpa_per_90, color = team)) +
  geom_point(alpha = 0.75) +
  geom_text_repel(aes(label = ifelse(gpa_per_90 >= 0.541, name, ifelse(shots_kp_game >= 3.89, name, ''))), angle = 30, size = 2) +
  scale_color_manual(values = c("Amiens" = "darkgray",
                                "Angers" = "black", 
                                "Bordeaux" = "navy",
                                "Caen" = "dodgerblue3",
                                "Dijon" = "firebrick2", 
                                "Guingamp" = "darkred",
                                "Lille" = "red2",
                                "Lyon" = "blue1",
                                "Marseille" = "darkturquoise",
                                "Monaco" = "goldenrod",
                                "Montpellier" = "darkorange1",
                                "Nantes" = "yellow2",
                                "Nice" = "gray8",
                                "Nimes" = "red3",
                                "Paris Saint Germain" = "midnightblue",
                                "Reims" = "brown1",
                                "Rennes" = "brown4",
                                "Saint Etienne" = "green4",
                                "Strasbourg" = "cadetblue2",
                                "Toulouse" = "mediumorchid4")) + 
  labs(title = "Ligue 1 Offensive Leaders at Christmas Break", 
       y = "Goals plus Assists per 90", 
       x = "Shots plus Key Passes per game", 
       color = "Player's Team",
       caption = "Data: WhoScored Graph: @ericdavidmorris
       Marked players are in top 10% of either category") + 
  scale_x_continuous(breaks = seq(0, 6.2, 0.10)) + 
  scale_y_continuous(breaks = seq(0, 1.7, 0.10)) + 
  theme_bw() +
  theme(legend.position = "bottom") 
```

<img src="WhoScoredDataEDA_files/figure-markdown_github/France Chart-1.png" style="display: block; margin: auto;" />

EPL
---

All data was pulled manually from WhoScored (scraping was difficult due to UI). Statistics were filtered for players with a WhoScored's automatic minimum appearances filter (players with total appearances greater than the average number of appearances in Serie A) and then further filtered to only include players with &gt;500 minutes played. Statistics were pulled on 12/30.

``` r
ws_epl_stats = 
  read_excel("./data/EPLWhoScoredStats.xlsx", col_names = TRUE) %>% 
  janitor::clean_names() %>% 
  separate(player, into = c("name", "team"), sep = ",") %>%
  mutate(goals_per_90 = (goals / mins) * 90,
         assists_per_90 = (assists / mins) * 90,
         gpa_per_90 = goals_per_90 + assists_per_90,
         shots_kp_game = shots_per_game + key_passes_per_g)
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 241 rows [1, 2,
    ## 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
ws_epl_stats %>% 
  group_by(team) %>% 
  summarize()
```

    ## # A tibble: 20 x 1
    ##    team                   
    ##    <chr>                  
    ##  1 Arsenal                
    ##  2 Bournemouth            
    ##  3 Brighton               
    ##  4 Burnley                
    ##  5 Cardiff                
    ##  6 Chelsea                
    ##  7 Crystal Palace         
    ##  8 Everton                
    ##  9 Fulham                 
    ## 10 Huddersfield           
    ## 11 Leicester              
    ## 12 Liverpool              
    ## 13 Manchester City        
    ## 14 Manchester United      
    ## 15 Newcastle United       
    ## 16 Southampton            
    ## 17 Tottenham              
    ## 18 Watford                
    ## 19 West Ham               
    ## 20 Wolverhampton Wanderers

``` r
quantile(ws_epl_stats$shots_kp_game, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95))
```

    ## 50% 75% 80% 85% 90% 95% 
    ## 1.7 2.6 2.7 2.9 3.4 4.1

``` r
quantile(ws_epl_stats$gpa_per_90, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95))
```

    ##       50%       75%       80%       85%       90%       95% 
    ## 0.1682243 0.4136029 0.4979253 0.5689001 0.6382979 0.8309659

``` r
quantile(ws_epl_stats$ws_rating, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95))
```

    ##  50%  75%  80%  85%  90%  95% 
    ## 6.77 6.97 7.00 7.11 7.19 7.35

``` r
ws_epl_stats %>% 
  ggplot(aes(x = shots_kp_game, y = gpa_per_90, color = team)) +
  geom_point(alpha = 0.75) +
  geom_text_repel(aes(label = ifelse(gpa_per_90 >= 0.638, name, ifelse(shots_kp_game >= 3.4, name, ''))), angle = 30, size = 2) +
  scale_color_manual(values = c("Arsenal" = "firebrick1",
                                "Bournemouth" = "gray0",
                                "Brighton" = "dodgerblue",
                                "Burnley" = "maroon",
                                "Cardiff" = "brown1",     
                                "Chelsea" = "blue",
                                "Crystal Palace" = "purple1",
                                "Everton" = "mediumblue",
                                "Fulham" = "gray48",        
                                "Huddersfield" = "yellow3",   
                                "Leicester" = "cornflowerblue",       
                                "Liverpool" = "red2",       
                                "Manchester City" = "cadetblue2",  
                                "Manchester United" = "firebrick3",
                                "Newcastle United" = "gray12",       
                                "Southampton" = "tomato3",    
                                "Tottenham" = "snow3",       
                                "Watford" = "yellow3",       
                                "West Ham" = "violetred4",      
                                "Wolverhampton Wanderers" = "orange1")) + 
  labs(title = "Premier League Offensive Leaders after Matchday 20", 
       y = "Goals plus Assists per 90", 
       x = "Shots plus Key Passes per game", 
       color = "Player's Team",
       caption = "Data: WhoScored Graph: @ericdavidmorris
       Marked players are in top 10% of either category") + 
  scale_x_continuous(breaks = seq(0, 5.7, 0.10)) + 
  scale_y_continuous(breaks = seq(0, 1.2, 0.10)) + 
  theme_bw() +
  theme(legend.position = "bottom") 
```

<img src="WhoScoredDataEDA_files/figure-markdown_github/EPL Chart-1.png" style="display: block; margin: auto;" />

top 5
=====

``` r
top5 = 
  bind_rows(ws_liga_stats, ws_seriea_stats, ws_bundesliga_stats, ws_epl_stats, ws_ligue_stats)

quantile(top5$shots_kp_game, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975))
```

    ##   50%   75%   80%   85%   90%   95% 97.5% 
    ##   1.7   2.6   2.8   3.1   3.6   4.3   4.9

``` r
quantile(top5$gpa_per_90, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975))
```

    ##       50%       75%       80%       85%       90%       95%     97.5% 
    ## 0.1750973 0.3501946 0.4107755 0.4880544 0.5785623 0.7594500 0.9234702

``` r
quantile(top5$ws_rating, c(0.5, 0.75, 0.80, 0.85, 0.90, 0.95, 0.975))
```

    ##    50%    75%    80%    85%    90%    95%  97.5% 
    ## 6.7800 6.9500 7.0100 7.0800 7.1600 7.3085 7.4100

``` r
top5 %>% 
  ggplot(aes(x = shots_kp_game, y = gpa_per_90, color = league)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(aes(label = ifelse(gpa_per_90 >= 0.7594, name, ifelse(shots_kp_game >= 4.3, name, ''))), angle = 25, size = 1.5) +
  labs(title = "Top 5 European Leagues' Best Creators as of 12/30", 
       y = "Goals plus Assists per 90", 
       x = "Shots plus Key Passes per game", 
       color = "League",
       caption = "Data: WhoScored Graph: @ericdavidmorris
       Marked players are in top 5% of either category") + 
  scale_x_continuous(breaks = seq(0, 8.4, 0.20)) + 
  scale_y_continuous(breaks = seq(0, 2.2, 0.10)) + 
  theme_bw() +
  scale_color_manual(values = c("La Liga" = "black",
                                "Serie A" = "green4",
                                "Bundesliga" = "red1",
                                "Premier League" = "purple4",
                                "Ligue 1" = "yellow3")) + 
  theme(legend.position = "bottom") 
```

<img src="WhoScoredDataEDA_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

``` r
top5 %>% 
  ggplot(aes(x = ws_rating, y = shots_kp_game, color = league)) +
  geom_point(alpha = 0.5) +
  geom_text_repel(aes(label = ifelse(ws_rating >= 7.3085, name, '')), angle = 25, size = 1.5) +
  labs(title = "Top 5 European Leagues' Best according to WhoScored as of 12/30", 
       y = "Shots plus Key Passes per game", 
       x = "WhoScored Rating", 
       color = "League",
       caption = "Data: WhoScored Graph: @ericdavidmorris
       Marked players are in top 5% of WhoScored Rating") + 
  scale_x_continuous(breaks = seq(0, 8.8, 0.20)) + 
  scale_y_continuous(breaks = seq(0, 8.4, 0.20)) + 
  theme_bw() +
  scale_color_manual(values = c("La Liga" = "black",
                                "Serie A" = "green4",
                                "Bundesliga" = "red1",
                                "Premier League" = "purple4",
                                "Ligue 1" = "yellow3")) + 
  theme(legend.position = "bottom") 
```

<img src="WhoScoredDataEDA_files/figure-markdown_github/unnamed-chunk-3-2.png" style="display: block; margin: auto;" />
