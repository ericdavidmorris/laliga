WhoScored Data
================
Eric Morris
12/30/2018

La Liga
-------

All data was pulled manually from WhoScored (scraping was difficult due to UI). Statistics were filtered for players with a WhoScored's automatic minimum appearances filter (players with total appearances greater than the average number of appearances in Primera Division) and then furthere filtered to only include players with &gt;500 minutes played. Statistics were pulled on 12/29, during La Liga's Winter Break (all teams have played 17/38 matches except Real Madrid with 16).

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
  labs(title = "La Liga Offensive Leaders at Christmas Break", 
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
