FIFA Player Value Prediction
================
Shariq Ahmed Siddiqui
2022-01-14

``` r
fifa <- read.csv('FIFA_train.csv')
test <- read.csv("FIFA_test_player_data.csv")
```

``` r
head(fifa,3)
```

    ##   sofifa_id                                                     player_url
    ## 1    216393     https://sofifa.com/player/216393/youri-tielemans/19/159222
    ## 2    187878       https://sofifa.com/player/187878/lukas-marecek/19/159222
    ## 3    178628 https://sofifa.com/player/178628/fernando-forestieri/20/159586
    ##      short_name                  long_name age        dob height_cm weight_kg
    ## 1  Y. Tielemans            Youri Tielemans  21 1997-05-07       176        72
    ## 2    L. Mareček              Lukáš Mareček  28 1990-04-17       183        79
    ## 3 F. Forestieri Fernando Martín Forestieri  29 1990-01-15       172        67
    ##      nationality                club overall potential    value
    ## 1        Belgium           AS Monaco      79        89 20000000
    ## 2 Czech Republic    Sporting Lokeren      71        71  2400000
    ## 3          Italy Sheffield Wednesday      73        73  4000000
    ##   player_positions preferred_foot international_reputation weak_foot
    ## 1          CM, CDM          Right                        2         5
    ## 2     CM, CDM, CAM          Right                        1         3
    ## 3          ST, CAM          Right                        1         3
    ##   skill_moves     work_rate body_type real_face release_clause_eur
    ## 1           4 Medium/Medium    Normal       Yes            4.2e+07
    ## 2           3 Medium/Medium      Lean        No            3.5e+06
    ## 3           4   High/Medium    Normal        No            7.6e+06
    ##         player_tags team_position team_jersey_number loaned_from     joined
    ## 1 #Distance Shooter           RDM                  8             2017-07-01
    ## 2                             LDM                 23             2018-01-29
    ## 3          #Acrobat           SUB                 45             2015-08-29
    ##   contract_valid_until nation_position nation_jersey_number pace shooting
    ## 1                 2022             SUB                   17   53       79
    ## 2                 2021                                   NA   70       57
    ## 3                 2021                                   NA   78       73
    ##   passing dribbling defending physic gk_diving gk_handling gk_kicking
    ## 1      80        77        68     70        NA          NA         NA
    ## 2      70        69        69     71        NA          NA         NA
    ## 3      68        79        48     67        NA          NA         NA
    ##   gk_reflexes gk_speed gk_positioning
    ## 1          NA       NA             NA
    ## 2          NA       NA             NA
    ## 3          NA       NA             NA
    ##                                                                       player_traits
    ## 1 Long Passer (CPU AI Only), Long Shot Taker (CPU AI Only), Playmaker (CPU AI Only)
    ## 2                                           Avoids Using Weaker Foot, Early Crosser
    ## 3                Selfish, Argues with Officials, Crowd Favourite, Skilled Dribbling
    ##   attacking_crossing attacking_finishing attacking_heading_accuracy
    ## 1                 76                  73                         57
    ## 2                 68                  48                         60
    ## 3                 64                  73                         59
    ##   attacking_short_passing attacking_volleys skill_dribbling skill_curve
    ## 1                      80                78              76          84
    ## 2                    72-3                57              67          69
    ## 3                      72                71              78          70
    ##   skill_fk_accuracy skill_long_passing skill_ball_control movement_acceleration
    ## 1                74                 80                 81                    52
    ## 2                68               69-2                 74                    69
    ## 3                65                 60                 77                    82
    ##   movement_sprint_speed movement_agility movement_reactions movement_balance
    ## 1                    53               67                 75               76
    ## 2                    71               69               67-2               65
    ## 3                    74               90                 70               88
    ##   power_shot_power power_jumping power_stamina power_strength power_long_shots
    ## 1               88            65            73           69-2               88
    ## 2               74            68            82             65               63
    ## 3               75            84            68             63               73
    ##   mentality_aggression mentality_interceptions mentality_positioning
    ## 1                   70                      71                    74
    ## 2                 72-4                    70-5                    57
    ## 3                   73                      42                    70
    ##   mentality_vision mentality_penalties mentality_composure defending_marking
    ## 1               83                  76                  81                66
    ## 2               72                  55                68-2                69
    ## 3               73                  67                  71                50
    ##   defending_standing_tackle defending_sliding_tackle goalkeeping_diving
    ## 1                        72                       69                  6
    ## 2                      72-1                     68-2                  8
    ## 3                        53                       31                  6
    ##   goalkeeping_handling goalkeeping_kicking goalkeeping_positioning
    ## 1                    8                  10                      14
    ## 2                    9                  11                      14
    ## 3                    6                   7                      12
    ##   goalkeeping_reflexes   ls   st   rs   lw   lf   cf   rf   rw  lam  cam  ram
    ## 1                   12 73+2 73+2 73+2 74+2 75+2 75+2 75+2 74+2 76+2 76+2 76+2
    ## 2                    9 63+2 63+2 63+2 66+2 65+2 65+2 65+2 66+2 67+2 67+2 67+2
    ## 3                    8 71+2 71+2 71+2 74+2 73+2 73+2 73+2 74+2 73+2 73+2 73+2
    ##     lm  lcm   cm  rcm   rm  lwb  ldm  cdm  rdm  rwb   lb  lcb   cb  rcb   rb
    ## 1 74+2 78+2 78+2 78+2 74+2 71+2 74+2 74+2 74+2 71+2 69+2 69+2 69+2 69+2 69+2
    ## 2 68+2 69+2 69+2 69+2 68+2 71+2 71+2 71+2 71+2 71+2 70+2 68+2 68+2 68+2 70+2
    ## 3 72+2 68+2 68+2 68+2 72+2 60+2 60+2 60+2 60+2 60+2 58+2 56+2 56+2 56+2 58+2

``` r
#Removing unwanted columns
fifa = subset(fifa, select = -c(player_url,long_name,dob,real_face,
                                nation_position))
test = subset(test, select = -c(player_url,long_name,dob,real_face,
                                nation_position))
```

``` r
#parsing names correctly
fifa$short_name = parse_character(fifa$short_name, locale = locale(encoding = 'UTF-8'))
fifa$club = parse_character(fifa$club, locale = locale(encoding = 'UTF-8'))

test$short_name = parse_character(test$short_name, locale = locale(encoding = 'UTF-8'))
test$club = parse_character(test$club, locale = locale(encoding = 'UTF-8'))
```

``` r
#ordering by player rating
fifa = fifa[order(fifa$overall, decreasing = TRUE),]
fifa = fifa[!(is.na(fifa$sofifa_id) == TRUE),]

test = test[order(test$overall, decreasing = TRUE),]
test = test[!(is.na(test$sofifa_id) == TRUE),]
```

``` r
#Correcting body type
unique(fifa$body_type)
```

    ##  [1] "Messi"               "C. Ronaldo"          "Normal"             
    ##  [4] "Neymar"              "Lean"                "Courtois"           
    ##  [7] "PLAYER_BODY_TYPE_25" "Stocky"              "Shaqiri"            
    ## [10] "Akinfenwa"

``` r
unique(test$body_type)
```

    ## [1] "Messi"     "Normal"    "Lean"      "Stocky"    "Courtois"  "Akinfenwa"

``` r
#assigning the correct body type to outliers
fifa$body_type = plyr::mapvalues(fifa$body_type, from=c("Messi","C. Ronaldo","Neymar","Courtois","PLAYER_BODY_TYPE_25","Shaqiri","Akinfenwa"), to=c('Normal', 'Normal', 'Lean', 'Normal', 'Normal', 'Stocky', 'Stocky'))

test$body_type = plyr::mapvalues(test$body_type, from=c("Messi","C. Ronaldo","Neymar","Courtois","PLAYER_BODY_TYPE_25","Shaqiri","Akinfenwa"), to=c('Normal', 'Normal', 'Lean', 'Normal', 'Normal', 'Stocky', 'Stocky'))
```

    ## The following `from` values were not present in `x`: C. Ronaldo, Neymar, PLAYER_BODY_TYPE_25, Shaqiri

``` r
unique(fifa$body_type)
```

    ## [1] "Normal" "Lean"   "Stocky"

``` r
unique(test$body_type)
```

    ## [1] "Normal" "Lean"   "Stocky"

``` r
#Scaling work rate on a numerical scale
unique(fifa$work_rate)
```

    ## [1] "Medium/Medium" "High/Low"      "High/Medium"   "High/High"    
    ## [5] "Medium/High"   "Low/High"      "Medium/Low"    "Low/Medium"   
    ## [9] "Low/Low"

``` r
#mapping values
fifa$work_rate = plyr::mapvalues(fifa$work_rate, from=c("High/High","High/Medium","High/Low","Medium/High","Medium/Medium","Medium/Low","Low/High","Low/Medium","Low/Low"), to=c(9,8,7,6,5,4,3,2,1))

test$work_rate = plyr::mapvalues(test$work_rate, from=c("High/High","High/Medium","High/Low","Medium/High","Medium/Medium","Medium/Low","Low/High","Low/Medium","Low/Low"), to=c(9,8,7,6,5,4,3,2,1))
```

``` r
unique(fifa$work_rate)
```

    ## [1] "5" "7" "8" "9" "6" "3" "4" "2" "1"

``` r
unique(test$work_rate)
```

    ## [1] "4" "5" "9" "8" "3" "7" "6" "2" "1"

``` r
#classifying player positions
unique(fifa$team_position)
```

    ##  [1] "RW"  "LW"  "ST"  "CAM" "GK"  "RCM" "LCB" "LCM" "SUB" "CDM" "RS"  "RCB"
    ## [13] "LS"  "LDM" "RES" "LB"  "RM"  "RDM" "LM"  "CM"  "CF"  "CB"  "RAM" "RF" 
    ## [25] "RB"  "LF"  "LAM" "RWB" ""    "LWB"

``` r
#creating position classes
defence <- c("CB", "RB", "LB", "LWB", "RWB", "LCB", "RCB")
midfielder <- c("CM", "CDM","LM","RM", "LCM", "RCM", "LDM", "RDM")
attack <- c("RW", "LW", "ST", "CF", "RAM", "CAM", "LAM", "RS", "LS", "RF", "LF")  
```

``` r
#players in data set having no on-field position defined 
head(fifa[fifa$team_position ==  "RES" | fifa$team_position ==  "SUB" | fifa$team_position ==  "", 1:20])
```

    ##       sofifa_id     short_name age height_cm weight_kg nationality
    ## 4993     197781           Isco  26       176        79       Spain
    ## 11598      1179      G. Buffon  40       192        92       Italy
    ## 16057     41236 Z. Ibrahimović  35       195        95      Sweden
    ## 12123    175943     D. Mertens  32       169        61     Belgium
    ## 24591    175943     D. Mertens  31       169        61     Belgium
    ## 35523    193041       K. Navas  31       185        80  Costa Rica
    ##                      club overall potential    value player_positions
    ## 4993          Real Madrid      89        92 83000000      CAM, CM, LW
    ## 11598 Paris Saint-Germain      88        88  4000000               GK
    ## 16057   Manchester United      88        88 27000000               ST
    ## 12123              Napoli      87        87 40000000           CF, ST
    ## 24591              Napoli      87        87 45000000           CF, ST
    ## 35523         Real Madrid      87        87 30500000               GK
    ##       preferred_foot international_reputation weak_foot skill_moves work_rate
    ## 4993           Right                        3         3           4         8
    ## 11598          Right                        4         2           1         5
    ## 16057          Right                        5         4           4         4
    ## 12123          Right                        3         4           4         7
    ## 24591          Right                        3         4           4         7
    ## 35523          Right                        3         3           1         5
    ##       body_type release_clause_eur
    ## 4993     Normal          176400000
    ## 11598    Normal            7400000
    ## 16057    Normal           50000000
    ## 12123    Normal           68000000
    ## 24591    Normal           76500000
    ## 35523    Normal           62500000
    ##                                                                                         player_tags
    ## 4993                                                                        #Dribbler, #Playmaker  
    ## 11598                                                                                              
    ## 16057 #Poacher, #Aerial Threat, #Distance Shooter, #Strength, #Clinical Finisher, #Complete Forward
    ## 12123                                    #Dribbler, #Acrobat, #Clinical Finisher, #Complete Forward
    ## 24591                                                       #Dribbler, #Acrobat, #Clinical Finisher
    ## 35523                                                                                              
    ##       team_position
    ## 4993            SUB
    ## 11598           SUB
    ## 16057           RES
    ## 12123           SUB
    ## 24591           SUB
    ## 35523           SUB

``` r
#specifying positions for substitute or reserve players 
for(i in c(1:nrow(fifa))){
  if(fifa$team_position[i] %in% c("SUB", "RES", "")){
    fifa$team_position[i] = sub("\\,.*", "", fifa$player_positions[i])}}
unique(fifa$team_position)
```

    ##  [1] "RW"  "LW"  "ST"  "CAM" "GK"  "RCM" "LCB" "LCM" "CDM" "RS"  "RCB" "LS" 
    ## [13] "LDM" "LB"  "RM"  "RDM" "LM"  "CM"  "CF"  "CB"  "RAM" "RF"  "RB"  "LF" 
    ## [25] "LAM" "RWB" "LWB"

``` r
for(i in c(1:nrow(test))){
  if(test$team_position[i] %in% c("SUB", "RES", "")){
    test$team_position[i] = sub("\\,.*", "", test$player_positions[i])}}
unique(test$team_position)
```

    ##  [1] "RW"  "GK"  "RCM" "LCB" "LCM" "RS"  "ST"  "RM"  "CDM" "CAM" "RCB" "LM" 
    ## [13] "LW"  "RDM" "LB"  "RB"  "CB"  "CF"  "LS"  "CM"  "LDM" "LWB" "LAM" "RWB"
    ## [25] "RAM" "RF"  "LF"

``` r
#assigning players to their respective classes
fifa %<>% mutate(class = if_else(team_position %in% "GK", "Goal Keeper", 
                                if_else(team_position %in% defence, "Defender",
                                       if_else(team_position %in% midfielder, "Midfielder",
                                               if_else(team_position %in% attack, "Forward","NA"))))) 

test %<>% mutate(class = if_else(team_position %in% "GK", "Goal Keeper", 
                                if_else(team_position %in% defence, "Defender",
                                       if_else(team_position %in% midfielder, "Midfielder",
                                               if_else(team_position %in% attack, "Forward","NA")))))          
```

``` r
head(fifa$class)
```

    ## [1] "Forward" "Forward" "Forward" "Forward" "Forward" "Forward"

``` r
head(test$class)
```

    ## [1] "Forward"     "Goal Keeper" "Goal Keeper" "Midfielder"  "Defender"   
    ## [6] "Midfielder"

``` r
fifa = replace(fifa,fifa == 'NA',NA)
fifa = replace(fifa,fifa == '',NA)

test = replace(test,test == 'NA',NA)
test = replace(test,test == '',NA)
```

``` r
#Keeping only the relevant columns
fifa1 = fifa[,c(1:38,99)]
```

``` r
#Keeping only the relevant columns
test1 = test[,c(1:37,98)]
```

``` r
head(fifa1,3)
```

    ##       sofifa_id        short_name age height_cm weight_kg nationality
    ## 29705    158023          L. Messi  31       170        72   Argentina
    ## 30869     20801 Cristiano Ronaldo  32       185        80    Portugal
    ## 31826     20801 Cristiano Ronaldo  33       187        83    Portugal
    ##               club overall potential     value player_positions preferred_foot
    ## 29705 FC Barcelona      94        94 110500000       CF, RW, ST           Left
    ## 30869  Real Madrid      94        94  95500000           LW, ST          Right
    ## 31826     Juventus      94        94  77000000           ST, LW          Right
    ##       international_reputation weak_foot skill_moves work_rate body_type
    ## 29705                        5         4           4         5    Normal
    ## 30869                        5         4           5         7    Normal
    ## 31826                        5         4           5         7    Normal
    ##       release_clause_eur
    ## 29705          226500000
    ## 30869          195800000
    ## 31826          127100000
    ##                                                                                     player_tags
    ## 29705                #Dribbler, #Distance Shooter, #FK Specialist, #Acrobat, #Clinical Finisher
    ## 30869 #Speedster, #Dribbler, #Distance Shooter, #Acrobat, #Clinical Finisher, #Complete Forward
    ## 31826 #Speedster, #Dribbler, #Distance Shooter, #Acrobat, #Clinical Finisher, #Complete Forward
    ##       team_position team_jersey_number loaned_from     joined
    ## 29705            RW                 10        <NA> 2004-07-01
    ## 30869            LW                  7        <NA> 2009-07-01
    ## 31826            LW                  7        <NA> 2018-07-10
    ##       contract_valid_until nation_jersey_number pace shooting passing dribbling
    ## 29705                 2021                   10   88       91      88        96
    ## 30869                 2021                    7   90       93      82        90
    ## 31826                 2022                    7   90       93      81        89
    ##       defending physic gk_diving gk_handling gk_kicking gk_reflexes gk_speed
    ## 29705        32     61        NA          NA         NA          NA       NA
    ## 30869        33     80        NA          NA         NA          NA       NA
    ## 31826        35     79        NA          NA         NA          NA       NA
    ##       gk_positioning
    ## 29705             NA
    ## 30869             NA
    ## 31826             NA
    ##                                                                                                                                      player_traits
    ## 29705 Finesse Shot, Long Shot Taker (CPU AI Only), Speed Dribbler (CPU AI Only), Playmaker (CPU AI Only), One Club Player, Chip Shot (CPU AI Only)
    ## 30869                                            Power Free-Kick, Flair, Long Shot Taker (CPU AI Only), Speed Dribbler (CPU AI Only), Through Ball
    ## 31826                                     Power Free-Kick, Diver, Flair, Long Shot Taker (CPU AI Only), Speed Dribbler (CPU AI Only), Through Ball
    ##         class
    ## 29705 Forward
    ## 30869 Forward
    ## 31826 Forward

``` r
summary(fifa1)
```

    ##    sofifa_id       short_name             age          height_cm    
    ##  Min.   :   164   Length:37548       Min.   :16.00   Min.   :154.0  
    ##  1st Qu.:199511   Class :character   1st Qu.:21.00   1st Qu.:177.0  
    ##  Median :221168   Mode  :character   Median :25.00   Median :181.0  
    ##  Mean   :213684                      Mean   :25.21   Mean   :181.3  
    ##  3rd Qu.:235896                      3rd Qu.:29.00   3rd Qu.:186.0  
    ##  Max.   :252905                      Max.   :47.00   Max.   :205.0  
    ##                                                                     
    ##    weight_kg      nationality            club              overall     
    ##  Min.   : 49.00   Length:37548       Length:37548       Min.   :46.00  
    ##  1st Qu.: 70.00   Class :character   Class :character   1st Qu.:62.00  
    ##  Median : 75.00   Mode  :character   Mode  :character   Median :66.00  
    ##  Mean   : 75.29                                         Mean   :66.29  
    ##  3rd Qu.: 80.00                                         3rd Qu.:71.00  
    ##  Max.   :110.00                                         Max.   :94.00  
    ##                                                                        
    ##    potential         value           player_positions   preferred_foot    
    ##  Min.   :46.00   Min.   :        0   Length:37548       Length:37548      
    ##  1st Qu.:67.00   1st Qu.:   325000   Class :character   Class :character  
    ##  Median :71.00   Median :   700000   Mode  :character   Mode  :character  
    ##  Mean   :71.43   Mean   :  2467933                                        
    ##  3rd Qu.:75.00   3rd Qu.:  2100000                                        
    ##  Max.   :95.00   Max.   :123000000                                        
    ##                                                                           
    ##  international_reputation   weak_foot      skill_moves     work_rate        
    ##  Min.   :1.000            Min.   :1.000   Min.   :1.000   Length:37548      
    ##  1st Qu.:1.000            1st Qu.:3.000   1st Qu.:2.000   Class :character  
    ##  Median :1.000            Median :3.000   Median :2.000   Mode  :character  
    ##  Mean   :1.117            Mean   :2.947   Mean   :2.347                     
    ##  3rd Qu.:1.000            3rd Qu.:3.000   3rd Qu.:3.000                     
    ##  Max.   :5.000            Max.   :5.000   Max.   :5.000                     
    ##                                                                             
    ##   body_type         release_clause_eur  player_tags        team_position     
    ##  Length:37548       Min.   :    13000   Length:37548       Length:37548      
    ##  Class :character   1st Qu.:   538000   Class :character   Class :character  
    ##  Mode  :character   Median :  1200000   Mode  :character   Mode  :character  
    ##                     Mean   :  4695537                                        
    ##                     3rd Qu.:  3700000                                        
    ##                     Max.   :236800000                                        
    ##                     NA's   :3020                                             
    ##  team_jersey_number loaned_from           joined          contract_valid_until
    ##  Min.   : 1.00      Length:37548       Length:37548       Min.   :2017        
    ##  1st Qu.: 8.00      Class :character   Class :character   1st Qu.:2019        
    ##  Median :17.00      Mode  :character   Mode  :character   Median :2020        
    ##  Mean   :19.92                                            Mean   :2020        
    ##  3rd Qu.:27.00                                            3rd Qu.:2021        
    ##  Max.   :99.00                                            Max.   :2026        
    ##  NA's   :502                                              NA's   :502         
    ##  nation_jersey_number      pace          shooting        passing     
    ##  Min.   : 1.00        Min.   :23.00   Min.   :14.00   Min.   :24.00  
    ##  1st Qu.: 6.00        1st Qu.:62.00   1st Qu.:42.00   1st Qu.:50.00  
    ##  Median :12.00        Median :69.00   Median :55.00   Median :58.00  
    ##  Mean   :12.09        Mean   :67.89   Mean   :52.37   Mean   :57.15  
    ##  3rd Qu.:18.00        3rd Qu.:76.00   3rd Qu.:63.00   3rd Qu.:64.00  
    ##  Max.   :87.00        Max.   :96.00   Max.   :93.00   Max.   :92.00  
    ##  NA's   :35147        NA's   :4192    NA's   :4192    NA's   :4192   
    ##    dribbling       defending        physic        gk_diving      gk_handling   
    ##  Min.   :23.00   Min.   :15.0   Min.   :27.00   Min.   :39.00   Min.   :43.00  
    ##  1st Qu.:57.00   1st Qu.:36.0   1st Qu.:59.00   1st Qu.:60.00   1st Qu.:58.00  
    ##  Median :64.00   Median :56.0   Median :66.00   Median :65.00   Median :63.00  
    ##  Mean   :62.35   Mean   :51.3   Mean   :64.95   Mean   :65.34   Mean   :62.95  
    ##  3rd Qu.:69.00   3rd Qu.:65.0   3rd Qu.:72.00   3rd Qu.:70.00   3rd Qu.:68.00  
    ##  Max.   :96.00   Max.   :91.0   Max.   :92.00   Max.   :91.00   Max.   :92.00  
    ##  NA's   :4192    NA's   :4192   NA's   :4192    NA's   :33356   NA's   :33356  
    ##    gk_kicking     gk_reflexes       gk_speed     gk_positioning 
    ##  Min.   :35.00   Min.   :37.00   Min.   :12.0    Min.   :38.00  
    ##  1st Qu.:56.00   1st Qu.:60.00   1st Qu.:30.0    1st Qu.:58.00  
    ##  Median :61.00   Median :66.00   Median :40.0    Median :64.00  
    ##  Mean   :61.62   Mean   :66.28   Mean   :38.6    Mean   :63.19  
    ##  3rd Qu.:66.00   3rd Qu.:72.00   3rd Qu.:46.0    3rd Qu.:69.00  
    ##  Max.   :93.00   Max.   :92.00   Max.   :65.0    Max.   :91.00  
    ##  NA's   :33356   NA's   :33356   NA's   :33356   NA's   :33356  
    ##  player_traits         class          
    ##  Length:37548       Length:37548      
    ##  Class :character   Class :character  
    ##  Mode  :character   Mode  :character  
    ##                                       
    ##                                       
    ##                                       
    ## 

``` r
colnames(fifa1)
```

    ##  [1] "sofifa_id"                "short_name"              
    ##  [3] "age"                      "height_cm"               
    ##  [5] "weight_kg"                "nationality"             
    ##  [7] "club"                     "overall"                 
    ##  [9] "potential"                "value"                   
    ## [11] "player_positions"         "preferred_foot"          
    ## [13] "international_reputation" "weak_foot"               
    ## [15] "skill_moves"              "work_rate"               
    ## [17] "body_type"                "release_clause_eur"      
    ## [19] "player_tags"              "team_position"           
    ## [21] "team_jersey_number"       "loaned_from"             
    ## [23] "joined"                   "contract_valid_until"    
    ## [25] "nation_jersey_number"     "pace"                    
    ## [27] "shooting"                 "passing"                 
    ## [29] "dribbling"                "defending"               
    ## [31] "physic"                   "gk_diving"               
    ## [33] "gk_handling"              "gk_kicking"              
    ## [35] "gk_reflexes"              "gk_speed"                
    ## [37] "gk_positioning"           "player_traits"           
    ## [39] "class"

``` r
colnames(test1)
```

    ##  [1] "sofifa_id"                "short_name"              
    ##  [3] "age"                      "height_cm"               
    ##  [5] "weight_kg"                "nationality"             
    ##  [7] "club"                     "overall"                 
    ##  [9] "potential"                "player_positions"        
    ## [11] "preferred_foot"           "international_reputation"
    ## [13] "weak_foot"                "skill_moves"             
    ## [15] "work_rate"                "body_type"               
    ## [17] "release_clause_eur"       "player_tags"             
    ## [19] "team_position"            "team_jersey_number"      
    ## [21] "loaned_from"              "joined"                  
    ## [23] "contract_valid_until"     "nation_jersey_number"    
    ## [25] "pace"                     "shooting"                
    ## [27] "passing"                  "dribbling"               
    ## [29] "defending"                "physic"                  
    ## [31] "gk_diving"                "gk_handling"             
    ## [33] "gk_kicking"               "gk_reflexes"             
    ## [35] "gk_speed"                 "gk_positioning"          
    ## [37] "player_traits"            "class"

``` r
#factorising columns -> 1,6,7,12,17,20,21,24,25,39
for(i in c(1,6,7,12,17,20,21,24,25,39)) {
  fifa1[,i] = as.factor(fifa1[,i])
}
#factorising columns -> 1,6,7,11,16,19,20,23,24,38
for(i in c(1,6,7,11,16,19,20,23,24,38)) {
  test1[,i] = as.factor(test1[,i])
}
```

``` r
#GKs have values for only GK related variables, therefore separate models will be needed
head(fifa1[fifa1$class== 'Goal Keeper',],3)
```

    ##       sofifa_id short_name age height_cm weight_kg nationality
    ## 3530     200389   J. Oblak  26       188        87    Slovenia
    ## 2460     200389   J. Oblak  25       188        87    Slovenia
    ## 20320    193080     De Gea  26       193        76       Spain
    ##                    club overall potential    value player_positions
    ## 3530    Atlético Madrid      91        93 77500000               GK
    ## 2460    Atlético Madrid      90        93 68000000               GK
    ## 20320 Manchester United      90        92 64500000               GK
    ##       preferred_foot international_reputation weak_foot skill_moves work_rate
    ## 3530           Right                        3         3           1         5
    ## 2460           Right                        3         3           1         5
    ## 20320          Right                        4         3           1         5
    ##       body_type release_clause_eur player_tags team_position team_jersey_number
    ## 3530     Normal          164700000        <NA>            GK                 13
    ## 2460     Normal          144500000        <NA>            GK                 13
    ## 20320      Lean          124200000        <NA>            GK                  1
    ##       loaned_from     joined contract_valid_until nation_jersey_number pace
    ## 3530         <NA> 2014-07-16                 2023                    1   NA
    ## 2460         <NA> 2014-07-17                 2021                    1   NA
    ## 20320        <NA> 2011-07-01                 2019                    1   NA
    ##       shooting passing dribbling defending physic gk_diving gk_handling
    ## 3530        NA      NA        NA        NA     NA        87          92
    ## 2460        NA      NA        NA        NA     NA        86          92
    ## 20320       NA      NA        NA        NA     NA        90          85
    ##       gk_kicking gk_reflexes gk_speed gk_positioning
    ## 3530          78          89       52             90
    ## 2460          78          89       52             88
    ## 20320         87          90       58             86
    ##                                   player_traits       class
    ## 3530                 Flair, Acrobatic Clearance Goal Keeper
    ## 2460  Puncher, GK Long Throw, Comes For Crosses Goal Keeper
    ## 20320            GK Long Throw, Saves with Feet Goal Keeper

``` r
head(test1[test1$class== 'Goal Keeper',],3)
```

    ##       sofifa_id    short_name age height_cm weight_kg nationality
    ## 1        167495      M. Neuer  31       193        92     Germany
    ## 5287     193080        De Gea  27       193        76       Spain
    ## 10574    192448 M. ter Stegen  27       187        85     Germany
    ##                    club overall potential player_positions preferred_foot
    ## 1     FC Bayern München      92        92               GK          Right
    ## 5287  Manchester United      91        93               GK          Right
    ## 10574      FC Barcelona      90        93               GK          Right
    ##       international_reputation weak_foot skill_moves work_rate body_type
    ## 1                            5         4           1         5    Normal
    ## 5287                         4         3           1         5      Lean
    ## 10574                        3         4           1         5    Normal
    ##       release_clause_eur player_tags team_position team_jersey_number
    ## 1              100700000        <NA>            GK                  1
    ## 5287           138600000        <NA>            GK                  1
    ## 10574          143400000        <NA>            GK                  1
    ##       loaned_from     joined contract_valid_until nation_jersey_number pace
    ## 1            <NA> 2011-07-01                 2021                    1   NA
    ## 5287         <NA> 2011-07-01                 2019                    1   NA
    ## 10574        <NA> 2014-07-01                 2022                   22   NA
    ##       shooting passing dribbling defending physic gk_diving gk_handling
    ## 1           NA      NA        NA        NA     NA        91          90
    ## 5287        NA      NA        NA        NA     NA        90          85
    ## 10574       NA      NA        NA        NA     NA        88          85
    ##       gk_kicking gk_reflexes gk_speed gk_positioning
    ## 1             95          89       60             91
    ## 5287          87          94       58             88
    ## 10574         88          90       45             88
    ##                                                           player_traits
    ## 1     GK Long Throw, 1-on-1 Rush, Rushes Out Of Goal, Comes For Crosses
    ## 5287                                     GK Long Throw, Saves with Feet
    ## 10574                    Swerve Pass, Acrobatic Clearance, Flair Passes
    ##             class
    ## 1     Goal Keeper
    ## 5287  Goal Keeper
    ## 10574 Goal Keeper

``` r
#train-test split for gk and non gk
set.seed(100)

nongkdf1 <- fifa1[fifa1$class != 'Goal Keeper', ]
gkdf1 <- fifa1[fifa1$class == 'Goal Keeper', ]

nongktest <- test1[test1$class != 'Goal Keeper', ]
gktest <- test1[test1$class == 'Goal Keeper', ]

#split of 70-30 for both non gk and gk data frames 
trainnongk <- sample(1: nrow(nongkdf1), nrow(nongkdf1) /1.4)
traingk <- sample(1: nrow(gkdf1), nrow(gkdf1) /1.4)
```

``` r
nrow(nongkdf1)
```

    ## [1] 33356

``` r
nrow(gkdf1)
```

    ## [1] 4192

``` r
nrow(nongktest)
```

    ## [1] 14283

``` r
nrow(gktest)
```

    ## [1] 1809

``` r
length(trainnongk)
```

    ## [1] 23825

``` r
#evaluating release clause NAs
nrow(nongkdf1[is.na(nongkdf1$release_clause_eur),])
```

    ## [1] 2760

``` r
hist(nongkdf1[is.na(nongkdf1$release_clause_eur), 'value'])
```

![](FIFA-Player-Value-Prediction_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
summary(nongkdf1[is.na(nongkdf1$release_clause_eur), 'value'])
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##        0   300000   825000  2036040  2100000 57000000

``` r
nongkdf1[is.na(nongkdf1$release_clause_eur),]$release_clause_eur <- nongkdf1[is.na(nongkdf1$release_clause_eur),]$value
```

``` r
summary(gkdf1[is.na(gkdf1$release_clause_eur), 'value'])
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##        0        0   350000  1252096   975000 16000000

``` r
gkdf1[is.na(gkdf1$release_clause_eur),]$release_clause_eur <- gkdf1[is.na(gkdf1$release_clause_eur),]$value
```

``` r
nrow(nongktest[is.na(nongktest$release_clause_eur),])
```

    ## [1] 1146

``` r
hist(nongktest$release_clause_eur)
```

![](FIFA-Player-Value-Prediction_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
summary(nongktest$release_clause_eur)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
    ##     13000    580000   1300000   4658862   3700000 195800000      1146

``` r
nongktest[is.na(nongktest$release_clause_eur),]$release_clause_eur <- exp(1.71 + 0.188*(nongktest[is.na(nongktest$release_clause_eur),]$overall))
```

``` r
nrow(gktest[is.na(gktest$release_clause_eur),])
```

    ## [1] 123

``` r
gktest[is.na(gktest$release_clause_eur),]$release_clause_eur <- exp(2.341 + 0.1739*(gktest[is.na(gktest$release_clause_eur),]$overall))
```

``` r
#model building
```

``` r
#NonGK model
lmnongk.fit <- lm(value~age+height_cm+weight_kg+overall+potential+international_reputation+skill_moves+class+release_clause_eur+pace+shooting+passing+dribbling+defending+physic, data = nongkdf1, subset = trainnongk)

summary(lmnongk.fit)
```

    ## 
    ## Call:
    ## lm(formula = value ~ age + height_cm + weight_kg + overall + 
    ##     potential + international_reputation + skill_moves + class + 
    ##     release_clause_eur + pace + shooting + passing + dribbling + 
    ##     defending + physic, data = nongkdf1, subset = trainnongk)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -8457167  -167610   -15770   112930 18977868 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              -1.397e+06  2.293e+05  -6.090 1.14e-09 ***
    ## age                      -3.277e+04  2.484e+03 -13.189  < 2e-16 ***
    ## height_cm                -2.497e+03  1.281e+03  -1.949  0.05133 .  
    ## weight_kg                 1.095e+03  1.245e+03   0.879  0.37924    
    ## overall                   5.244e+04  2.449e+03  21.411  < 2e-16 ***
    ## potential                -1.987e+04  2.034e+03  -9.768  < 2e-16 ***
    ## international_reputation  6.155e+05  1.741e+04  35.358  < 2e-16 ***
    ## skill_moves               3.351e+04  1.147e+04   2.922  0.00349 ** 
    ## classForward              6.993e+04  2.365e+04   2.956  0.00312 ** 
    ## classMidfielder           4.562e+04  1.723e+04   2.648  0.00810 ** 
    ## release_clause_eur        4.900e-01  7.212e-04 679.503  < 2e-16 ***
    ## pace                      1.460e+03  6.444e+02   2.266  0.02348 *  
    ## shooting                 -6.597e+02  8.187e+02  -0.806  0.42037    
    ## passing                  -1.603e+02  1.142e+03  -0.140  0.88840    
    ## dribbling                -2.099e+03  1.313e+03  -1.598  0.11000    
    ## defending                -2.155e+03  6.740e+02  -3.198  0.00139 ** 
    ## physic                    2.900e+03  8.972e+02   3.232  0.00123 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 767900 on 23808 degrees of freedom
    ## Multiple R-squared:  0.9817, Adjusted R-squared:  0.9817 
    ## F-statistic: 7.981e+04 on 16 and 23808 DF,  p-value: < 2.2e-16

``` r
#GK model
lmgk.fit <- lm(value~age+height_cm+weight_kg+overall+potential+international_reputation+body_type+release_clause_eur+gk_diving+gk_handling+gk_kicking+gk_reflexes+gk_speed+gk_positioning, data = gkdf1, subset = traingk)

summary(lmgk.fit)
```

    ## 
    ## Call:
    ## lm(formula = value ~ age + height_cm + weight_kg + overall + 
    ##     potential + international_reputation + body_type + release_clause_eur + 
    ##     gk_diving + gk_handling + gk_kicking + gk_reflexes + gk_speed + 
    ##     gk_positioning, data = gkdf1, subset = traingk)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -4717639  -107775   -12960    68565  6432174 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              -1.504e+06  4.217e+05  -3.566 0.000368 ***
    ## age                      -1.923e+04  3.536e+03  -5.438 5.83e-08 ***
    ## height_cm                 1.423e+03  2.491e+03   0.571 0.567792    
    ## weight_kg                 2.059e+03  1.953e+03   1.054 0.291796    
    ## overall                   3.022e+04  1.280e+04   2.361 0.018312 *  
    ## potential                -1.006e+04  3.547e+03  -2.836 0.004593 ** 
    ## international_reputation  3.310e+05  3.246e+04  10.197  < 2e-16 ***
    ## body_typeNormal          -1.888e+04  2.351e+04  -0.803 0.421927    
    ## body_typeStocky          -1.998e+04  4.282e+04  -0.467 0.640876    
    ## release_clause_eur        4.907e-01  1.406e-03 348.985  < 2e-16 ***
    ## gk_diving                -4.951e+02  4.100e+03  -0.121 0.903888    
    ## gk_handling               7.460e+02  3.746e+03   0.199 0.842141    
    ## gk_kicking               -9.970e+02  1.936e+03  -0.515 0.606520    
    ## gk_reflexes               5.960e+03  4.088e+03   1.458 0.144973    
    ## gk_speed                 -8.318e+02  9.757e+02  -0.853 0.393999    
    ## gk_positioning           -2.934e+03  3.785e+03  -0.775 0.438421    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 484300 on 2978 degrees of freedom
    ## Multiple R-squared:  0.9891, Adjusted R-squared:  0.989 
    ## F-statistic: 1.796e+04 on 15 and 2978 DF,  p-value: < 2.2e-16

``` r
#NonGK model validation prediction
yhatlmnongk <- predict(lmnongk.fit , newdata = nongkdf1[-trainnongk,])
nongkdf1.testlm <- nongkdf1[-trainnongk,'value']

plot(yhatlmnongk , nongkdf1.testlm)
abline (0, 1)
```

![](FIFA-Player-Value-Prediction_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

``` r
nongkrmselm <- (mean((yhatlmnongk - nongkdf1.testlm)^2, na.rm = TRUE))^0.5
nongkrsslm <- sum((yhatlmnongk - nongkdf1.testlm)^2, na.rm= TRUE)
nongktsslm <- sum((nongkdf1.testlm - mean(nongkdf1.testlm))^2,  na.rm= TRUE)

#model r-squared and root mean squared error
nongkr2lm <- 1-(nongkrsslm/nongktsslm)
nongkr2lm
```

    ## [1] 0.9797071

``` r
nongkrmselm
```

    ## [1] 850527.2

``` r
#GK model validation prediction
yhatlmgk <- predict(lmgk.fit , newdata = gkdf1[-traingk,])
gkdf1.testlm <- gkdf1[-traingk,'value']

plot(yhatlmgk , gkdf1.testlm)
abline (0, 1)
```

![](FIFA-Player-Value-Prediction_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

``` r
gkrmselm <- (mean((yhatlmgk - gkdf1.testlm)^2, na.rm = TRUE))^0.5
gkrsslm <- sum((yhatlmgk - gkdf1.testlm)^2, na.rm= TRUE)
gktsslm <- sum((gkdf1.testlm - mean(gkdf1.testlm))^2,  na.rm= TRUE)


#model r-squared and root mean squared error
gkr2lm <- 1-(gkrsslm/gktsslm)
gkr2lm
```

    ## [1] 0.9803657

``` r
gkrmselm
```

    ## [1] 610416.3

``` r
#prediction on test data

#nongk
nongkyhat <- predict(lmnongk.fit , newdata = nongktest)
hist(nongkyhat)
```

![](FIFA-Player-Value-Prediction_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

``` r
length(nongkyhat)
```

    ## [1] 14283

``` r
nrow(nongktest)
```

    ## [1] 14283

``` r
nongktest$yhat <- nongkyhat
head(nongktest[, c(1:3, 17,39)])
```

    ##       sofifa_id   short_name age release_clause_eur     yhat
    ## 10572    158023     L. Messi  32          195800000 99458226
    ## 10573    192985 K. De Bruyne  28          166500000 84460399
    ## 2        155862 Sergio Ramos  31          106600000 54904092
    ## 3        182521     T. Kroos  27          162000000 82164206
    ## 4        138956 G. Chiellini  32           62700000 33331032
    ## 5289     194765 A. Griezmann  27          165800000 84115794

``` r
#gk
gkyhat <- predict(lmgk.fit , newdata = gktest)
hist(gkyhat)
```

![](FIFA-Player-Value-Prediction_files/figure-gfm/unnamed-chunk-38-2.png)<!-- -->

``` r
length(gkyhat)
```

    ## [1] 1809

``` r
nrow(gktest)
```

    ## [1] 1809

``` r
gktest$yhat <- gkyhat
head(gktest[, c(1:3, 17,39)])
```

    ##       sofifa_id    short_name age release_clause_eur     yhat
    ## 1        167495      M. Neuer  31          100700000 51407645
    ## 5287     193080        De Gea  27          138600000 69740990
    ## 10574    192448 M. ter Stegen  27          143400000 71713130
    ## 5288     192448 M. ter Stegen  26          123300000 61858824
    ## 5296     167948     H. Lloris  31           66600000 34317791
    ## 10577    192119   T. Courtois  27          102000000 51757514

``` r
#function that picks the latest entry for players with more than one entry
latest_entry <- function(df, id){
  latest <- max(df[df$sofifa_id == id,'age'])
  target <- df[df$sofifa_id == id & df$age == latest,]
  return(target)
}
```

``` r
#submission dataset
submission <- read.delim('FIFA_test.csv')
submission <- data.frame(submission)
```

``` r
#objective: finding the most valuable player from each list of 11 player IDs 
submission[1:3,]
```

    ## [1] "[238577, 189730, 212616, 209729, 246565, 233090, 8631, 216047, 196143, 247728, 193092]"  
    ## [2] "[227675, 177793, 199511, 204387, 199964, 244727, 176619, 169181, 207734, 211840, 242197]"
    ## [3] "[199522, 225810, 227740, 243139, 229771, 183475, 198970, 170084, 180403, 240904, 204555]"

``` r
#1462 lists with 11 IDs in each
nrow(submission)
```

    ## [1] 1462

``` r
#converting string of list to list of numeric IDs
submission %<>% mutate(lists = strsplit(submission[,1], ", "))
```

``` r
head(submission[,2])
```

    ## [[1]]
    ##  [1] "[238577" "189730"  "212616"  "209729"  "246565"  "233090"  "8631"   
    ##  [8] "216047"  "196143"  "247728"  "193092]"
    ## 
    ## [[2]]
    ##  [1] "[227675" "177793"  "199511"  "204387"  "199964"  "244727"  "176619" 
    ##  [8] "169181"  "207734"  "211840"  "242197]"
    ## 
    ## [[3]]
    ##  [1] "[199522" "225810"  "227740"  "243139"  "229771"  "183475"  "198970" 
    ##  [8] "170084"  "180403"  "240904"  "204555]"
    ## 
    ## [[4]]
    ##  [1] "[223953" "222399"  "231307"  "196143"  "212814"  "244794"  "208335" 
    ##  [8] "173305"  "160179"  "142721"  "245747]"
    ## 
    ## [[5]]
    ##  [1] "[53110"  "229137"  "213887"  "179891"  "228595"  "211736"  "243305" 
    ##  [8] "228886"  "212000"  "233276"  "244564]"
    ## 
    ## [[6]]
    ##  [1] "[243122" "199890"  "173673"  "236692"  "239028"  "187072"  "201891" 
    ##  [8] "187570"  "209423"  "158133"  "237838]"

``` r
head(submission[,2])
```

    ## [[1]]
    ##  [1] "238577" "189730" "212616" "209729" "246565" "233090" "8631"   "216047"
    ##  [9] "196143" "247728" "193092"
    ## 
    ## [[2]]
    ##  [1] "227675" "177793" "199511" "204387" "199964" "244727" "176619" "169181"
    ##  [9] "207734" "211840" "242197"
    ## 
    ## [[3]]
    ##  [1] "199522" "225810" "227740" "243139" "229771" "183475" "198970" "170084"
    ##  [9] "180403" "240904" "204555"
    ## 
    ## [[4]]
    ##  [1] "223953" "222399" "231307" "196143" "212814" "244794" "208335" "173305"
    ##  [9] "160179" "142721" "245747"
    ## 
    ## [[5]]
    ##  [1] "53110"  "229137" "213887" "179891" "228595" "211736" "243305" "228886"
    ##  [9] "212000" "233276" "244564"
    ## 
    ## [[6]]
    ##  [1] "243122" "199890" "173673" "236692" "239028" "187072" "201891" "187570"
    ##  [9] "209423" "158133" "237838"

``` r
#function which finds the most valuable player from a list of player IDs
maxima <- function(l1){
  x = list()
  #creates a list of IDs as integer-type instead of character
  for (j in c(1:11)){
    x = append(x, as.integer(l1[[1]][[j]]))
  }
  y = list()
  #creates a list of player value predictions for the IDs
  for (i in c(1:11)) {
    #prediction from latest entry is taken from non-gk or gk dataframes  
    if (x[[i]] %in% unique(nongktest$sofifa_id)) {
      target<-latest_entry(nongktest, x[[i]])}
    else {target<-latest_entry(gktest, x[[i]])}
    y = append(y, target$yhat)
  }
  #returns the ID which has the maximum predicted value
  y <- data.frame(y)
  return(x[[match(max(y), table = y)]])
}
```

``` r
submission[1:3,]
```

    ##                                                                                 player_ids
    ## 1   [238577, 189730, 212616, 209729, 246565, 233090, 8631, 216047, 196143, 247728, 193092]
    ## 2 [227675, 177793, 199511, 204387, 199964, 244727, 176619, 169181, 207734, 211840, 242197]
    ## 3 [199522, 225810, 227740, 243139, 229771, 183475, 198970, 170084, 180403, 240904, 204555]
    ##                                                                                    lists
    ## 1   238577, 189730, 212616, 209729, 246565, 233090, 8631, 216047, 196143, 247728, 193092
    ## 2 227675, 177793, 199511, 204387, 199964, 244727, 176619, 169181, 207734, 211840, 242197
    ## 3 199522, 225810, 227740, 243139, 229771, 183475, 198970, 170084, 180403, 240904, 204555
    ##   most_valued_player_id
    ## 1                212616
    ## 2                207734
    ## 3                180403

``` r
#final look at the predicted most valuable player 
submission[1:3,]
```

    ##                                                                                 player_ids
    ## 1   [238577, 189730, 212616, 209729, 246565, 233090, 8631, 216047, 196143, 247728, 193092]
    ## 2 [227675, 177793, 199511, 204387, 199964, 244727, 176619, 169181, 207734, 211840, 242197]
    ## 3 [199522, 225810, 227740, 243139, 229771, 183475, 198970, 170084, 180403, 240904, 204555]
    ##   most_valued_player_id
    ## 1                212616
    ## 2                207734
    ## 3                180403

``` r
write.csv(submission, 'submission.csv' , row.names = FALSE)
```

``` r
#Thank You
```
