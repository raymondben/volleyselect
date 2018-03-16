
<!-- README.md is generated from README.Rmd. Please edit that file -->

# volleyselect

Simple example of selecting players for teams.

First define our players (could alternatively read in from csv file or
something):

``` r
library(dplyr)
library(knitr)

px <- tribble(~player, ~skill, ~position_1, ~position_2,
"Patin Bradford",5,"OH","",
"Avictor McLachlan",46,"OH","",
"Sheelagh Saltman",41,"OH","",
"Lyn Ferenczy",86,"OH","",
"Ginevra Shyres",65,"OH","",
"Saudra Loncaster",21,"OH","",
"Shannen Bladen",101,"OH","",
"Syd Saffer",115,"OH","",
"Candida Snow",14,"S","",
"Alleyn Ilyas",18,"S","",
"Myles Evangelinos",94,"S","",
"Britney Glide",16,"S","",
"Lorine Cashford",42,"M","",
"Rene Manshaw",48,"M","",
"Meryl Frisel",70,"M","",
"Estele Slatcher",10,"M","",
"Beau Heims",64,"M","",
"Adriano Clerk",101,"M","",
"Dane Didball",97,"M","",
"Shaine McCrea",33,"M","",
"Jae Scartifield",87,"OPP","",
"Nancee Allonby",6,"OPP","",
"Aksel Algeo",29,"OPP","",
"Elsi Keyser",84,"OPP","")
## note that position_2 is currently ignored
```

Greedy selection algorithm:

``` r
## data frame to hold teams
teams <- tibble(team=integer(), player=character(), position=character())
px$available <- TRUE ## all players start as available
for (round in 1:6) {
    team_order <- sample.int(4,4) ## random order
    for (tm in team_order) {
        ## which players does this team have?
        have_pos <- teams %>% dplyr::filter(team==tm) %>% pull(position)
        ## so which players does it need
        needed_positions <- c(rep("S", 1-sum(have_pos=="S")), rep("OPP", 1-sum(have_pos=="OPP")),
                             rep("M", 2-sum(have_pos=="M")), rep("OH", 2-sum(have_pos=="OH")))
        ## which players are available for selection for these positions? order by decreasing skill
        available_players <- px %>% dplyr::filter(available & (position_1 %in% needed_positions)) %>%
            arrange(desc(skill))
        ## choose best available
        chosen <- available_players[1,]
        ## add to team
        teams <- bind_rows(teams, chosen %>% dplyr::select(player, skill, position=position_1) %>% mutate(team=tm))
        ## remove chosen player from "available" pool
        px$available[px$player==chosen$player] <- FALSE
    }
}
```

The result:

``` r
teams %>% dplyr::arrange(team, position) %>% kable
```

| team | player            | position | skill |
| ---: | :---------------- | :------- | ----: |
|    1 | Rene Manshaw      | M        |    48 |
|    1 | Estele Slatcher   | M        |    10 |
|    1 | Syd Saffer        | OH       |   115 |
|    1 | Sheelagh Saltman  | OH       |    41 |
|    1 | Elsi Keyser       | OPP      |    84 |
|    1 | Britney Glide     | S        |    16 |
|    2 | Dane Didball      | M        |    97 |
|    2 | Shaine McCrea     | M        |    33 |
|    2 | Ginevra Shyres    | OH       |    65 |
|    2 | Saudra Loncaster  | OH       |    21 |
|    2 | Nancee Allonby    | OPP      |     6 |
|    2 | Myles Evangelinos | S        |    94 |
|    3 | Meryl Frisel      | M        |    70 |
|    3 | Lorine Cashford   | M        |    42 |
|    3 | Shannen Bladen    | OH       |   101 |
|    3 | Lyn Ferenczy      | OH       |    86 |
|    3 | Aksel Algeo       | OPP      |    29 |
|    3 | Candida Snow      | S        |    14 |
|    4 | Adriano Clerk     | M        |   101 |
|    4 | Beau Heims        | M        |    64 |
|    4 | Avictor McLachlan | OH       |    46 |
|    4 | Patin Bradford    | OH       |     5 |
|    4 | Jae Scartifield   | OPP      |    87 |
|    4 | Alleyn Ilyas      | S        |    18 |

And the team
skills:

``` r
teams %>% group_by(team) %>% dplyr::summarize(skill_mean=mean(skill), skill_sd=sd(skill)) %>% kable
```

| team | skill\_mean | skill\_sd |
| ---: | ----------: | --------: |
|    1 |    52.33333 |  40.47057 |
|    2 |    52.66667 |  38.45343 |
|    3 |    57.00000 |  34.07052 |
|    4 |    53.50000 |  37.83517 |
