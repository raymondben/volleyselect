
<!-- README.md is generated from README.Rmd. Please edit that file -->

# volleyselect

Simple example of selecting players for teams.

First define our players (could alternatively read in from csv file or
something):

``` r
library(dplyr)
library(knitr)

px <- tribble(~player, ~skill_1, ~position_1, ~position_2,
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

## populate each player's second-choice position (randomly reorder the position_1 column)
px$position_2 <- px$position_1[sample.int(nrow(px), nrow(px))]
## and an associated skill, assume it's slightly less strong than their primary position
px$skill_2 <- round(px$skill_1*0.9-px$skill_1*0.25*runif(nrow(px)))
```

Greedy selection
algorithm:

``` r
max_attempts <- 10 ## sometimes get to a partial solution that can't be completed
for (att in seq_len(max_attempts)) {
    was_ok <- TRUE
    ## data frame to hold teams
    teams <- tibble(team=integer(), player=character(), position=character())
    ## all players start as available
    px$available <- TRUE
    for (round in 1:6) {
        team_order <- sample.int(4,4) ## random order
        for (tm in team_order) {
            ## which players does this team have?
            have_pos <- teams %>% dplyr::filter(team==tm) %>% pull(position)
            ## so which players does it need
            needed_positions <- c(rep("S", 1-sum(have_pos=="S")), rep("OPP", 1-sum(have_pos=="OPP")),
                                 rep("M", 2-sum(have_pos=="M")), rep("OH", 2-sum(have_pos=="OH")))
            ## which players are available for selection for these positions? order by decreasing skill
            available_players <- px %>%
                ## players on basis of their primary position/skill
                dplyr::filter(available & (position_1 %in% needed_positions)) %>% dplyr::select(player, skill=skill_1, position=position_1) %>%
                ## and secondary position/skill
                bind_rows(px %>% dplyr::filter(available & (position_2 %in% needed_positions)) %>% dplyr::select(player, skill=skill_2, position=position_2)) %>%
                arrange(desc(skill))        
            ## choose best available
            chosen <- available_players[1,]
            if (is.na(chosen$player)) {
                ## we can't fill the needed positions
                ## the players that could play the needed positions here have been allocated elsewhere
                ## perhaps on the basis of their second-choice position
                was_ok <- FALSE
                break
            } else {
                ## add to team
                teams <- bind_rows(teams, chosen %>% dplyr::select(player, skill, position) %>% mutate(team=tm))
                ## remove chosen player from "available" pool
                px$available[px$player==chosen$player] <- FALSE
            }
        }
        if (!was_ok) break
    }
    if (was_ok) break ## got a solution, stop trying!
}

if (!was_ok) stop("did not find solution")
```

The result:

``` r
teams %>% dplyr::arrange(team, position) %>% kable
```

| team | player            | position | skill |
| ---: | :---------------- | :------- | ----: |
|    1 | Dane Didball      | M        |    97 |
|    1 | Rene Manshaw      | M        |    48 |
|    1 | Lyn Ferenczy      | OH       |    86 |
|    1 | Avictor McLachlan | OH       |    46 |
|    1 | Aksel Algeo       | OPP      |    29 |
|    1 | Candida Snow      | S        |    14 |
|    2 | Adriano Clerk     | M        |   101 |
|    2 | Meryl Frisel      | M        |    70 |
|    2 | Sheelagh Saltman  | OH       |    41 |
|    2 | Saudra Loncaster  | OH       |    21 |
|    2 | Nancee Allonby    | OPP      |     6 |
|    2 | Myles Evangelinos | S        |    94 |
|    3 | Shaine McCrea     | M        |    33 |
|    3 | Estele Slatcher   | M        |    10 |
|    3 | Syd Saffer        | OH       |   115 |
|    3 | Ginevra Shyres    | OH       |    65 |
|    3 | Jae Scartifield   | OPP      |    87 |
|    3 | Alleyn Ilyas      | S        |    18 |
|    4 | Beau Heims        | M        |    64 |
|    4 | Lorine Cashford   | M        |    42 |
|    4 | Shannen Bladen    | OH       |   101 |
|    4 | Patin Bradford    | OH       |     5 |
|    4 | Elsi Keyser       | OPP      |    84 |
|    4 | Britney Glide     | S        |    16 |

And the team
skills:

``` r
teams %>% group_by(team) %>% dplyr::summarize(skill_mean=mean(skill), skill_sd=sd(skill)) %>% kable
```

| team | skill\_mean | skill\_sd |
| ---: | ----------: | --------: |
|    1 |    53.33333 |  32.23456 |
|    2 |    55.50000 |  39.02179 |
|    3 |    54.66667 |  41.47610 |
|    4 |    52.00000 |  37.87875 |
