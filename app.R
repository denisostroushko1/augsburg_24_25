
library(shiny)
library(DT)
library(tidyverse)
library(rsconnect)

###########################################################################

# Load in and prepare the data 
data_as_is <- read.csv('scores.csv')

group1 <- c('Vandals', 'Ajax', 'Dunesday', 'Nighthawks', 'Brits')
group2 <- c('Clann', 'Grumpys', 'Swamp Dragons', 'Tsunami', 'Du Nord')

produce_table <- function(data, G){
  left_join(
    x = 
      data %>% 
        group_by(home) %>% 
        summarise(games_home = n(), 
                  wins_h = sum(ifelse(scorehome > scoreaway, 1, 0)),
                  ties_h = sum(ifelse(scorehome == scoreaway, 1, 0)),
                  losses_h = sum(ifelse(scorehome < scoreaway, 1, 0)), 
                  
                  goals_for_home = sum(scorehome), 
                  goals_against_home = sum(scoreaway)
                  ) %>% 
        ungroup() %>% 
        mutate(pts_home = 3 * wins_h + 1 * ties_h) %>% 
        rename(team = home) %>% 
        select(team, games_home, pts_home, goals_for_home, goals_against_home, 
               wins_h, ties_h, losses_h)
    , 
    y = 
      data %>% 
        group_by(away) %>% 
        summarise(games_away = n(), 
                  wins_a = sum(ifelse(scorehome < scoreaway, 1, 0)),
                  ties_a = sum(ifelse(scorehome == scoreaway, 1, 0)),
                  losses_a = sum(ifelse(scorehome > scoreaway, 1, 0)), 
                  
                  goals_for_away = sum(scoreaway ), 
                  goals_against_away = sum(scorehome)
                  ) %>% 
        ungroup() %>% 
        mutate(pts_away = 3 * wins_a + 1 * ties_a) %>% 
        rename(team = away) %>% 
        select(team, games_away, pts_away, goals_for_away, goals_against_away, 
               wins_a, ties_a, losses_a)
    , 
    
    by = 'team'
  ) -> complete_record
  
  cols_with_missing <- apply(complete_record, 2, function(x){length(which(is.na(x)))})
  cols_with_missing <- cols_with_missing[cols_with_missing > 0]
  
  combined_table <- 
    complete_record %>% 
      mutate_at(vars(names(cols_with_missing)), ~ replace_na(., 0)) %>% 
      mutate(
        games = games_home + games_away, 
        wins = wins_h + wins_a, 
        ties = ties_h + ties_a, 
        losses = losses_h + losses_a, 
        total_pts = pts_home + pts_away, 
        goals_for = goals_for_home + goals_for_away, 
        goals_against = goals_against_home + goals_against_away,
        diff = goals_for - goals_against
      ) %>% 
      select(team, wins, ties, losses, total_pts, goals_for, goals_against, diff) %>% 
      mutate(group = ifelse(team %in% group1, 'G1', 'G2') )
  
  ##################
  
  combined_table %>% filter(group == G) %>% 
    select(-group) %>% 
    arrange(-total_pts, -diff) %>% 
    {colnames(.) = c(
      	'Team',	'Wins',	'Ties',	'Losses',	'Total Points',	'Goals For',	'Goals Against',	'Goal Difference'
    ); 
    .
    }
}

###########################################################################
# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "@import url('https://fonts.googleapis.com/css2?family=Maison+Neue:wght@300;400;500&display=swap');
       body { font-family: 'Maison Neue', sans-serif; }
       .dataTables_wrapper .dataTables_filter input { font-family: 'Maison Neue', sans-serif; }
      "
    ))
  ),
  titlePanel("Winter Augsburg Dome Soccer League"),
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Season:",
                  choices  = unique(as.character(data_as_is$season)),
                  selected = unique(as.character(data_as_is$season))[1],
                  ), 
      selectInput("session", "Session:",
                  choices  = unique(as.character(data_as_is$session)),
                  selected = unique(as.character(data_as_is$session))[1]
                  )
    ),
    mainPanel(
      h2(strong("Group 1")),
      DTOutput("table1"),
      br(),
      h2(strong("Group 2")),
      DTOutput("table2")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    data_as_is %>% filter(session == input$session & season == input$season)
  })

  output$table1 <- renderDT({
    datatable(
      produce_table(filtered_data(), G = 'G1'),
       options = list(dom = 't',
                      columnDefs = 
                        list(
                          list(className = 'dt-center', targets = 2:8), 
                          list(className = 'dt-bold', targets = 4)
                             )
                        ) ) %>%
    formatStyle(
      'Total Points', # Column name
      fontWeight = 'bold',
      fontSize = '18px'
    )
      
  })

  output$table2 <- renderDT({
    datatable(
      produce_table(filtered_data(), G = 'G2'),
       options = list(dom = 't',
                      columnDefs = 
                        list(
                          list(className = 'dt-center', targets = 2:8), 
                          list(className = 'dt-bold', targets = 4)
                             )
                        ) ) %>%
    formatStyle(
      'Total Points', # Column name
      fontWeight = 'bold',
      fontSize = '18px'
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
