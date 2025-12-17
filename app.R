library(shiny)
library(bslib)
library(tidyverse)
library(sportyR)


Team_Data <- read_csv("team_info.csv")
Field_Zones <- read_csv("master_field_zone_data.csv") |>
  rename("team" = possession_team)
Close_Pcts <- read_csv("master_close_pct.csv") |>
  rename("team" = defensive_team)

data_list <- list(
  "Field Zones" = Field_Zones,
  "Close Percentages" = Close_Pcts
)

team_abbrs <- Team_Data$team
team_names <- Team_Data$team_name

teams <- setNames(as.list(team_abbrs), team_names)
teams["Full League"] <- "ALL"

teams_flipped <- setNames(as.list(team_names), team_abbrs)
teams_flipped["ALL"] <- "Full League"


coverage_names <- c("All Coverages", "Cover 1", "Cover 2", "Cover 3", "Cover 4", "Cover 6")
coverage_keys <- c("ALL", "COVER_1_MAN", "COVER_2_ZONE", "COVER_3_ZONE", "COVER_4_ZONE", "COVER_6_ZONE")

coverages_flipped <- setNames(as.list(coverage_names), coverage_keys)

coverages <- setNames(as.list(coverage_keys), coverage_names)

field_background <- geom_football(
  league = "nfl",
  display_range = "in_bounds_only",
  x_trans = 60,
  y_trans = 26.6667,
  xlims = c(35, 75),
  rotation = 90
  #color_updates = field_params
)

viz_df <- tibble(
  region = c("Right Flat", "Left Flat", "Right Shallow SHC",
             "Left Shallow SHC", "Shallow Middle",
             "Right Out", "Left Out", "Right SHC", "Left SHC", "Middle Hook",
             "Left Deep Outside", "Right Deep Outside", "Deep Middle"),
  xmin = c(-12, -53.3, -41.3, -21.65, -31.65, 
           -12, -53.3, -41.3, -21.65, -31.65,
           -53.3, -17.8, -35.5),
  xmax = c(0, -41.3, -31.65, -12, -21.65,
           0, -41.3, -31.65, -12, -21.65,
           -35.5, 0,
           -17.8),
  ymin = c(40, 40, 40, 40, 40,
           49, 49, 49, 49, 49,
           59, 59, 59),
  ymax = c(49, 49, 49, 49, 49,
           59, 59, 59, 59, 59,
           75, 75, 75),
  percentages = c(-0.717, -0.717, -0.141, -0.141, -0.187, 
                  -0.208, -0.208, -0.192, -0.192, -0.08,
                  2.28, 2.28, 0.567)
)

ui <- fluidPage(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    
    tags$head(
      tags$style(HTML("
    body {
      background-color: #C0C0C0;
    }

    .custom-main {
      background-color: #C0C0C0;
      padding: 15px;
      border-radius: 8px;
    }
      .custom-sidebar {
      background-color: #132257 !important;
      color: white;
    }

    .custom-sidebar label {
      color: white;
    }

    .custom-sidebar .form-control,
    .custom-sidebar .selectize-input {
      background-color: #0f1c45;
      color: white;
      border: 1px solid #2a3a7a;
    }

    .custom-sidebar .selectize-dropdown {
      background-color: #0f1c45;
      color: white;
    }
    .plot-wrapper {
  position: relative;
  width: 100%;
}

.team-logo {
  position: absolute;
  bottom: 5%;
  left: 37.5%;
  width: 10vw;
  max-width: 160px;
  height: auto;
  opacity: 0.9;
  pointer-events: none;
} @media (max-width: 768px) {
  .team-logo {
    max-width: 50px;
    width: 18vw;
    bottom: 8%;
    left: 6px;
  }
}
    "))
    ),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("team", "Team", choices = teams),
        selectInput("coverage", "Coverage", choices = coverages),
        selectInput("statistic", "Statistic", choices = c("Pair Preference", "Completion %", "Separation Change %")),
        class = "custom-sidebar"
      ),
      
      mainPanel(
        div(
          class = "app-title",
          "Window Shopping Dashboard"
        ),
        div(class = "plot-wrapper",
            plotOutput("field_plot", height = "800px"),
            textOutput("num_reps"),
            uiOutput("team_logo"))
    )
  ))
server <- function(input, output, session) {
  dataset <- reactive({
    if (input$statistic == "Pair Preference") {
      Field_Zones
    } else{
      Close_Pcts
    }
  })
  filtered_dataset <- reactive({
    req(dataset, input$team)
    if (input$team == "ALL"){
      dataset()
    } else{
      dataset() |>
        filter(team == input$team)
    }
  })
  
  num_reps <- reactive({
    req(filtered_dataset(), input$coverage, input$statistic)
    all_data <- filtered_dataset()
    if(input$coverage == "ALL"){
      all_data |>
        filter(ball_field_zone != "Behind_LOS") |>
        nrow()
    } else {
      all_data |>
        filter(ball_field_zone != "Behind_LOS", team_coverage_type == input$coverage) |>
        nrow()
    }
  })
  
  plot_data <- reactive({
    req(filtered_dataset, input$statistic, input$coverage)
    if (input$statistic == "Completion %"){
      if (input$coverage == "ALL"){
        completion_pct_df <- filtered_dataset() |>
          group_by(ball_field_zone) |>
          filter(ball_field_zone != "Behind LOS") |>
          mutate(count = 1, 
                 completion = ifelse(pass_result == "C", 1, 0)) |>
          mutate(ball_field_zone = ifelse(ball_field_zone == "Left Flat" | 
                                            ball_field_zone == "Right Flat",
                                          "Flat", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left Shallow SHC" | 
                                            ball_field_zone == "Right Shallow SHC",
                                          "Shallow SHC", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left Out" | 
                                            ball_field_zone == "Right Out", 
                                          "Out", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left SHC" | 
                                            ball_field_zone == "Right SHC",
                                          "SHC", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left Deep Out" | 
                                            ball_field_zone == "Right Deep Out", 
                                          "Deep Out", ball_field_zone)) |>
          summarize(total_reps = sum(count),
                    completion_pct = mean(completion),
                    avg_close_pct = mean(as.numeric(pct_change))/100,
                    separation_at_throw = mean(dist_at_throw),
                    separation_at_arrival = mean(dist_to_receiver)) |>
          select(c(ball_field_zone, completion_pct)) |>
          pivot_wider(names_from = ball_field_zone, values_from = completion_pct)
        info_vec <- c(coalesce(completion_pct_df$`Flat`[[1]], 0),
                      coalesce(completion_pct_df$`Flat`[[1]], 0),
                      coalesce(completion_pct_df$`Shallow SHC`[[1]], 0),
                      coalesce(completion_pct_df$`Shallow SHC`[[1]], 0),
                      coalesce(completion_pct_df$`Shallow Middle`[[1]], 0),
                      coalesce(completion_pct_df$`Out`[[1]], 0),
                      coalesce(completion_pct_df$`Out`[[1]], 0),
                      coalesce(completion_pct_df$`SHC`[[1]], 0),
                      coalesce(completion_pct_df$`SHC`[[1]], 0),
                      coalesce(completion_pct_df$`Middle Hook`[[1]], 0),
                      coalesce(completion_pct_df$`Deep Out`[[1]], 0),
                      coalesce(completion_pct_df$`Deep Out`[[1]], 0),
                      coalesce(completion_pct_df$`Deep Middle`[[1]], 0))
        final_data <- viz_df
        final_data$percentages <- info_vec
        final_data
      } else {
        completion_pct_df <- filtered_dataset() |>
          group_by(ball_field_zone) |>
          filter(ball_field_zone != "Behind LOS", team_coverage_type == input$coverage) |>
          mutate(count = 1, 
                 completion = ifelse(pass_result == "C", 1, 0)) |>
          mutate(ball_field_zone = ifelse(ball_field_zone == "Left Flat" | 
                                            ball_field_zone == "Right Flat",
                                          "Flat", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left Shallow SHC" | 
                                            ball_field_zone == "Right Shallow SHC",
                                          "Shallow SHC", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left Out" | 
                                            ball_field_zone == "Right Out", 
                                          "Out", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left SHC" | 
                                            ball_field_zone == "Right SHC",
                                          "SHC", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left Deep Out" | 
                                            ball_field_zone == "Right Deep Out", 
                                          "Deep Out", ball_field_zone)) |>
          summarize(total_reps = sum(count),
                    completion_pct = mean(completion),
                    avg_close_pct = mean(as.numeric(pct_change))/100,
                    separation_at_throw = mean(dist_at_throw),
                    separation_at_arrival = mean(dist_to_receiver)) |>
          select(c(ball_field_zone, completion_pct)) |>
          pivot_wider(names_from = ball_field_zone, values_from = completion_pct)
        info_vec <- c(coalesce(completion_pct_df$`Flat`[[1]], 0),
                      coalesce(completion_pct_df$`Flat`[[1]], 0),
                      coalesce(completion_pct_df$`Shallow SHC`[[1]], 0),
                      coalesce(completion_pct_df$`Shallow SHC`[[1]], 0),
                      coalesce(completion_pct_df$`Shallow Middle`[[1]], 0),
                      coalesce(completion_pct_df$`Out`[[1]], 0),
                      coalesce(completion_pct_df$`Out`[[1]], 0),
                      coalesce(completion_pct_df$`SHC`[[1]], 0),
                      coalesce(completion_pct_df$`SHC`[[1]], 0),
                      coalesce(completion_pct_df$`Middle Hook`[[1]], 0),
                      coalesce(completion_pct_df$`Deep Out`[[1]], 0),
                      coalesce(completion_pct_df$`Deep Out`[[1]], 0),
                      coalesce(completion_pct_df$`Deep Middle`[[1]], 0))
        final_data <- viz_df
        final_data$percentages <- info_vec
        final_data
      }
    } else if (input$statistic == "Separation Change %"){
      if (input$coverage == "ALL"){
        completion_pct_df <- filtered_dataset() |>
          group_by(ball_field_zone) |>
          filter(ball_field_zone != "Behind LOS") |>
          mutate(count = 1, 
                 completion = ifelse(pass_result == "C", 1, 0)) |>
          mutate(ball_field_zone = ifelse(ball_field_zone == "Left Flat" | 
                                            ball_field_zone == "Right Flat",
                                          "Flat", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left Shallow SHC" | 
                                            ball_field_zone == "Right Shallow SHC",
                                          "Shallow SHC", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left Out" | 
                                            ball_field_zone == "Right Out", 
                                          "Out", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left SHC" | 
                                            ball_field_zone == "Right SHC",
                                          "SHC", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left Deep Out" | 
                                            ball_field_zone == "Right Deep Out", 
                                          "Deep Out", ball_field_zone)) |>
          summarize(total_reps = sum(count),
                    completion_pct = mean(completion),
                    avg_close_pct = mean(as.numeric(pct_change))/100,
                    separation_at_throw = mean(dist_at_throw),
                    separation_at_arrival = mean(dist_to_receiver)) |>
          select(c(ball_field_zone, avg_close_pct)) |>
          pivot_wider(names_from = ball_field_zone, values_from = avg_close_pct)
        info_vec <- c(coalesce(completion_pct_df$`Flat`[[1]], 0),
                      coalesce(completion_pct_df$`Flat`[[1]], 0),
                      coalesce(completion_pct_df$`Shallow SHC`[[1]], 0),
                      coalesce(completion_pct_df$`Shallow SHC`[[1]], 0),
                      coalesce(completion_pct_df$`Shallow Middle`[[1]], 0),
                      coalesce(completion_pct_df$`Out`[[1]], 0),
                      coalesce(completion_pct_df$`Out`[[1]], 0),
                      coalesce(completion_pct_df$`SHC`[[1]], 0),
                      coalesce(completion_pct_df$`SHC`[[1]], 0),
                      coalesce(completion_pct_df$`Middle Hook`[[1]], 0),
                      coalesce(completion_pct_df$`Deep Out`[[1]], 0),
                      coalesce(completion_pct_df$`Deep Out`[[1]], 0),
                      coalesce(completion_pct_df$`Deep Middle`[[1]], 0))
        final_data <- viz_df
        final_data$percentages <- info_vec
        final_data
      } else {
        completion_pct_df <- filtered_dataset() |>
          group_by(ball_field_zone) |>
          filter(ball_field_zone != "Behind LOS", team_coverage_type == input$coverage) |>
          mutate(count = 1, 
                 completion = ifelse(pass_result == "C", 1, 0)) |>
          mutate(ball_field_zone = ifelse(ball_field_zone == "Left Flat" | 
                                            ball_field_zone == "Right Flat",
                                          "Flat", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left Shallow SHC" | 
                                            ball_field_zone == "Right Shallow SHC",
                                          "Shallow SHC", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left Out" | 
                                            ball_field_zone == "Right Out", 
                                          "Out", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left SHC" | 
                                            ball_field_zone == "Right SHC",
                                          "SHC", ball_field_zone),
                 ball_field_zone = ifelse(ball_field_zone == "Left Deep Out" | 
                                            ball_field_zone == "Right Deep Out", 
                                          "Deep Out", ball_field_zone)) |>
          summarize(total_reps = sum(count),
                    completion_pct = mean(completion),
                    avg_close_pct = mean(as.numeric(pct_change))/100,
                    separation_at_throw = mean(dist_at_throw),
                    separation_at_arrival = mean(dist_to_receiver)) |>
          select(c(ball_field_zone, avg_close_pct)) |>
          pivot_wider(names_from = ball_field_zone, values_from = avg_close_pct)
        info_vec <- c(coalesce(completion_pct_df$`Flat`[[1]], 0),
                      coalesce(completion_pct_df$`Flat`[[1]], 0),
                      coalesce(completion_pct_df$`Shallow SHC`[[1]], 0),
                      coalesce(completion_pct_df$`Shallow SHC`[[1]], 0),
                      coalesce(completion_pct_df$`Shallow Middle`[[1]], 0),
                      coalesce(completion_pct_df$`Out`[[1]], 0),
                      coalesce(completion_pct_df$`Out`[[1]], 0),
                      coalesce(completion_pct_df$`SHC`[[1]], 0),
                      coalesce(completion_pct_df$`SHC`[[1]], 0),
                      coalesce(completion_pct_df$`Middle Hook`[[1]], 0),
                      coalesce(completion_pct_df$`Deep Out`[[1]], 0),
                      coalesce(completion_pct_df$`Deep Out`[[1]], 0),
                      coalesce(completion_pct_df$`Deep Middle`[[1]], 0))
        final_data <- viz_df
        final_data$percentages <- info_vec
        final_data
      }
    } else if (input$statistic == "Pair Preference"){
      if (input$coverage == "ALL"){
        final_data <- viz_df
        final_data$percentages <- c(0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0,
                                    0, 0, 0)
        final_data
      } else{
        coverage_df <- filtered_dataset() |>
          mutate(count = 1) |>
          filter(team_coverage_type == input$coverage,
                 ball_field_zone != "Behind LOS") |>
          group_by(ball_field_zone) |>
          summarise(LF_PP = sum(LF),
                    LSSHC_PP = sum(LSSHC),
                    SM_PP = sum(SM),
                    RSSHC_PP = sum(RSSHC),
                    RF_PP = sum(RF),
                    LO_PP = sum(LO),
                    LSHC_PP = sum(LSHC),
                    MH_PP = sum(MH),
                    RSHC_PP = sum(RSHC),
                    RO_PP = sum(RO),
                    LDO_PP = sum(LDO),
                    DM_PP = sum(DM),
                    RDO_PP = sum(RDO),
                    total_reps = sum(count)) |>
          mutate(Flat = case_when(grepl("Left", ball_field_zone)~LF_PP,
                                  grepl("Right", ball_field_zone)~RF_PP,
                                  grepl("Middle", ball_field_zone)~RF_PP+LF_PP),
                 Shallow_SHC = case_when(grepl("Left", ball_field_zone)~LSSHC_PP,
                                         grepl("Right", ball_field_zone)~RSSHC_PP,
                                         grepl("Middle", ball_field_zone)~RSSHC_PP+LSSHC_PP),
                 Out = case_when(grepl("Left", ball_field_zone)~LO_PP,
                                 grepl("Right", ball_field_zone)~RO_PP,
                                 grepl("Middle", ball_field_zone)~RO_PP+LO_PP),
                 SHC = case_when(grepl("Left", ball_field_zone)~LSHC_PP,
                                 grepl("Right", ball_field_zone)~RSHC_PP,
                                 grepl("Middle", ball_field_zone)~RSHC_PP+LSHC_PP),
                 Deep_Out = case_when(grepl("Left", ball_field_zone)~LDO_PP,
                                      grepl("Right", ball_field_zone)~RDO_PP,
                                      grepl("Middle", ball_field_zone)~RDO_PP+LDO_PP)) |>
          rename("Deep_Middle" = DM_PP, "Middle_Hook" = MH_PP, "Shallow_Middle" = SM_PP) |>
          select(c(ball_field_zone, Flat, Shallow_SHC, Shallow_Middle, 
                   Out, SHC, Middle_Hook,
                   Deep_Out, Deep_Middle, total_reps)) |>
          mutate(ball_field_zone = case_when(grepl("Flat", ball_field_zone)~"Flat",
                                             grepl("Shallow SHC", ball_field_zone)~"Shallow SHC",
                                             grepl("Shallow Middle", ball_field_zone)~"Shallow Middle",
                                             grepl("t SHC", ball_field_zone)~"SHC",
                                             grepl("t Out", ball_field_zone)~"Out",
                                             grepl("Hook", ball_field_zone)~"Middle Hook",
                                             grepl("Deep Out", ball_field_zone)~"Deep Out",
                                             grepl("Deep Middle", ball_field_zone)~"Deep Middle")) |>
          group_by(ball_field_zone) |>
          summarize(Flat = sum(Flat),
                    Shallow_SHC = sum(Shallow_SHC),
                    Shallow_Middle = sum(Shallow_Middle),
                    Out = sum(Out),
                    SHC = sum(SHC),
                    Middle_Hook = sum(Middle_Hook),
                    Deep_Out = sum(Deep_Out),
                    Deep_Middle = sum(Deep_Middle),
                    total_reps = sum(total_reps)) |>
          mutate(Coverage = "Coverage") |>
          group_by(Coverage) |>
          summarize(Flat = sum(Flat),
                    Shallow_SHC = sum(Shallow_SHC),
                    Shallow_Middle = sum(Shallow_Middle),
                    Out = sum(Out),
                    SHC = sum(SHC),
                    Middle_Hook = sum(Middle_Hook),
                    Deep_Out = sum(Deep_Out),
                    Deep_Middle = sum(Deep_Middle),
                    total_recievers = (Flat + Shallow_SHC + Shallow_Middle + 
                                         Out + SHC + Middle_Hook + Deep_Out + Deep_Middle))
        cov_total_receivers <- coverage_df$total_recievers[[1]]
        
        all_df <- filtered_dataset() |>
          mutate(count = 1) |>
          filter(ball_field_zone != "Behind LOS") |>
          group_by(ball_field_zone) |>
          summarise(LF_PP = sum(LF),
                    LSSHC_PP = sum(LSSHC),
                    SM_PP = sum(SM),
                    RSSHC_PP = sum(RSSHC),
                    RF_PP = sum(RF),
                    LO_PP = sum(LO),
                    LSHC_PP = sum(LSHC),
                    MH_PP = sum(MH),
                    RSHC_PP = sum(RSHC),
                    RO_PP = sum(RO),
                    LDO_PP = sum(LDO),
                    DM_PP = sum(DM),
                    RDO_PP = sum(RDO),
                    total_reps = sum(count)) |>
          mutate(Flat = case_when(grepl("Left", ball_field_zone)~LF_PP,
                                  grepl("Right", ball_field_zone)~RF_PP,
                                  grepl("Middle", ball_field_zone)~RF_PP+LF_PP),
                 Shallow_SHC = case_when(grepl("Left", ball_field_zone)~LSSHC_PP,
                                         grepl("Right", ball_field_zone)~RSSHC_PP,
                                         grepl("Middle", ball_field_zone)~RSSHC_PP+LSSHC_PP),
                 Out = case_when(grepl("Left", ball_field_zone)~LO_PP,
                                 grepl("Right", ball_field_zone)~RO_PP,
                                 grepl("Middle", ball_field_zone)~RO_PP+LO_PP),
                 SHC = case_when(grepl("Left", ball_field_zone)~LSHC_PP,
                                 grepl("Right", ball_field_zone)~RSHC_PP,
                                 grepl("Middle", ball_field_zone)~RSHC_PP+LSHC_PP),
                 Deep_Out = case_when(grepl("Left", ball_field_zone)~LDO_PP,
                                      grepl("Right", ball_field_zone)~RDO_PP,
                                      grepl("Middle", ball_field_zone)~RDO_PP+LDO_PP)) |>
          rename("Deep_Middle" = DM_PP, "Middle_Hook" = MH_PP, "Shallow_Middle" = SM_PP) |>
          select(c(ball_field_zone, Flat, Shallow_SHC, Shallow_Middle, 
                   Out, SHC, Middle_Hook,
                   Deep_Out, Deep_Middle, total_reps)) |>
          mutate(ball_field_zone = case_when(grepl("Flat", ball_field_zone)~"Flat",
                                             grepl("Shallow SHC", ball_field_zone)~"Shallow SHC",
                                             grepl("Shallow Middle", ball_field_zone)~"Shallow Middle",
                                             grepl("t SHC", ball_field_zone)~"SHC",
                                             grepl("t Out", ball_field_zone)~"Out",
                                             grepl("Hook", ball_field_zone)~"Middle Hook",
                                             grepl("Deep Out", ball_field_zone)~"Deep Out",
                                             grepl("Deep Middle", ball_field_zone)~"Deep Middle")) |>
          group_by(ball_field_zone) |>
          summarize(Flat = sum(Flat),
                    Shallow_SHC = sum(Shallow_SHC),
                    Shallow_Middle = sum(Shallow_Middle),
                    Out = sum(Out),
                    SHC = sum(SHC),
                    Middle_Hook = sum(Middle_Hook),
                    Deep_Out = sum(Deep_Out),
                    Deep_Middle = sum(Deep_Middle),
                    total_reps = sum(total_reps)) |>
          mutate(Coverage = "All") |>
          group_by(Coverage) |>
          summarize(Flat = sum(Flat),
                    Shallow_SHC = sum(Shallow_SHC),
                    Shallow_Middle = sum(Shallow_Middle),
                    Out = sum(Out),
                    SHC = sum(SHC),
                    Middle_Hook = sum(Middle_Hook),
                    Deep_Out = sum(Deep_Out),
                    Deep_Middle = sum(Deep_Middle),
                    total_recievers = (Flat + Shallow_SHC + Shallow_Middle + 
                                         Out + SHC + Middle_Hook + Deep_Out + Deep_Middle)) |>
          mutate(Flat = Flat/total_recievers,
                 Shallow_SHC = Shallow_SHC/total_recievers,
                 Shallow_Middle = Shallow_Middle/total_recievers,
                 Out = Out/total_recievers,
                 SHC = SHC/total_recievers,
                 Middle_Hook = Middle_Hook/total_recievers,
                 Deep_Out = Deep_Out/total_recievers,
                 Deep_Middle = Deep_Middle/total_recievers,
                 total_recievers = cov_total_receivers) |>
          mutate(Flat = Flat*total_recievers,
                 Shallow_SHC = Shallow_SHC*total_recievers,
                 Shallow_Middle = Shallow_Middle*total_recievers,
                 Out = Out*total_recievers,
                 SHC = SHC*total_recievers,
                 Middle_Hook = Middle_Hook*total_recievers,
                 Deep_Out = Deep_Out*total_recievers,
                 Deep_Middle = Deep_Middle*total_recievers)
        observed <- c(coverage_df$Flat[[1]], coverage_df$Flat[[1]],
                      coverage_df$Shallow_SHC[[1]], coverage_df$Shallow_SHC[[1]],
                      coverage_df$Shallow_Middle[[1]],
                      coverage_df$Out[[1]], coverage_df$Out[[1]],
                      coverage_df$SHC[[1]], coverage_df$SHC[[1]],
                      coverage_df$Middle_Hook[[1]],
                      coverage_df$Deep_Out[[1]], coverage_df$Deep_Out[[1]],
                      coverage_df$Deep_Middle[[1]])
        expected <- c(all_df$Flat[[1]], all_df$Flat[[1]],
                      all_df$Shallow_SHC[[1]], all_df$Shallow_SHC[[1]],
                      all_df$Shallow_Middle[[1]],
                      all_df$Out[[1]], all_df$Out[[1]],
                      all_df$SHC[[1]], all_df$SHC[[1]],
                      all_df$Middle_Hook[[1]],
                      all_df$Deep_Out[[1]], all_df$Deep_Out[[1]],
                      all_df$Deep_Middle[[1]])
        sqrt_exp <- sqrt(expected)
        
        pears <- ((observed - expected)/sqrt_exp)
        final_data <- viz_df
        final_data$percentages <- pears
        final_data
      }
    }
  })
  
  plot_output <- reactive({
    req(plot_data())
    graphing_data <- plot_data()
    plot_width <- session$clientData$output_field_plot_width
    title_size <- ifelse(plot_width < 600, 6, 23)
    subtitle_size <- ifelse(plot_width < 600, 5, 18)
    label_size <- ifelse(plot_width < 600, 5, 12)
    coverage_choice <- coverages_flipped[[input$coverage]]
    team_choice <- teams_flipped[[input$team]]
    if (input$statistic == "Completion %"){
      field_background +
        geom_rect(
          data = graphing_data,
          aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax,
            fill = percentages
          ),
          alpha = 0.9,
          color = "black"
        ) +
        scale_fill_gradient(
          low = "blue",
          high = "orange",
          name = "Completion %",
          labels = scales::percent,
          limits = c(0, 1),
          oob = scales::squish)+
        geom_text(
          data = graphing_data |> mutate(ifelse(is.na(percentages), 0, percentages)),
          aes(
            x = (xmin + xmax) / 2,
            y = (ymin + ymax) / 2,
            label = scales::percent(round(percentages, 2))
          ),
          angle = 0,
          fontface = "bold",
          color = "white",
          size = label_size)+
        geom_segment(aes(x = -53.3, y = 40, xend = 0, yend = 40), color = "blue", linewidth = 1.2)+
        labs(title = sprintf("% s Completion Percentage Allowed", 
                             team_choice),
             subtitle = sprintf("% s Reps", coverage_choice))+
        theme(plot.title = element_text(hjust = 0.5, size =title_size),
              plot.subtitle = element_text(hjust = 0.5, size = subtitle_size),
              legend.text = element_text(size = rel(0.5)))
    } else if (input$statistic == "Separation Change %"){
      field_background +
        geom_rect(
          data = graphing_data,
          aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax,
            fill = percentages
          ),
          alpha = 0.9,
          color = "black"
        ) +
        scale_fill_gradient(
          low = "blue",
          high = "orange",
          name = "Separation Change %",
          labels = scales::percent,
          limits = c(-0.5, 0.75),
          oob = scales::squish)+
        geom_text(
          data = graphing_data |> mutate(ifelse(is.na(percentages), 0, percentages)),
          aes(
            x = (xmin + xmax) / 2,
            y = (ymin + ymax) / 2,
            label = scales::percent(round(percentages, 2))
          ),
          angle = 0,
          fontface = "bold",
          color = "white",
          size = label_size)+
        geom_segment(aes(x = -53.3, y = 40, xend = 0, yend = 40), color = "blue", linewidth = 1.2)+
        labs(title = sprintf("% s Separation Change Percent", 
                             team_choice),
             subtitle = sprintf("% s Reps", coverage_choice))+
        theme(plot.title = element_text(hjust = 0.5, size = subtitle_size),
              plot.subtitle = element_text(hjust = 0.5, size = subtitle_size),
              legend.text = element_text(size = rel(0.5)))
    } else{
      field_background +
        geom_rect(
          data = graphing_data,
          aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax,
            fill = percentages
          ),
          alpha = 0.9,
          color = "black"
        ) +
        scale_fill_gradient2(
          low = "blue",
          mid = "white",
          high = "orange",
          midpoint = 0,
          name = "Pair Preference",
          limits = c(-4, 4),
          oob = scales::squish)+
        geom_text(
          data = graphing_data,
          aes(
            x = (xmin + xmax) / 2,
            y = (ymin + ymax) / 2,
            label = round(percentages, 2)
          ),
          angle = 0,
          fontface = "bold",
          color = "black",
          size = label_size)+
        geom_segment(aes(x = -53.3, y = 40, xend = 0, yend = 40), color = "blue", linewidth = 1.2)+
        labs(title = sprintf("Pair Preferences for % s Offense Non-Targeted Routes", team_choice),
             subtitle = sprintf("% s Reps vs. All Reps", coverage_choice))+
        theme(plot.title = element_text(hjust = 0.5, size = title_size),
              plot.subtitle = element_text(hjust = 0.5, size = subtitle_size))
    }
  })
   
   output$summary <- renderPrint({
     input$team
   })
  output$field_plot <- renderPlot({
    plot_output()
  })
  
  output$num_reps <- renderText({
    req(num_reps)
    paste0(
      "Total Reps: ", num_reps()
    )
    
    })
  output$team_logo <- renderUI({
    req(input$team)
    
    # Handle "ALL" case if needed
    if (input$team == "ALL"){
      if (input$team == "ALL") return(NULL)
    } else{
      logo_url <- paste0("https://raw.githubusercontent.com/nflverse/nflverse-pbp/master/wordmarks/", input$team, ".png")
    }
    
    
    # Return the img tag
    tags$img(src = logo_url, class = "team-logo")
  })
  
}
shinyApp(ui, server)