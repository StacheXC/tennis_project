
library(tidyverse)

# directories ------------------------------------------------------------

pbp_dir <- "tennis_project/data/play_by_play"
trajectory_dir <- "tennis_project/data/ball_trajectory"



# player lookup ----------------------------------------------------------

player_df <- read_csv(
  "tennis_project/data/misc/player_ids.csv"
) %>% 
  select(name, id, player_handedness) %>% 
  filter(!is.na(player_handedness))



# match catalogue --------------------------------------------------------

match_df <- read_csv(
  "tennis_project/data/misc/catalogue_all_matches_available.csv"
)



# compile all matches ----------------------------------------------------

pbp_df <- map_dfr(match_df$match_id, function(match_id) {
  
  message("Processing: ", match_id)
  
  
  # file paths -----------------------------------------------------------
  
  pbp_file <- file.path(
    pbp_dir,
    paste0(match_id, "_pbp.csv")
  )
  
  trajectory_file <- file.path(
    trajectory_dir,
    paste0(match_id, "_ball_trajectory.csv")
  )
  
  
  # skip missing pbp files ----------------------------------------------
  
  if (!file.exists(pbp_file)) {
    
    message("Missing pbp file: ", match_id)
    
    return(NULL)
  }
  
  
  # read pbp -------------------------------------------------------------
  
  pbp <- read_csv(
    pbp_file,
    col_types = cols(
      server_id = col_character(),
      returner_id = col_character(),
      point_winner_id = col_character(),
      player1 = col_character(),
      player2 = col_character()
    ),
    show_col_types = FALSE
  ) %>%
    mutate(
      match_id = match_id
    )
  
  
  # trajectory data ------------------------------------------------------
  
  if (file.exists(trajectory_file)) {
    
    traj <- read_csv(
      trajectory_file,
      show_col_types = FALSE
    )
    
    
    required_cols <- c(
      "strike_index",
      "position"
    )
    
    
    if (all(required_cols %in% names(traj))) {
      
      
      # bounce coordinates ----------------------------------------------
      
      traj_bounce <- traj %>%
        filter(
          strike_index == 1,
          position == "bounce"
        ) %>%
        select(
          point_ID,
          bounce_x = x,
          bounce_y = y,
          bounce_z = z
        )
      
      
      # serve impact coordinates ----------------------------------------
      
      traj_hit <- traj %>%
        filter(
          strike_index == 1,
          position == "hit"
        ) %>%
        select(
          point_ID,
          hit_x = x,
          hit_y = y,
          hit_z = z
        )
      
      
      # overwrite pbp coordinates ---------------------------------------
      
      pbp <- pbp %>%
        
        left_join(
          traj_bounce,
          by = "point_ID"
        ) %>%
        
        left_join(
          traj_hit,
          by = "point_ID"
        ) %>%
        
        mutate(
          
          # bounce coords -----------------------------------------------
          
          x_serve_bounce = coalesce(
            bounce_x,
            x_serve_bounce
          ),
          
          y_serve_bounce = coalesce(
            bounce_y,
            y_serve_bounce
          ),
          
          z_serve_bounce = coalesce(
            bounce_z,
            z_serve_bounce
          ),
          
          
          # serve impact coords -----------------------------------------
          
          x_ball_serve_impact = coalesce(
            hit_x,
            x_ball_serve_impact
          ),
          
          y_ball_serve_impact = coalesce(
            hit_y,
            y_ball_serve_impact
          ),
          
          z_ball_serve_impact = coalesce(
            hit_z,
            z_ball_serve_impact
          )
          
        ) %>%
        
        select(
          -bounce_x,
          -bounce_y,
          -bounce_z,
          -hit_x,
          -hit_y,
          -hit_z
        )
      
    } else {
      
      message("Skipping malformed trajectory file: ", match_id)
      
    }
    
  } else {
    
    message("Missing trajectory file: ", match_id)
    
  }
  
  
  # rotate coordinates AFTER overwrite ----------------------------------
  
  pbp <- pbp %>%
    mutate(
      rotate = x_ball_serve_impact > 0,
      
      x_serve_bounce = ifelse(
        rotate,
        -x_serve_bounce,
        x_serve_bounce
      ),
      
      y_serve_bounce = ifelse(
        rotate,
        -y_serve_bounce,
        y_serve_bounce
      ),
      
      x_ball_serve_impact = ifelse(
        rotate,
        -x_ball_serve_impact,
        x_ball_serve_impact
      ),
      
      y_ball_serve_impact = ifelse(
        rotate,
        -y_ball_serve_impact,
        y_ball_serve_impact
      )
    )
  
  
  # add player names -----------------------------------------------------
  
  pbp %>%
    left_join(
      player_df %>% rename(server_name = name, 
                           server_id = id, 
                           server_handedness = player_handedness),
      by = "server_id"
    ) %>%
    left_join(
      player_df %>% rename(returner_name = name, 
                           returner_id = id, 
                           returner_handedness = player_handedness),
      by = "returner_id"
    ) %>%
    mutate(
      serve_speed_kph = as.numeric(str_extract(serve_speed_kph, "\\d+"))
    ) %>% 
    select(
      -rotate
    )
  
})



# save -------------------------------------------------------------------

saveRDS(pbp_df, "tennis_project/data/pbp_df.rds")


