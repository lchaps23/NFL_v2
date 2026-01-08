defense_basic_stats <- function(
    player_stats
) {
  
  player_stats |> 
    
    dplyr::group_by(
      season,
      opponent_team
    ) |> 
    
    dplyr::summarise(
      pass_att = sum(attempts),
      pass_cmp = sum(completions),
      cmp_pct  = round(pass_cmp / pass_att *100, 1),
      pass_yds = sum(passing_yards + sack_yards_lost),
      yds_att  = round(pass_yds / pass_att, 1),
      pass_tds = sum(passing_tds),
      
      carries  = sum(carries),
      rush_yds = sum(rushing_yards),
      ypc      = round(rush_yds / carries, 1),
      rush_tds = sum(rushing_tds),
      
      total_yds = pass_yds + rush_yds,
      total_tds = pass_tds + rush_tds,
      first_down = sum(passing_first_downs) + sum(rushing_first_downs),
      
      ints     = sum(passing_interceptions),
      int_pct  = round(ints / pass_att * 100, 1),
      sacks    = sum(sacks_suffered),
      sack_pct = round(sacks / (sacks + pass_att) * 100, 1),
      fumbles_forced  = sum(sack_fumbles) + sum(rushing_fumbles),
      fumbles_won = sum(sack_fumbles_lost) + sum(rushing_fumbles_lost),
      fumble_win_pct = round(fumbles_won / fumbles_forced * 100, 1),
      
      .groups = "drop"
    ) |> 
    
    dplyr::arrange(
      total_yds
    )
}