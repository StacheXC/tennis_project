
geom_halfcourt <- function()  {
  court_dat <- data.frame(
    x = c(0, 0, 11.887, 0, 0, 0, 0, 6.4),
    xend = c(11.887, 0, 11.887, 11.887, 11.887, 11.887, 6.4, 6.4),
    y = c(5.486, 5.486, 5.486, -5.486, 4.115, -4.115, 0, 4.115),
    yend = c(5.486, -5.486, -5.486, -5.486, 4.115, -4.115, 0, -4.115)
  )
  geom_segment(aes(x = x,
                   xend = xend,
                   y = y,
                   yend = yend),
               data = court_dat,
               color = "gray40")
}

# currently, court limits are not used
# in the original, t_interp used w2 instead of w
interpolate_deuce <- function(x, y,
                              mu_W, mu_T,
                              sig_W, sig_T,
                              corr_W, corr_T,
                              t_W, t_T,
                              server_pos = c(-11.9, 0),
                              court_limit_wide = c(3, 4.11),
                              court_limit_tee  = c(6.41, 0)) {
  
  # 1. Compute angles relative to server
  angle_target <- atan2(y - server_pos[2], x - server_pos[1])
  angle_wide   <- atan2(mu_W[2] - server_pos[2], mu_W[1] - server_pos[1])
  angle_tee    <- atan2(mu_T[2] - server_pos[2], mu_T[1] - server_pos[1])
  
  # 2. Define allowable aiming span (court limits)
  angle_max <- atan2(mu_W[2] - server_pos[2], mu_W[1] - server_pos[1])
  angle_min <- atan2(mu_T[2] - server_pos[2], mu_T[1] - server_pos[1])
  
  # 3. Clamp angle_target to valid angular range
  angle_target_clamped <- min(max(angle_target, angle_min), angle_max)
  
  # 4. Compute weight based on target position between T and W
  # Assume T is always smaller angle than W (true for deuce court)
  w <- (angle_target_clamped - angle_tee) / (angle_wide - angle_tee)
  w2 <- (angle_target - angle_tee) / (angle_wide - angle_tee) 
  #w <- min(max(w, 0), 1)  # extra safety clamp to [0, 1] - Don't want this
  
  # 5. Linearly interpolate all parameters
  sig_interp  <- (1 - w) * sig_T  + w * sig_W
  corr_interp <- (1 - w) * corr_T + w * corr_W
  t_interp    <- (1 - w) * t_T    + w * t_W
  
  # 6. Clamp correlation to [-.9, .9]
  corr_interp <- max(min(corr_interp, .9), -.9)
  
  return(list(
    corr = corr_interp,
    sig = sig_interp,
    t = t_interp
  ))
}

interpolate_ad <- function(x, y,
                           mu_W, mu_T,
                           sig_W, sig_T,
                           corr_W, corr_T,
                           t_W, t_T,
                           server_pos = c(-11.9, 0),
                           court_limit_wide = c(3, -4.11),
                           court_limit_tee  = c(6.41, 0)) {
  
  # 1. Compute angles relative to server
  angle_target <- atan2(y - server_pos[2], x - server_pos[1])
  angle_wide   <- atan2(mu_W[2] - server_pos[2], mu_W[1] - server_pos[1])
  angle_tee    <- atan2(mu_T[2] - server_pos[2], mu_T[1] - server_pos[1])
  
  # 2. Define allowable aiming span (court limits)
  angle_max <- atan2(mu_T[2] - server_pos[2], mu_T[1] - server_pos[1])
  angle_min <- atan2(mu_W[2] - server_pos[2], mu_W[1] - server_pos[1])
  
  # 3. Clamp angle_target to valid angular range
  angle_target_clamped <- max(min(angle_target, angle_max), angle_min)
  
  # 5. Compute interpolation weight (may extrapolate)
  w <- (angle_target_clamped - angle_tee) / (angle_wide - angle_tee)
  w2 <- (angle_target - angle_tee) / (angle_wide - angle_tee) 
  
  # 6. Interpolate parameters
  sig_interp  <- (1 - w) * sig_T  + w * sig_W
  corr_interp <- (1 - w) * corr_T + w * corr_W
  t_interp    <- (1 - w) * t_T    + w * t_W
  
  # 7. Clamp correlation to safe range
  corr_interp <- max(min(corr_interp, 0.9), -0.9)
  
  return(list(
    corr = corr_interp,
    sig = sig_interp,
    t = t_interp
  ))
}

find_local_max_single <- function(data, grid_res = 0.1) {
  offsets <- list(
    c(-1, -1), c(-1,  0), c(-1,  1),
    c( 0, -1),             c( 0,  1),
    c( 1, -1), c( 1,  0), c( 1,  1)
  )
  lookup <- setNames(
    data$ev_hat,
    paste(round(data$x_serve_bounce, 1), round(data$y_serve_bounce, 1))
  )
  is_max <- rep(TRUE, nrow(data))
  for (off in offsets) {
    nbr_key <- paste(round(data$x_serve_bounce + off[1] * grid_res, 1),
                     round(data$y_serve_bounce + off[2] * grid_res, 1))
    nbr_ev  <- lookup[nbr_key]
    nbr_ev[is.na(nbr_ev)] <- -Inf
    is_max  <- is_max & (data$ev_hat > nbr_ev)
  }
  data[is_max, ]
}

players <- c("N.DJOKOVIC", "R.NADAL", "C.ALCARAZ", "J.SINNER", "D.MEDVEDEV",
             "A.ZVEREV", "J.ISNER", "R.FEDERER", "A.RUBLEV",
             "A.BARTY", "S.WILLIAMS", "A.SABALENKA", "N.OSAKA", "S.KENIN",
             "I.SWIATEK", "C.GAUFF", "E.SVITOLINA")
