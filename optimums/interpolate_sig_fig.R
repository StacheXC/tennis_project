
# Make a visual of interpolating sigma ------------------------------------

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
  # I changed this part
  angle_max <- atan2(court_limit_wide[2] - server_pos[2], court_limit_wide[1] - server_pos[1])
  angle_min <- atan2(court_limit_tee[2] - server_pos[2], court_limit_tee[1] - server_pos[1])
  
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
  t_interp    <- (1 - w2) * t_T    + w2 * t_W
  
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
  angle_max <- atan2(court_limit_tee[2] - server_pos[2], court_limit_tee[1] - server_pos[1])
  angle_min <- atan2(court_limit_wide[2] - server_pos[2], court_limit_wide[1] - server_pos[1])
  
  # 3. Clamp angle_target to valid angular range
  angle_target_clamped <- max(min(angle_target, angle_max), angle_min)
  
  # 5. Compute interpolation weight (may extrapolate)
  w <- (angle_target_clamped - angle_tee) / (angle_wide - angle_tee)
  w2 <- (angle_target - angle_tee) / (angle_wide - angle_tee) 
  
  # 6. Interpolate parameters
  sig_interp  <- (1 - w) * sig_T  + w * sig_W
  corr_interp <- (1 - w) * corr_T + w * corr_W
  t_interp    <- (1 - w2) * t_T    + w2 * t_W
  
  # 7. Clamp correlation to safe range
  corr_interp <- max(min(corr_interp, 0.9), -0.9)
  
  return(list(
    corr = corr_interp,
    sig = sig_interp,
    t = t_interp
  ))
}


# mu_W <- c(5,4)
# mu_T <- c(5,1)
# sig_W <- c(1.7,1.4)
# sig_T <- c(5,1)
# corr_W <- .6
# corr_T <- .1
# t_W <- 3
# t_T <- 2
# x <- 6.41
# y <- -2
# 
# interp <- interpolate_deuce(x, y, mu_W, mu_T, 
#                           sig_W, sig_T, 
#                           corr_W, corr_T,
#                           t_W, t_T)

# interp <- interpolate_ad(x, y, mu_W, mu_T, 
#                             sig_W, sig_T, 
#                             corr_W, corr_T,
#                             t_W, t_T)

# mu_interp <- c(x, y)
# tau_interp <- interp$sig
# rho_interp <- interp$corr

# Create ellipses
# Sigma_wide <- matrix(c(
#   sig_W[1]^2, corr_W * sig_W[1] * sig_W[2],
#   corr_W * sig_W[1] * sig_W[2], sig_W[2]^2
# ), nrow = 2)
# 
# Sigma_t <- matrix(c(
#   sig_T[1]^2, corr_T * sig_T[1] * sig_T[2],
#   corr_T * sig_T[1] * sig_T[2], sig_T[2]^2
# ), nrow = 2)
# 
# Sigma_interp <- matrix(c(
#   tau_interp[1]^2, rho_interp * tau_interp[1] * tau_interp[2],
#   rho_interp * tau_interp[1] * tau_interp[2], tau_interp[2]^2
# ), nrow = 2)
# 
# ellipse_wide <- as.data.frame(ellipse(Sigma_wide, centre = mu_W, level = 0.95, npoints = 200)) %>%
#   mutate(type = "Wide")
# ellipse_t <- as.data.frame(ellipse(Sigma_t, centre = mu_T, level = 0.95, npoints = 200)) %>%
#   mutate(type = "T")
# ellipse_interp <- as.data.frame(ellipse(Sigma_interp, centre = mu_interp, level = 0.95, npoints = 200)) %>%
#   mutate(type = "Interpolated")
# 
# # Combine all ellipses
# ellipse_all <- bind_rows(ellipse_wide, ellipse_t, ellipse_interp)