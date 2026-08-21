data {
  int<lower=0> N_obs;
  int<lower=0> N_cens;
  int<lower=0> N_player;
  int<lower=0> N_serve;
  int<lower=0> N_court;
  array[N_obs] vector[2] z_obs;
  array[N_obs] int<lower=1, upper=N_player> z_ind_player;
  array[N_obs] int<lower=1, upper=N_serve> z_ind_serve;
  array[N_obs] int<lower=1, upper=N_court> z_ind_court;

  array[N_player, N_serve, N_court, 3] real t_upper;
  array[N_cens] int<lower=1, upper=N_player> c_ind_player;
  array[N_cens] int<lower=1, upper=N_serve> c_ind_serve;
  array[N_cens] int<lower=1, upper=N_court> c_ind_court;
}

parameters {
  // Player-level parameters (p is first index)
  array[N_player, N_serve, N_court, 2] real mu_x;
  array[N_player, N_serve, N_court]    real<lower=4.5, upper=6> mu_body_x;
  // mu_y is ordered[2] covering d=1 (lower-y) and d=3 (higher-y) only.
  // d=2 (body) uses mu_body_y_abs with court sign applied in transformed parameters.
  array[N_player, N_serve, N_court] ordered[2]               mu_y;
  array[N_player, N_serve, N_court] real<lower=2, upper=2.2> mu_body_y_abs;
  array[N_player, N_serve, N_court, 2] vector<lower=0>[2] tau;
  array[N_player, N_serve, N_court]    real<lower=0, upper=1>   tau_body_x;
  array[N_player, N_serve, N_court]    real<lower=0, upper=0.3> tau_body_y;
  array[N_player, N_serve, N_court] ordered[3] rho_raw;
  array[N_player, N_serve, N_court, 3] real<lower=0, upper=t_upper> t;
  array[N_player, N_serve, N_court] simplex[3] theta;

  // Population-level hyperparameters (pooling across players)
  array[N_serve, N_court, 2] real mu_g_x;
  array[N_serve, N_court]    real<lower=4.5, upper=6> mu_body_x_g;
  array[N_serve, N_court] ordered[2]               mu_g_y;
  array[N_serve, N_court] real<lower=2, upper=2.2> mu_body_y_abs_g;
  real<lower=0> sig_mu;

  array[N_serve, N_court, 2] vector<lower=0>[2] tau_g;
  array[N_serve, N_court]    real<lower=0, upper=1>   tau_body_x_g;
  array[N_serve, N_court]    real<lower=0, upper=0.3> tau_body_y_g;
  real<lower=0> sig_tau;

  array[N_serve, N_court] ordered[3] rho_g_raw;
  real<lower=0> sig_rho;

  array[N_serve, N_court, 3] real<lower=0> t_g;
  real<lower=0> sig_t;
}

transformed parameters {
  array[N_serve, N_court, 3] real                  rho_g;
  array[N_player, N_serve, N_court, 3] real         rho;
  array[N_player, N_serve, N_court] real           mu_body_y;
  array[N_player, N_serve, N_court, 3] vector[2]   mu;
  array[N_player, N_serve, N_court, 3] matrix[2,2] L;

  for (s in 1:N_serve)
    for (c in 1:N_court)
      for (d in 1:3)
        rho_g[s, c, d] = 2 * inv_logit(rho_g_raw[s, c][d]) - 1;

  for (p in 1:N_player)
    for (s in 1:N_serve)
      for (c in 1:N_court)
        for (d in 1:3)
          rho[p, s, c, d] = 2 * inv_logit(rho_raw[p, s, c][d]) - 1;

  for (p in 1:N_player)
    for (s in 1:N_serve)
      for (c in 1:N_court) {
        // Apply court sign: body y is positive on Deuce (c=1), negative on Ad (c=2)
        mu_body_y[p, s, c] = (c == 1 ? 1.0 : -1.0) * mu_body_y_abs[p, s, c];

        for (d in 1:3) {
          // d=1 and d=3 index into ordered[2] mu_y/tau as positions 1 and 2; d=2 uses body params
          real mu_x_d  = (d == 2) ? mu_body_x[p, s, c]  : mu_x[p, s, c, d == 1 ? 1 : 2];
          real mu_y_d  = (d == 2) ? mu_body_y[p, s, c]  : mu_y[p, s, c][d == 1 ? 1 : 2];
          real tau_x_d = (d == 2) ? tau_body_x[p, s, c] : tau[p, s, c, d == 1 ? 1 : 2][1];
          real tau_y_d = (d == 2) ? tau_body_y[p, s, c] : tau[p, s, c, d == 1 ? 1 : 2][2];
          mu[p, s, c, d] = [mu_x_d, mu_y_d]';

          L[p, s, c, d][1, 1] = tau_x_d;
          L[p, s, c, d][2, 1] = rho[p, s, c, d] * tau_y_d;
          L[p, s, c, d][1, 2] = 0;
          L[p, s, c, d][2, 2] = sqrt(1 - square(rho[p, s, c, d])) * tau_y_d;
        }
      }
}

model {
  // Hyperpriors
  for (s in 1:N_serve) {
    for (c in 1:N_court) {
      mu_g_y[s, c][1]       ~ normal(0, 2.5);
      mu_g_y[s, c][2]       ~ normal(0, 2.5);
      mu_body_y_abs_g[s, c] ~ normal(2.1, 0.05);
      tau_body_x_g[s, c]    ~ normal(0, 2.5);
      tau_body_y_g[s, c]    ~ normal(0, 2.5);
      for (d in 1:2) {
        tau_g[s, c, d][1] ~ normal(0, 2.5);
        tau_g[s, c, d][2] ~ normal(0, 2.5);
      }
      mu_body_x_g[s, c] ~ normal(5.25, 0.5);
      for (d in 1:2) {
        mu_g_x[s, c, d]  ~ normal(6, 1.5);
      }
      for (d in 1:3) {
        rho_g_raw[s, c][d] ~ normal(0, 0.5);
        t_g[s, c, d]     ~ normal(3, 1);
      }
    }
  }
  sig_mu  ~ normal(0, 2.5);
  sig_tau ~ normal(0, 2.5);
  sig_rho ~ normal(0, 1);
  sig_t   ~ normal(0, 1);

  // Player-level priors (drawn from population)
  for (p in 1:N_player) {
    for (s in 1:N_serve) {
      for (c in 1:N_court) {
        mu_y[p, s, c][1]        ~ normal(mu_g_y[s, c][1],        sig_mu);
        mu_y[p, s, c][2]        ~ normal(mu_g_y[s, c][2],        sig_mu);
        mu_body_y_abs[p, s, c]  ~ normal(mu_body_y_abs_g[s, c],  sig_mu);
        tau_body_x[p, s, c]     ~ normal(tau_body_x_g[s, c],     sig_tau);
        tau_body_y[p, s, c]     ~ normal(tau_body_y_g[s, c],     sig_tau);
        for (d in 1:2) {
          tau[p, s, c, d][1] ~ normal(tau_g[s, c, d][1], sig_tau);
          tau[p, s, c, d][2] ~ normal(tau_g[s, c, d][2], sig_tau);
        }
        mu_body_x[p, s, c] ~ normal(mu_body_x_g[s, c], sig_mu);
        for (d in 1:2) {
          mu_x[p, s, c, d]  ~ normal(mu_g_x[s, c, d],  sig_mu);
        }
        for (d in 1:3) {
          rho_raw[p, s, c][d] ~ normal(rho_g_raw[s, c][d], sig_rho);
          t[p, s, c, d]     ~ normal(t_g[s, c, d],     sig_t);
        }
      }
    }
  }

  // Observed likelihood
  for (i in 1:N_obs) {
    int p = z_ind_player[i];
    int s = z_ind_serve[i];
    int c = z_ind_court[i];
    array[3] real lps;
    for (d in 1:3) {
      lps[d] = log(theta[p, s, c][d]) +
        multi_normal_cholesky_lpdf(z_obs[i] | mu[p, s, c, d], L[p, s, c, d]);
    }
    target += log_sum_exp(lps);
  }

  // Censored likelihood
  for (i in 1:N_cens) {
    int p = c_ind_player[i];
    int s = c_ind_serve[i];
    int c = c_ind_court[i];
    real p_cens = 0;
    for (d in 1:3) {
      real mu_x_d  = (d == 2) ? mu_body_x[p, s, c] : mu_x[p, s, c, d == 1 ? 1 : 2];
      real tau_x_d = (d == 2) ? tau_body_x[p, s, c] : tau[p, s, c, d == 1 ? 1 : 2][1];
      p_cens += theta[p, s, c][d] *
        normal_cdf(t[p, s, c, d] | mu_x_d, tau_x_d);
    }
    target += log(p_cens + 1e-12);
  }
}
