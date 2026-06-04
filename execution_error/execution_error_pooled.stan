data {
  int<lower=0> N_obs;
  int<lower=0> N_cens;
  int<lower=0> N_player;
  int<lower=0> N_serve;
  int<lower=0> N_court;
  int<lower=0> N_direction;

  array[N_obs] vector[2] z_obs;
  array[N_obs] int<lower=1, upper=N_player> z_ind_player;
  array[N_obs] int<lower=1, upper=N_serve> z_ind_serve;
  array[N_obs] int<lower=1, upper=N_court> z_ind_court;

  array[N_player, N_serve, N_court, N_direction] real t_upper;
  array[N_cens] int<lower=1, upper=N_player> c_ind_player;
  array[N_cens] int<lower=1, upper=N_serve> c_ind_serve;
  array[N_cens] int<lower=1, upper=N_court> c_ind_court;
}

parameters {
  // Player-level parameters (p is first index)
  array[N_player, N_serve, N_court, N_direction] real mu_x;
  array[N_player, N_serve, N_court] ordered[N_direction] mu_y;
  array[N_player, N_serve, N_court, N_direction] vector<lower=0>[2] tau;
  array[N_player, N_serve, N_court, N_direction] real<lower=-1, upper=1> rho;
  array[N_player, N_serve, N_court, N_direction] real<lower=0, upper=t_upper> t;
  array[N_player, N_serve, N_court] simplex[N_direction] theta;

  // Population-level hyperparameters (pooling across players)
  array[N_serve, N_court, N_direction] real mu_g_x;
  array[N_serve, N_court] ordered[N_direction] mu_g_y;
  real<lower=0> sig_mu;

  array[N_serve, N_court, N_direction] vector<lower=0>[2] tau_g;
  real<lower=0> sig_tau;

  array[N_serve, N_court, N_direction] real<lower=-1, upper=1> rho_g;
  real<lower=0> sig_rho;

  array[N_serve, N_court, N_direction] real<lower=0> t_g;
  real<lower=0> sig_t;
}

transformed parameters {
  array[N_player, N_serve, N_court, N_direction] vector[2] mu;
  array[N_player, N_serve, N_court, N_direction] matrix[2, 2] L;

  for (p in 1:N_player) {
    for (s in 1:N_serve) {
      for (c in 1:N_court) {
        for (d in 1:N_direction) {
          mu[p, s, c, d] = [mu_x[p, s, c, d], mu_y[p, s, c][d]]';

          L[p, s, c, d][1, 1] = tau[p, s, c, d][1];
          L[p, s, c, d][2, 1] = rho[p, s, c, d] * tau[p, s, c, d][2];
          L[p, s, c, d][1, 2] = 0;
          L[p, s, c, d][2, 2] = sqrt(1 - square(rho[p, s, c, d])) * tau[p, s, c, d][2];
        }
      }
    }
  }
}

model {
  // Hyperpriors
  for (s in 1:N_serve) {
    for (c in 1:N_court) {
      for (d in 1:N_direction) {
        mu_g_x[s, c, d] ~ normal(0, 2.5);
        mu_g_y[s, c][d] ~ normal(0, 2.5);
        tau_g[s, c, d][1] ~ normal(0, 2.5);
        tau_g[s, c, d][2] ~ normal(0, 2.5);
        rho_g[s, c, d] ~ uniform(-1, 1);
        t_g[s, c, d] ~ normal(3, 1);
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
        for (d in 1:N_direction) {
          mu_x[p, s, c, d]    ~ normal(mu_g_x[s, c, d],    sig_mu);
          mu_y[p, s, c][d]    ~ normal(mu_g_y[s, c][d],    sig_mu);
          tau[p, s, c, d][1]  ~ normal(tau_g[s, c, d][1],  sig_tau);
          tau[p, s, c, d][2]  ~ normal(tau_g[s, c, d][2],  sig_tau);
          rho[p, s, c, d]     ~ normal(rho_g[s, c, d],     sig_rho);
          t[p, s, c, d]       ~ normal(t_g[s, c, d],       sig_t);
        }
      }
    }
  }

  // Observed likelihood
  for (i in 1:N_obs) {
    int p = z_ind_player[i];
    int s = z_ind_serve[i];
    int c = z_ind_court[i];
    array[N_direction] real lps;
    for (d in 1:N_direction) {
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
    for (d in 1:N_direction) {
      p_cens += theta[p, s, c][d] *
        normal_cdf(t[p, s, c, d] | mu_x[p, s, c, d], tau[p, s, c, d][1]);
    }
    target += log(p_cens + 1e-12);
  }
}
