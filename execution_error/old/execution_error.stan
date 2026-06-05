data {
  int<lower=0> N_obs;
  array[N_obs] vector[2] z_obs;
  array[N_obs] int<lower=1, upper=2> z_ind_serve;
  array[N_obs] int<lower=1, upper=2> z_ind_court;
  
  int<lower=0> N_cens;
  array[2, 2, 2] real t_upper;
  array[N_cens] int<lower=1, upper=2> c_ind_serve;
  array[N_cens] int<lower=1, upper=2> c_ind_court;
}

transformed data {
  int N_serve = 2;
  int N_court = 2;
  int N_direction = 2;
}

parameters {
  array[N_serve, N_court, N_direction] real mu_x;
  array[N_serve, N_court] ordered[N_direction] mu_y;
  array[N_court, N_direction] real mu_g_x;
  array[N_court] ordered[N_direction] mu_g_y;
  real<lower=0> sig_mu;

  array[N_serve, N_court, N_direction] vector<lower=0>[2] tau;
  vector<lower=0>[2] tau_g;
  real<lower=0> sig_tau;

  array[N_serve, N_court, N_direction] real<lower=-1, upper=1> rho;
  array[N_court, N_direction] real<lower=-1, upper=1> rho_g;
  real<lower=0> sig_rho;

  array[N_serve, N_court, N_direction] real<lower=0, upper=t_upper> t;
  real<lower=0> t_g;
  real<lower=0> sig_t;

  array[N_serve, N_court] simplex[N_direction] theta;
}

transformed parameters {
  array[N_serve, N_court, N_direction] vector[2] mu;
  array[N_serve, N_court, N_direction] matrix[2, 2] L;

  for (s in 1:N_serve) {
    for (c in 1:N_court) {
      for (d in 1:N_direction) {
        mu[s, c, d] = [mu_x[s, c, d], mu_y[s, c][d]]';

        L[s, c, d][1, 1] = tau[s, c, d][1];
        L[s, c, d][2, 1] = rho[s, c, d] * tau[s, c, d][2];
        L[s, c, d][1, 2] = 0;
        L[s, c, d][2, 2] = sqrt(1 - square(rho[s, c, d])) * tau[s, c, d][2];
      }
    }
  }
}

model {
  
  for (c in 1:N_court) {
    for (d in 1:N_direction) {
      mu_g_x[c, d] ~ normal(0, 2.5);
      mu_g_y[c][d] ~ normal(0, 2.5);
      rho_g[c, d] ~ uniform(-1, 1);
    }
  }
  sig_mu ~ normal(0, 2.5);
  sig_rho ~ normal(0, 1);
  
  tau_g[1] ~ normal(0, 2.5);
  tau_g[2] ~ normal(0, 2.5);
  sig_tau ~ normal(0, 2.5);

  t_g ~ normal(4, 1);
  sig_t ~ normal(0, 1);

  for (s in 1:N_serve) {
    for (c in 1:N_court) {
      for (d in 1:N_direction) {
        mu_x[s, c, d] ~ normal(mu_g_x[c, d], sig_mu);
        mu_y[s, c][d] ~ normal(mu_g_y[c][d], sig_mu);
        tau[s, c, d][1] ~ normal(tau_g[1], sig_tau);
        tau[s, c, d][2] ~ normal(tau_g[2], sig_tau);
        rho[s, c, d] ~ normal(rho_g[c, d], sig_rho);
        t[s, c, d] ~ normal(t_g, sig_t);
      }
    }
  }

  for (i in 1:N_obs) {
    int s = z_ind_serve[i];
    int c = z_ind_court[i];
    array[N_direction] real lps;
    for (d in 1:N_direction) {
      lps[d] = log(theta[s, c][d]) +
        multi_normal_cholesky_lpdf(z_obs[i] | mu[s, c, d], L[s, c, d]);
    }
    target += log_sum_exp(lps);
  }

  for (i in 1:N_cens) {
    int s = c_ind_serve[i];
    int c = c_ind_court[i];
    real p_cens = 0;
    for (d in 1:N_direction) {
      p_cens += theta[s, c][d] *
        normal_cdf(t[s, c, d] | mu_x[s, c, d], tau[s, c, d][1]);
    }
    target += log(p_cens + 1e-12);
  }
}
