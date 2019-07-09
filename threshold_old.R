stan_data = with(md, list(
  N = nrow(md),
  D = max(pct),
  R = length(unique(race)),
  d = as.integer(pct),
  r = as.integer(race),
  n = numstops,
  s = numsearches,
  h = numhits))

model <- stan_model(file = 'threshold_old.stan')

fit <- sampling(
  model, data = stan_data, iter=5000,
  init = 'random', chains=5,
  cores=5, refresh=50, warmup = 2500,
  control = list(adapt_delta = 0.95,
                 max_treedepth = 12,
                 adapt_engaged = TRUE))

post = rstan::extract(fit)

signal_to_p = function(x, phi, delta){
  #Checked. Converts x -> p. 
  #Explain this equation. dnorm is probability density function
  p = phi * dnorm(x, delta, 1) / (phi * dnorm(x, delta, 1) + (1 - phi) * dnorm(x, 0, 1));
  return(p)
}

plot_department_thresholds = function(obs, post) {
  colors = c('blue', 'black', 'red')
  races = as.character(levels(obs$race))
  obs$thresholds = colMeans(signal_to_p(post$t_i, post$phi, post$delta))
  mx = max(obs$thresholds)
  df = obs %>% filter(race == 'W') %>%
    right_join(obs %>% filter(race != 'W'), by = 'pct')
  
  ggplot(df) + 
    geom_point(aes(x=thresholds.x, y=thresholds.y, size = numstops.y), alpha=0.8, shape = 1) +
    geom_abline(slope=1, intercept=0, linetype='dashed') +
    scale_y_continuous('Minority threshold\n', limits=c(0,mx), labels=percent, expand=c(0, 0)) +
    scale_x_continuous('\nWhite threshold', limits=c(0,mx), labels=percent, expand=c(0, 0)) +
    scale_size_area(max_size=15) +
    theme(legend.position=c(0.0,1.0),
          legend.justification=c(0,1),
          legend.title = element_blank(),
          legend.background = element_rect(fill = 'transparent'),
          panel.spacing.x=unit(1.5, "cm")) +
    scale_color_manual(values = colors[-1], labels=races[-1]) +
    guides(size=FALSE) + facet_grid(.~race.y)
}

plot_department_thresholds(md, post)

