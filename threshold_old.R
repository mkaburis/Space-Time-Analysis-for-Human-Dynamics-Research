library(rstan)
library(scales)
library(ggplot2)
library(reshape2)
library(matrixStats)
library(scales)
library(knitr)
library(tikzDevice)

md_list = list(md1, md2, md3, md4)
stan_data_list = list()

for(i in 1: length(md_list)) {
  stan_data_list[[i]] = with(md_list[[i]], list(
    N = nrow(md_list[[i]]),
    D = length(unique(pct)),
    R = length(unique(race)),
    d = as.integer(pct),
    r = as.integer(race),
    n = numstops,
    s = numsearches,
    h = numhits))
}


# stan_data_2 = with(md2, list(
#   N = nrow(md2),
#   D = length(unique(pct)),
#   R = length(unique(race)),
#   d = as.integer(pct),
#   r = as.integer(race),
#   n = numstops,
#   s = numsearches,
#   h = numhits))
# 
# stan_data_3 = with(md3, list(
#   N = nrow(md3),
#   D = length(unique(pct)),
#   R = length(unique(race)),
#   d = as.integer(pct),
#   r = as.integer(race),
#   n = numstops,
#   s = numsearches,
#   h = numhits))
# 
# stan_data_4 = with(md4, list(
#   N = nrow(md4),
#   D = length(unique(pct)),
#   R = length(unique(race)),
#   d = as.integer(pct),
#   r = as.integer(race),
#   n = numstops,
#   s = numsearches,
#   h = numhits))

pctNames1 = unique(md1$pct)
pctNames2 = unique(md4$pct)


# stan_data_list = list(stan_data_1, stan_data_2, stan_data_3, stan_data_4)
model <- stan_model(file = 'threshold_old.stan')
fit = list()
post = list()
for (i in 1: length(stan_data_list))
{
  fit[[i]] = sampling(
    model, data = stan_data_list[[i]], iter=5000,
    init = 'random', chains=5,
    cores=5, refresh=50, warmup = 2500,
    control = list(adapt_delta = 0.95,
                   max_treedepth = 12,
                   adapt_engaged = TRUE))
  
  post[[i]] = rstan::extract(fit[[i]])
}

signal_to_p = function(x, phi, delta){
  #Checked. Converts x -> p. 
  #Explain this equation. dnorm is probability density function
  p = phi * dnorm(x, delta, 1) / (phi * dnorm(x, delta, 1) + (1 - phi) * dnorm(x, 0, 1));
  return(p)
}

md_list[[1]]$thresholds = colMeans(signal_to_p(post[[1]]$t_i, post[[1]]$phi, post[[1]]$delta))
md_list[[2]]$thresholds = colMeans(signal_to_p(post[[2]]$t_i, post[[2]]$phi, post[[2]]$delta))
md_list[[3]]$thresholds = colMeans(signal_to_p(post[[3]]$t_i, post[[3]]$phi, post[[3]]$delta))
md_list[[4]]$thresholds = colMeans(signal_to_p(post[[4]]$t_i, post[[4]]$phi, post[[4]]$delta))


plot_department_thresholds = function(obs, post) {
  colors = c('blue', 'black', 'red')
  races = as.character(levels(obs$race))
  mx = max(obs$thresholds)
  df = obs %>% filter(race == 'W') %>%
    right_join(obs %>% filter(race != 'W'), by = 'pct')
  
  allResids <<- sqrt(abs(df$thresholds.x - df$thresholds.y)) * sign((df$thresholds.y - df$thresholds.x))
  
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
          panel.spacing.x=unit(1.5, "cm"),
          plot.title = element_text(hjust = 0.45)) +
    scale_color_manual(values = colors[-1], labels=races[-1]) +
    guides(size=FALSE) + facet_grid(.~race.y) +
    ggtitle("2015 - 2018")
}



plot_crime_thresholds = function(obs, post) {
  mx = max(obs$thresholds)
  cx = max(obs$numcrimes)
  df = obs %>% filter(race == 'W') %>%
    right_join(obs %>% filter(race == 'B'), by = 'pct') %>%
    right_join(obs %>% filter(race == 'H'), by = 'pct')
  
  ggplot(df) + 
    geom_line(aes(x=numcrimes.x, y=thresholds.x, color = "White"), stat = "identity",
              alpha=0.8) + #white
    geom_line(aes(x=numcrimes.x, y=thresholds.y, color = "Black"), stat = "identity",
              alpha=0.8) + #black
    geom_line(aes(x=numcrimes.x, y=thresholds, color = "Hispanic"), stat = "identity",
              alpha=0.8) + #hispanic
    geom_abline(slope=1, intercept=0, linetype='dashed') +
    scale_y_continuous('Threshold\n', limits=c(0,mx), labels=percent, expand=c(0, 0)) +
    scale_x_continuous('\nCrime Numbers', limits=c(0,cx), expand=c(0, 0)) +
    scale_size_area(max_size=15) +
    theme(legend.position=c(0.8, 0.3),
          legend.justification=c(0,1),
          legend.title = element_blank(),
          legend.background = element_rect(fill = 'transparent'),
          panel.spacing.x=unit(1.5, "cm"),
          plot.title = element_text(hjust = 0.45)) + 
    scale_color_manual(values = c("White" = "blue", "Black" = "black", 
                                  "Hispanic" = "red")) +
    ggtitle("2015 - 2018")
}




search_rate_ppc <- function(obs, post, ylim = 0.03) {
  obs$pred_search_rate = colMeans(post$searchrate)
  ggplot(data=obs, aes(x=pred_search_rate, y=pred_search_rate-searchrate)) +
    geom_point(aes(size=numstops, color=race), alpha = 0.8) + 
    scale_size_area(max_size=10) +
    scale_x_continuous('\nPredicted search rate', labels=percent)+
    scale_y_continuous('Search rate prediction error\n', labels=percent, limits=c(-ylim, ylim)) +
    geom_abline(slope=0, intercept=0, linetype='dashed') +
    theme(legend.position=c(1.0,0),
          legend.justification=c(1,0),
          legend.title = element_blank(),
          legend.background = element_rect(fill = 'transparent'),
          plot.title = element_text(hjust = 0.45)) +
    scale_color_manual(values=c('blue','black','red', 'green4')) +
    guides(size=FALSE) +
    ggtitle("2015 - 2018")
}

hit_rate_ppc <- function(obs, post, ylim = 0.3) {
  obs$pred_hit_rate = colMeans(post$hitrate)
  ggplot(data=obs, aes(x=pred_hit_rate, y=hitrate-pred_hit_rate)) +
    geom_point(aes(size=numstops, color=race), alpha=0.8) + 
    scale_size_area(max_size=10) +
    scale_x_continuous('\nPredicted hit rate', labels=percent) +
    scale_y_continuous('Hit rate prediction error\n', labels=percent, limits = c(-ylim, ylim)) +
    geom_abline(slope=0, intercept=0, linetype='dashed') +
    theme(legend.position=c(1.0,0),
          legend.justification=c(1,0), 
          legend.title = element_blank(),
          legend.background = element_rect(fill = 'transparent'),
          plot.title = element_text(hjust = 0.45))+
    scale_color_manual(values=c('blue','black','red', 'green4')) +
    guides(size=FALSE) +
    ggtitle("2015 - 2018")
}

disc_indx_list = list()

for (i in 1: length(md_list)){
  plot_department_thresholds(md_list[[i]], post[[i]])
  
  distVals = split(allResids, 1:2)
  # Black Distance
  dist_B = unlist(distVals[1])
  #plots thresholds across crime numbers
  
  # Hispanic Distance
  dist_H = unlist(distVals[2])
  
  if (i == 1 | i == 2) {
    disc_indx_list[[i]] = data.frame(pct = pctNames1, black_disc_index = dist_B,
                                     hispanic_disc_index = dist_H)
  }

  
  else {
    disc_indx_list[[i]] = data.frame(pct = pctNames2, black_disc_index = dist_B,
                                     hispanic_disc_index = dist_H)
  }
  
  plot_crime_thresholds(md_list[[i]], post[[i]])
  search_rate_ppc(md_list[[i]], post[[i]])
  hit_rate_ppc(md_list[[i]], post[[i]])
}


write.csv((disc_indx_list[[1]]), file = "discriminationIndex1.csv", row.names = F)
write.csv((disc_indx_list[[2]]), file = "discriminationIndex2.csv", row.names = F)
write.csv((disc_indx_list[[3]]), file = "discriminationIndex3.csv", row.names = F)
write.csv((disc_indx_list[[4]]), file = "discriminationIndex4.csv", row.names = F)


for(i in 1: length(md_list)) {
  mdmod = md_list[[i]] %>% 
    mutate(thresholds = colMeans(signal_to_p(post[[i]]$t_i, post[[i]]$phi, post[[i]]$delta))) %>%
    group_by(pct) %>%
    mutate(total_stops = sum(numstops)) %>%
    ungroup()
  
  na_replace = function(x, r) ifelse(is.finite(x), x, r)
  
  accumrowMeans = function(M, i, w = rep(1, nrow(M)), imax = max(i)) {
    t(sapply(1:imax, function(j) (i == j)*na_replace(w/sum(w[i == j]),0))) %*% M
  }
  
  avg_thresh = accumrowMeans(t(signal_to_p(post[[i]]$t_i, post[[i]]$phi, post[[i]]$delta)),
                             as.integer(mdmod$race), mdmod$total_stops)
  
  data.frame(levels(mdmod$race),
             sprintf('%.3f', rowMeans(avg_thresh)),
             apply(rowQuantiles(avg_thresh, probs = c(0.025, 0.975)), 1,
                   function(x) paste0('(', paste0(sprintf('%.3f',x), collapse = ', '), ')'))
  ) %>%
    setNames(c('Driver Race', 'Average Threshold', '95% Credible Interval'))
}




