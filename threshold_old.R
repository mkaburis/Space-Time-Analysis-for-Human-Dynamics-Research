library(rstan)
library(scales)
library(ggplot2)
library(reshape2)
library(matrixStats)
library(scales)
library(knitr)
library(tikzDevice)
library(RColorBrewer)
library(grid)
library(gridExtra)

# md1 = read.csv(file="mergeddata1.csv")
# md2 = read.csv(file ="mergeddata2.csv")
# md3 = read.csv(file ="mergeddata3.csv")
# md4 = read.csv(file ="mergeddata4.csv")

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
    h = numhits )) 
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

precinctGraphs = list()
crimeGraphs = list()
searchGraphs = list()
hitGraphs = list()
plot_department_thresholds = function(obs, post, itr) {
  colors = c('blue', 'black', 'red')
  races = as.character(levels(obs$race))
  mx = max(obs$thresholds)
  df = obs %>% filter(race == 'W') %>%
    right_join(obs %>% filter(race != 'W'), by = 'pct')
  
  allResids <<- sqrt(abs(df$thresholds.x - df$thresholds.y)) * sign((df$thresholds.y - df$thresholds.x))
  
  precinctGraphs[[itr]] <<- ggplot(df) + 
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
          plot.title = element_text(hjust = 0.45),
          plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm")) +
    scale_color_manual(values = colors[-1], labels=races[-1]) +
    guides(size=FALSE) + facet_grid(.~race.y) +
    
    if(itr == 1)
      ggtitle("2003 - 2006")
    else if (itr == 2)
      ggtitle("2007 - 2010")
    else if (itr == 3)
      ggtitle("2011 - 2014")
    else if (itr == 4)
    ggtitle("2015 - 2018")
}

plot_crime_thresholds = function(obs, post, itr) {
  mx = max(obs$thresholds)
  cx = max(obs$numcrimes)
  df = obs %>% filter(race == 'W') %>%
    right_join(obs %>% filter(race == 'B'), by = 'pct') %>%
    right_join(obs %>% filter(race == 'H'), by = 'pct')
  
 crimeGraphs[[itr]] <<- ggplot(df) + 
    geom_line(aes(x=numcrimes.x, y=thresholds.x, color = "White"), stat = "identity",
              alpha=0.8) + #white
    geom_line(aes(x=numcrimes.x, y=thresholds.y, color = "Black"), stat = "identity",
              alpha=0.8) + #black
    geom_line(aes(x=numcrimes.x, y=thresholds, color = "Hispanic"), stat = "identity",
              alpha=0.8) + #hispanic
    geom_abline(slope=1, intercept=0, linetype='dashed') +
    scale_y_continuous('Threshold\n', limits=c(0,mx), labels=percent, expand=c(0, 0)) +
    scale_x_continuous('\nCrime Numbers', limits=c(0,cx), expand=c(0, 0)) +
    theme(legend.position=c(0.8, 0.3),
          legend.justification=c(0,1),
          legend.title = element_blank(),
          legend.background = element_rect(fill = 'transparent'),
          panel.spacing.x=unit(1.5, "cm"),
          plot.title = element_text(hjust = 0.45)) + 
    scale_color_manual(values = c("White" = "blue", "Black" = "black", 
                                  "Hispanic" = "red")) +
  if(itr == 1)
      ggtitle("2003 - 2006")
  else if (itr == 2)
    ggtitle("2007 - 2010")
  else if (itr == 3)
    ggtitle("2011 - 2014")
  else if (itr == 4)
    ggtitle("2015 - 2018")
}




search_rate_ppc <- function(obs, post, itr, ylim = 0.03) {
  obs$pred_search_rate = colMeans(post$searchrate)
  searchGraphs[[itr]] <<- ggplot(data=obs, aes(x=pred_search_rate, y=pred_search_rate-searchrate)) +
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
    
  if(itr == 1)
      ggtitle("2003 - 2006")
  else if (itr == 2)
    ggtitle("2007 - 2010")
  else if (itr == 3)
    ggtitle("2011 - 2014")
  else if (itr == 4)
    ggtitle("2015 - 2018")
}

hit_rate_ppc <- function(obs, post, itr, ylim = 0.3) {
  obs$pred_hit_rate = colMeans(post$hitrate)
  hitGraphs[[itr]] <<- ggplot(data=obs, aes(x=pred_hit_rate, y=hitrate-pred_hit_rate)) +
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
    
  if(itr == 1)
      ggtitle("2003 - 2006")
  else if (itr == 2)
    ggtitle("2007 - 2010")
  else if (itr == 3)
    ggtitle("2011 - 2014")
  else if (itr == 4)
    ggtitle("2015 - 2018")
}

disc_indx_list = list()

for (i in 1: length(md_list)){
  print(plot_department_thresholds(md_list[[i]], post[[i]], i))
  
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
  
  print(plot_crime_thresholds(md_list[[i]], post[[i]], i))
  print(search_rate_ppc(md_list[[i]], post[[i]], i))
  print(hit_rate_ppc(md_list[[i]], post[[i]], i))
}

graphTitle = textGrob("NYPD Precinct Minority v. White Thresholds", gp=gpar(fontsize = 18, fontface="bold"))
grid.arrange(precinctGraphs[[1]], precinctGraphs[[2]],
             precinctGraphs[[3]], precinctGraphs[[4]], ncol = 2, nrow = 2,
             top = graphTitle)

graphTitle = textGrob("Thresholds v. Crime Numbers Across Races", gp=gpar(fontsize = 18, fontface="bold"))
grid.arrange(crimeGraphs[[1]], crimeGraphs[[2]],
             crimeGraphs[[3]], crimeGraphs[[4]], ncol = 2, nrow = 2,
             top = graphTitle)

graphTitle = textGrob("Search Rate Posterior Predictive Checks", gp=gpar(fontsize = 18, fontface="bold"))
grid.arrange(searchGraphs[[1]], searchGraphs[[2]],
             searchGraphs[[3]], searchGraphs[[4]], ncol = 2, nrow = 2,
             top = graphTitle)

graphTitle = textGrob("Hit Rate Posterior Predictive Checks", gp=gpar(fontsize = 18, fontface="bold"))
grid.arrange(hitGraphs[[1]], hitGraphs[[2]],
             hitGraphs[[3]], hitGraphs[[4]], ncol = 2, nrow = 2,
             top = graphTitle)

write.csv((disc_indx_list[[1]]), file = "discriminationIndex1.csv", row.names = F)
write.csv((disc_indx_list[[2]]), file = "discriminationIndex2.csv", row.names = F)
write.csv((disc_indx_list[[3]]), file = "discriminationIndex3.csv", row.names = F)
write.csv((disc_indx_list[[4]]), file = "discriminationIndex4.csv", row.names = F)

avg_thresh_df = list()
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
  
  avg_thresh_df[[i]] <- data.frame(levels(mdmod$race),
             sprintf('%.3f', rowMeans(avg_thresh)),
             apply(rowQuantiles(avg_thresh, probs = c(0.025, 0.975)), 1,
                   function(x) paste0('(', paste0(sprintf('%.3f',x), collapse = ', '), ')'))
  ) %>%
    setNames(c('Driver Race', 'Average Threshold', '95% Credible Interval'))
}

disc_indx_list[[1]] = rbind(disc_indx_list[[1]], data.frame(pct = as.factor(121), black_disc_index = 0, hispanic_disc_index = 0))
disc_indx_list[[2]] = rbind(disc_indx_list[[2]], data.frame(pct = as.factor(121), black_disc_index = 0, hispanic_disc_index = 0))

discIndxdf = data.frame(pct = disc_indx_list[[1]]$pct, B1 = disc_indx_list[[1]]$black_disc_index, 
                        H1 = disc_indx_list[[1]]$hispanic_disc_index, B2 = disc_indx_list[[2]]$black_disc_index, 
                        H2 = disc_indx_list[[2]]$hispanic_disc_index, B3 = disc_indx_list[[3]]$black_disc_index, 
                        H3 = disc_indx_list[[3]]$hispanic_disc_index, B4 = disc_indx_list[[4]]$black_disc_index, 
                        H4 = disc_indx_list[[4]]$hispanic_disc_index)

yearIntervals = c("2003-2006", "2007-2010", "2011-2014", "2015-2018")

chunk_df = data.frame(chunks = yearIntervals, disc = t(discIndxdf[,c(2,4,6,8)]))
chunk1b_df = data.frame(chunks = yearIntervals, disc = t(discIndxdf[,c(2)]))

chunk_df_mod = melt(chunk_df)
chunk1b_df = melt(chunk1b_df, )

ggplot(data = chunk_df_mod, aes(x = chunks, y=value, group = variable)) +
  geom_line(aes(color=variable)) +
  geom_point()


d1 = density(discIndxdf$B1)
d2 = density(discIndxdf$B2)
d3 = density(discIndxdf$B3)
d4 = density(discIndxdf$B4)
d5 = density(discIndxdf$H1)
d6 = density(discIndxdf$H2)
d7 = density(discIndxdf$H3)
d8 = density(discIndxdf$H4)
kd1 <- data.frame(Time_Frame = "2003 - 2006", x = d1$x, y = d1$y)
kd2 <- data.frame(Time_Frame = "2007 - 2010", x = d2$x, y = d2$y)
kd3 <- data.frame(Time_Frame = "2011 - 2014", x = d3$x, y = d3$y)
kd4 <- data.frame(Time_Frame = "2015 - 2018", x = d4$x, y = d4$y)
kd5 <- data.frame(Time_Frame = "2003 - 2006", x = d5$x, y = d5$y)
kd6 <- data.frame(Time_Frame = "2007 - 2010", x = d6$x, y = d6$y)
kd7 <- data.frame(Time_Frame = "2011 - 2014", x = d7$x, y = d7$y)
kd8 <- data.frame(Time_Frame = "2015 - 2018", x = d8$x, y = d8$y)

merged_kd1 = Reduce(function(x, y) merge(x, y, all=TRUE), list(kd1, kd2, kd3, kd4))
merged_kd2 = Reduce(function(x, y) merge(x, y, all=TRUE), list(kd5, kd6, kd7, kd8))

p1 = ggplot() +
  geom_line(data = merged_kd1, aes(x, y, color = Time_Frame), size = 1) +
  geom_vline(xintercept = -0.05, linetype = "dashed", color = "green4") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "green4") +
  scale_size_area(max_size = 5) +
  scale_color_manual(values = c("#b3ccff", "#6699ff", "#3377ff", "#004de6")) +
  theme(legend.position = "bottom", legend.title = element_blank())

p1 = p1 + labs(title = "Black", x = "Discrimination Index", y = "Number of Precincts")

p2 = ggplot() +
  geom_line(data = merged_kd2, aes(x, y, color = Time_Frame), size = 1) +
  geom_vline(xintercept = -0.05, linetype = "dashed", color = "green4") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "green4") +
  scale_size_area(max_size = 5) +
  scale_color_manual(values = c("#ff9999", "#ff4d4d", "#ff0000", "#b30000")) +
  theme(legend.position = "bottom", legend.title = element_blank())

p2 = p2 + labs(title = "Hispanic", x = "Discrimination Index", y = "Number of Precincts")

graphTitle = textGrob("Racial Bias Over Time", gp=gpar(fontsize = 18, fontface="bold"))
grid.arrange(p1, p2, ncol = 2, top = graphTitle)
  
ggplot() +
  geom_line(data = kd1, aes(x, y), color = "orangered4", size = 1.5) +
  geom_line(data = kd2, aes(x, y), color = "orangered3", size = 1.5) +
  geom_line(data = kd3, aes(x, y), color = "orangered2", size = 1.5) +
  geom_line(data = kd4, aes(x, y), color = "orangered1", size = 1.5) +
  theme(legend.position = "right")
  
ggplot(discIndxdf, aes(x = B1, color = pct)) +
  geom_histogram(binwidth = 0.01)

ggplot() +
  geom_histogram(data = discIndxdf, aes(x = B1), binwidth = 0.01) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_line(data = kd1, aes(x, y), color = "red", size = 1) +
  labs(x = "Discrimination Index", y = "Number of Precincts") +
  ggtitle("Kernel Density Distribution Plotted Over a Histogram" )

ggplot(data = kd1, aes(x, y)) + geom_line(colour = "red")

avg_dist = data.frame(chunks = yearIntervals, avg=0)
avg_dist[[2]][1] = mean(discIndxdf$B1)
avg_dist[[2]][2] = mean(discIndxdf$B2)
avg_dist[[2]][3] = mean(discIndxdf$B3)
avg_dist[[2]][4] = mean(discIndxdf$B4)

ggplot(data = avg_dist, aes(x = avg)) +
  geom_histogram(binwidth = 0.01)


