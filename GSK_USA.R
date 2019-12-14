###############
##
## R Script to build a sample presentation about ML applications in commercial business
## Created by Joshua King
## 
##
###############

# # library repo
library(tibble)
library(readr)
library(magrittr)
library(dplyr)
library(maps)
library(ggplot2)
library(tidyr)
library(factoextra)

################
# # set gg theme
################
theme_jk <- theme_void() + 
  theme(legend.position='none',
        panel.background = element_rect(fill = "transparent", color=NA), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        panel.border = element_blank(),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"))

################
# # define hull function
################
find_hull <- function(df) df[chull(df$longitude, df$latitude), ]


################
# # simulate data
################

# # rand points
locs <- read_csv('https://download.data.world/s/egtbc6cvlkwnp3qjjhx75txuhomoog') %>% 
  drop_na() %>%
  sample_n(5700)
# https://data.world/datafiniti/fast-food-restaurants-across-america/workspace/query?queryid=sample-0

states <- map_data("state") 

set.seed(2820)
km_loc <- kmeans(locs %>% select(latitude, longitude), 25)
locs <- cbind(locs, loc_cluster=km_loc$cluster)

recs <- length(locs$latitude)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

df <- cbind(locs,
            # # vars describing the consumers in the area
            med_inc = rnorm(recs, mean=90000, sd=15000), # median household income
            age = rnorm(recs, mean=36, sd=3), # median age
            education = rnorm(recs, mean=16, sd=2), # educational attainment 
            pop_dens_day = rnorm(recs, mean=15000, sd=2500), # population density during the day
            pop_dens_night = rnorm(recs, mean=15000, sd=2500), # population density during the night
            hispanicity = pmax(rnorm(recs, mean=.25, sd=.2), 0), # average hispanicity
            temp = rnorm(recs, mean=75, sd=10), # average temperature
            snow = pmax(rnorm(recs, mean=4, sd=2), 0), # average snowfall
            crime = pmax(rnorm(recs, mean=.1, sd=.05), 0), # crime rate
            competition = pmax(rnorm(recs, mean = 6, sd=2), 0) # number of direct competitors
            
            # # while these variables describe the underlying consumers in the area 
            # # # (which should be indicative of the general affinity or proclivity to 
            # # # be a likely consumer)
) %>% as_tibble 


km_val <- kmeans(df %>% select(med_inc:competition), 10)
df %<>% cbind(val_cluster = km_val$cluster) %>% 
  left_join(tibble(cluster=1:10, mult=sample(10)), c('val_cluster' = 'cluster')) %>%
  rowwise %>%
  mutate(sales = pmax(rnorm(1, mean=2500*mult, sd=1000*mult), 0)) %>%
  ungroup

x_lim <- c(min(df$longitude-1), max(df$longitude+1))
y_lim <- c(min(df$latitude-1), max(df$latitude+1))

ggplot() + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="lightgrey", fill="lightgrey") +
  scale_x_continuous(limits=x_lim, expand=c(0,0)) +
  scale_y_continuous(limits=y_lim, expand=c(0,0)) +
  theme_jk
ggsave(filename = '1-country.png', bg='transparent', width=11.2, height=6.05)

ggplot() + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="lightgrey") +
  geom_point(data=locs, aes(x=longitude, y=latitude), color='red', fill='red', size=.25, alpha=.4) +
  scale_x_continuous(limits=x_lim, expand=c(0,0)) +
  scale_y_continuous(limits=y_lim, expand=c(0,0)) +
  theme_jk
ggsave(filename = '2-locations.png', bg='transparent', width=11.2, height=6.05)

ggplot(data=df) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="lightgrey") +
  geom_point(aes(x=longitude, y=latitude, color=as.factor(loc_cluster), fill=as.factor(loc_cluster)), size=.25, alpha=.4) +
  scale_x_continuous(limits=x_lim, expand=c(0,0)) +
  scale_y_continuous(limits=y_lim, expand=c(0,0)) +
  theme_jk
ggsave(filename = '3-geo_cluster.png', bg='transparent', width=11.2, height=6.05)

# # and you have your sales force and marketing team out working in the field working with these locations to make sure they're happy
# # # and see if there's anything that can be done to help them achieve their business goals
hulls <- plyr::ddply(df, "loc_cluster", find_hull)
ggplot(data=df, aes(x=longitude, y=latitude, color=as.factor(loc_cluster), fill=as.factor(loc_cluster))) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="lightgrey") +
  geom_point(size=.25, alpha=.4) +
  geom_polygon(data=hulls, alpha=.4, size=.05) +
  scale_x_continuous(limits=x_lim, expand=c(0,0)) +
  scale_y_continuous(limits=y_lim, expand=c(0,0)) +
  theme_jk
ggsave(filename = '4-loc_cluster_poly.png', bg='transparent', width=11.2, height=6.05)

# # as your salesforce is working with them, they all say they want to make more money
# # # they're working harder than ever but margins are tight and they can't seem to make it work
# # # they see other people in the local area that are doing better
ggplot(data=df, aes(x=longitude, y=latitude, color=as.factor(loc_cluster), fill=as.factor(loc_cluster))) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="lightgrey") +
  geom_point(aes(size=sales^3), alpha=.4) + scale_size_area() +
  geom_polygon(data=hulls, alpha=.4, size=.05) +
  scale_x_continuous(limits=x_lim, expand=c(0,0)) +
  scale_y_continuous(limits=y_lim, expand=c(0,0)) +
  theme_jk
ggsave(filename = '5-cluster_with_sales.png', bg='transparent', width=11.2, height=6.05)

ggplot(data=df, aes(x=longitude, y=latitude, color=as.factor(loc_cluster), fill=as.factor(loc_cluster))) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="lightgrey") +
  geom_point(aes(size=sales^3, color=as.factor(val_cluster)), alpha=.4) + scale_size_area() +
  geom_polygon(data=hulls, alpha=.4, size=.05) +
  scale_x_continuous(limits=x_lim, expand=c(0,0)) +
  scale_y_continuous(limits=y_lim, expand=c(0,0)) +
  theme_jk
ggsave(filename = '6-cluster_with_sales_val_cluster_poly.png', bg='transparent', width=11.2, height=6.05)

ggplot(data=df, aes(x=longitude, y=latitude, color=as.factor(loc_cluster), fill=as.factor(loc_cluster))) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="lightgrey") +
  geom_point(aes(size=sales^3, color=as.factor(val_cluster)), alpha=.4) + scale_size_area() +
  scale_x_continuous(limits=x_lim, expand=c(0,0)) +
  scale_y_continuous(limits=y_lim, expand=c(0,0)) +
  theme_jk
ggsave(filename = '7-cluster_with_sales_val_cluster.png', bg='transparent', width=11.2, height=6.05)

ggplot(data=df, aes(x=longitude, y=latitude, color=as.factor(loc_cluster), fill=as.factor(loc_cluster))) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="lightgrey") +
  geom_point(aes(size=sales^3, color=as.factor(val_cluster)), alpha=.4) + scale_size_area() +
  scale_x_continuous(limits=x_lim, expand=c(0,0)) +
  scale_y_continuous(limits=y_lim, expand=c(0,0)) +
  theme_jk + theme(legend.position = 'right') + guides(size=F, fill=F)
ggsave(filename = '8-cluster_with_sales_val_cluster_legend.png', bg='transparent', width=11.2, height=6.05)

df %>% group_by(val_cluster) %>%
  summarise(sales = mean(sales))

ggplot(data=df, aes(x=val_cluster, y=sales, color=as.factor(val_cluster), fill=as.factor(val_cluster))) + 
  geom_boxplot(alpha=.4) + coord_flip() +
  theme_jk 
ggsave(filename = '8-legend.png', bg='transparent')

ggplot(data=df %>% filter(val_cluster==6), aes(x=longitude, y=latitude, color=as.factor(loc_cluster), fill=as.factor(loc_cluster))) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="lightgrey") +
  geom_point(aes(size=sales^3), color='#2AB3B5', alpha=.4) + scale_size_area() +
  scale_x_continuous(limits=x_lim, expand=c(0,0)) +
  scale_y_continuous(limits=y_lim, expand=c(0,0)) +
  theme_jk + theme(legend.position = 'right') + guides(size=F, fill=F)
ggsave(filename = '9-cluster_with_sales_cluster4.png', bg='transparent', width=11.2, height=6.05)

samp <- 
  tibble(val_cluster = (1:10),
         potential = runif(10),
         sales = runif(10),
         locations = rnorm(10, 2500, 750))

samp %>% 
  ggplot(aes(x=sales, y=potential, color=as.factor(val_cluster), size=locations)) + 
  geom_point(alpha=.4) + theme_minimal() + scale_size(range=c(0,50)) +
  geom_hline(yintercept=.5, color='lightgrey', size=2) + geom_vline(xintercept=.5, color='lightgrey', size=2) +
  theme_jk + 
  xlab('Sales') + ylab('Potential') +
  scale_x_continuous(limits=c(0,1)) + scale_y_continuous(limits=c(0,1)) + coord_fixed()

ggsave(filename = '10-sales v potential.png', bg='transparent', width=7, height=7)

simul <- tibble(val_cluster = integer(),
                sales = double())

for (i in 1:10){
  
  locs <- as.numeric(samp %>% filter(val_cluster==i) %>% select(locations) * 1)
  mu <- as.numeric(samp %>% filter(val_cluster==i) %>% select(sales) * 1)
  std <- as.numeric(samp %>% filter(val_cluster==i) %>% select(potential) * 1)
  
  simul %<>% rbind(tibble(val_cluster = rep(i, locs),
                          sales = rnorm(locs, mean = mu, sd = std)))
}
simul$sales %<>% range01

simul %>% ggplot(aes(x=val_cluster, y=sales, color=as.factor(val_cluster), fill=as.factor(val_cluster))) + 
  geom_boxplot(alpha=.4) + coord_flip() +
  theme_jk 
ggsave(filename = '11-legend.png', bg='transparent')
