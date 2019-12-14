###############
##
## R Script to build a sample presentation about ML applications in business
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
# # simulate data
################

# # rand points
locs <- read_csv('https://download.data.world/s/egtbc6cvlkwnp3qjjhx75txuhomoog') %>% 
  drop_na() %>%
  sample_n(5700)
# https://data.world/datafiniti/fast-food-restaurants-across-america/workspace/query?queryid=sample-0

states <- map_data("state") 

set.seed(803)
km_loc <- kmeans(locs %>% select(latitude, longitude), 100)
locs <- cbind(locs, loc_cluster=km_loc$cluster)

recs <- length(locs$latitude)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

df <- cbind(locs,
            # # vars describing the consumers in the area
            med_inc = rnorm(recs, mean=90000, sd=15000),
            hispanicity = pmax(rnorm(recs, mean=.25, sd=.2), 0),
            temp = rnorm(recs, mean=75, sd=10),
            crime = pmax(rnorm(recs, mean=.1, sd=.05), 0),
            type2 = range01(rnorm(recs)),
            chf = range01(rnorm(recs)),
            perc_insured = range01(rnorm(recs)),
            
            # # vars describing the physician
            perc_prescribed = range01(rnorm(recs)),
            tenure = runif(recs, max=30),
            avg_promo = range01(rnorm(recs))
) %>% as_tibble 
  

km_val <- kmeans(df %>% select(med_inc:avg_promo), 8)
df %<>% cbind(val_cluster = km_val$cluster) %>% 
  rowwise %>%
  mutate(sales = rnorm(1, mean=25000*val_cluster*runif(1), sd=10000*val_cluster*runif(1)))
  
km_sales <- kmeans(df %>% select(sales), 8)
df %<>% cbind(sales_cluster = km_sales$cluster)

ggplot() + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey20", fill="grey20") +
  # geom_point(data=locs, aes(x=longitude, y=latitude), color='red', fill='red', size=.1, alpha=.4) +
  theme_jk

ggplot() + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="grey20") +
  geom_point(data=locs, aes(x=longitude, y=latitude), color='red', fill='red', size=.25, alpha=.4) +
  theme_jk


ggplot(data=df) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="grey20") +
  geom_point(aes(x=longitude, y=latitude, color=as.factor(loc_cluster), fill=as.factor(loc_cluster)), size=.25, alpha=.4) +
  theme_jk

ggplot(data=df) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="grey20") +
  geom_point(aes(x=longitude, y=latitude, color=as.factor(loc_cluster), fill=as.factor(loc_cluster), size=(sales)^3), alpha=.4, stroke=0) +
  scale_size_area() +
  theme_jk

find_hull <- function(df) df[chull(df$longitude, df$latitude), ]
hulls <- plyr::ddply(df, "loc_cluster", find_hull)

ggplot(data=df, aes(x=longitude, y=latitude, color=as.factor(loc_cluster), fill=as.factor(loc_cluster))) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="grey20") +
  geom_point(size=.25, alpha=.4) +
  geom_polygon(data=hulls, alpha=.1, size=.01) +
  # stat_ellipse(geom='polygon',
               # aes(x=longitude, y=latitude, color=as.factor(loc_cluster)), fill=NA) +
  theme_jk

ggplot(data=df, aes(x=longitude, y=latitude, color=as.factor(loc_cluster), fill=as.factor(loc_cluster))) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="grey20") +
  geom_point(aes(size=sales^3), alpha=.4) + scale_size_area() +
  geom_polygon(data=hulls, alpha=.1, size=.01) +
  # stat_ellipse(geom='polygon',
  # aes(x=longitude, y=latitude, color=as.factor(loc_cluster)), fill=NA) +
  theme_jk

ggsave(filename = 'test.png', bg='transparent')
# # physician must have patients in need of the product

ggplot(data=df) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="grey20") +
  geom_point(aes(x=longitude, y=latitude, color=as.factor(sales_cluster), fill=as.factor(sales_cluster), size=(sales)^3), alpha=.4, stroke=0) +
  scale_size_area() +
  stat_ellipse(geom='polygon',
               aes(x=longitude, y=latitude, color=as.factor(loc_cluster)), fill=NA) +
  theme_void() + theme(legend.position='none')

ggplot(data=df) + coord_fixed(1.3) +
  geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="grey", fill="grey20") +
  geom_point(aes(x=longitude, y=latitude, color=as.factor(val_cluster), fill=as.factor(val_cluster), size=sales^2), alpha=.5, stroke=0) +
  scale_fill_brewer(palette = "Spectral") +
  scale_color_brewer(palette = 'Spectral') +
  scale_size_area() +
  # stat_ellipse(geom='polygon',
  #              aes(x=longitude, y=latitude, color=as.factor(loc_cluster)), fill=NA) +
  theme_void() + theme(legend.position='none')

df %>% group_by(val_cluster) %>%
  summarise(sales_mu = mean(sales),
            sd_sd = sd(sales, na.rm=T))

df %>% select(med_inc:sales) %>% 
  group_by(val_cluster) %>% 
  summarise_each(funs='mean') %>%
  mutate_at(vars(-val_cluster), range01)

  
            