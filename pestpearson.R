library(tidyverse)
library(plotly)
library(egg)

pest <- read.csv("Data/pest_data_clean.csv") %>%
  mutate(month = format(as.Date(date,"%m/%d/%y"), "%y.%m"))

pest.mn <- pest %>% group_by(site_name, month) %>%
  summarize(across(Miner:mite_den, mean), .groups = "drop") %>%
  select(-Obs_roya)

# landscape
lc <- read.csv("landcover_buffers.csv") %>%
  rename(site_name = farm) %>% filter(site_name != "ADJUCP")

lcr <- read.csv("landscape_rings.csv") %>%
  rename(site_name = farm) %>% filter(site_name != "ADJUCP")

# roya pearson correlation
lc_names = names(lc)[2:9]
buffers <- unique(lc$radius)

# full data join
lc_pest <- full_join(lc, pest.mn, by = "site_name")
lcr_pest <- full_join(lcr, pest.mn, by = "site_name")

lc_pears <- lc_pest %>% 
  pivot_longer(cols=coffee:water, names_to = "land", values_to = "pc_cov") %>%
  group_by(radius, month, land) %>%
  summarize(across(Miner:mite_den, \(x) cor(x, pc_cov, use="complete.obs")),
            # roya = cor(roya, pc_cov),
            # miner = cor(Miner, pc_cov),
            # GCS = cor(GCS, pc_cov),
            # GCS_lec = cor(GCS_lecan, pc_cov),
            # flatid = cor(num_flatids, pc_cov),
            # CBB = cor(CBB, pc_cov)
            n = n(),
            .groups = "drop")

lcr_pears <- lcr_pest %>% 
  pivot_longer(cols=coffee:water, names_to = "land", values_to = "pc_cov") %>%
  group_by(inner.radius, outer.radius, month, land) %>%
  summarize(across(Miner:mite_den, \(x) cor(x, pc_cov, use="complete.obs")),
            # roya = cor(roya, pc_cov),
            # miner = cor(Miner, pc_cov),
            # GCS = cor(GCS, pc_cov),
            # GCS_lec = cor(GCS_lecan, pc_cov),
            # flatid = cor(num_flatids, pc_cov),
            # CBB = cor(CBB, pc_cov)
            n = n(),
            .groups = "drop")


lc_spear <- lc_pest %>% 
  pivot_longer(cols=coffee:water, names_to = "land", values_to = "pc_cov") %>%
  group_by(radius, month, land) %>%
  summarize(across(Miner:mite_den, \(x) cor(x, pc_cov, method = "spearman", use="complete.obs")),
            # roya = cor(roya, pc_cov),
            # miner = cor(Miner, pc_cov),
            # GCS = cor(GCS, pc_cov),
            # GCS_lec = cor(GCS_lecan, pc_cov),
            # flatid = cor(num_flatids, pc_cov),
            # CBB = cor(CBB, pc_cov)
            n = n(),
            .groups = "drop")

lcr_spear <- lcr_pest %>% 
  pivot_longer(cols=coffee:water, names_to = "land", values_to = "pc_cov") %>%
  group_by(inner.radius, outer.radius, month, land) %>%
  summarize(across(Miner:mite_den, \(x) cor(x, pc_cov, method="spearman", use="complete.obs")),
            # roya = cor(roya, pc_cov),
            # miner = cor(Miner, pc_cov),
            # GCS = cor(GCS, pc_cov),
            # GCS_lec = cor(GCS_lecan, pc_cov),
            # flatid = cor(num_flatids, pc_cov),
            # CBB = cor(CBB, pc_cov)
            n = n(),
            .groups = "drop")

pests <- names(lc_spear)[4:21]

for(i in pests){
  plot.i <- ggplot(lc_pears, 
                   aes(x=month, y=radius)) + 
    geom_tile(aes(fill=!!sym(i)))  +
    scale_fill_distiller(type = "div", limit = c(-1,1)) +
    facet_wrap(~land) +
    theme_article() +
    # scale_x_continuous(breaks=1:12) +
    theme(axis.text = element_text(size=8,angle = 45, hjust = 1))
  
  ggsave(paste0("plots/", "cor_grid_pears_",i,".png"), plot=plot.i, bg="white", 
         height=4, width=6.5, units="in", dpi=300)
}

for(i in pests){
  plot.i <- ggplot(lc_spear, 
                   aes(x=month, y=radius)) + 
    geom_tile(aes(fill=!!sym(i)))  +
    scale_fill_distiller(type = "div", limit = c(-1,1)) +
    facet_wrap(~land) +
    theme_article() +
    # scale_x_continuous(breaks=1:12) +
    theme(axis.text = element_text(size=8,angle = 45, hjust = 1))
  
  ggsave(paste0("plots/", "cor_grid_spear_",i,".png"), plot=plot.i, bg="white", 
         height=4, width=6.5, units="in", dpi=300)
}

for(i in pests){
  plot.i <- ggplot(lcr_pears, 
                   aes(x=month, y=inner.radius)) + 
    geom_tile(aes(fill=!!sym(i)))  +
    scale_fill_distiller(type = "div", limit = c(-1,1)) +
    facet_wrap(~land) +
    theme_article() +
    # scale_x_continuous(breaks=1:12) +
    theme(axis.text = element_text(size=8, angle = 45, hjust = 1))
  
  ggsave(paste0("plots/", "ringcor_grid_pears_",i,".png"), plot=plot.i, bg="white", 
         height=4, width=6.5, units="in", dpi=300)
}

for(i in pests){
  plot.i <- ggplot(lcr_spear, 
                   aes(x=month, y=inner.radius)) + 
    geom_tile(aes(fill=!!sym(i)))  +
    scale_fill_distiller(type = "div", limit = c(-1,1)) +
    facet_wrap(~land) +
    theme_article() +
    # scale_x_continuous(breaks=1:12) +
    theme(axis.text = element_text(size=8, angle = 45, hjust = 1))
  
  ggsave(paste0("plots/", "ringcor_grid_spear_",i,".png"), plot=plot.i, bg="white", 
         height=4, width=6.5, units="in", dpi=300)
}

lc_pears_long <- lc_pears %>% pivot_longer()

ggplot(lc_pears, aes())

lc200_month <- lc_pest %>% filter(radius==200) %>% select(-developed, -water) %>%
  pivot_longer(cols=coffee:shrub, names_to = "land", values_to = "pccov")

lc1200_month <- lc_pest %>% filter(radius==1200) %>% select(-developed, -water) %>%
  pivot_longer(cols=coffee:shrub, names_to = "land", values_to = "pccov")

ggplot(lc1200_month, aes(x=pccov, y=roya)) + 
  geom_point() + facet_grid(land~month) + theme_article() + 
  xlab("% cover 1200m")
         
ggplot(lc200_month, aes(x=pccov, y=lecani)) + 
  geom_point() + facet_grid(land~month) + theme_article() + 
  xlab("% cover 200m")

ggplot(lc1200_month, aes(x=pccov, y=Miner)) + 
  geom_point() + facet_grid(land~month) + theme_article() + 
  xlab("% cover 1200m")
