library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr)
library(ggplot2)

lf <- read.csv(here::here("data","MELNHE Litterfall EDI Data June2026.csv"))


lf[lf$Stand=="C1","Age"]<- "Young"
lf[lf$Stand=="C2","Age"]<- "Young"
lf[lf$Stand=="C3","Age"]<- "Young"
lf[lf$Stand=="C4","Age"]<- "Mid-aged"
lf[lf$Stand=="C5","Age"]<- "Mid-aged"
lf[lf$Stand=="C6","Age"]<- "Mid-aged"
lf[lf$Stand=="HBM","Age"]<- "Mid-aged"
lf[lf$Stand=="JBM","Age"]<- "Mid-aged"
lf[lf$Stand=="C7","Age"]<- "Mature"
lf[lf$Stand=="C8","Age"]<- "Mature"
lf[lf$Stand=="C9","Age"]<- "Mature"
lf[lf$Stand=="HBO","Age"]<- "Mature"
lf[lf$Stand=="JBO","Age"]<- "Mature"

w <- read.csv(here::here("wide_sorted_masses.csv"))

w$Treatment <- stdf$staplo <-paste(stdf$stand, stdf$plot)

w$Treatment<-sapply(w[ ,"staplo"],switch,
                    "C1 1"="P",   "C1 2"="N",   "C1 3"="Control", "C1 4"="NP",
                    "C2 1"="NP",  "C2 2"="Control","C2 3"="P",    "C2 4"="N",
                    "C3 1"="NP",  "C3 2"="P",   "C3 3"="N",    "C3 4"="Control",
                    "C4 1"="NP",  "C4 2"="N",   "C4 3"="Control", "C4 4"="P",
                    "C5 1"="Control","C5 2"="NP",  "C5 3"="N",    "C5 4"="P",
                    "C6 1"="NP",  "C6 2"="Control","C6 3"="N",    "C6 4"="P","C6 5"="Ca",
                    "C7 1"="N",   "C7 2"="NP",  "C7 3"="P",    "C7 4"="Control",
                    "C8 1"="P",   "C8 2"="Control","C8 3"="N",    "C8 4"="NP","C8 5"="Ca",
                    "C9 1"="Control","C9 2"="P",   "C9 3"="NP",   "C9 4"="N",
                    "HBM 1"="NP", "HBM 2"="N",  "HBM 3"="Control","HBM 4"="P",
                    "HBO 1"="P",  "HBO 2"="N",  "HBO 3"="NP",  "HBO 4"="Control", "HBO 7"="Control",
                    "JBM 1"="NP", "JBM 2"="N",  "JBM 3"="Control","JBM 4"="P",
                    "JBO 1"="NP", "JBO 2"="P",  "JBO 3"="N",   "JBO 4"="Control")


# ---- 0. Species columns ----
sp_list <- c("ACPE","ACRU","ACSA3","ACSP2","BEPA","BEAL2","FAGR",
             "FRAM2","PRPE2","QURU","SOAM3","TIAM","VILA11")
# (dropped Unknown, PO_sp, Seeds, Nonleaf from the analysis)

# ---- 1. Proportion of basket for each species ----
# proportion = species mass / sorted leaf mass in that basket
# use sort_sum_leaf as the denominator (leaf-only mass), not Total_Mass
for (sp in sp_list) {
  w[[paste0("prop_", sp)]] <- w[[sp]] / w$sort_sum_leaf
}
# guard against divide-by-zero baskets
w[ , paste0("prop_", sp_list)][is.na(w[ , paste0("prop_", sp_list)])] <- 0

# ---- 2. Aggregate baskets -> plot-level mean proportion per year ----
# each stand-plot-year gets one proportion (mean across baskets)
prop_cols <- paste0("prop_", sp_list)

plot_year <- w %>%
  group_by(Stand, Plot, staplo, Treatment, Lityear) %>%
  summarise(across(all_of(prop_cols), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

# ---- 3. Presence screen: species must occur in ALL 4 plots of a stand ----
# "present in a plot" = mean proportion > 0 in at least one year of that plot
presence <- plot_year %>%
  group_by(Stand, Plot) %>%
  summarise(across(all_of(prop_cols), ~ as.integer(any(.x > 0))),
            .groups = "drop") %>%
  group_by(Stand) %>%
  summarise(across(all_of(prop_cols), ~ sum(.x)),   # # plots present (0-4)
            .groups = "drop")

# a species is usable in a stand only if present in all 4 plots
usable <- presence %>%
  pivot_longer(all_of(prop_cols), names_to = "prop_sp", values_to = "n_plots") %>%
  mutate(species = sub("^prop_", "", prop_sp),
         full = n_plots == 4)

# summary table you asked for
presence_summary <- usable %>%
  select(Stand, species, n_plots, full) %>%
  arrange(species, Stand)

# stands where each species is present in all 4 plots
usable_by_species <- usable %>%
  filter(full) %>%
  count(species, name = "n_stands_full") %>%
  arrange(desc(n_stands_full))

print(usable_by_species)



############



###########################################################################


library(lme4)
library(lmerTest)
library(ggplot2)

species7 <- c("BEPA","ACPE","ACSA3","BEAL2","FAGR","ACRU","PRPE2")

age_lookup <- unique(lf[ , c("Stand","Age")])

# ============================================================
# 1. Loop species: proportions, presence screen, per-plot slopes
# ============================================================
all_coefs <- list()   # 52-row (or fewer) slope dataframe per species
pres_list <- list()   # presence tables





for (sp in species7) {
  pc <- paste0("prop_", sp)
  w[[pc]] <- w[[sp]] / w$sort_sum_leaf
  w[[pc]][is.na(w[[pc]])] <- 0
  
  # presence: species in all 4 plots of a stand
  pres <- aggregate(w[[pc]], by = list(Stand = w$Stand, Plot = w$Plot),
                    FUN = function(x) as.integer(any(x > 0)))
  names(pres)[3] <- "present"
  pres_tab <- aggregate(present ~ Stand, data = pres, FUN = sum)
  pres_tab$full <- pres_tab$present >= 4
  pres_list[[sp]] <- pres_tab
  
  keep_stands <- pres_tab$Stand[pres_tab$full]
  d <- w[w$Stand %in% keep_stands, ]
  d$staplo <- paste(d$Stand, d$Plot)
  
  # per-plot slope + intercept from raw baskets
  cf <- do.call(rbind, lapply(split(d, d$staplo), function(x) {
    if (length(unique(x$Lityear)) < 3) return(NULL)
    fit <- lm(x[[pc]] ~ x$Lityear)
    data.frame(species   = sp,
               staplo    = x$staplo[1],
               Stand     = x$Stand[1],
               Plot      = x$Plot[1],
               Treatment = x$Treatment[1],
               intercept = coef(fit)[1],
               slope     = coef(fit)[2],
               n_baskets = nrow(x),
               row.names = NULL)
  }))
  cf$Ntrmt <- factor(ifelse(cf$Treatment %in% c("N","NP"), "N", "NoN"))
  cf$Ptrmt <- factor(ifelse(cf$Treatment %in% c("P","NP"), "P", "NoP"))
  cf <- merge(cf, age_lookup, by = "Stand", all.x = TRUE)
  all_coefs[[sp]] <- cf
}

# ---- the raw 52-row slope dataframe (stacked for all 7 species) ----
slope_df <- do.call(rbind, all_coefs)
rownames(slope_df) <- NULL
print(slope_df)
# write.csv(slope_df, "per_plot_slopes_7species.csv", row.names = FALSE)

# how many plots per species (max 52 = 13 stands x 4)
print(table(slope_df$species))

# ggplot(slope_df, aes(x=Age, y=slope))+
#   geom_point(aes(color=Treatment),
#              position=position_dodge(.2)+
#   facet_wrap(~species)


# ============================================================
# 2. Faceted figures — one per species, faceted by Stand
# ============================================================

for (sp in species7) {
  pc <- paste0("prop_", sp)
  keep_stands <- pres_list[[sp]]$Stand[pres_list[[sp]]$full]
  dd <- w[w$Stand %in% keep_stands, ]
  
  p <- ggplot(dd[!dd$Treatment=="Ca",], aes(x = Lityear, y = .data[[pc]], col = Treatment)) +
    geom_point(alpha = 0.6) +
    scale_color_manual(values=c("black","blue","red","purple"))+
    geom_smooth(method = "lm", aes(group = Treatment), se = FALSE) +
    facet_wrap(~ Stand, scales = "free_y") +
    labs(title = sp, y = "Proportion of basket", x = "Litter year") +
    theme_bw()
  
  print(p)
   ggsave(paste0("facet_", sp, ".png"), p, width = 11, height = 7, dpi = 150)
}






# ============================================================
# 4. Stehman & Meredith models + p-value summary table
# ============================================================
pval_rows <- list()

for (sp in species7) {
  cf <- all_coefs[[sp]]
  # need >1 stand and both treatment levels present to fit the factorial
  if (length(unique(cf$Stand)) < 2) next
  
  mod <- lmer(slope ~ Ntrmt * Ptrmt + Age + (1 | Stand), data = cf)
  a <- anova(mod)   # lmerTest Satterthwaite F-tests
  
  pval_rows[[sp]] <- data.frame(
    species  = sp,
    n_plots  = nrow(cf),
    n_stands = length(unique(cf$Stand)),
    p_N      = a["Ntrmt",       "Pr(>F)"],
    p_P      = a["Ptrmt",       "Pr(>F)"],
    p_NxP    = a["Ntrmt:Ptrmt", "Pr(>F)"],
    p_Age    = a["Age",         "Pr(>F)"],
    row.names = NULL)
}

pval_table <- do.call(rbind, pval_rows)
pval_table[ , 4:7] <- round(pval_table[ , 4:7], 4)
print(pval_table)
# write.csv(pval_table, "slope_pvalues_7species.csv", row.names = FALSE)




##############


# ============================================================
# 3. Integrative slope-comparison graph
#    x = slope (0 centered), each plot a point, species on y, colored by treatment
# ============================================================
# raw slopes centered on 0
p_int <- ggplot(slope_df, aes(x = slope, y = species, col = Treatment)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_jitter(height = 0.15, width = 0, alpha = 0.7, size = 2) +
  scale_color_manual(values=c("black","blue","red","purple"))+
  stat_summary(fun = mean, geom = "point", shape = 18, size = 6,
               color = "cyan", aes(group = species)) +

  labs(title = "Per-plot slopes of litter proportion over time",
       subtitle = "Each point = one treatment plot; diamond = species mean; 0 = no trend",
       x = "Slope (change in basket proportion per year)", y = "Species") +
  theme_bw()
print(p_int)

# normalized version: slope / SD of slopes within species (comparable scale)
slope_df$slope_z <- ave(slope_df$slope, slope_df$species,
                        FUN = function(x) (x - mean(x)) / sd(x))
p_intz <- ggplot(slope_df, aes(x = slope_z, y = species, col = Treatment)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_jitter(height = 0.15, width = 0, alpha = 0.7, size = 2) +
  labs(title = "Normalized per-plot slopes (z-scored within species)",
       x = "Slope (SD units, 0 = species mean)", y = "Species") +
  theme_bw()
print(p_intz)

##############

################################################################################


# ============================================================
# Integrative slope graph + p-value table
# ============================================================
# 0-centered slope comparison, each plot a point
p_int <- ggplot(slope_df, aes(x = slope, y = species, col = Treatment)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_jitter(height = 0.15, width = 0, alpha = 0.7, size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
               color = "black", aes(group = species)) +
  labs(title = "Per-plot slopes of litter proportion over time (plot-level fit)",
       subtitle = "Each point = one plot; diamond = species mean; 0 = no trend",
       x = "Slope (proportion change per year)", y = "Species") +
  theme_bw()
print(p_int)

# p-value summary
pval_rows <- list()
for (sp in species7) {
  cf <- all_coefs[[sp]]
  if (length(unique(cf$Stand)) < 2) next
  mod <- lmer(slope ~ Ntrmt * Ptrmt + Age + (1 | Stand), data = cf)
  a <- anova(mod)
  pval_rows[[sp]] <- data.frame(
    species  = sp,
    n_plots  = nrow(cf),
    n_stands = length(unique(cf$Stand)),
    p_N      = a["Ntrmt","Pr(>F)"],
    p_P      = a["Ptrmt","Pr(>F)"],
    p_NxP    = a["Ntrmt:Ptrmt","Pr(>F)"],
    p_Age    = a["Age","Pr(>F)"],
    row.names = NULL)
}
pval_table <- do.call(rbind, pval_rows)
pval_table[ , 4:7] <- round(pval_table[ , 4:7], 4)
print(pval_table)
# write.csv(pval_table, "slope_pvalues_plotlevel_7species.csv", row.names = FALSE)



