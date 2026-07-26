library(ggplot2)

table(w$Stand, w$Plot)

species7 <- c("BEPA","ACPE","ACSA3","BEAL2","FAGR","ACRU","PRPE2")


w[w$Plot==7, "Plot"] <- 4
# ============================================================
# 1. Plot-year mean mass: total, and each species
#    (average across the 5 baskets, in mass units)
# ============================================================
grp <- list(Stand = w$Stand, Plot = w$Plot,
            Treatment = w$Treatment, Lityear = w$Lityear)

# mean total sorted leaf mass per plot-year
pm <- aggregate(w$sort_sum_leaf, by = grp, FUN = mean, na.rm = TRUE)
names(pm)[ncol(pm)] <- "mean_total"

# mean mass of each species per plot-year
for (sp in species7) {
  a <- aggregate(w[[sp]], by = grp, FUN = mean, na.rm = TRUE)
  names(a)[ncol(a)] <- sp
  pm <- merge(pm, a, by = c("Stand","Plot","Treatment","Lityear"))
}

pm$staplo <- paste(pm$Stand, pm$Plot)

# "Other" = everything not in the 7 focal species
pm$Other <- pm$mean_total - rowSums(pm[ , species7], na.rm = TRUE)
pm$Other[pm$Other < 0] <- 0   # guard against rounding

# ============================================================
# 2. Proportion per year = mean species mass / mean total mass
# ============================================================
for (sp in c(species7, "Other")) {
  pm[[paste0("prop_", sp)]] <- pm[[sp]] / pm$mean_total
}
pm[ , paste0("prop_", c(species7,"Other"))][is.na(pm[ , paste0("prop_", c(species7,"Other"))])] <- 0

head(pm)
# write.csv(pm, "plot_year_species_proportions.csv", row.names = FALSE)

pm <- pm[!pm$Treatment=="Ca",]

# ============================================================
# 3. Long format for stacking
# ============================================================
prop_cols <- paste0("prop_", c(species7, "Other"))

long <- reshape(pm[ , c("Stand","Plot","Treatment","Lityear","staplo", prop_cols)],
                direction = "long",
                varying   = prop_cols,
                v.names   = "proportion",
                timevar   = "species",
                times     = c(species7, "Other"),
                idvar     = c("staplo","Lityear"))
rownames(long) <- NULL

# order species consistently; Other last
long$species <- factor(long$species, levels = c(species7, "Other"))

# ============================================================
# 4. Stacked bar, facet_wrap by Stand
# ============================================================
cols7 <- c(BEPA  = "#E69F00", ACPE  = "#56B4E9", ACSA3 = "#009E73",
           BEAL2 = "#F0E442", FAGR  = "#0072B2", ACRU  = "#D55E00",
           PRPE2 = "#CC79A7", Other = "grey70")

# --- (a) averaged across the 4 plots: one bar per stand-year ---
stand_yr <- aggregate(proportion ~ Stand + Lityear + species,
                      data = long, FUN = mean)

p_stand <- ggplot(stand_yr, aes(x = Lityear, y = proportion, fill = species)) +
  geom_col(width = 0.8) +
  facet_wrap(~ Stand, scales = "free_x") +
  scale_fill_manual(values = cols7) +
  labs(title = "Species composition of litterfall over time",
       subtitle = "Plot-level mean masses -> proportions, averaged across 4 plots per stand",
       x = "Litter year", y = "Proportion of sorted leaf mass") +
  theme_bw() +
  theme(legend.position = "bottom")
print(p_stand)

# --- (b) treatment kept separate: bars side by side within each year ---
p_trt <- ggplot(long, aes(x = factor(Lityear), y = proportion, fill = species)) +
  geom_col(width = 0.85, col="black") +
  facet_grid(Treatment ~ Stand, scales = "free_x") +
  scale_fill_manual(values = cols7) +
  labs(title = "Litterfall composition by stand and treatment",
       x = "Litter year", y = "Proportion of sorted leaf mass") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6))
print(p_trt)

# ggsave("stacked_composition_by_stand.png", p_stand, width = 12, height = 8, dpi = 150)
# ggsave("stacked_composition_by_stand_treatment.png", p_trt, width = 16, height = 9, dpi = 150)