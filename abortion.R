library(caper); library(tidyverse); library(ape); library(brms); library(sf); library(patchwork)
library(ggtree); library(ggtreeExtra); library(ggnewscale)

d <- read_csv("abortion_societies.csv", show_col_types = FALSE) %>%
  mutate(evidence = factor(evidence, c("practice", "knowledge", "denied", "none")),
         coverage = log1p(n_other), mobility = -fixity_sccs150,
         restrict = case_when(premarital_sccs282 == 2 ~ 2, premarital_sccs282 %in% 3:4 ~ 1, premarital_sccs282 %in% 5:6 ~ 0),
         payment = as.integer(transactions_sccs208 %in% 1:2),
         sx = cos(latitude * pi / 180) * cos(longitude * pi / 180), sy = cos(latitude * pi / 180) * sin(longitude * pi / 180),
         sz = sin(latitude * pi / 180))
tr <- read.tree("abortion_tree.nwk")

# descriptives

d %>% count(evidence)
d %>% group_by(region) %>% summarise(n = n(), abortion = sum(abortion), practice = sum(practice), physical = sum(physical), substance = sum(substance))
themes <- c("physical", "substance", "ritual", "unmarried", "adultery", "spacing", "support", "coercion",
            "death", "harm", "secrecy", "disapproval", "specialist", "foetus", "change", "conflict")
d %>% summarise(across(all_of(themes), sum)) %>% pivot_longer(everything())
d %>% filter(practice == 1) %>% summarise(n = n(), paternity = sum(unmarried | adultery | support),
                                          any_reason = sum(unmarried | adultery | support | spacing | coercion))

# frayser (1985) codes
d %>% filter(!is.na(frayser_approval_sccs954) | !is.na(frayser_reason_sccs955)) %>%
  select(society, evidence, unmarried, adultery, spacing, support, frayser_approval_sccs954, frayser_reason_sccs955)

# figure 1: map

ev_cols <- c(practice = "#D55E00", knowledge = "#F4A582", denied = "#8C8C8C", none = "#E3E3E3")
ev_labs <- c(practice = "practice reported", knowledge = "known, no case reported",
             denied = "stated to be absent", none = "no clear evidence")
ev_shp <- c(practice = 21, knowledge = 24, denied = 22, none = 23)
lab <- sprintf("%s (%d)", ev_labs, table(d$evidence))
moll <- "+proj=moll"; laea <- "+proj=laea +lat_0=52 +lon_0=-112"
world <- st_as_sf(rnaturalearthdata::countries110)
grat <- st_graticule(lat = seq(-60, 60, 30), lon = seq(-180, 180, 60))
pts <- st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
xy <- function(crs) { z <- st_coordinates(st_transform(pts, crs)); bind_cols(d, x = z[, 1], y = z[, 2]) }
dm <- xy(moll); dl <- xy(laea)
na <- d$longitude < -50 & d$latitude > 25

p1a <- ggplot() +
  geom_sf(data = st_transform(grat, moll), colour = "#E6E4E0", linewidth = 0.2) +
  geom_sf(data = st_transform(world, moll), fill = "#F0EFEC", colour = "#C4C1BB", linewidth = 0.15) +
  annotate("rect", xmin = min(dm$x[na]) - 4e5, xmax = max(dm$x[na]) + 4e5, ymin = min(dm$y[na]) - 4e5,
           ymax = max(dm$y[na]) + 4e5, fill = NA, colour = "grey40", linetype = "dashed", linewidth = 0.3) +
  geom_point(data = dm, aes(x, y, fill = evidence, shape = evidence), size = 2.6, stroke = 0.35) +
  ggrepel::geom_text_repel(data = dm[!na, ], aes(x, y, label = society), size = 2.7, colour = "#333333",
                           segment.colour = "grey60", segment.size = 0.2, box.padding = 0.3, seed = 4) +
  scale_fill_manual(values = ev_cols, labels = lab) + scale_shape_manual(values = ev_shp, labels = lab) +
  coord_sf(crs = moll, xlim = c(-16.5e6, 15.5e6), ylim = c(-6.6e6, 8.4e6), expand = FALSE) +
  labs(fill = NULL, shape = NULL, caption = "Mollweide equal-area projection") +
  theme_void(base_size = 10) +
  theme(legend.position = "inside", legend.position.inside = c(0.14, 0.24), legend.text = element_text(size = 9.5),
        plot.caption = element_text(hjust = 0.5, colour = "grey45", size = 8))
p1b <- ggplot() +
  geom_sf(data = st_transform(world, laea), fill = "#F0EFEC", colour = "#C4C1BB", linewidth = 0.15) +
  geom_point(data = dl[na, ], aes(x, y, fill = evidence, shape = evidence), size = 2.6, stroke = 0.35, show.legend = FALSE) +
  ggrepel::geom_text_repel(data = dl[na, ], aes(x, y, label = society), size = 2.8, colour = "#333333", segment.colour = "grey60",
                           segment.size = 0.2, box.padding = 0.35, min.segment.length = 0, seed = 2) +
  scale_fill_manual(values = ev_cols) + scale_shape_manual(values = ev_shp) +
  coord_sf(crs = laea, xlim = c(-3.3e6, 3.6e6), ylim = c(-2.9e6, 2.4e6), expand = FALSE) +
  labs(title = "North America") + theme_void(base_size = 10) +
  theme(panel.border = element_rect(fill = NA, colour = "grey40", linewidth = 0.3), plot.title = element_text(hjust = 0.5, size = 11))
p1 <- p1a + p1b + plot_layout(widths = c(2.15, 1))
ggsave("abortion_fig1_map.tiff", p1, width = 15, height = 5.8, dpi = 400, bg = "white", compression = "lzw")
ggsave("abortion_fig1_map.pdf", p1, width = 15, height = 5.8)

# figure 2: themes

th <- tribble(~name, ~label, ~group,
  "physical", "physical methods", "methods",
  "substance", "ingested or applied substances", "methods",
  "ritual", "ritual or magical means", "methods",
  "unmarried", "unmarried woman", "circumstances",
  "adultery", "adultery or illicit affair", "circumstances",
  "spacing", "spacing or limiting births", "circumstances",
  "support", "husband or father absent, dead or unsupportive", "circumstances",
  "coercion", "imposed by husband", "circumstances",
  "death", "risk of death to mother", "consequences and social context",
  "harm", "death or serious harm", "consequences and social context",
  "secrecy", "secrecy or concealment", "consequences and social context",
  "disapproval", "disapproval or sanction", "consequences and social context",
  "specialist", "specialist practitioner", "consequences and social context",
  "foetus", "disposal of aborted foetus described", "consequences and social context",
  "change", "change after contact", "consequences and social context")
fd <- d %>% summarise(across(all_of(th$name), sum)) %>% pivot_longer(everything(), values_to = "n") %>%
  left_join(th, by = "name") %>% mutate(group = factor(group, unique(th$group)), label = fct_reorder(label, n))
p2 <- ggplot(fd, aes(n, label, fill = group)) + geom_col(width = 0.7) +
  geom_text(aes(label = n), hjust = -0.4, size = 3.3) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c(methods = "#0072B2", circumstances = "#CC79A7", "consequences and social context" = "#D55E00"), guide = "none") +
  scale_x_continuous(limits = c(0, 21), breaks = seq(0, 20, 5), expand = c(0, 0)) +
  labs(x = paste0("societies (of ", nrow(d), ")"), y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        strip.text.y = element_text(angle = 0, hjust = 0, face = "bold"), axis.text.y = element_text(colour = "black"))
ggsave("abortion_fig2_themes.tiff", p2, width = 9, height = 5.5, dpi = 400, bg = "white", compression = "lzw")
ggsave("abortion_fig2_themes.pdf", p2, width = 9, height = 5.5)

# figure 3: phylogeny

reg_cols <- c("Africa" = "#B7950B", "Asia" = "#E67E22", "North America" = "#8E44AD",
              "Oceania" = "#34495E", "South America" = "#D81B60")
rings <- c(physical = "physical", substance = "substance", unmarried = "unmarried", adultery = "adultery",
           spacing = "spacing", death = "risk of death")
ring_cols <- c(physical = "#0072B2", substance = "#56B4E9", unmarried = "#CC79A7", adultery = "#882255",
               spacing = "#009E73", "risk of death" = "#000000")
tipd <- d %>% transmute(label = tip, society, region)
rd <- d %>% transmute(label = tip, across(all_of(names(rings)))) %>%
  pivot_longer(-label, names_to = "ring", values_to = "v") %>%
  mutate(ring = factor(rings[ring], rings), state = if_else(v == 1, as.character(ring), "not recorded"))
ed <- d %>% transmute(label = tip, ring = "evidence", ev = evidence)

p3 <- ggtree(tr, layout = "fan", open.angle = 20, branch.length = "none", linewidth = 0.4, colour = "grey55") %<+% tipd +
  geom_tippoint(aes(colour = region), size = 2.6) +
  scale_colour_manual(values = reg_cols, name = "Region", guide = guide_legend(order = 3)) +
  new_scale_fill() +
  geom_fruit(data = ed, geom = geom_tile, mapping = aes(y = label, x = ring, fill = ev),
             offset = 0.08, width = 1.3, colour = "white", linewidth = 0.4,
             axis.params = list(axis = "x", text.angle = -90, text.size = 3, hjust = 0, vjust = 0.5, line.colour = NA)) +
  scale_fill_manual(values = ev_cols, labels = ev_labs, name = "Evidence", guide = guide_legend(order = 1)) +
  new_scale_fill() +
  geom_fruit(data = rd, geom = geom_tile, mapping = aes(y = label, x = ring, fill = state),
             offset = 0.03, pwidth = 0.42, colour = "white", linewidth = 0.4,
             axis.params = list(axis = "x", text.angle = -90, text.size = 3, hjust = 0, vjust = 0.5, line.colour = NA)) +
  scale_fill_manual(values = c(ring_cols, "not recorded" = "#EDEDED"), breaks = unname(c(rings, "not recorded")),
                    name = "Recorded", guide = guide_legend(order = 2)) +
  geom_tiplab(aes(label = society), size = 3.6, offset = 11.6, colour = "black") +
  expand_limits(x = 35) +
  theme(legend.position = "right", legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 12), legend.key.size = unit(0.6, "cm"), plot.margin = margin(10, 10, 10, 10))
ggsave("abortion_fig3_phylogeny.tiff", p3, width = 13, height = 11, dpi = 400, bg = "white", compression = "lzw")
ggsave("abortion_fig3_phylogeny.pdf", p3, width = 13, height = 11)

# phylogenetic signal

A <- vcv(tr, corr = TRUE)
dstat <- function(v, dat = d) {
  dd <- dat %>% transmute(tip, x = .data[[v]]) %>% as.data.frame()
  set.seed(1); r <- phylo.d(comparative.data(tr, dd, tip), binvar = x, permut = 5000)
  tibble(trait = v, n = sum(dd$x), d = r$DEstimate, p_random = r$Pval1, p_brownian = r$Pval0)
}
sig <- map_dfr(c("abortion", "practice", "physical", "substance", "unmarried"), dstat)
sig

pri <- c(prior(normal(0, 1.5), class = Intercept), prior(normal(0, 1), class = b),
         prior(exponential(2), class = sd), prior(exponential(2), class = sdgp))
phy0 <- brm(abortion ~ 1 + (1 | gr(tip, cov = A)), data = d, data2 = list(A = A), family = bernoulli,
            prior = pri[c(1, 3), ], chains = 4, iter = 4000, seed = 1, control = list(adapt_delta = 0.99))
icc <- as_draws_df(phy0) %>% transmute(icc = sd_tip__Intercept^2 / (sd_tip__Intercept^2 + pi^2 / 3))
c(median = median(icc$icc), quantile(icc$icc, c(0.05, 0.95)))
set.seed(1); pr <- rexp(1e5, 2)^2; quantile(pr / (pr + pi^2 / 3), c(0.5, 0.05, 0.95))

# socioecological models

preds <- c(mobility = "mobility", weaning_sccs45 = "weaning age", starvation_sccs1263 = "seasonal starvation",
           polygyny_sccs872 = "polygyny", femcontrib_sccs826 = "women's subsistence contribution",
           restrict = "premarital restrictiveness", payment = "bridewealth or bride-service", coverage = "ethnographic coverage")

fitm <- function(y, x, dat = d) {
  dd <- dat %>% drop_na(all_of(c(y, x))) %>% mutate(yy = .data[[y]], xx = as.numeric(scale(.data[[x]])), cov = as.numeric(scale(coverage)))
  if (x == "coverage") dd$cov <- 0
  a <- vcv(keep.tip(tr, dd$tip), corr = TRUE)
  if (!exists("tmpl")) return(tmpl <<- brm(yy ~ xx + cov + (1 | gr(tip, cov = a)) + gp(sx, sy, sz, scale = FALSE),
                                           data = dd, data2 = list(a = a), family = bernoulli, prior = pri,
                                           chains = 4, iter = 4000, seed = 1, control = list(adapt_delta = 0.99)))
  update(tmpl, newdata = dd, data2 = list(a = a), recompile = FALSE, seed = 1, control = list(adapt_delta = 0.99))
}

ms <- expand_grid(y = c("abortion", "practice"), x = names(preds)) %>% mutate(fit = map2(y, x, fitm))

summ <- function(ms) ms %>% mutate(n = map_int(fit, nobs), b = map(fit, ~ as_draws_df(.x)$b_xx),
  est = map_dbl(b, median), lo = map_dbl(b, ~ quantile(.x, 0.05)), hi = map_dbl(b, ~ quantile(.x, 0.95)),
  pd = map_dbl(b, ~ max(mean(.x > 0), mean(.x < 0))),
  div = map_dbl(fit, ~ sum(nuts_params(.x)$Value[nuts_params(.x)$Parameter == "divergent__"])),
  rhat = map_dbl(fit, ~ max(rhat(.x), na.rm = TRUE))) %>%
  select(y, x, n, est, lo, hi, pd, div, rhat)
res <- summ(ms)
res
write_csv(res, "abortion_models.csv")

# early marriage as most or least restrictive
d <- d %>% mutate(restrict_hi = if_else(premarital_sccs282 == 1, 2, restrict), restrict_lo = if_else(premarital_sccs282 == 1, 0, restrict))
rese <- expand_grid(y = c("abortion", "practice"), x = c("restrict_hi", "restrict_lo")) %>% mutate(fit = map2(y, x, fitm)) %>% summ()
rese

# figure 4: coverage, women's contribution and model estimates

strip <- function(v, xl) {
  ggplot(drop_na(d, all_of(v)), aes(.data[[v]], evidence)) +
    stat_summary(fun = median, fun.min = median, fun.max = median, geom = "crossbar", orientation = "y",
                 width = 0.55, linewidth = 0.3, colour = "grey30") +
    geom_point(aes(fill = evidence, shape = evidence), size = 2.4, stroke = 0.35,
               position = position_jitter(width = 0, height = 0.12, seed = 1), show.legend = FALSE) +
    scale_fill_manual(values = ev_cols) + scale_shape_manual(values = ev_shp) +
    scale_y_discrete(limits = rev, labels = ev_labs) +
    labs(x = xl, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), axis.text = element_text(colour = "black"))
}
p4a <- strip("n_other", "other paragraphs returned by the search") +
  scale_x_continuous(transform = "log1p", breaks = c(0, 1, 2, 5, 10, 20, 50))
p4b <- strip("femcontrib_sccs826", "women's contribution to subsistence (%)")

fr <- res %>% mutate(x = factor(preds[x], rev(preds)),
                     y = factor(y, c("practice", "abortion"), c("practice reported", "any evidence of abortion")))
p4c <- ggplot(fr, aes(est, x, colour = y)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.55), size = 0.45, linewidth = 0.7) +
  geom_text(aes(x = 2.7, label = if_else(pd > 0.995, ">0.99", sprintf("%.2f", pd))), position = position_dodge(width = 0.55), size = 3.2, show.legend = FALSE) +
  annotate("text", x = 2.7, y = length(preds) + 0.6, label = "pd", fontface = "italic", size = 3.4) +
  scale_colour_manual(values = c("any evidence of abortion" = "#D55E00", "practice reported" = "#0072B2"),
                      breaks = c("any evidence of abortion", "practice reported"), name = NULL) +
  scale_x_continuous(limits = c(-2.2, 2.9), breaks = seq(-2, 2, 1)) +
  coord_cartesian(clip = "off") +
  labs(x = "log-odds per sd (median and 90% interval)", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        axis.text = element_text(colour = "black"))
p4 <- ((p4a / p4b) | p4c) + plot_layout(widths = c(1, 1.25)) + plot_annotation(tag_levels = "a")
ggsave("abortion_fig4_models.tiff", p4, width = 14, height = 6.5, dpi = 400, bg = "white", compression = "lzw")
ggsave("abortion_fig4_models.pdf", p4, width = 14, height = 6.5)

# focal communities only

df <- d %>% mutate(abortion = as.integer(evidence_focal %in% c("practice", "knowledge")),
                   practice = as.integer(evidence_focal == "practice"), coverage = log1p(n_other_focal))
df %>% count(evidence_focal)
df %>% group_by(abortion) %>% summarise(n_other = median(n_other_focal), femcontrib = median(femcontrib_sccs826, na.rm = TRUE))
map_dfr(c("abortion", "practice"), dstat, dat = df)
resf <- expand_grid(y = c("abortion", "practice"), x = names(preds)) %>% mutate(fit = map2(y, x, ~ fitm(.x, .y, df))) %>% summ()
resf
write_csv(resf, "abortion_models_focal.csv")
