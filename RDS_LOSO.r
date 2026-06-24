# ==============================================================================

# Data and citation

# ==============================================================================

# This code is provided to reproduce the analyses described in the manuscript.

# Readers may refer to the following report for more details on the original data

# collection and study context:

#

# Okech, D., Kelly, J., Mbaye Soukeye, M., Cronberg, A., Yi, H., Cody, A. M.,

# ... & Diagne Barre, A. (2022). Sex Trafficking in the Gold Mining Areas of

# Kédougou, Senegal: A Mixed-Methods Study Estimating Baseline Prevalence and

# Identifying Perceived Gaps in Prevention, Prosecution, and Protection Response.

# Center on Human Trafficking Research & Outreach, University of Georgia.

# https://doi.org/10.71927/uga.26739

#

# Note: The original study data are not included in this repository due to

# confidentiality and privacy considerations. A de-identified sample dataset is

# provided for demonstration purposes only.

# ==============================================================================

library(RDS)
library(sspse)
library(dplyr)
library(tidyr)
library(tibble)
library(survey)
library(ggplot2)
library(ggrepel)


data <- read.csv("sample_deidentified_data.csv")

x = which(data[,"Department"] == 1)
Kedougou_data = data[x,]
nrow(Kedougou_data)     
x = which(data[,"Department"] == 2)
Saraya_data = data[x,]
nrow(Saraya_data) 


sample.pro <- sum(data$victim==1)/nrow(data)  
K.pro <- sum(Kedougou_data$victim==1)/nrow(Kedougou_data)  
S.pro <- sum(Saraya_data$victim==1)/nrow(Saraya_data)  

rds <- as.rds.data.frame(
  data,
  id = "id",
  recruiter.id = "recruiter.id",
  network.size = "network.size",
  max.coupons = 3,
  time = "recruit.time",
  check.valid = TRUE
)

# Prevalence by seed
data_wave <- data %>%
  mutate(
    id = as.character(id),
    recruiter.id = as.character(recruiter.id),
    is_seed = recruiter.id == id | recruiter.id == "-1"
  )
data_wave$wave <- NA_integer_
data_wave$wave[data_wave$is_seed] <- 0

max_iter <- 50
for (i in 1:max_iter) {
  updated <- FALSE
  
  for (j in seq_len(nrow(data_wave))) {
    if (is.na(data_wave$wave[j])) {
      rid <- data_wave$recruiter.id[j]
      w_rec <- data_wave$wave[data_wave$id == rid]
      if (length(w_rec) == 1 && !is.na(w_rec)) {
        data_wave$wave[j] <- w_rec + 1
        updated <- TRUE
      }
    }
  }
  
  if (!updated) break
}

seed_raw <- data %>%
  mutate(seed = as.character(seed),
         Dept = ifelse(Department==1, "Kedougou", "Saraya")) %>%
  group_by(Dept, seed) %>%
  summarise(n=n(), raw_prev=mean(victim==1), .groups="drop")

p <- ggplot(seed_raw, aes(x = reorder(seed, raw_prev), y = raw_prev)) +
  geom_point() +
  facet_wrap(~Dept, scales="free_x") +
  # geom_point(data = seed_raw %>% filter(seed %in% c("2","6")),
  #            size = 3) +
  coord_flip() +
  labs(x="Seed", y="Observed prevalence", title="Observed prevalence by seed") +
  theme_minimal()
ggsave("raw_pre_byseed.pdf", plot = p, width = 7, height = 3)


# Homophily
edges <- data %>%
  mutate(
    id_chr = as.character(id),
    recruiter_chr = as.character(recruiter.id)
  ) %>%
  filter(!is.na(recruiter_chr),
         recruiter_chr %in% id_chr,
         recruiter_chr != id_chr) %>%
  left_join(
    data %>%
      transmute(
        recruiter_chr = as.character(id),
        recruiter_victim = victim,
        recruiter_dept = Department
      ),
    by = "recruiter_chr"
  ) %>%
  transmute(
    recruit_id = id_chr,
    recruiter_id = recruiter_chr,
    seed = seed,            
    recruit_victim = victim,
    recruiter_victim = recruiter_victim,
    recruit_dept = Department,
    recruiter_dept = recruiter_dept
  ) %>%
  filter(!is.na(recruit_victim), !is.na(recruiter_victim))


## Function: VRV / NRV 
calc_vrv_nrv <- function(df) {
  df %>%
    summarise(
      # denominators
      edges_from_victim = sum(recruiter_victim == 1),
      edges_from_nonvictim = sum(recruiter_victim == 0),
      
      # numerators
      victim_recruited_by_victim =
        sum(recruiter_victim == 1 & recruit_victim == 1),
      
      victim_recruited_by_nonvictim =
        sum(recruiter_victim == 0 & recruit_victim == 1),
      
      # probabilities
      vrv = if (edges_from_victim > 0)
        victim_recruited_by_victim / edges_from_victim else NA_real_,
      
      nrv = if (edges_from_nonvictim > 0)
        victim_recruited_by_nonvictim / edges_from_nonvictim else NA_real_
    )
}

res_all <- calc_vrv_nrv(edges) %>%
  mutate(stratum = "All regions")

res_ked <- calc_vrv_nrv(
  edges %>% filter(recruiter_dept == 1, recruit_dept == 1)
) %>%
  mutate(stratum = "Kedougou (Dept=1)")

res_sar <- calc_vrv_nrv(
  edges %>% filter(recruiter_dept == 2, recruit_dept == 2)
) %>%
  mutate(stratum = "Saraya (Dept=2)")

results <- bind_rows(res_all, res_ked, res_sar) %>%
  select(
    stratum,
    edges_from_victim,
    victim_recruited_by_victim,
    vrv,
    edges_from_nonvictim,
    victim_recruited_by_nonvictim,
    nrv
  )

results


## Homophily be seed
calc_seed_vrv_nrv <- function(df_edges) {
  df_edges %>%
    summarise(
      n_edges = n(),
      edges_from_victim = sum(recruiter_victim == 1),
      edges_from_nonvictim = sum(recruiter_victim == 0),
      
      victim_recruited_by_victim = sum(recruiter_victim == 1 & recruit_victim == 1),
      victim_recruited_by_nonvictim = sum(recruiter_victim == 0 & recruit_victim == 1),
      
      vrv = ifelse(edges_from_victim > 0,
                   victim_recruited_by_victim / edges_from_victim, NA_real_),
      nrv = ifelse(edges_from_nonvictim > 0,
                   victim_recruited_by_nonvictim / edges_from_nonvictim, NA_real_),
      
      diff = vrv - nrv,
      rr = ifelse(!is.na(vrv) & !is.na(nrv) & nrv > 0, vrv / nrv, NA_real_),
      
      or = ifelse(!is.na(vrv) & !is.na(nrv) & vrv < 1 & nrv < 1 & nrv > 0 & vrv > 0,
                  (vrv/(1-vrv)) / (nrv/(1-nrv)), NA_real_)
    )
}

seed_homophily <- edges %>%
  # within-region ties only
  filter(recruit_dept == recruiter_dept) %>%
  group_by(recruit_dept, seed) %>%
  group_modify(~ calc_seed_vrv_nrv(.x)) %>%
  ungroup() %>%
  mutate(
    region = ifelse(recruit_dept == 1, "Kedougou", "Saraya")
  )

print(seed_homophily)

p <- ggplot(seed_homophily, aes(x = nrv, y = vrv)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(aes(size = n_edges), alpha = 0.8) +
  scale_size_continuous(range = c(1, 5)) +   # <- smaller dots overall
  geom_text_repel(aes(label = seed), color = "blue", size = 3, show.legend = FALSE) +
  facet_wrap(~ region) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "NRV = P(recruit victim | recruiter non-victim)",
    y = "VRV = P(recruit victim | recruiter victim)",
    size = "# edges",
    title = "Recruitment homophily by seed: VRV vs NRV"
  ) +
  theme_minimal()
ggsave("homophily_byseed.pdf", plot = p, width = 7, height = 3)


# Estimate prevalence
## Bootstrap SH, Vh, and HCG
make_rds <- function(df, max_coupons = 3) {
  df <- df %>%
    mutate(
      id = as.character(id),
      recruiter.id = as.character(recruiter.id),
      network.size = as.numeric(network.size),
      victim = as.numeric(victim),
      recruit.time = as.POSIXct(recruit.time)
    )
  
  as.rds.data.frame(
    df,
    id = "id",
    recruiter.id = "recruiter.id",
    network.size = "network.size",
    max.coupons = max_coupons,
    time = "recruit.time",
    check.valid = TRUE
  )
}

subset_region_within <- function(df, dept_val, seed_recruiter_id = "-1") {
  sub <- df %>%
    filter(Department == dept_val) %>%
    mutate(
      id = as.character(id),
      recruiter.id = as.character(recruiter.id)
    )
  
  ids <- unique(sub$id)
  
  sub %>%
    mutate(
      recruiter.id = ifelse(!is.na(recruiter.id) & recruiter.id %in% ids,
                            recruiter.id, seed_recruiter_id)
    )
}


one_method <- function(rds_obj,
                       outcome = "victim",
                       method = c("RDS-II", "RDS-I", "HCG"),
                       N_hcg = 10000,
                       B = 2000,
                       conf.level = 0.95) {
  
  method <- match.arg(method)
  
  # Point estimate object
  est_obj <- if (method == "RDS-II") {
    RDS.II.estimates(rds.data = rds_obj, outcome.variable = outcome)
  } else if (method == "RDS-I") {
    RDS.I.estimates(rds.data = rds_obj, outcome.variable = outcome)
  } else {
    RDS.HCG.estimates(rds.data = rds_obj, outcome.variable = outcome, N = N_hcg)
  }
  
  point <- as.numeric(est_obj$estimate[[outcome]])
  
  # Bootstrap intervals object (this is what you printed as out$interval)
  boot_obj <- RDS.bootstrap.intervals(
    rds.data = rds_obj,
    outcome.variable = outcome,
    weight.type = method,                               # "RDS-II", "RDS-I", "HCG"
    uncertainty = if (method == "HCG") "HCG" else "Salganik",
    N = if (method == "HCG") N_hcg else NULL,
    number.of.bootstrap.samples = B,
    conf.level = conf.level
  )
  
  # In your version: boot_obj$interval is a matrix with columns: point/lower/upper/...
  lower <- as.numeric(boot_obj$interval[outcome, "lower"])
  upper <- as.numeric(boot_obj$interval[outcome, "upper"])
  
  # safety: enforce ordering
  ci_low <- min(lower, upper)
  ci_high <- max(lower, upper)
  
  tibble(
    method = dplyr::case_when(
      method == "RDS-II" ~ "VH (RDS-II)",
      method == "RDS-I"  ~ "SH (RDS-I)",
      TRUE               ~ "HCG"
    ),
    estimate = point,
    ci_low = ci_low,
    ci_high = ci_high,
    # optional extras if you want:
    se = as.numeric(boot_obj$interval[outcome, "s.e."]),
    design_effect = as.numeric(boot_obj$interval[outcome, "Design Effect"]),
    n = as.numeric(boot_obj$interval[outcome, "n"])
  )
}

run_stratum <- function(df, stratum_name,
                        outcome = "victim",
                        N_hcg = 30000,
                        B = 2000,
                        conf.level = 0.95,
                        max_coupons = 3) {
  
  rds_obj <- make_rds(df, max_coupons = max_coupons)
  
  bind_rows(
    one_method(rds_obj, outcome, "RDS-II", N_hcg, B, conf.level),
    one_method(rds_obj, outcome, "RDS-I",  N_hcg, B, conf.level),
    one_method(rds_obj, outcome, "HCG",    N_hcg, B, conf.level)
  ) %>%
    mutate(stratum = stratum_name) %>%
    select(stratum, method, estimate, ci_low, ci_high, se, design_effect, n)
}


set.seed(123)
B <- 2000
N_hcg <- 10000

### All regions
tab_all <- run_stratum(data, "All regions", N_hcg = N_hcg, B = B)

### Kedougou (Dept=1), within-region
df_k <- subset_region_within(data, dept_val = 1, seed_recruiter_id = "-1")
tab_k <- run_stratum(df_k, "Kedougou (Dept=1)", N_hcg = N_hcg, B = B)

### Saraya (Dept=2), within-region
df_s <- subset_region_within(data, dept_val = 2, seed_recruiter_id = "-1")
tab_s <- run_stratum(df_s, "Saraya (Dept=2)", N_hcg = N_hcg, B = B)

results_ci <- bind_rows(tab_all, tab_k, tab_s) %>%
  arrange(stratum, method)

print(results_ci)


# Sensitive Analysis
# Wave convergence
compute_wave <- function(df, seed_recruiter_id = "-1", max_iter = 100) {
  df <- df %>%
    mutate(
      id = as.character(id),
      recruiter.id = as.character(recruiter.id),
      is_seed = recruiter.id == id | recruiter.id == seed_recruiter_id
    )
  
  df$wave <- NA_integer_
  df$wave[df$is_seed] <- 0L
  
  for (iter in 1:max_iter) {
    updated <- FALSE
    for (j in seq_len(nrow(df))) {
      if (is.na(df$wave[j])) {
        rid <- df$recruiter.id[j]
        w_rec <- df$wave[df$id == rid]
        if (length(w_rec) == 1 && !is.na(w_rec)) {
          df$wave[j] <- w_rec + 1L
          updated <- TRUE
        }
      }
    }
    if (!updated) break
  }
  df
}

make_rds <- function(df, max_coupons = 3, seed_recruiter_id = "-1") {
  df <- df %>%
    mutate(
      id = as.character(id),
      recruiter.id = as.character(recruiter.id),
      network.size = as.numeric(network.size),
      recruit.time = as.POSIXct(recruit.time)
    )
  
  ids <- unique(df$id)
  df <- df %>%
    mutate(
      recruiter.id = ifelse(!is.na(recruiter.id) & recruiter.id %in% ids,
                            recruiter.id, seed_recruiter_id)
    )
  
  as.rds.data.frame(
    df,
    id = "id",
    recruiter.id = "recruiter.id",
    network.size = "network.size",
    max.coupons = max_coupons,
    time = "recruit.time",
    check.valid = TRUE
  )
}

## Cumulative-by-wave estimates: Observed + SH + VH + HCG
estimate_by_wave <- function(df, outcome = "victim", N_hcg = 30000,
                             max_coupons = 3, seed_recruiter_id = "-1") {
  
  if (!("wave" %in% names(df))) {
    df <- compute_wave(df, seed_recruiter_id = seed_recruiter_id)
  }
  
  df <- df %>%
    mutate(
      victim = as.numeric(.data[[outcome]]),
      wave = as.integer(wave)
    ) %>%
    filter(!is.na(wave))
  
  maxw <- max(df$wave, na.rm = TRUE)
  
  out <- vector("list", length = maxw + 1)
  
  for (w in 0:maxw) {
    sub <- df %>% filter(wave <= w)
    
    obs <- mean(sub$victim == 1, na.rm = TRUE)
    
    rds_sub <- make_rds(sub, max_coupons = max_coupons, seed_recruiter_id = seed_recruiter_id)
    
    est_sh  <- as.numeric(RDS.I.estimates(rds_sub,  outcome.variable = outcome)$estimate[[outcome]])
    est_vh  <- as.numeric(RDS.II.estimates(rds_sub, outcome.variable = outcome)$estimate[[outcome]])
    est_hcg <- as.numeric(RDS.HCG.estimates(rds_sub, outcome.variable = outcome, N = N_hcg)$estimate[[outcome]])
    
    out[[w + 1]] <- tibble(
      wave = w,
      Observed = obs,
      SH = est_sh,
      VH = est_vh,
      HCG = est_hcg
    )
  }
  
  bind_rows(out)
}


method_levels <- c("Observed", "SH", "VH", "HCG", "NE4NS")

method_colors <- c(
  Observed = "#4A4A4A",  # dark gray
  SH       = "#00B5F7",  # bright blue
  VH       = "#FF9F1C",  # vivid orange
  HCG      = "#B5179E",   # vivid purple-magenta
  NE4NS    = "#00C853"  # vivid green
)
method_labels <- c(
  Observed = "Observed",
  SH       = "SH",
  VH       = "VH",
  HCG      = "HCG",
  NE4NS    = "NE4NS"
)

wave_est <- estimate_by_wave(data, outcome = "victim", N_hcg = N_hcg)

wave_long <- wave_est %>%
  pivot_longer(cols = c("Observed", "SH", "VH", "HCG"),
               names_to = "series", values_to = "prev") %>%
  mutate(series = factor(series, levels = c("Observed", "SH", "VH", "HCG")))

p <- ggplot(wave_long, aes(x = wave, y = prev, color = series)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2)+
  scale_color_manual(
    values = method_colors
  )  +
  scale_x_continuous(
    breaks = sort(unique(wave_long$wave))
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Wave",
    y = "Prevalence estimate",
    color = "Series",
    title = "Convergence plot by wave: Observed vs SH/VH/HCG"
  ) +
  theme_minimal()
ggsave("wave_convergence_both.pdf", plot = p, width = 4, height = 3)


## Run wave estimates for each region
wave_est_k <- estimate_by_wave(data %>% filter(Department == 1),
                               outcome = "victim", N_hcg = N_hcg) %>%
  mutate(region = "Kedougou")

wave_est_s <- estimate_by_wave(data %>% filter(Department == 2),
                               outcome = "victim", N_hcg = N_hcg) %>%
  mutate(region = "Saraya")

wave_long_region <- bind_rows(wave_est_k, wave_est_s) %>%
  pivot_longer(cols = c("Observed", "SH", "VH", "HCG"),
               names_to = "series", values_to = "prev") %>%
  mutate(series = factor(series, levels = c("Observed", "SH", "VH", "HCG")))

max_wave <- max(wave_long_region$wave, na.rm = TRUE)
p <- ggplot(wave_long_region, aes(x = wave, y = prev, color = series)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = method_colors
  ) +
  scale_x_continuous(breaks = 0:max_wave) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~ region, ncol = 2) +
  labs(
    x = "Wave",
    y = "Prevalence estimate",
    color = "Series",
    title = "Convergence by wave: Observed vs SH/VH/HCG (by region)"
  ) +
  theme_minimal()
ggsave("wave_convergence_region.pdf", plot = p, width =6, height = 3)


# Seed convergence
one_chain_est <- function(df_chain, outcome="victim", N_hcg = 30000){
  # raw prevalence
  raw_prev <- mean(df_chain[[outcome]] == 1, na.rm = TRUE)
  
  # build rds object (for this chain only)
  rds_chain <- make_rds(df_chain)
  
  # RDS estimates
  est_sh  <- as.numeric(RDS.I.estimates(rds_chain, outcome.variable=outcome)$estimate[[outcome]])
  est_vh  <- as.numeric(RDS.II.estimates(rds_chain, outcome.variable=outcome)$estimate[[outcome]])
  est_hcg <- as.numeric(RDS.HCG.estimates(rds_chain, outcome.variable=outcome, N=N_hcg)$estimate[[outcome]])
  
  tibble(
    n = nrow(df_chain),
    raw = raw_prev,
    SH = est_sh,
    VH = est_vh,
    HCG = est_hcg
  )
}


seed_results_all <- data %>%
  mutate(seed = as.character(seed)) %>%
  group_by(seed) %>%
  group_modify(~ one_chain_est(.x, outcome="victim", N_hcg=N_hcg)) %>%
  ungroup() %>%
  mutate(stratum = "Both regions")

seed_results_k <- data %>%
  filter(Department == 1) %>%
  mutate(seed = as.character(seed)) %>%
  group_by(seed) %>%
  group_modify(~ one_chain_est(.x, outcome="victim", N_hcg=N_hcg)) %>%
  ungroup() %>%
  mutate(stratum = "Kedougou")

seed_results_s <- data %>%
  filter(Department == 2) %>%
  mutate(seed = as.character(seed)) %>%
  group_by(seed) %>%
  group_modify(~ one_chain_est(.x, outcome="victim", N_hcg=N_hcg)) %>%
  ungroup() %>%
  mutate(stratum = "Saraya")

seed_results <- bind_rows(seed_results_all, seed_results_k, seed_results_s)

seed_results


seed_order <- seed_results %>%
  filter(stratum == "Both regions") %>%   
  arrange(raw) %>%
  pull(seed)

seed_long <- seed_results %>%
  pivot_longer(cols = c(raw, SH, VH, HCG),
               names_to = "method", values_to = "prev") %>%
  mutate(
    method = factor(method, levels = c("raw","SH","VH","HCG")),
    seed = factor(seed, levels = seed_order)
  )

ggplot(seed_long, aes(x = prev, y = seed, color = method)) +
  geom_point(size = 2) +
  facet_wrap(~ stratum, scales = "free_y") +
  scale_x_continuous(limits = c(0, 1)) +
  labs(x="Prevalence within seed chain", y="Seed chain", color="Method") +
  theme_minimal()


## LOSO（Leave-One-Seed-Out）
make_rds_subset <- function(df, max_coupons = 3, seed_recruiter_id = "-1") {
  df <- df %>%
    mutate(
      id = as.character(id),
      recruiter.id = as.character(recruiter.id),
      network.size = as.numeric(network.size),
      victim = as.numeric(victim),
      recruit.time = as.POSIXct(recruit.time)
    )
  
  ids <- unique(df$id)
  
  df <- df %>%
    mutate(
      recruiter.id = ifelse(!is.na(recruiter.id) & recruiter.id %in% ids,
                            recruiter.id, seed_recruiter_id)
    )
  
  as.rds.data.frame(
    df,
    id = "id",
    recruiter.id = "recruiter.id",
    network.size = "network.size",
    max.coupons = max_coupons,
    time = "recruit.time",
    check.valid = TRUE
  )
}


run_estimators <- function(df, outcome = "victim", N_hcg = 30000,
                           max_coupons = 3, seed_recruiter_id = "-1") {
  
  rds_obj <- make_rds_subset(df, max_coupons = max_coupons, seed_recruiter_id = seed_recruiter_id)
  
  est_sh  <- as.numeric(RDS.I.estimates(rds_obj,  outcome.variable = outcome)$estimate[[outcome]])
  est_vh  <- as.numeric(RDS.II.estimates(rds_obj, outcome.variable = outcome)$estimate[[outcome]])
  est_hcg <- as.numeric(RDS.HCG.estimates(rds_obj, outcome.variable = outcome, N = N_hcg)$estimate[[outcome]])
  
  tibble(
    method = c("SH", "VH", "HCG"),
    estimate = c(est_sh, est_vh, est_hcg)
  )
}


loso_by_seed <- function(df, stratum_name,
                         outcome = "victim", N_hcg = 10000,
                         max_coupons = 3, seed_recruiter_id = "-1") {
  
  df <- df %>% mutate(seed = as.character(seed))
  
  # baseline (full stratum)
  base <- run_estimators(df, outcome = outcome, N_hcg = N_hcg,
                         max_coupons = max_coupons, seed_recruiter_id = seed_recruiter_id) %>%
    rename(base_est = estimate)
  
  seeds <- sort(unique(df$seed))
  
  # leave one seed out each time
  out_list <- lapply(seeds, function(s) {
    df_sub <- df %>% filter(seed != s)
    
    # handle potential errors gracefully
    res <- try(run_estimators(df_sub, outcome = outcome, N_hcg = N_hcg,
                              max_coupons = max_coupons, seed_recruiter_id = seed_recruiter_id),
               silent = TRUE)
    
    if (inherits(res, "try-error")) {
      return(tibble(
        stratum = stratum_name,
        left_out_seed = s,
        n_remaining = nrow(df_sub),
        method = c("SH", "VH", "HCG"),
        estimate = NA_real_,
        error = TRUE
      ))
    } else {
      return(res %>%
               mutate(
                 stratum = stratum_name,
                 left_out_seed = s,
                 n_remaining = nrow(df_sub),
                 error = FALSE
               ))
    }
  })
  
  loso <- bind_rows(out_list) %>%
    left_join(base, by = "method") %>%
    mutate(
      delta = estimate - base_est,
      rel_delta = delta / base_est
    ) %>%
    select(stratum, left_out_seed, n_remaining, method, estimate, base_est, delta, rel_delta, error)
  
  loso
}

## Run LOSO for All / Kedougou / Saraya
loso_all <- loso_by_seed(data, "All regions", N_hcg = N_hcg)

loso_k <- loso_by_seed(data %>% filter(Department == 1),
                       "Kedougou (Dept=1)", N_hcg = N_hcg)

loso_s <- loso_by_seed(data %>% filter(Department == 2),
                       "Saraya (Dept=2)", N_hcg = N_hcg)

loso_res <- bind_rows(loso_all, loso_k, loso_s)

method_order <- c("SH", "VH", "HCG")

p1 <- loso_res %>%
  filter(!error, method %in% method_order) %>%
  mutate(
    left_out_seed_num = as.numeric(as.character(left_out_seed)),
    method = factor(method, levels = method_order)
  ) %>%
  ggplot(aes(x = left_out_seed_num, y = delta, color = method)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  facet_grid(method ~ stratum, scales = "free_y") +
  scale_color_manual(
    values = method_colors,
    breaks = method_order
  ) +
  scale_x_continuous(
    breaks = sort(unique(as.numeric(as.character(loso_res$left_out_seed))))
  ) +
  labs(
    x = "Left-out seed",
    y = "Change in estimate (LOSO minus baseline)",
    color = "Method",
    title = "LOSO sensitivity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

print(p1)
ggsave("LOSO.pdf", plot = p1, width = 7, height = 5)


