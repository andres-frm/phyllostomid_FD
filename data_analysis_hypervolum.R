pks <- c('lubridate', 'tidyr', 'dplyr', 'forcats', 
         'cmdstanr', 'magrittr', 'readxl', 'ggplot2', 
         'tibble', 'rethinking', 'hypervolume')

source('functions_mod_diagnostics.r')

sapply(pks, FUN = function(x) library(x, character.only = T))

d <- as_tibble(read.csv('RawData.csv'))

d$date <- gsub('^(.*)(/)(.*)(/)(.*)$', '2016-\\3-\\1', d$date)
d$date <- date(d$date)
unique(month(d$date))

d$month <- month(d$date)

d %$% aggregate(sex ~ month + species + LU, FUN = length)
d %$% aggregate(sex ~ month + species, FUN = length)
d %$% aggregate(sex ~ species, FUN = length)

GGally::ggpairs(na.omit(d[, 7:11]))

colnames(d)[7:16] <- 
  c('BM', 'FA', 'Tail', 'ear', 'HF', 'TL', 'wing_area',
    'wing_span', 'wing_loading', 'aspect_ratio')

d$species <- gsub('^(.*)(\\s)(.*)$', '\\1_\\3', d$species)

d <- split(d, d$species)

lapply(d, function(x) apply(x, 2, function(i) sum(is.na(i))))

d$Artibeus_lituratus$Tail[2] <- 0
d$Artibeus_planirostris$Tail[is.na(d$Artibeus_planirostris$Tail)] <- 0
d$Uroderma_bilobatum$Tail[is.na(d$Uroderma_bilobatum$Tail)] <- 0
d$Sturnira_lillium$Tail[is.na(d$Sturnira_lillium$Tail)] <- 0
d$Desmodus_rotundus$Tail[is.na(d$Desmodus_rotundus$Tail)] <- 0

dat <- lapply(d$Artibeus_lituratus[7:14], function(x) x)

na_indx <- lapply(dat, function(x) which(is.na(x)))
na_indx <- na_indx[lapply(na_indx, function(x) length(x) > 0) |> unlist()]
names(na_indx) <- paste('NAindx_', names(na_indx), sep = '')

v <- (length(dat)+1):(length(dat)+length(na_indx))

for (i in seq_along(v)) {
  dat[[v[i]]] <- na_indx[[i]]
  names(dat)[v[i]] <- names(na_indx)[i]
}

lapply(d, FUN = 
         function(x) {
           x1 <- na.omit(x[, 7:14])
           x1 <- cbind(x[1:nrow(x1), c('species', 'month')], x1)
           x1 %$% aggregate(Tail ~ month, FUN = length)
         })

d <- 
  lapply(d, FUN = 
         function(x) {
           x1 <- na.omit(x[, 7:14])
           x1 <- cbind(x[1:nrow(x1), c('date', 'month','LU', 'sex', 'species')], x1)
           x1
         })

d <- as_tibble(do.call('rbind', d))

d$sex <- ifelse(d$sex == 'M', 'M',
                ifelse(d$sex == 'F', 'F', 'I'))

d$sex[is.na(d$sex)] <- 'U'

summary(d)

unique(d$species)

unique(d[, c("date", "month")]) %$% aggregate(date ~ month, FUN = length)

d$aspect_ratio <- (d$wing_span^2)/d$wing_area
d$w_loading <- (d$BM)/(d$wing_area)

d_scaled <- d

for (i in 6:ncol(d_scaled)) {
  d_scaled[[i]] <- as.vector(scale(d_scaled[[i]]))
}

d_scaled <- 
  d_scaled |> 
  group_by(month, species) |> 
  mutate(abundance = length(ear))

dist_days <- 
  sapply(sort(unique(d$date)), FUN = 
         function(j) {
           sapply(sort(unique(d$date)), FUN = 
                    function(i) {
                      abs(j-i)
                    })
         })

colnames(dist_days) <- as.character(sort(unique(d$date)))
rownames(dist_days) <- as.character(sort(unique(d$date)))

dis_sites <- 
  as.matrix(tribble(
    ~lu1, ~lu2, ~lu3, ~lu4, ~lu5,
    0,  5.04,  6.9,  10.5, 21.6,
    5.04, 0,   4.3,  6.2,  16.6,
    6.9, 4.3,  0,    4.2,  17.2, 
    10.5, 6.2, 4.2,  0,    13.2, 
    21.6, 16.6, 17.2, 13.2, 0
  ))

dist_month <- 
  sapply(sort(unique(d_scaled$month)), FUN = 
         function(i) {
           sapply(sort(unique(d_scaled$month)), FUN = 
                    function(j) {
                      abs(i-j)
                    })
         })

colnames(dist_month) <- paste(unique(sort(unique(d_scaled$month))))

d_scaled$date_id <- as.factor(d_scaled$date)
d_scaled$site_id <- as.factor(d_scaled$LU)

d_scaled$site_id <- factor(d_scaled$site_id,
                           labels = c('LU1', 'LU2', 
                                      paste('LU', 2:5, sep = '')))

d_scaled$month_id <- as.factor(d_scaled$month)
d_scaled$sex[which(is.na(d_scaled$sex))] <- 'U'
d_scaled$sex[which(d_scaled$sex == '-')] <- 'U'
d_scaled$sex_id <- as.factor(d_scaled$sex)
d_scaled$species[which(d_scaled$species == "Uroderma_sp.")] <- "Uroderma_bilobatum"
d_scaled$species[which(d_scaled$species == 'Platirrinus_sp1.' |
                         d_scaled$species == 'Platirrinus_sp2.')] <- "Platyrrinus_sp"
d_scaled$species <- as.factor(d$species)

dat <- 
  lapply(d_scaled[, c("date_id", "month_id", 
                    "site_id", "sex_id", "species", 
                    'BM', 'FA', "Tail", 'ear', 
                    'HF', 'TL', "wing_area", "wing_span", 
                    "aspect_ratio", "w_loading")], 
       FUN = 
         function(x) {
           
           if (is.factor(x)) as.numeric(x)
           else x
           
         })

dat$N <- nrow(d_scaled)
dat$N_day <- nlevels(d_scaled$date_id)
dat$N_month <- nlevels(d_scaled$month_id)
dat$N_site <- nlevels(d_scaled$site_id)
dat$N_sex <- nlevels(d_scaled$sex_id)
dat$N_spp <- nlevels(d_scaled$species)
dat$dist_day <- dist_days
dat$dist_month <- dist_month
dat$dist_site <- dis_sites


cat(file = 'body_mass.stan', 
    '
    functions {
      matrix cov_GPL2(matrix x, 
                      real eta, 
                      real rho,
                      real delta) {
                      
                      int N = dims(x)[1];
                      matrix[N, N] K;
    
                      for (i in 1:(N-1)) {
                        K[i, i] = eta + delta;
                        for (j in (i + 1):N) {
                          K[i, j] = eta * exp(-rho * square(x[i, j]));
                          K[j, i] = K[i, j];
                        }
                      }
                      K[N, N] = eta + delta;
                      return K;
                      } 
    }
    
    data {
    
    int N;
    int N_day;
    int N_month;
    int N_site;
    int N_sex;
    int N_spp;
    array[N] int date_id;
    array[N] int month_id;
    array[N] int site_id;
    array[N] int sex_id;
    array[N] int species;
    vector[N] BM;
    vector[N] FA;
    vector[N] Tail;
    vector[N] ear;
    vector[N] HF;
    vector[N] TL;
    vector[N] wing_area;
    vector[N] wing_span;
    vector[N] aspect_ratio;
    vector[N] w_loading;
    matrix[N_day, N_day] dist_day;
    matrix[N_month, N_month] dist_month;
    matrix[N_site, N_site] dist_site;
    
    }
    
    parameters {
    
      vector[N_spp] z_spp;
      real mu_spp;
      real<lower = 0> sigma_spp;
    
      vector[N_sex] z_sex;
      real mu_sex;
      real<lower = 0> sigma_sex;
    
      vector[N_day] z_day;
      real<lower = 0> eta_day;
      real<lower = 0> rho_day;
    
      vector[N_month] z_month;
      real<lower = 0> eta_month;
      real<lower = 0> rho_month;
    
      vector[N_site] z_site;
      real<lower = 0> eta_site;
      real<lower = 0> rho_site;
    
      real<lower = 0> sigma;
    
    }
    
    transformed parameters {
    
      vector[N_day] day;
      matrix[N_day, N_day] sigma_day;
      matrix[N_day, N_day] L_sigma_day;
      sigma_day = cov_GPL2(dist_day, eta_day, rho_day, 0.001);
      L_sigma_day = cholesky_decompose(sigma_day);
      day = L_sigma_day * z_day;
    
      vector[N_month] month;
      matrix[N_month, N_month] sigma_month;
      matrix[N_month, N_month] L_sigma_month;
      sigma_month = cov_GPL2(dist_month, eta_month, rho_month, 0.001);
      L_sigma_month = cholesky_decompose(sigma_month);
      month = L_sigma_month * z_month;
    
      vector[N_site] site;
      matrix[N_site, N_site] sigma_site;
      matrix[N_site, N_site] L_sigma_site;
      sigma_site = cov_GPL2(dist_site, eta_site, rho_site, 0.001);
      L_sigma_site = cholesky_decompose(sigma_site);
      site = L_sigma_site * z_site;
    
      vector[N_sex] sex;
      sex = mu_sex + z_sex * sigma_sex;
    
      vector[N_spp] spp;
      spp = mu_spp + z_spp * sigma_spp;
    
    }
    
    model{
    
      vector[N] mu;
    
      z_spp ~ normal(0, 1);
      mu_spp ~ normal(0, 1);
      sigma_spp ~ exponential(0.5);
    
      z_sex ~ normal(0, 1);
      mu_sex ~ normal(0, 1);
      sigma_sex ~ exponential(0.5);
    
      z_day ~ normal(0, 1);
      eta_day ~ exponential(3);
      rho_day ~ exponential(0.5);
    
      z_month ~ normal(0, 1);
      eta_month ~ exponential(3);
      rho_month ~ exponential(0.5);
    
      z_site ~ normal(0, 1);
      eta_site ~ exponential(3);
      rho_site ~ exponential(0.5);
    
      for (i in 1:N) {
        BM[i] ~ student_t(2, 
                          spp[species[i]] + day[date_id[i]] + month[month_id[i]] +
                          site[site_id[i]] + sex[sex_id[i]], 
                          sigma);
      }
    
    }
    
    generated quantities {
       
      vector[N] mu;
      array[N] real ppcheck;
    
      for (i in 1:N) {
        mu[i] = spp[species[i]] + day[date_id[i]] + month[month_id[i]] +
                site[site_id[i]] + sex[sex_id[i]];
      }
    
      ppcheck = student_t_rng(2, mu, sigma);  
    
      }
    
    ')

file <- paste(getwd(), '/body_mass.stan', sep = '')
fit_bm <- cmdstan_model(file, compile = T)

mod_bm <- 
  fit_bm$sample(
    data = dat, 
    chains = 3, 
    parallel_chains = 3, 
    iter_warmup = 500, 
    iter_sampling = 4e3, 
    thin = 3, 
    refresh = 500, 
    seed = 123
  )

summary_bm <- mod_bm$summary(c('spp', 'sex', 'day', 'month', 'site', 
                               'sigma', 'ppcheck'))

mod_diagnostics(mod_bm, summary_bm)

ppcheck_bm <- mod_bm$draws('ppcheck', format = 'matrix')

plot(density(dat$BM), main = '')
for (i in 1:100) lines(density(ppcheck_bm[i, ]), lwd = 0.1)
lines(density(dat$BM), col = 'red')

unique(data.frame(dat$species, d_scaled$species))


