pks <- c('lubridate', 'tidyr', 'dplyr', 'forcats', 
         'cmdstanr', 'magrittr', 'readxl', 'ggplot2', 
         'tibble')

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





d_scaled[] <- 
  lapply(d_scaled, function(x) if(!is.numeric(x)) as.factor())



  
d_scaled <- split(d_scaled, list(d_scaled$month))

d_scaled <- d_scaled[lapply(d_scaled, function(x) nrow(x) != 0) |>  unlist(use.names = F)]



hv <- 
  lapply(d_scaled, FUN = 
           function(x){
             
             hypervolume(x[, 6:15])
             
           })

test <- hypervolume(d_scaled[[1]][, 6:15])
bias <- hypervolume_resample('bat bias', test, 
                             "bootstrap seq", n = 10, 
                             seq = seq(100, 1700, 400), cores = 5)


cat(file = 'imputing_missing.stan', 
    '
    functions {
      vector merge_missing(int[], miss_indexex, vector x_obs, vector x_miss) {
        int N = dims(x_obs)[1];
        int N_miss = dims(x_miss)[1];
        vector[N] merge;
        merge = x_obs;
        for (i in 1:N_miss) {
          merge[miss_indxex[i]] = x_miss[i];
        }
        return merge
      }
    }
    ')



