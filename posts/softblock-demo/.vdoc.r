#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
suppressPackageStartupMessages({
library(DT)
library(readr)
library(dplyr)
library(sf)
library(quickblock)
library(MASS)
library(purrr)
library(glmnet)
library(ggplot2)
source("https://raw.githubusercontent.com/ddimmery/softblock/master/r_implementation.R")
})
set.seed(123)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
writeLines(readLines("https://raw.githubusercontent.com/ddimmery/softblock/master/r_implementation.R"))
```
#
#
#
#
#
#
#
#
#
#
#
#
url <- "http://dl.ncsbe.gov/ENRS/2020_11_03/results_pct_20201103.zip"
zip_file <- tempfile(fileext = ".zip")
download.file(url, zip_file, mode = "wb")
spec = cols(
  County = col_character(),
  `Election Date` = col_character(),
  Precinct = col_character(),
  `Contest Group ID` = col_double(),
  `Contest Type` = col_character(),
  `Contest Name` = col_character(),
  Choice = col_character(),
  `Choice Party` = col_character(),
  `Vote For` = col_double(),
  `Election Day` = col_double(),
  `One Stop` = col_double(),
  `Absentee by Mail` = col_double(),
  Provisional = col_double(),
  `Total Votes` = col_double(),
  `Real Precinct` = col_character(),
  X16 = col_skip()
)
results <- readr::read_tsv(zip_file, col_types=spec)
#
#
#
#
url = "https://s3.amazonaws.com/dl.ncsbe.gov/ShapeFiles/Precinct/SBE_PRECINCTS_20201018.zip"
temp <- tempfile()
temp2 <- tempfile()
download.file(url, temp)
unzip(zipfile = temp, exdir = temp2)
nc_SHP_file <- list.files(temp2, pattern = ".shp$",full.names=TRUE)
shapes <- sf::read_sf(nc_SHP_file)
#
#
#
#
results %>%
    filter(`Real Precinct` == 'Y') %>%
    group_by(County, Precinct) %>%
    summarize(
        total_vote_pres=sum(`Total Votes`[`Contest Name` == 'US PRESIDENT'], na.rm=TRUE),
        dem_share_pres=sum(`Total Votes`[`Contest Name` == 'US PRESIDENT' & `Choice Party` == 'DEM'], na.rm=TRUE)/total_vote_pres,
        gop_share_pres=sum(`Total Votes`[`Contest Name` == 'US PRESIDENT' & `Choice Party` == 'REP'], na.rm=TRUE)/total_vote_pres,
        total_vote_senate=sum(`Total Votes`[`Contest Name` == 'US SENATE'], na.rm=TRUE),
        dem_share_senate=sum(`Total Votes`[`Contest Name` == 'US SENATE' & `Choice Party` == 'DEM'], na.rm=TRUE)/total_vote_senate,
        gop_share_senate=sum(`Total Votes`[`Contest Name` == 'US SENATE' & `Choice Party` == 'REP'], na.rm=TRUE)/total_vote_senate,
        total_vote_gov=sum(`Total Votes`[`Contest Name` == 'NC GOVERNOR'], na.rm=TRUE),
        dem_share_gov=sum(`Total Votes`[`Contest Name` == 'NC GOVERNOR' & `Choice Party` == 'DEM'], na.rm=TRUE)/total_vote_gov,
        gop_share_gov=sum(`Total Votes`[`Contest Name` == 'NC GOVERNOR' & `Choice Party` == 'REP'], na.rm=TRUE)/total_vote_gov,
        total_vote_house=sum(`Total Votes`[grepl('US HOUSE OF REPRESENTATIVES DISTRICT', `Contest Name`)], na.rm=TRUE),
        dem_share_house=sum(`Total Votes`[grepl('US HOUSE OF REPRESENTATIVES DISTRICT', `Contest Name`) & `Choice Party` == 'DEM'], na.rm=TRUE)/total_vote_house,
        gop_share_house=sum(`Total Votes`[grepl('US HOUSE OF REPRESENTATIVES DISTRICT', `Contest Name`) & `Choice Party` == 'REP'], na.rm=TRUE)/total_vote_house
    ) %>% ungroup() -> results_agg
inner_join(results_agg, shapes, by=c('County'='county_nam', 'Precinct'='prec_id')) -> df_joined
DT::datatable(df_joined %>% sample_n(size=100) %>% dplyr::select(-geometry), rownames = FALSE, options=list(scrollX=TRUE, autoWidth = TRUE))
#
#
#
#
#
#
#
#
#
df_joined$geometry %>%
    st_centroid() %>%
    st_transform("+init=epsg:4326") %>%
    st_coordinates() -> latlong
df_joined$longitude = latlong[, 'X']
df_joined$latitude = latlong[, 'Y']
area = df_joined$geometry %>% st_transform("+init=epsg:4326") %>% st_area()
df_joined$area_km2 = units::drop_units(area) / 1e6 # convert m^2 to km^2
df_joined <- df_joined %>% mutate(vote_density_pres = total_vote_pres / area_km2)
```
#
#
#
#
#
#
start_time = lubridate::now()
df_joined %>% assign_softblock(c(
    longitude, latitude, area_km2, vote_density_pres, # geographic
    total_vote_pres, dem_share_pres, gop_share_pres, # 2020 presidential
    total_vote_senate, dem_share_senate, gop_share_senate, # 2020 senate
    total_vote_gov, dem_share_gov, gop_share_gov, # 2020 governor
    total_vote_house, dem_share_house, gop_share_house # 2020 house
)) %>%
rename(treatment_sb=treatment)-> df_joined
softblock_weights <- attr(df_joined, "laplacian")
end_time = lubridate::now()
print(end_time - start_time)
#
#
#
#
start_time = lubridate::now()
df_joined %>% assign_greedy_neighbors(c(
    longitude, latitude, area_km2, vote_density_pres, # geographic
    total_vote_pres, dem_share_pres, gop_share_pres, # 2020 presidential
    total_vote_senate, dem_share_senate, gop_share_senate, # 2020 senate
    total_vote_gov, dem_share_gov, gop_share_gov, # 2020 governor
    total_vote_house, dem_share_house, gop_share_house # 2020 house
)) %>%
rename(treatment_nn=treatment)-> df_joined
nn_weights <- attr(df_joined, "laplacian")
end_time = lubridate::now()
print(end_time - start_time)
#
#
#
#
start_time = lubridate::now()
df_joined %>% dplyr::select(
    longitude, latitude, area_km2, vote_density_pres, # geographic
    total_vote_pres, dem_share_pres, gop_share_pres, # 2020 presidential
    total_vote_senate, dem_share_senate, gop_share_senate, # 2020 senate
    total_vote_gov, dem_share_gov, gop_share_gov, # 2020 governor
    total_vote_house, dem_share_house, gop_share_house # 2020 house
) -> df_qb
qb_blocks = quickblock(as.data.frame(df_qb), size_constraint = 6L)
df_joined$treatment_qb = as.integer(as.character(assign_treatment(qb_blocks, treatments=c('0', '1'))))
end_time = lubridate::now()
print(end_time - start_time)
#
#
#
#
# This is extremely slow.
# start_time = lubridate::now()
# df_joined %>% assign_matched_pairs(c(
#     longitude, latitude, area_km2, vote_density_pres, # geographic
#     total_vote_pres, dem_share_pres, gop_share_pres, # 2020 presidential
#     total_vote_senate, dem_share_senate, gop_share_senate, # 2020 senate
#     total_vote_gov, dem_share_gov, gop_share_gov, # 2020 governor
#     total_vote_house, dem_share_house, gop_share_house # 2020 house
# )) %>%
# rename(treatment_mp=treatment)-> df_joined
# mp_weights <- attr(df_joined, "laplacian")
# end_time = lubridate::now()
# print(end_time - start_time)
```
#
#
#
#
#
#
#
#
#
#
#
#
#
#
create_power_simulator = function(W, A, Y) {
    dL = diag(W)
    Dinv = (2 * A - 1) / dL
    estimates = drop((Dinv %*% W) %*% Y)
    residuals = Y - estimates
    x_mat = cbind(A, 1)
    xlx = t(x_mat) %*% W %*% x_mat
    bread = MASS::ginv(as.matrix(xlx))
    detect_effect = function(effect=1) {
        sim_outcome = estimates + sample(residuals) + A * effect
        coefs = bread %*% t(x_mat) %*% W %*% sim_outcome
        r = diag(drop((sim_outcome - (x_mat %*% coefs)) ^ 2))
        meat = t(x_mat) %*% (W %*% r %*% W) %*% x_mat
        vcv = bread %*% meat %*% bread
        upr = coefs[1] + qnorm(0.975) * sqrt(vcv[1])
        lwr = coefs[1] + qnorm(0.025) * sqrt(vcv[1])
        lwr > 0
    }
    detect_effect
}
create_power_simulator_qb = function(blocks, A, Y) {
    estimate_by_block = tapply(Y[A==1], blocks[A==1], mean) - tapply(Y[A==0], blocks[A==0], mean)
    estimates = unlist(purrr::map(blocks, function(b) estimate_by_block[as.character(b)]))
    residuals = Y - estimates
    detect_effect = function(effect=1) {
        sim_outcome = estimates + sample(residuals) + A * effect
        result = quickblock::blocking_estimator(sim_outcome, blocks, A)
        upr = result$effects[2,1] + qnorm(0.975) * sqrt(result$effect_variances[2,1])
        lwr = result$effects[2,1] + qnorm(0.025) * sqrt(result$effect_variances[2,1])
        lwr > 0
    }
    detect_effect
}
```
#
#
#
#
#
#
simulate_power = create_power_simulator(softblock_weights, df_joined$treatment_sb, df_joined$dem_share_pres)
estimate_power_for_effect = function(effect) mean(replicate(25, simulate_power(effect=effect)))
effects = seq(0, 0.05, length=25)
power_sb = unlist(purrr::map(effects, estimate_power_for_effect))
#
#
#
#
simulate_power = create_power_simulator(nn_weights, df_joined$treatment_nn, df_joined$dem_share_pres)
estimate_power_for_effect = function(effect) mean(replicate(25, simulate_power(effect=effect)))
power_nn = unlist(purrr::map(effects, estimate_power_for_effect))
#
#
#
#
simulate_power = create_power_simulator_qb(qb_blocks, df_joined$treatment_qb, df_joined$dem_share_pres)
estimate_power_for_effect = function(effect) mean(replicate(25, simulate_power(effect=effect)))
effects = seq(0, 0.05, length=25)
power_qb = unlist(purrr::map(effects, estimate_power_for_effect))
```
#
#
#
#
#
#
ggplot(
    tibble(
        effects=c(effects, effects, effects),
        power=c(power_sb, power_nn, power_qb),
        design=c(rep('SoftBlock', length(power_sb)), rep('Greedy Neighbors', length(power_sb)), rep('QuickBlock', length(power_sb)))
    ), aes(effects, power, color=design)) + geom_line() +
    scale_x_continuous('Effect (pp)', labels=scales::percent) +
    scale_y_continuous("Power", labels=scales::percent) +
    scale_color_discrete("Design") +
    theme_minimal()
#
#
#
#
#
#
#
#
#
#
#
df_joined$outcome = df_joined$dem_share_pres
df_joined$ite = with(df_joined, 0.005 + 0.005 * (plogis((dem_share_pres - median(dem_share_pres)) / sd(dem_share_pres))))
df_joined$outcome_sb = with(df_joined, outcome + treatment_sb * ite)
df_joined$outcome_nn = with(df_joined, outcome + treatment_nn * ite)
df_joined$outcome_qb = with(df_joined, outcome + treatment_qb * ite)
#
#
#
#
#
#
estimate_effect = function(W, A, Y) {
    dL = diag(W)
    Dinv = (2 * A - 1) / dL
    x_mat = cbind(A, 1)
    xlx = t(x_mat) %*% W %*% x_mat
    bread = MASS::ginv(as.matrix(xlx))
    coefs = bread %*% t(x_mat) %*% W %*% Y
    r = diag(drop((Y - (x_mat %*% coefs)) ^ 2))
    meat = t(x_mat) %*% (W %*% r %*% W) %*% x_mat
    vcv = bread %*% meat %*% bread
    list(estimate=coefs[1], std.error=sqrt(vcv[1,1]))
}
sb_est = estimate_effect(softblock_weights, df_joined$treatment_sb, df_joined$outcome_sb)
#
#
#
#
nn_est = estimate_effect(nn_weights, df_joined$treatment_nn, df_joined$outcome_nn)
#
#
#
#
result = quickblock::blocking_estimator(df_joined$outcome_qb, qb_blocks, df_joined$treatment_qb)
qb_est = list(estimate=result$effects[2,1], std.error=sqrt(result$effect_variances[2,1]))
#
#
#
#
#
#
tibble(
    estimate=c(sb_est$estimate, nn_est$estimate, qb_est$estimate),
    std.error=c(sb_est$std.error, nn_est$std.error, qb_est$std.error),
    design=c("SoftBlock", "Greedy Neighbors", "QuickBlock")
) %>%
ggplot(aes(x=design, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    geom_pointrange() +
    scale_x_discrete("Design") +
    scale_y_continuous("ATE (pp)", labels=scales::percent) +
    coord_flip() +
    theme_minimal()
#
#
#
#
#
#
#
predict.hte.split = function(x, a, y, s, predict.s=4) {
    s.pi = (predict.s) %% 4 + 1
    s.mu = (predict.s + 1) %% 4 + 1
    s.dr = (predict.s + 2) %% 4 + 1
    pihat <- predict(cv.glmnet(x[s==s.pi,],a[s==s.pi], family="binomial", nfolds=10), newx=x, type="response", s="lambda.min")
    mu0hat <- predict(cv.glmnet(x[a==0 & s==s.mu,],y[a==0 & s==s.mu], nfolds=10), newx=x, type="response", s="lambda.min")
    mu1hat <- predict(cv.glmnet(x[a==1 & s==s.mu,],y[a==1 & s==s.mu], nfolds=10),newx=x, type="response", s="lambda.min")
    pseudo <- ((a-pihat)/(pihat*(1-pihat)))*(y-a*mu1hat-(1-a)*mu0hat) + mu1hat - mu0hat
    drl <- predict(cv.glmnet(x[s==s.dr,],pseudo[s==s.dr]),newx=x[s==predict.s, ], s="lambda.min")
    drl
}
predict.hte.crossfit = function(x, a, y) {
    N = length(a)
    s = sample(1:4, N, replace=TRUE)
    hte = rep(NA_real_, N)
    for (split in 1:4) {
        hte[s==split] = predict.hte.split(x, a, y, s, predict.s=split)
    }
    hte
}
calculate_hte <- function(.data, cols, .treatment='treatment', .outcome='outcome') {
    expr <- rlang::enquo(cols)
    pos <- tidyselect::eval_select(expr, data = .data)
    df_cov <- rlang::set_names(.data[pos], names(pos))
    cov_mat = scale(model.matrix(~.+0, df_cov))
    .data$hte = predict.hte.crossfit(cov_mat, .data[[.treatment]], .data[[.outcome]])
    .data
}
#
#
#
#
#
#
#
#
df_joined %>% calculate_hte(c(
    longitude, latitude, area_km2, vote_density_pres, # geographic
    total_vote_pres, # 2020 presidential
    total_vote_senate, dem_share_senate, gop_share_senate, # 2020 senate
    total_vote_gov, dem_share_gov, gop_share_gov, # 2020 governor
    total_vote_house, dem_share_house, gop_share_house # 2020 house
), .treatment='treatment_sb', .outcome='outcome_sb') %>% rename(hte_sb=hte) -> df_joined
summary(df_joined$hte_sb)
#
#
#
#
df_joined %>% calculate_hte(c(
    longitude, latitude, area_km2, vote_density_pres, # geographic
    total_vote_pres, # 2020 presidential
    total_vote_senate, dem_share_senate, gop_share_senate, # 2020 senate
    total_vote_gov, dem_share_gov, gop_share_gov, # 2020 governor
    total_vote_house, dem_share_house, gop_share_house # 2020 house
), .treatment='treatment_nn', .outcome='outcome_nn') %>% rename(hte_nn=hte) -> df_joined
summary(df_joined$hte_nn)
#
#
#
#
df_joined %>% calculate_hte(c(
    longitude, latitude, area_km2, vote_density_pres, # geographic
    total_vote_pres, # 2020 presidential
    total_vote_senate, dem_share_senate, gop_share_senate, # 2020 senate
    total_vote_gov, dem_share_gov, gop_share_gov, # 2020 governor
    total_vote_house, dem_share_house, gop_share_house # 2020 house
), .treatment='treatment_qb', .outcome='outcome_qb') %>% rename(hte_qb=hte) -> df_joined
summary(df_joined$hte_qb)
#
#
#
#
#
#
#
ggplot(df_joined, aes()) +
geom_histogram(aes(x=hte_sb, fill='SoftBlock'), bins=50, alpha=0.4) +
geom_histogram(aes(x=hte_nn, fill='Greedy Neighbors'), bins=50, alpha=0.4) +
geom_histogram(aes(x=hte_qb, fill='QuickBlock'), bins=50, alpha=0.4) +
scale_x_continuous("Effect (pp)", labels=scales::percent) +
theme_minimal()
#
#
#
