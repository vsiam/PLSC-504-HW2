##HW2
##11/5/2025
##Viktar Siamionau
P<-c("lme4","plm","dplyr",'lmtest',
     "stargazer","haven", "fixest",
     "car", "interactions", "texreg", 
     "purrr", "tibble", "tseries")
for (pkg in P) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE, quiet = TRUE)
  }
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}
rm(P)
##
country <- as.data.frame(read_dta("Macro_JOP_ACW.dta"))

#replication of results
country_sub <- country %>%
  filter(year >= 1975, oecdmember == 10, mainstreamL == 1)
model1 <- feols(
  pervote ~ lpervote + investment_simpler + union_add + cosmolib_pos +
    incumbent |
    party,  # party fixed effects
  data = country_sub,
  cluster = ~party + date
)
summary(model1)
model2<- feols(pervote ~ lpervote + investment_simpler + union_add +
                       right_inv_simpler_left + market + cosmolib_pos +
                       deindust + radright_pres_parlgov + radleft_pres_parlgov +
                       unemp + growth + incumbent |
                       party,   
                     data = country_sub,
                     cluster = ~party + date)  

summary(model2)
model3 <- feols(
  pervote ~ lpervote + investment_simpler*union_add + cosmolib_pos +
    incumbent |
    party,
  data = country_sub,
  cluster = ~party + date
)
summary(model3)
model4 <- feols(
  pervote ~ lpervote + investment_simpler*union_add + 
    right_inv_simpler_left + market + cosmolib_pos +
    deindust + radright_pres_parlgov + radleft_pres_parlgov +
    unemp + growth + incumbent |
    party,   # fixed effects
  data = country_sub,
  cluster = ~party + date  # multi-way clustered SEs
)
summary(model4)
model5 <- feols(
  pervote ~ lpervote + investment_simpler*cosmolib_pos + union_add +
    incumbent |
    party,
  data = country_sub,
  cluster = ~party + date
)
summary(model5)
model6 <- feols(
  pervote ~ lpervote + investment_simpler*cosmolib_pos + 
    union_add + right_inv_simpler_left + market +
    radright_pres_parlgov + radleft_pres_parlgov +
    unemp + growth + deindust + incumbent |
    party,   # party fixed effects
  data = country_sub,
  cluster = ~party + date  # multi-way clustered SEs
)
summary(model6)
og_list <- list(model1, model2, model3, model4, model5, model6)
coef_names <- list(
  "lpervote" = "Lag Vote Share",
  "investment_simpler" = "Investment position",
  "union_add" = "Union Influence",
  "cosmolib_pos" = "Second Dimension",
  "incumbent" = "Incumbent",
  "investment_simpler:union_add" = "Investment × Union Strength",
  "investment_simpler:cosmolib_pos" = "Investment × second-dimension position",
  "right_inv_simpler_left"="Mainstream Right Investment Position",
  "market" = "State-Market Position", 
  "radright_pres_parlgov" = "Radical Right in gov't",
  "radleft_pres_parlgov" = "Radical Left in gov't",
  "unemp" = "Unemployment",
  "growth" = "GDP Growth",
  "deindust" = "Deindustrialization"
)
texreg(og_list,
       custom.model.names = c("Model 1", "Model 2", "Model 3","Model 4",
                              "Model 5", "Model 6"),
       custom.coef.map = coef_names,
       booktabs = TRUE,
       use.packages = FALSE,
       caption = "Differenced panel regression models"
)
######tests 
##ploting data points temporally by party-election
df %>%
  select(
    lpervote, investment_simpler, union_add,
    cosmolib_pos, incumbent, party, pervote, edate
  ) %>%
  pivot_longer(cols = -c(edate, party)) %>%
  mutate(
    party = as.factor(party),
    name = dplyr::recode(
      name,
      "lpervote" = "Lagged Vote Share",
      "pervote" = "Vote Share",
      "investment_simpler" = "Investment Position",
      "union_add" = "Union Strength",
      "cosmolib_pos" = "Second Dimension",
      "incumbent" = "Incumbent"
    )
  ) %>%
  arrange(edate) %>%
  ggplot(aes(x = edate, y = value, color = party, group = party)) +
  geom_line(linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 1.5, alpha = 0.8) +
  facet_wrap(~name, ncol = 2, scales = "free_y") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",        
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Election Date",
    y = "Value",
    title = "Party-level Time Series of Key Variables",
    subtitle = "Each facet shows variation by party and election date"
  )


mdl_df <- df %>% 
  arrange(party, edate) %>%
  select(lpervote, investment_simpler, union_add, 
         cosmolib_pos, incumbent, party, pervote, edate) %>%
  na.omit %>% 
  group_by(party) %>% mutate(ID=row_number()) %>% ungroup %>%
  select(-matches("date"))
mdl_df

###adf tests 
mdl_df$party <- factor(mdl_df$party)
adf_df <- data.frame()
for (i in unique(mdl_df$party)){
  df_ <- plm_df %>% filter(party==i) %>% select(2,3,4,5,7)
  if (nrow(df_) > 5){
    adf_df <- bind_rows(
      adf_df, 
      cbind.data.frame(party=i, p_value = lapply(df_, tseries::adf.test) %>% purrr::map("p.value") %>% unlist)%>%
        tibble::rownames_to_column("variable")
    )
    
  }
}
adf_df$variable <- dplyr::recode(adf_df$variable,
                          "union_add" = "Union Strength",
                          "pervote" = "Vote Share",
                          "investment_simpler" = "Investment Position",
                          "incumbent" = "Incumbent",
                          "cosmolib_pos" = "Second Dimension"
)
ggplot(adf_df, aes(x=p_value, y=variable, fill=variable)) + geom_boxplot(alpha=0.15) + 
  geom_point(alpha=0.5) +
  geom_vline(aes(xintercept = 0.05), color="red", linetype="dashed") +
  labs(
    title = "ADF test results for each party/covariate combination",
    x = "p value", 
    y = "Variable") +
  theme_minimal()
####
#prepping data for modeling
##restricting to parties with at least 5 observations
nParty <- table(mdl_df$party)
nParty <- nParty[nParty >= 5] %>% names
plm_df <- mdl_df %>% filter(party %in% nParty) %>%
  pdata.frame(., index=c("party"))

###models 
mdl_list <- list(
  plm(
    pervote ~ lag(pervote, 1) + 
      investment_simpler + union_add + cosmolib_pos + incumbent, 
    data=plm_df
  ),
  plm(
    pervote ~ lag(pervote, 1) + 
      lag(investment_simpler, 1) + lag(union_add, 1) + lag(cosmolib_pos,1) + lag(incumbent,1), 
    data=plm_df
  ),
  plm(
    pervote ~ diff(pervote) + 
      diff(investment_simpler) + diff(union_add) + diff(cosmolib_pos) + diff(incumbent), 
    data=plm_df
  )
)
coef_names <- list(
  "lag(pervote, 1)" = "Lag Vote Share",
  "investment_simpler" = "Investment",
  "union_add" = "Union Influence",
  "cosmolib_pos" = "Second Dimension",
  "incumbent" = "Incumbent",
  "lag(investment_simpler, 1)" = "Lagged Investment",
  "lag(union_add, 1)" = "Lagged Union Influence",
  "lag(cosmolib_pos, 1)" = "Lagged Second Dimension",
  "lag(incumbent, 1)" = "Lagged Incumbent",
  "diff(pervote)" = "ΔVote Share",
  "diff(investment_simpler)" = "ΔInvestment",
  "diff(union_add)" = "ΔUnion Influence",
  "diff(cosmolib_pos)" = "ΔSecond Dimension",
  "diff(incumbent)" = "ΔIncumbent"
)

texreg(
  mdl_list,
  custom.model.names = c("Model 1", "Model 2", "Model 3"),
  custom.coef.map = coef_names,
  booktabs = TRUE,
  use.packages = FALSE,
  caption = "Differenced panel regression models"
)
###
#conducting tests 
pbg_results <- lapply(mdl_list, function(m) pbgtest(m, order = 1))
pbg_results
#
lapply(mdl_list, pwartest)
###
###fitting the other two models
mdl_list2 <- list(
  plm(
    pervote ~ diff(pervote) + 
      diff(investment_simpler) + diff(union_add) + diff(cosmolib_pos) + diff(incumbent), 
    data=plm_df
    ),
  plm(
     pervote ~ diff(pervote) + diff(investment_simpler) + diff(union_add) + diff(cosmolib_pos) + diff(incumbent) +
       diff(investment_simpler)*diff(union_add),
     data=plm_df
    ),
  plm(
    pervote ~ diff(pervote) + diff(investment_simpler) + diff(union_add) + diff(cosmolib_pos) + diff(incumbent) +
      diff(investment_simpler) * diff(cosmolib_pos),
    data=plm_df)
)
coef_names <- list(
  "diff(pervote)" = "ΔVote Share",
  "diff(investment_simpler)" = "ΔInvestment",
  "diff(union_add)" = "ΔUnion Influence",
  "diff(cosmolib_pos)" = "ΔSecond Dimension",
  "diff(incumbent)" = "ΔIncumbent",
  "diff(investment_simpler):diff(union_add)" = "ΔInvestment × ΔUnion influence",
  "diff(investment_simpler):diff(cosmolib_pos)" = "ΔInvestment × ΔSecond Dimension"
)
texreg(
  mdl_list2,
  custom.model.names = c("Model 1", "Model 2", "Model 3"),
  custom.coef.map = coef_names,
  booktabs = TRUE,
  use.packages = FALSE,
  caption = "Differenced panel regression models"
)
####
#plots 
plot_marginal_effect <- function(plm_model, pred, modx, n_points = 100, title = NULL) {
  coefs <- coef(plm_model)
  vcov_mat <- vcov(plm_model)
  interaction_name <- paste0(pred, ":", modx)
  if (!all(c(pred, interaction_name) %in% names(coefs))) {
    stop("Predictor or interaction term not found in model.")
  }
  b_pred <- coefs[pred]
  b_int <- coefs[interaction_name]
  var_pred <- vcov_mat[pred, pred]
  var_int <- vcov_mat[interaction_name, interaction_name]
  cov_pred_int <- vcov_mat[pred, interaction_name]
  mod_values <- seq(min(plm_model$model[[modx]], na.rm = TRUE),
                    max(plm_model$model[[modx]], na.rm = TRUE),
                    length.out = n_points)
  marginal_effect <- b_pred + b_int * mod_values
  se_marginal <- sqrt(var_pred + (mod_values^2) * var_int + 2 * mod_values * cov_pred_int)
  upper <- marginal_effect + 1.96 * se_marginal
  lower <- marginal_effect - 1.96 * se_marginal
  df_plot <- data.frame(
    moderator = mod_values,
    marginal_effect = marginal_effect,
    lower = lower,
    upper = upper
  )
  if (is.null(title)) {
    title <- paste("Marginal effect of", pred, "×", modx)
}
  ggplot(df_plot, aes(x = moderator, y = marginal_effect)) +
    geom_line(color = "blue", size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
    labs(x = modx,
         y = paste("Marginal effect of", pred),
         title = title) +
    theme_minimal()
}
plot_marginal_effect(mdl_list2[[2]], 
                     pred = "diff(investment_simpler)", 
                     modx = "diff(union_add)", 
                     title = "Marginal effect of Δ Investment at different values of Δ Union Influence") +
  labs(x = "Change in ΔUnion Influence",
      y = "Marginal Effect of Δ Investment on Vote Share")

plot_marginal_effect(mdl_list2[[3]], 
                     pred = "diff(investment_simpler)", 
                     modx = "diff(cosmolib_pos)", 
                     title = "Marginal effect of Δ Investment at different values of Δ Second Dimension") + 
  labs(
    x = "Change in ΔSecond Dimension)",
    y = "Marginal Effect of Δ Investment on Vote Share"
  )
