library("R2jags")
library(tidyverse)
library(readxl)
library(ggridges)

# ---- reading date ---
df <- read_excel("df_duration.xlsx", 
                          col_types = c("skip", "text", "text", 
                                        "numeric", "text", "numeric", "numeric", 
                                        "numeric", "numeric", "text", "text"))
# ---- data prepration ----
names(df) = gsub(" ", "", gsub("-","",tolower(names(df))))

df = df %>% 
  mutate(technique = case_when(technique == "Fast_Articulated_Forte" ~ "Fast_Articulated",
                               technique == "Fast_Articulated_Piano" ~ "Fast_Articulated",
                               technique == "Slow_Legato_Piano"  ~ "Slow_Legato",
                               technique == "Slow_Legato_Forte" ~ "Slow_Legato",
                               technique == "Trillo" ~ "Trill",
                               technique == "Slow_MoltoVibrato" ~ "MoltoVibrato",
                               technique == "VeryBreathy" ~ "Breathy" ,
                               technique == "Slow_StraightTone" ~ "StraightTone",
                               T ~ technique),
         intervaltothefollowingnote = case_when(is.na(intervaltothefollowingnote) ~ 0,
                                                T ~ intervaltothefollowingnote),
         intervaltothepreviousnote = case_when(is.na(intervaltothepreviousnote) ~ 0,
                                               T ~ intervaltothepreviousnote)) %>% 
  filter(!is.na(groundtruthnoteduration)) %>% 
  mutate(y = (duration - groundtruthnoteduration)) %>% #groundtruthnoteduration
  group_by(singername) %>% 
  mutate(across(groundtruthmidicode:intervaltothefollowingnote, ~ . - mean(., na.rm = T))) %>%  # removing means of the numerics to make the intercept meaningfull
  ungroup()

technique_df = data.frame(technique = unique(df$technique), technique_id = seq(1,length(unique(df$technique)),1))
pre_notetype_df = data.frame(previousnotetype = unique(df$previousnotetype), pre_notetype_id = c(1,0))
post_notetype_df = data.frame(followingnotetype = unique(df$followingnotetype), post_notetype_id = c(0,1))


df = df %>% 
  list(.,technique_df,pre_notetype_df,post_notetype_df) %>% reduce(left_join)

# Adding jitter to combat the discretization caused by the measurement instrument  
jitter = rnorm(nrow(df),0, 0.2)
df$y = df$y + jitter


# ------------------ JAGS full model --------------------

mod_string = " model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(mu[i], prec[person[i]]^-2)
    mu[i] = technique[type[i], person[i]] + 
    b_dur[person[i]]*duration[i] + 
    b_midi_code[person[i]]*midi_code[i] +
    b_int_following[person[i]] *int_following[i] + 
    b_int_previous[person[i]] *int_previous[i] + 
    b_is_rest_pre[person[i]] * is_rest_pre[i] +
    b_is_rest_post[person[i]] * is_rest_post[i]
    
    y_pred[i] ~ dnorm(mu[i], prec[person[i]]^-2)
    
  }
  
  # ------- hierarchical priors -------
  
  for (i in 1:max(person)) {
  
    technique[1,i] ~ dnorm(mu_glob_tech[1], prec_glob_tech[1]^-2)
    technique[2,i] ~ dnorm(mu_glob_tech[2], prec_glob_tech[2]^-2)
    technique[3,i] ~ dnorm(mu_glob_tech[3], prec_glob_tech[3]^-2)
    technique[4,i] ~ dnorm(mu_glob_tech[4], prec_glob_tech[4]^-2)
   

    b_dur[i] ~ dnorm(mu_glob_dur, prec_glob_dur^-2)

    b_midi_code[i] ~ dnorm(mu_glob_midi, prec_glob_midi^-2)

    b_int_following[i] ~ dnorm(mu_glob_int_following, prec_glob_int_following^-2) 

    b_int_previous[i] ~ dnorm(mu_glob_int_previous, prec_glob_int_previous^-2)
    
    b_is_rest_pre[i] ~ dnorm(mu_glob_rest_pre, prec_glob_rest_pre^-2)
    
    b_is_rest_post[i] ~ dnorm(mu_glob_rest_post, prec_glob_rest_post^-2)
    
    prec[i] ~ dnorm(0,10)T(0,)

  }
  
  # --- hyper priors ------
  
  for (j in 1:4){
  mu_glob_tech[j] ~ dnorm(0,10)
  prec_glob_tech[j] ~ dnorm(0,10)T(0,) 
  }
  
  mu_glob_dur ~ dnorm(0,10)
  mu_glob_midi ~ dnorm(0,10)
  mu_glob_int_following ~ dnorm(0,10)
  mu_glob_int_previous ~ dnorm(0,10)
  mu_glob_rest_pre ~ dnorm(0,10)
  mu_glob_rest_post ~ dnorm(0,10)
  
  prec_glob_dur ~ dnorm(0,10)T(0,)
  prec_glob_midi ~ dnorm(0,10)T(0,)
  prec_glob_int_following ~ dnorm(0,10)T(0,)
  prec_glob_int_previous ~ dnorm(0,10)T(0,)
  prec_glob_rest_pre ~ dnorm(0,10)T(0,)
  prec_glob_rest_post ~ dnorm(0,10)T(0,)
} "


data_jags = list(y = df$y, 
                 type = df$technique_id,
                 person = as.numeric(as.factor(df$singername)),
                 duration = df$groundtruthnoteduration,
                 midi_code = df$groundtruthmidicode,
                 int_following = df$intervaltothefollowingnote,
                 int_previous = df$intervaltothepreviousnote, 
                 is_rest_pre = df$pre_notetype_id,
                 is_rest_post = df$post_notetype_id
)



params = c('technique', 'b_dur', 'b_midi_code', 'b_int_following', 'b_int_previous', 'b_is_rest_pre', 'b_is_rest_post',
           'mu_glob_tech','mu_glob_dur','mu_glob_midi','mu_glob_int_following','mu_glob_int_previous','mu_glob_rest_pre',
           'mu_glob_rest_post', 'y_pred',
           'prec_glob_tech', 'prec_glob_dur','prec_glob_midi','prec_glob_int_following','prec_glob_int_previous','prec_glob_rest_pre','prec_glob_rest_post', 'prec')



set.seed(123)
# Run the model
model_run <- jags(
  data = data_jags,
  parameters.to.save = params,
  model.file = textConnection(mod_string)
)


plot(model_run)
print(model_run$BUGSoutput$DIC)

# ------- Posterior Predictive check -----
preds <- model_run$BUGSoutput$sims.list$y_pred
df_preds = as.data.frame(t(preds))
samp = sample(1:ncol(df_preds), 100)
df_preds = df_preds[,samp]

colors = c("Posterior prediction" = "cadetblue", 
           "Observed" = "navyblue")

(duration_pp_check = df_preds %>% 
  mutate(index = seq(1,nrow(.), 1)) %>% 
  pivot_longer(names_to = 'group', values_to = 'samples', -index) %>% 
  ggplot() +
  stat_density(aes(x=samples, 
                   group = group,
                   color = "Posterior prediction"),
               geom = "line",  # for legend
               position = "identity",
               lwd = 0.1) + #1 sim = 1 line
  stat_density(data = df, aes(x=y, 
                                color = "Observed"),
               geom = "line",  
               position = "identity",
               lwd = 0.8) +
  scale_color_manual(values = colors) +
  xlim(-3.5, 3.5) +
  labs(x = "Deviation from the ground truth duration\n(seconds)", y = "Density") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, face = 'bold'),
        axis.title.y = element_text(size = 15, face = 'bold'),
        title =  element_text(size = 15, face = 'bold'),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.justification=c(1,1),
        legend.position=c(.95,0.95),
        legend.title = element_blank(),
        legend.text = element_text(size = 15)))


ggpubr::ggarrange(midi_pp_check, duration_pp_check, ncol = 2, nrow = 1)

# ------------------ Plotting effects ---------------


posteriors = model_run$BUGSoutput$sims.list
betas_1 = posteriors[c('mu_glob_dur','mu_glob_int_following','mu_glob_int_previous',
                       'mu_glob_midi','mu_glob_rest_post','mu_glob_rest_pre')] 

beta_2 = posteriors['mu_glob_tech']

df_post_1 = do.call(cbind.data.frame, betas_1)
names(df_post_1) = c("Durataion","Interval to the following note", "Interval to the previous note","Ground truth MIDI code",
                     "Post rest","Pre rest")

df_post_2 = as.data.frame(beta_2[[1]])
#names(df_post_2) = technique_df$technique
names(df_post_2) = c( "Fast Articulated","Slow Legato","Straight Tone","Molto Vibrato")


plt_theme = theme(axis.title.x = element_text(size = 15, face = 'bold'),
                  axis.title.y = element_text(size = 15, face = 'bold'),
                  title =  element_text(size = 15, face = 'bold'),
                  axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 15))

p1 = df_post_1 %>% 
  mutate(id = rownames(.)) %>% 
  pivot_longer(values_to = 'sample', names_to = 'variable', -id) %>% 
  ggplot(
    aes(x = sample, y = reorder(variable,sample))) +
  geom_density_ridges(scale = 1, color = 'white', fill = 'blue', alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  labs(y = NULL, x = 'Effect') +
  plt_theme

p2 = df_post_2 %>% 
  mutate(id = rownames(.)) %>% 
  pivot_longer(values_to = 'sample', names_to = 'variable', -id) %>% 
  ggplot(
    aes(x = sample, y = reorder(variable,sample))) +
  geom_density_ridges(scale = 1, color = 'white', fill = 'blue', alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed') +
  labs(y = NULL, x = 'Effect') +
  plt_theme


ggpubr::ggarrange(p1, p2, ncol = 2, nrow = 1)


# Buidling results table
results = cbind(df_post_1, df_post_2) %>% 
  mutate(row = rownames(.)) %>% 
  pivot_longer(values_to = 'samples', names_to = 'variable', -row) %>% 
  group_by(variable) %>% 
  summarise(Mean = mean(samples),
            Lower_bound = quantile(samples, 0.025),
            Higher_bound = quantile(samples,0.975)) 


par(mfrow = c(1,2))
hist(df$y, breaks = 'FD')
hist(post_pred, breaks = 'FD')


plot(df$y, post_pred)
hist(post_pred - df$y, breaks = 'FD')

# -------- Uncertainty calibration check ------------------
post_pred = model_run$BUGSoutput$sims.list$y_pred
preds = model_run$BUGSoutput$mean$mu

summary_tbl <- as.data.frame(t(apply(post_pred, 2, quantile, probs = c(0.025,0.5, 0.975), na.rm = T))) # total

summary_tbl$obs = df$y
summary_tbl = summary_tbl %>% mutate(test = case_when(obs > `2.5%` & obs < `97.5%` ~ 1,
                                                      T ~ 0))

mean(summary_tbl$test) # Checking the % of observations within the 95% CI

# ------------------ Plotting obs vs pred (with CIs) ------------------
names(summary_tbl) = c('low', 'mean','high', 'obs', 'test')


func_obs_fit = function(df){
  
  up_lim = max(df[,c("obs", "mean")], na.rm = T) 
  low_lim = min(df[,c("obs", "mean")], na.rm = T) 
  
  df %>% 
    ggplot(aes(obs, mean)) +
    geom_point() +
    geom_abline(slope=1, color = "darkblue") +
    coord_cartesian(ylim = c(low_lim,up_lim), xlim = c(low_lim,up_lim)) + 
    labs(x = 'Observed', y = 'Fitted') + 
    theme_bw() +
    plt_theme
  
}

func_obs_fit(summary_tbl)
summary(lm(summary_tbl$mean ~ summary_tbl$obs))

# -- Calculating maximum variance among the singers ---
s = model_run$BUGSoutput$mean$prec
max_variance = max(((3.14^2) * s^2)/3)
(max_sd = sqrt(max_variance))
