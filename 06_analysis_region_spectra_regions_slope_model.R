# Start Generation Here
cat("\n", crayon::bold$cyan("╔════════════════════════════════════════════════════════════╗"))
cat("\n", crayon::bold$cyan("║              APERIODIC SLOPE BY LOBE PLOT                  ║"))
cat("\n", crayon::bold$cyan("╚════════════════════════════════════════════════════════════╝\n"))
# End Generation Here

# Load required packages
library(ggplot2)
library(dplyr)
library(viridis)
library(patchwork)

# Create a combined group-sex variable for easier plotting
cat(crayon::blue("Preparing data for aperiodic slope by lobe comparison...\n"))

# Ensure we have the lobe information in the data
region_fooof_data_atlas <- region_fooof_data %>%
  mutate(node = map_roi_to_atlas(node)) %>% 
  left_join(atlas, by = c("node" = "Name"))  %>% 
  mutate(group = factor(group, levels = c("TDC", "FXS")),
       sex = factor(sex, levels = c("M", "F")),
       lobe = factor(lobe),
       node = factor(node), eegid = factor(eegid),
       region = factor(region))

# Use the pre-calculated aperiodic exponent (slope) from the dataset
cat(crayon::blue("Using pre-calculated aperiodic exponents for each region...\n"))

# First, average across nodes within each lobe for each subject
# This ensures we're capturing subject-level variability correctly
lobe_slopes_by_subject <- region_fooof_data_atlas %>%
  filter(!is.na(lobe)) %>%  # Remove any entries with missing lobe information
  mutate(group_sex = paste(group, sex, sep = "-")) %>%
  # Group by subject, lobe, and demographic info
  group_by(eegid, group, sex, lobe, group_sex) %>%
  # Calculate mean slope for each lobe for each subject
  summarize(
    subject_lobe_slope = mean(aperiodic_exponent, na.rm = TRUE),
    .groups = "drop"
  ) 

# r$> region_fooof_data_atlas
# # A tibble: 9,588 × 16
#       V1 eegid group sex   node                      aperiodic_offset aperiodic_knee aperiodic_exponent r_squared  error `theta-alpha_centerfreq` `theta-alpha_power` `theta-alpha_bandwidth` region Acronyms lobe     
#    <int> <fct> <fct> <fct> <fct>                                <dbl>          <dbl>              <dbl>     <dbl>  <dbl>                    <dbl>               <dbl>                   <dbl> <fct>  <chr>    <fct>    
#  1     0 79    FXS   F     bankssts L                           -19.3          33.9                1.49     0.987 0.0362                     9.22               0.774                    1.98 LT     BSTS L   Temporal 
#  2     1 79    FXS   F     bankssts R                           -19.7           9.61               1.29     0.982 0.0448                     8.99               0.723                    2.08 RT     BSTS R   Temporal 
#  3     2 79    FXS   F     caudalanteriorcingulate L            -19.8           7.92               1.53     0.994 0.0282                     9.05               0.726                    2.61 LL     cACC L   Cingulate
#  4     3 79    FXS   F     caudalanteriorcingulate R            -19.4          26.0                1.78     0.995 0.0296                     8.96               0.708                    2.50 RL     cACC R   Cingulate
#  5     4 79    FXS   F     caudalmiddlefrontal L                -20.3           1.32               1.18     0.990 0.0354                     9.19               0.798                    2.48 LF     cMFG L   Frontal  
#  6     5 79    FXS   F     caudalmiddlefrontal R                -19.5          45.7                1.79     0.988 0.0448                     8.95               0.834                    2.49 RF     cMFG R   Frontal  
#  7     6 79    FXS   F     cuneus L                             -17.3        2582.                 2.88     0.993 0.0291                     9.49               0.560                    1.98 LO     CUN L    Occipital
#  8     7 79    FXS   F     cuneus R                             -18.2         293.                 2.28     0.987 0.0451                     9.47               0.521                    1.98 RO     CUN R    Occipital
#  9     8 79    FXS   F     entorhinal L                         -19.4           2.43               1.26     0.989 0.0357                     9.19               0.702                    2.33 LT     ENT L    Temporal 
# 10     9 79    FXS   F     entorhinal R                         -19.3          14.5                1.58     0.995 0.0228                     9.19               0.627                    2.35 RT     ENT R    Temporal 
# # ℹ 9,578 more rows
# # ℹ Use `print(n = ...)` to see more rows


# Load required packages
library(nlme)      # For mixed-effects models
library(emmeans)   # For estimated marginal means and pairwise comparisons
library(ggplot2)   # For plotting
library(dplyr)     # For data manipulation
library(multcomp)  # For multiple comparison adjustment

# 1. Fit a linear mixed-effects model
# Including random intercepts for subjects to account for within-subject correlation
model <- lme(subject_lobe_slope ~ group * sex * lobe, 
             random = ~1|eegid,  # Random intercept for each subject
             data = lobe_slopes_by_subject)

# Print model summary
summary(model)
# Linear mixed-effects model fit by REML
#   Data: lobe_slopes_by_subject 
#        AIC     BIC    logLik
#   819.4283 965.405 -379.7141

# Random effects:
#  Formula: ~1 | eegid
#         (Intercept)  Residual
# StdDev:   0.5234614 0.2694375

# Fixed effects:  subject_lobe_slope ~ group * sex * lobe 
#                                   Value  Std.Error  DF    t-value p-value
# (Intercept)                   1.6887680 0.10248553 822  16.478111  0.0000
# groupTDC                      0.0124445 0.14851565 137   0.083793  0.9333
# sexM                         -0.2787109 0.14096481 137  -1.977167  0.0500
# lobeCingulate                -0.1705822 0.06633093 822  -2.571684  0.0103
# lobeFrontal                  -0.4374794 0.06633093 822  -6.595405  0.0000
# lobeOccipital                -0.2336952 0.06633093 822  -3.523171  0.0004
# lobeParietal                  0.0500446 0.06633093 822   0.754468  0.4508
# lobePrefrontal               -0.7364839 0.06633093 822 -11.103174  0.0000
# lobeTemporal                 -0.4966833 0.06633093 822  -7.487958  0.0000
# groupTDC:sexM                 0.4488542 0.19969616 137   2.247686  0.0262
# groupTDC:lobeCingulate        0.0415939 0.09612265 822   0.432717  0.6653
# groupTDC:lobeFrontal         -0.1119169 0.09612265 822  -1.164314  0.2446
# groupTDC:lobeOccipital        0.1294872 0.09612265 822   1.347104  0.1783
# groupTDC:lobeParietal         0.0660985 0.09612265 822   0.687648  0.4919
# groupTDC:lobePrefrontal      -0.1000510 0.09612265 822  -1.040868  0.2982
# groupTDC:lobeTemporal         0.0337627 0.09612265 822   0.351246  0.7255
# sexM:lobeCingulate            0.0243789 0.09123559 822   0.267208  0.7894
# sexM:lobeFrontal             -0.0328399 0.09123559 822  -0.359946  0.7190
# sexM:lobeOccipital            0.0434008 0.09123559 822   0.475700  0.6344
# sexM:lobeParietal             0.0230949 0.09123559 822   0.253134  0.8002
# sexM:lobePrefrontal          -0.0009486 0.09123559 822  -0.010397  0.9917
# sexM:lobeTemporal            -0.1433830 0.09123559 822  -1.571569  0.1164
# groupTDC:sexM:lobeCingulate   0.0107382 0.12924783 822   0.083082  0.9338
# groupTDC:sexM:lobeFrontal     0.0977012 0.12924783 822   0.755922  0.4499
# groupTDC:sexM:lobeOccipital  -0.1684284 0.12924783 822  -1.303143  0.1929
# groupTDC:sexM:lobeParietal   -0.0296362 0.12924783 822  -0.229298  0.8187
# groupTDC:sexM:lobePrefrontal  0.0178026 0.12924783 822   0.137740  0.8905
# groupTDC:sexM:lobeTemporal    0.0518447 0.12924783 822   0.401126  0.6884
#  Correlation: 
#                              (Intr) grpTDC sexM   lbCngl lbFrnt lbOccp lbPrtl lbPrfr lbTmpr grTDC:M gTDC:C gTDC:F gTDC:O grpTDC:lbPrt grpTDC:lbPrf gTDC:T sxM:lC sxM:lF sxM:lO sxM:lbPrt sxM:lbPrf sxM:lT gTDC:M:C gTDC:M:F
# groupTDC                     -0.690                                                                                                                                                                                        
# sexM                         -0.727  0.502                                                                                                                                                                                 
# lobeCingulate                -0.324  0.223  0.235                                                                                                                                                                          
# lobeFrontal                  -0.324  0.223  0.235  0.500                                                                                                                                                                   
# lobeOccipital                -0.324  0.223  0.235  0.500  0.500                                                                                                                                                            
# lobeParietal                 -0.324  0.223  0.235  0.500  0.500  0.500                                                                                                                                                     
# lobePrefrontal               -0.324  0.223  0.235  0.500  0.500  0.500  0.500                                                                                                                                              
# lobeTemporal                 -0.324  0.223  0.235  0.500  0.500  0.500  0.500  0.500                                                                                                                                       
# groupTDC:sexM                 0.513 -0.744 -0.706 -0.166 -0.166 -0.166 -0.166 -0.166 -0.166                                                                                                                                
# groupTDC:lobeCingulate        0.223 -0.324 -0.162 -0.690 -0.345 -0.345 -0.345 -0.345 -0.345  0.241                                                                                                                         
# groupTDC:lobeFrontal          0.223 -0.324 -0.162 -0.345 -0.690 -0.345 -0.345 -0.345 -0.345  0.241   0.500                                                                                                                 
# groupTDC:lobeOccipital        0.223 -0.324 -0.162 -0.345 -0.345 -0.690 -0.345 -0.345 -0.345  0.241   0.500  0.500                                                                                                          
# groupTDC:lobeParietal         0.223 -0.324 -0.162 -0.345 -0.345 -0.345 -0.690 -0.345 -0.345  0.241   0.500  0.500  0.500                                                                                                   
# groupTDC:lobePrefrontal       0.223 -0.324 -0.162 -0.345 -0.345 -0.345 -0.345 -0.690 -0.345  0.241   0.500  0.500  0.500  0.500                                                                                            
# groupTDC:lobeTemporal         0.223 -0.324 -0.162 -0.345 -0.345 -0.345 -0.345 -0.345 -0.690  0.241   0.500  0.500  0.500  0.500        0.500                                                                               
# sexM:lobeCingulate            0.235 -0.162 -0.324 -0.727 -0.364 -0.364 -0.364 -0.364 -0.364  0.228   0.502  0.251  0.251  0.251        0.251        0.251                                                                  
# sexM:lobeFrontal              0.235 -0.162 -0.324 -0.364 -0.727 -0.364 -0.364 -0.364 -0.364  0.228   0.251  0.502  0.251  0.251        0.251        0.251  0.500                                                           
# sexM:lobeOccipital            0.235 -0.162 -0.324 -0.364 -0.364 -0.727 -0.364 -0.364 -0.364  0.228   0.251  0.251  0.502  0.251        0.251        0.251  0.500  0.500                                                    
# sexM:lobeParietal             0.235 -0.162 -0.324 -0.364 -0.364 -0.364 -0.727 -0.364 -0.364  0.228   0.251  0.251  0.251  0.502        0.251        0.251  0.500  0.500  0.500                                             
# sexM:lobePrefrontal           0.235 -0.162 -0.324 -0.364 -0.364 -0.364 -0.364 -0.727 -0.364  0.228   0.251  0.251  0.251  0.251        0.502        0.251  0.500  0.500  0.500  0.500                                      
# sexM:lobeTemporal             0.235 -0.162 -0.324 -0.364 -0.364 -0.364 -0.364 -0.364 -0.727  0.228   0.251  0.251  0.251  0.251        0.251        0.502  0.500  0.500  0.500  0.500     0.500                            
# groupTDC:sexM:lobeCingulate  -0.166  0.241  0.228  0.513  0.257  0.257  0.257  0.257  0.257 -0.324  -0.744 -0.372 -0.372 -0.372       -0.372       -0.372 -0.706 -0.353 -0.353 -0.353    -0.353    -0.353                  
# groupTDC:sexM:lobeFrontal    -0.166  0.241  0.228  0.257  0.513  0.257  0.257  0.257  0.257 -0.324  -0.372 -0.744 -0.372 -0.372       -0.372       -0.372 -0.353 -0.706 -0.353 -0.353    -0.353    -0.353  0.500           
# groupTDC:sexM:lobeOccipital  -0.166  0.241  0.228  0.257  0.257  0.513  0.257  0.257  0.257 -0.324  -0.372 -0.372 -0.744 -0.372       -0.372       -0.372 -0.353 -0.353 -0.706 -0.353    -0.353    -0.353  0.500    0.500  
# groupTDC:sexM:lobeParietal   -0.166  0.241  0.228  0.257  0.257  0.257  0.513  0.257  0.257 -0.324  -0.372 -0.372 -0.372 -0.744       -0.372       -0.372 -0.353 -0.353 -0.353 -0.706    -0.353    -0.353  0.500    0.500  
# groupTDC:sexM:lobePrefrontal -0.166  0.241  0.228  0.257  0.257  0.257  0.257  0.513  0.257 -0.324  -0.372 -0.372 -0.372 -0.372       -0.744       -0.372 -0.353 -0.353 -0.353 -0.353    -0.706    -0.353  0.500    0.500  
# groupTDC:sexM:lobeTemporal   -0.166  0.241  0.228  0.257  0.257  0.257  0.257  0.257  0.513 -0.324  -0.372 -0.372 -0.372 -0.372       -0.372       -0.744 -0.353 -0.353 -0.353 -0.353    -0.353    -0.706  0.500    0.500  
#                              gTDC:M:O grpTDC:sxM:lbPrt grpTDC:sxM:lbPrf
# groupTDC                                                               
# sexM                                                                   
# lobeCingulate                                                          
# lobeFrontal                                                            
# lobeOccipital                                                          
# lobeParietal                                                           
# lobePrefrontal                                                         
# lobeTemporal                                                           
# groupTDC:sexM                                                          
# groupTDC:lobeCingulate                                                 
# groupTDC:lobeFrontal                                                   
# groupTDC:lobeOccipital                                                 
# groupTDC:lobeParietal                                                  
# groupTDC:lobePrefrontal                                                
# groupTDC:lobeTemporal                                                  
# sexM:lobeCingulate                                                     
# sexM:lobeFrontal                                                       
# sexM:lobeOccipital                                                     
# sexM:lobeParietal                                                      
# sexM:lobePrefrontal                                                    
# sexM:lobeTemporal                                                      
# groupTDC:sexM:lobeCingulate                                            
# groupTDC:sexM:lobeFrontal                                              
# groupTDC:sexM:lobeOccipital                                            
# groupTDC:sexM:lobeParietal    0.500                                    
# groupTDC:sexM:lobePrefrontal  0.500    0.500                           
# groupTDC:sexM:lobeTemporal    0.500    0.500            0.500          

# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -3.54100640 -0.50906644  0.01259547  0.52705444  3.85151150 

# Number of Observations: 987
# Number of Groups: 141 

anova(model)
#               numDF denDF  F-value p-value
# (Intercept)        1   822 942.4382  <.0001
# group              1   137   8.6509  0.0038
# sex                1   137   0.5787  0.4481
# lobe               6   822 195.7567  <.0001
# group:sex          1   137   6.0795  0.0149
# group:lobe         6   822   1.6481  0.1310
# sex:lobe           6   822   1.2295  0.2887
# group:sex:lobe     6   822   0.8352  0.5428

AIC(model)
BIC(model)
# r$> AIC(model)
# [1] 819.4283

# r$> BIC(model)
# [1] 965.405# [1] 819.4283

# Try random intercepts only, but with a different correlation structure
model_corAR1 <- lme(subject_lobe_slope ~ group * sex * lobe,
                   random = ~1|eegid,
                   correlation = corAR1(),  # Autoregressive correlation structure
                   data = lobe_slopes_by_subject)

# Compare models
anova(model, model_corAR1)
# r$> anova(model, model_corAR1)
#              Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# model            1 30 819.4283 965.4050 -379.7141                        
# model_corAR1     2 31 815.3088 966.1514 -376.6544 1 vs 2 6.119443  0.0134

# Change optimization method
model_optim <- lme(subject_lobe_slope ~ group * sex * lobe,
                  random = ~1|eegid,
                  method = "REML",  # Restricted maximum likelihood
                  control = lmeControl(opt = "optim"),  # Different optimizer
                  data = lobe_slopes_by_subject)

# Compare models
anova(model_optim)
#                numDF denDF  F-value p-value
# (Intercept)        1   822 942.4382  <.0001
# group              1   137   8.6509  0.0038
# sex                1   137   0.5787  0.4481
# lobe               6   822 195.7567  <.0001
# group:sex          1   137   6.0795  0.0149
# group:lobe         6   822   1.6481  0.1310
# sex:lobe           6   822   1.2295  0.2887
# group:sex:lobe     6   822   0.8352  0.5428

# Use compound symmetry correlation structure
model_compSym <- lme(subject_lobe_slope ~ group * sex * lobe,
                    random = ~1|eegid,
                    correlation = corCompSymm(form = ~1|eegid),
                    data = lobe_slopes_by_subject)

# Compare models
anova(model_compSym)
#                numDF denDF  F-value p-value
# (Intercept)        1   822 942.4382  <.0001
# group              1   137   8.6509  0.0038
# sex                1   137   0.5787  0.4481
# lobe               6   822 195.7567  <.0001
# group:sex          1   137   6.0795  0.0149
# group:lobe         6   822   1.6481  0.1310
# sex:lobe           6   822   1.2295  0.2887
# group:sex:lobe     6   822   0.8352  0.5428


# Simplify fixed effects structure
model_reduced_fixed <- lme(subject_lobe_slope ~ group + sex + lobe + 
                          group:sex + group:lobe + sex:lobe,
                         random = ~1|eegid,
                         data = lobe_slopes_by_subject)

anova(model_reduced_fixed)
# r$> anova(model_reduced_fixed)
#             numDF denDF  F-value p-value
# (Intercept)     1   828 942.4382  <.0001
# group           1   137   8.6509  0.0038
# sex             1   137   0.5787  0.4481
# lobe            6   828 195.9907  <.0001
# group:sex       1   137   6.0795  0.0149
# group:lobe      6   828   1.6500  0.1304
# sex:lobe        6   828   1.2309  0.2879

summary(model_reduced_fixed)
# Linear mixed-effects model fit by REML
#   Data: lobe_slopes_by_subject 
#        AIC      BIC    logLik
#   796.7007 913.6318 -374.3504

# Random effects:
#  Formula: ~1 | eegid
#         (Intercept)  Residual
# StdDev:   0.5234732 0.2692766

# Fixed effects:  subject_lobe_slope ~ group + sex + lobe + group:sex + group:lobe +      sex:lobe 
#                              Value  Std.Error  DF    t-value p-value
# (Intercept)              1.8707509 0.09017038 828  20.746844  0.0000
# groupFXS                -0.4600233 0.12802228 137  -3.593307  0.0005
# sexF                    -0.1687114 0.13491943 137  -1.250460  0.2133
# lobeCingulate           -0.0961476 0.05279435 828  -1.821172  0.0689
# lobeFrontal             -0.5052467 0.05279435 828  -9.570090  0.0000
# lobeOccipital           -0.1935304 0.05279435 828  -3.665741  0.0003
# lobeParietal             0.1158843 0.05279435 828   2.195014  0.0284
# lobePrefrontal          -0.8234548 0.05279435 828 -15.597404  0.0000
# lobeTemporal            -0.5654494 0.05279435 828 -10.710416  0.0000
# groupFXS:sexF            0.4460002 0.18088468 137   2.465661  0.0149
# groupFXS:lobeCingulate  -0.0475332 0.06422004 828  -0.740162  0.4594
# groupFXS:lobeFrontal     0.0578782 0.06422004 828   0.901249  0.3677
# groupFXS:lobeOccipital  -0.0363292 0.06422004 828  -0.565699  0.5718
# groupFXS:lobeParietal   -0.0497067 0.06422004 828  -0.774005  0.4391
# groupFXS:lobePrefrontal  0.0902044 0.06422004 828   1.404614  0.1605
# groupFXS:lobeTemporal   -0.0624381 0.06422004 828  -0.972252  0.3312
# sexF:lobeCingulate      -0.0297296 0.06458493 828  -0.460318  0.6454
# sexF:lobeFrontal        -0.0158436 0.06458493 828  -0.245315  0.8063
# sexF:lobeOccipital       0.0405253 0.06458493 828   0.627474  0.5305
# sexF:lobeParietal       -0.0083274 0.06458493 828  -0.128938  0.8974
# sexF:lobePrefrontal     -0.0079223 0.06458493 828  -0.122665  0.9024
# sexF:lobeTemporal        0.1175493 0.06458493 828   1.820073  0.0691
#  Correlation: 
#                         (Intr) grpFXS sexF   lbCngl lbFrnt lbOccp lbPrtl lbPrfr lbTmpr grpFXS:sF gFXS:C grpFXS:lF gFXS:O grpFXS:lbPrt grpFXS:lbPrf gFXS:T sxF:lC sxF:lF sxF:lO sxF:lbPrt sxF:lbPrf
# groupFXS                -0.673                                                                                                                                                                    
# sexF                    -0.632  0.397                                                                                                                                                             
# lobeCingulate           -0.293  0.145  0.124                                                                                                                                                      
# lobeFrontal             -0.293  0.145  0.124  0.500                                                                                                                                               
# lobeOccipital           -0.293  0.145  0.124  0.500  0.500                                                                                                                                        
# lobeParietal            -0.293  0.145  0.124  0.500  0.500  0.500                                                                                                                                 
# lobePrefrontal          -0.293  0.145  0.124  0.500  0.500  0.500  0.500                                                                                                                          
# lobeTemporal            -0.293  0.145  0.124  0.500  0.500  0.500  0.500  0.500                                                                                                                   
# groupFXS:sexF            0.425 -0.631 -0.673  0.000  0.000  0.000  0.000  0.000  0.000                                                                                                            
# groupFXS:lobeCingulate   0.169 -0.251  0.012 -0.577 -0.289 -0.289 -0.289 -0.289 -0.289  0.000                                                                                                     
# groupFXS:lobeFrontal     0.169 -0.251  0.012 -0.289 -0.577 -0.289 -0.289 -0.289 -0.289  0.000     0.500                                                                                           
# groupFXS:lobeOccipital   0.169 -0.251  0.012 -0.289 -0.289 -0.577 -0.289 -0.289 -0.289  0.000     0.500  0.500                                                                                    
# groupFXS:lobeParietal    0.169 -0.251  0.012 -0.289 -0.289 -0.289 -0.577 -0.289 -0.289  0.000     0.500  0.500     0.500                                                                          
# groupFXS:lobePrefrontal  0.169 -0.251  0.012 -0.289 -0.289 -0.289 -0.289 -0.577 -0.289  0.000     0.500  0.500     0.500  0.500                                                                   
# groupFXS:lobeTemporal    0.169 -0.251  0.012 -0.289 -0.289 -0.289 -0.289 -0.289 -0.577  0.000     0.500  0.500     0.500  0.500        0.500                                                      
# sexF:lobeCingulate       0.151  0.012 -0.239 -0.517 -0.258 -0.258 -0.258 -0.258 -0.258  0.000    -0.049 -0.025    -0.025 -0.025       -0.025       -0.025                                         
# sexF:lobeFrontal         0.151  0.012 -0.239 -0.258 -0.517 -0.258 -0.258 -0.258 -0.258  0.000    -0.025 -0.049    -0.025 -0.025       -0.025       -0.025  0.500                                  
# sexF:lobeOccipital       0.151  0.012 -0.239 -0.258 -0.258 -0.517 -0.258 -0.258 -0.258  0.000    -0.025 -0.025    -0.049 -0.025       -0.025       -0.025  0.500  0.500                           
# sexF:lobeParietal        0.151  0.012 -0.239 -0.258 -0.258 -0.258 -0.517 -0.258 -0.258  0.000    -0.025 -0.025    -0.025 -0.049       -0.025       -0.025  0.500  0.500  0.500                    
# sexF:lobePrefrontal      0.151  0.012 -0.239 -0.258 -0.258 -0.258 -0.258 -0.517 -0.258  0.000    -0.025 -0.025    -0.025 -0.025       -0.049       -0.025  0.500  0.500  0.500  0.500             
# sexF:lobeTemporal        0.151  0.012 -0.239 -0.258 -0.258 -0.258 -0.258 -0.258 -0.517  0.000    -0.025 -0.025    -0.025 -0.025       -0.025       -0.049  0.500  0.500  0.500  0.500     0.500   

# Standardized Within-Group Residuals:
#         Min          Q1         Med          Q3         Max 
# -3.52688223 -0.51575523  0.01643125  0.51786684  3.85584700 

# Number of Observations: 987
# Number of Groups: 141 


# Compare models
anova(model, model_reduced_fixed, model_corAR1, model_optim, model_compSym)
# r$> anova(model, model_reduced_fixed, model_corAR1, model_optim, model_compSym)
#                     Model df      AIC      BIC    logLik   Test   L.Ratio p-value
# model                   1 30 819.4283 965.4050 -379.7141                         
# model_reduced_fixed     2 24 796.7007 913.6318 -374.3504 1 vs 2 10.727505  0.0972
# model_corAR1            3 31 815.3088 966.1514 -376.6544 2 vs 3  4.608062  0.7077
# model_optim             4 30 819.4283 965.4050 -379.7141 3 vs 4  6.119443  0.0134
# model_compSym           5 31 821.4283 972.2709 -379.7141 4 vs 5  0.000000  1.0000
# Warning message:
# In anova.lme(model, model_reduced_fixed, model_corAR1, model_optim,  :
#   fitted objects with different fixed effects. REML comparisons are not meaningful.


# 1. Check model diagnostics
plot(model_reduced_fixed)
qqnorm(residuals(model_reduced_fixed))
qqline(residuals(model_reduced_fixed))

# 2. Get a summary of the reduced model
summary(model_reduced_fixed)
anova(model_reduced_fixed)

# 3. For your main research questions:

# A. For lobe effects (do slopes vary among brain regions?)
emm_lobe <- emmeans(model_reduced_fixed, ~ lobe)
#  lobe       emmean     SE  df lower.CL upper.CL
#  Central     1.668 0.0499 137    1.569    1.767
#  Cingulate   1.533 0.0499 137    1.434    1.632
#  Frontal     1.184 0.0499 137    1.085    1.282
#  Occipital   1.476 0.0499 137    1.378    1.575
#  Parietal    1.755 0.0499 137    1.656    1.853
#  Prefrontal  0.886 0.0499 137    0.787    0.984
#  Temporal    1.130 0.0499 137    1.031    1.229

# Results are averaged over the levels of: group, sex 
# Degrees-of-freedom method: containment 
# Confidence level used: 0.95 

pairs(emm_lobe, adjust = "tukey")  # Pairwise comparisons between lobes

# contrast               estimate     SE  df t.ratio p.value
#  Central - Cingulate      0.1348 0.0323 828   4.179  0.0006
#  Central - Frontal        0.4842 0.0323 828  15.013  <.0001
#  Central - Occipital      0.1914 0.0323 828   5.935  <.0001
#  Central - Parietal      -0.0869 0.0323 828  -2.693  0.1012
#  Central - Prefrontal     0.7823 0.0323 828  24.255  <.0001
#  Central - Temporal       0.5379 0.0323 828  16.677  <.0001
#  Cingulate - Frontal      0.3495 0.0323 828  10.835  <.0001
#  Cingulate - Occipital    0.0567 0.0323 828   1.757  0.5780
#  Cingulate - Parietal    -0.2216 0.0323 828  -6.872  <.0001
#  Cingulate - Prefrontal   0.6475 0.0323 828  20.076  <.0001
#  Cingulate - Temporal     0.4031 0.0323 828  12.498  <.0001
#  Frontal - Occipital     -0.2928 0.0323 828  -9.078  <.0001
#  Frontal - Parietal      -0.5711 0.0323 828 -17.707  <.0001
#  Frontal - Prefrontal     0.2981 0.0323 828   9.242  <.0001
#  Frontal - Temporal       0.0537 0.0323 828   1.664  0.6405
#  Occipital - Parietal    -0.2783 0.0323 828  -8.629  <.0001
#  Occipital - Prefrontal   0.5909 0.0323 828  18.320  <.0001
#  Occipital - Temporal     0.3465 0.0323 828  10.742  <.0001
#  Parietal - Prefrontal    0.8692 0.0323 828  26.948  <.0001
#  Parietal - Temporal      0.6248 0.0323 828  19.370  <.0001
#  Prefrontal - Temporal   -0.2444 0.0323 828  -7.578  <.0001

# Results are averaged over the levels of: group, sex 
# Degrees-of-freedom method: containment 
# P value adjustment: tukey method for comparing a family of 7 estimates 


# B. For group comparisons within sex (sex-matched comparisons)
# Males: FXS-M vs TDC-M
emm_group_by_sex <- emmeans(model_reduced_fixed, ~ group | sex)
male_contrasts <- pairs(emm_group_by_sex, by = "sex")

# sex = M:
#  contrast  estimate    SE  df t.ratio p.value
#  TDC - FXS   0.4669 0.121 137   3.861  0.0002

# sex = F:
#  contrast  estimate    SE  df t.ratio p.value
#  TDC - FXS   0.0209 0.135 137   0.155  0.8769

# Results are averaged over the levels of: lobe 
# Degrees-of-freedom method: containment 


# Load required packages
library(ggplot2)
library(dplyr)
library(patchwork)
library(viridis)
library(emmeans)
library(nlme)

# Extract the emmeans data for plotting
emmeans_lobe <- as.data.frame(summary(emm_lobe))
emmeans_group_sex <- as.data.frame(summary(emm_group_by_sex))
emmeans_lobe_by_group_sex <- as.data.frame(summary(emmeans(model_reduced_fixed, ~ lobe | group | sex)))

# 1. MAIN VISUALIZATION: Comprehensive panel figure showing key results
# Set a cohesive color scheme
lobe_colors <- c(
  "Central" = "#3498db",       # Blue
  "Cingulate" = "#e74c3c",     # Red
  "Frontal" = "#2ecc71",       # Green
  "Occipital" = "#f39c12",     # Orange
  "Parietal" = "#9b59b6",      # Purple
  "Prefrontal" = "#1abc9c",    # Turquoise
  "Temporal" = "#d35400"       # Burnt orange
)

group_sex_colors <- c(
  "FXS-F" = "#FF9AA2",  # Light salmon for FXS Female
  "FXS-M" = "#FF6B6B",  # Darker salmon for FXS Male  
  "TDC-F" = "#A0C4FF",  # Light blue for TDC Female
  "TDC-M" = "#4A7BFF"   # Darker blue for TDC Male
)

# Plot A: Lobes across all subjects
plot_lobe <- ggplot(emmeans_lobe  %>% broom.mixed::tidy(), aes(x = reorder(lobe, estimate), y = estimate, fill = lobe)) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = lobe_colors) +
  labs(title = "A. Aperiodic Slope by Brain Lobe",
       x = NULL,
       y = "Aperiodic Slope") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5)
  )

# Plot B: Group x Sex interaction
# Prepare data for group x sex plot (with custom grouping)
emmeans_group_sex$group_sex <- paste(emmeans_group_sex$group, emmeans_group_sex$sex, sep = "-")

emmeans_group_sex <- emmeans_group_sex %>% broom.mixed::tidy()

plot_group_sex <- ggplot(emmeans_group_sex, aes(x = group, y = estimate, fill = group_sex)) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.1) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = group_sex_colors) +
  facet_wrap(~ sex, labeller = labeller(sex = c("M" = "Males", "F" = "Females"))) +
  labs(title = "B. Group Differences by Sex",
       x = "Group",
       y = "Aperiodic Slope",
       subtitle = "*** p < 0.001, ns = not significant") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5)
  ) + 
  # Add significance indicators
  geom_text(data = data.frame(x = 1.5, y = max(emmeans_group_sex$estimate) + 0.15, 
                              sex = "M", group_sex = "FXS-M", 
                              label = "***"), 
            aes(x = x, y = y, label = label), inherit.aes = FALSE) +
  geom_text(data = data.frame(x = 1.5, y = max(emmeans_group_sex$estimate) + 0.15, 
                              sex = "F", group_sex = "FXS-F", 
                              label = "ns"), 
            aes(x = x, y = y, label = label), inherit.aes = FALSE)

# Plot C: Detailed lobe slopes by group and sex
# Add group_sex column to facilitate plotting
emmeans_lobe_by_group_sex$group_sex <- paste(emmeans_lobe_by_group_sex$group, 
                                           emmeans_lobe_by_group_sex$sex, sep = "-")

emmeans_lobe_by_group_sex <- emmeans_lobe_by_group_sex %>% broom.mixed::tidy()

plot_detailed <- ggplot(emmeans_lobe_by_group_sex, 
                       aes(x = reorder(lobe, estimate), y = estimate, 
                           fill = group_sex, group = group_sex)) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                position = position_dodge(0.8), width = 0.1) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +

  scale_fill_manual(values = group_sex_colors,
                   labels = c("FXS Female", "FXS Male", "TDC Female", "TDC Male")) +
  facet_wrap(~ lobe, ncol = 4) +
  labs(title = "C. Detailed View of Aperiodic Slopes",
       x = NULL,
       y = "Aperiodic Slope",
       fill = "Group") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5)
  )

# Plot D: Gradient brain map visualization
# This requires a matrix of average values by lobe
# Create a separate visualization for a "brain map" showing slopes
# Here we'll create a conceptual representation rather than anatomically precise

# Create a combined multi-panel figure
combined_plot <- (plot_lobe + plot_group_sex) +
  plot_layout(heights = c(1, 1.5)) +
  plot_annotation(
    title = "Aperiodic Slopes Across Brain Regions in FXS and TDC Groups",
    subtitle = "Higher values indicate shallower 1/f slopes",
    caption = "Error bars represent standard error. FXS = Fragile X Syndrome, TDC = Typically Developing Controls",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 1)
    )
  )

# Save high-resolution plot
ggsave("aperiodic_slopes_comprehensive.png", combined_plot, 
       width = 12, height = 14, dpi = 300)

# 2. SUPPLEMENTARY VISUALIZATION: Lobe-specific group comparisons for males
# Create a plot focusing on the significant male differences across lobes

# Get emmeans for males only, by lobe and group
emmeans_males_by_lobe <- as.data.frame(summary(emmeans(model_reduced_fixed, 
                                                    ~ group | lobe | sex, 
                                                    at = list(sex = "M"))))

# Plot male-specific contrasts
male_plot <- ggplot(emmeans_males_by_lobe, 
                   aes(x = lobe, y = estimate, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), 
                position = position_dodge(0.8), width = 0.3) +
  scale_fill_manual(values = c("FXS" = "#FF6B6B", "TDC" = "#4A7BFF"),
                   name = "Group",
                   labels = c("FXS Male", "TDC Male")) +
  labs(title = "Aperiodic Slopes in Males by Brain Lobe and Group",
       subtitle = "Significant differences between FXS and TDC males (p < 0.001)",
       x = "Brain Lobe",
       y = "Aperiodic Slope") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA, size = 0.5)
  )

# Save male-specific plot
ggsave("aperiodic_slopes_males.png", male_plot, 
       width = 10, height = 8, dpi = 300)