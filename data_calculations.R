################################################################################
################################################################################
##################  Data Calculations  ########################################
################################################################################
################################################################################

# ------------------------------------------------------------------------------
# -------------------- Libraries -----------------------------------------------
# ------------------------------------------------------------------------------
#library(readxl)
library(haven)
library(dplyr)
library(tidyr)


#library(openxlsx)
#library(stringr) 
#library(purrr)

# ------------------------------------------------------------------------------
# -------------------- Path setting --------------------------------------------
# ------------------------------------------------------------------------------
data_path <- "/Users/manuelestebanarias/Documents/GitHub/wid-world/work-data_updated/aggregate-regions-output.dta"
export_path <- "/Users/manuelestebanarias/Documents/GitHub/WIDDataCompleatness"

app_path <- file.path(export_path, "WIDDataCompleatness.R")
# ------------------------------------------------------------------------------
# -------------------- Data import ---------------------------------------------
# ------------------------------------------------------------------------------
data <- read_dta(data_path)

data$p[data$p == "pall"] <- "p0p100"

file_name <- sub("\\.dta$", "", basename(data_path))

# ------------------------------------------------------------------------------
# -------------------- Functions ---------------------------------------------
# ------------------------------------------------------------------------------
# 1. Function to add the country columns
add_country_groups <- function(data) {
  data %>%
    mutate(
      Coreterritories = ifelse(iso %in% coreterritories, 1, 0),
      Corecountries = ifelse(iso %in% corecountries, 1, 0),
      Coreterritoriesmer = ifelse(iso %in% coreterritoriesmer, 1, 0),
      Europe = ifelse(iso %in% EURO, 1, 0),
      NorthAmerica_Oceania = ifelse(iso %in% NAOC, 1, 0),
      LatinAmerica = ifelse(iso %in% LATA, 1, 0),
      MiddleEast_NorthAfrica = ifelse(iso %in% MENA, 1, 0),
      SubSaharanAfrica = ifelse(iso %in% SSAF, 1, 0),
      Russia_CentralAsia = ifelse(iso %in% RUCA, 1, 0),
      EastAsia = ifelse(iso %in% EASA, 1, 0),
      South_EastAsia = ifelse(iso %in% SSEA, 1, 0),
      Oil = ifelse(iso %in% oil, 1, 0),
      Not_corecountries = ifelse(grepl("^[A-NP-Z][A-Z]$", iso) & 
                                   !grepl("^[OQX]", iso) & 
                                   (iso %in% not_corecountries), 1, 0),
      Regions_PPP = ifelse(iso %in% regions, 1, 0),
      Regions_MER = ifelse(iso %in% regionsMER, 1, 0),
      Subcountries = ifelse(iso %in% subcountries, 1, 0),
    )
}

# 2. Function to add P groups
add_p_filters <- function(data) {
  data %>%
    mutate(
      Groupped = ifelse(p %in% group, 1, 0),
      Deciles = ifelse(p %in% deciles, 1, 0),
      Percentiles = ifelse(p %in% percentiles, 1, 0),
      Top = ifelse(p %in% top, 1, 0),
      Bottom = ifelse(p %in% bottom, 1, 0),
      p0p100 = ifelse(p=="p0p100", 1, 0)
    )
}

# 3. Funciton to add widcode filters:
add_widcodes_filters <- function (data) {
  data %>%
    mutate(
      # One letter indicator
      Average     = ifelse(grepl("^a",     widcode), 1, 0),
      Pareto_Lrnz = ifelse(grepl("^b",     widcode), 1, 0),
      Fem_pop     = ifelse(grepl("^f",     widcode), 1, 0),
      Gini        = ifelse(grepl("^g",     widcode), 1, 0),
      Idx_Xrate   = ifelse(grepl("^(i|x)", widcode), 1, 0),
      Pop         = ifelse(grepl("^n",     widcode), 1, 0),
      Share       = ifelse(grepl("^s",     widcode), 1, 0),
      Threshold   = ifelse(grepl("^t",     widcode), 1, 0),
      Total       = ifelse(grepl("^m",     widcode), 1, 0),
      Fem_por     = ifelse(grepl("^p",     widcode), 1, 0),
      Weal_Inc    = ifelse(grepl("^w",     widcode), 1, 0),
      Top_bot     = ifelse(grepl("^r",     widcode), 1, 0),
      Emissions   = ifelse(grepl("^e",     widcode), 1, 0),
      Emssns_pc   = ifelse(grepl("^k",     widcode), 1, 0),
      Emsn_Avg_g  = ifelse(grepl("^l",     widcode), 1, 0),
      # Five letter indicators
      Agg_Income_Macro   = ifelse(grepl(Agg_Income_Macro,   widcode),1, 0),
      Agg_Income_H_NPISH = ifelse(grepl(Agg_Income_H_NPISH, widcode),1, 0),
      Agg_Income_Corp    = ifelse(grepl(Agg_Income_Corp,    widcode),1, 0),
      Agg_Income_Govr    = ifelse(grepl(Agg_Income_Govr,    widcode),1, 0),
      Agg_Income_CA      = ifelse(grepl(Agg_Income_CA,      widcode),1, 0),
      Distr_Income   = ifelse(grepl(Distr_Income,   widcode),1, 0),
      Agg_Wealth     = ifelse(grepl(Agg_Wealth,     widcode),1, 0),
      Distr_wealth   = ifelse(grepl(Distr_wealth,   widcode),1, 0),
      Indexes        = ifelse(grepl(Indexes,        widcode),1, 0),
      Populations    = ifelse(grepl(Populations,    widcode),1, 0),
      Ratios         = ifelse(grepl(Ratios,         widcode),1, 0),
      Inequality_Idx = ifelse(grepl(Inequality_Idx, widcode),1, 0),
      Agg_Carbon     = ifelse(grepl(Agg_Carbon,     widcode),1, 0),
      Distr_Carbon   = ifelse(grepl(Distr_Carbon,   widcode),1, 0)
    )
}

# ------------------------------------------------------------------------------
# -------------------- Definitions ---------------------------------------------
# ------------------------------------------------------------------------------

#--------  Now. ----------------------------------------------------------------
current_date <- Sys.Date()
current_year <- as.numeric(format(Sys.Date(), "%Y"))

if (current_date < as.Date(paste0(current_year, "-05-01"))) {
  last_year <- current_year - 2
} else {
  last_year <- current_year - 1
}

#--------  Unique values. ------------------------------------------------------
# Widcodes STILL TO BE CHECKED!!!!!
Agg_Income_Macro     <- c("nninc|ndpro|gdpro|confc|nnfin|finrx|flcir|comrx|pinrx|fdirx|
                          ptfrx|ptdrx|pterx|ptrrx|ptfrr|fsubx|fpsub|fosub|finpx|
                          flcip|compx|pinpx|fdipx|ptfpx|ptdpx|ptepx|ptfrp|ftaxx|
                          fptax|fotax|flcin|pinnx|fdinx|ptfnx|ptdnx|ptenx|ptfrn|
                          taxnx|prtxn|optxn|nwnxa|fdixn|fdixa|fdixd|ptfxn|ptfxa|
                          ptexa|ptdxa|ptrxa|ptfxd|ptexd|ptdxd|prigo|prihn|priho|
                          prinp|prico|prinf|prifc|secgo|sechn|secho|secnp|secco|
                          secnf|secfc|prphn|prpco|prpgo|taxgo|tiwhn|taxco|sschn|
                          sscgo|sscco|ssbhn|ssbgo|ssbco|savin|savhn|savho|savnp|
                          savco|savig|saghn|sagho|sagnp|sagco|segnf|segfc|saggo|
                          cfchn|cfcho|cfcnp|cfcco|cfcnf|cfcfc|cfcgo|comhn|fkpin|
                          nsrhn|nmxho|ptxgo|labsh|capsh") #

Agg_Income_H_NPISH  <- c("prihn|comhn|prphn|nsmhn|nsrhn|nmxhn|prghn|gsmhn|gsrhn|
                          gmxhn|sechn|taxhn|sschn|tiwhn|ssbhn|seghn|savhn|conhn|
                          saghn|cfchn|ccshn|ccmhn|priho|comho|prpho|nsmho|nsrho|
                          nmxho|prgho|gsmho|gsrho|gmxho|secho|taxho|sscho|tiwho|
                          ssbho|segho|savho|conho|sagho|cfcho|ccsho|ccmho|prinp|
                          comnp|prpnp|nsrnp|prgnp|gsrnp|secnp|taxnp|sscnp|tiwnp|
                          ssbnp|segnp")

Agg_Income_Corp     <- c("prico|prpco|nsrco|prgco|prpco|gsrco|secco|prico|taxco|
                         sscco|ssbco|segco|prgco|taxco|sscco|ssbco|prgco|prico|
                         cfcco|segco|secco|cfcco|prinf|prpnf|nsrnf|prgnf|prpnf|
                         gsrnf|secnf|prinf|taxnf|sscnf|ssbnf|segnf|prgnf|taxnf|
                         sscnf|ssbnf|prgnf|prinf|cfcnf|segnf|secnf|cfcnf|prifc|
                         prpfc|nsrfc|prgfc|prpfc|gsrfc|secfc|prifc|taxfc|sscfc|
                         ssbfc|segfc|prgfc|taxfc|sscfc|ssbfc|prgfc|prifc|cfcfc|
                         segfc|secfc|cfcfc" )

Agg_Income_Govr     <- c("prigo|ptxgo|tpigo|tprgo|otpgp|spigo|sprgo|ospgo|prpgo|
                         nsrgo|prggo|ptxgo|tpigo|tprgo|otpgp|spigo|sprgo|ospgo|
                         prpgo|gsrgo|secgo|prigo|taxgo|tiwgo|sscgo|ssbgo|seggo|
                         prggo|taxgo|tiwgo|sscgo|ssbgo|savgo|secgo|congo|indgo|
                         colgo|saggo|seggo|congo|indgo|colgo|congo|gpsgo|defgo|
                         polgo|ecogo|envgo|hougo|heago|recgo|edugo|sopgo|othgo|
                         expgo|gpsge|defge|polge|ecoge|envge|houge|heage|recge|
                         eduge|edpge|edsge|edtge|sopge|spige|sacge|sakge|revgo|
                         pitgr|citgr|scogr|pwtgr|intgr|ottgr|ntrgr|retgo|revgo|
                         ntrgr|psugo|revgo|expgo|ssugo|psugo|inpgo" )

Agg_Income_CA       <- c("ncanx|pinnx|pinrx|pinpx|comnx|comrx|compx|tbnnx|tbxrx|
                         tbmpx|taxnx|fsubx|ftaxx|scgnx|scgrx|scgpx|scrnx|scrrx|
                         scrpx|sconx|scorx|scopx|tbnnx|tgnnx|tgxrx|tgmpx|tsnnx|
                         tsxrx|tsmpx|fkanx|fkarx|fkapx" )

Distr_Income        <- c("fiinc|filin|fiwag|fimil|ficap|firen|fiint|fidiv|fikgi|
                         fimik|fimix|fimil|fimik|ptinc|ptlin|ptkin|ptinc|pllin|
                         pkkin|diinc|cainc|fainc|flinc")

Agg_Wealth          <- c("nweal|nwnfa|nwhou|nwdwe|nwlan|nwbus|nwagr|nwnat|nwodk|nwnxa|
                      nwgxa|nwgxd|nwboo|nweal|cwres|nwdka|nweal|nwnxa|pweal|pwnfa|
                      pwhou|pwdwe|pwlan|pwbus|pwagr|pwnat|pwodk|pwfin|pwfiw|pwcud|
                      pwbol|pwequ|pweqi|pwoff|pwpen|pwdeb|pwfie|pwfin|pwcud|hweal|
                      hwnfa|hwhou|hwdwe|hwlan|hwbus|hwagr|hwnat|hwodk|hwfin|hwfiw|
                      hwcud|hwbol|hwequ|hweqi|hwoff|hwpen|hwdeb|hwfie|hwfin|hwcud|
                      iweal|iwnfa|iwhou|iwdwe|iwlan|iwbus|iwagr|iwnat|iwodk|iwfin|
                      iwfiw|iwcud|iwbol|iwequ|iweqi|iwoff|iwpen|iwdeb|iwfie|iwfin|
                      iwcud|cwboo|cwnfa|cwhou|cwdwe|cwlan|cwbus|cwagr|cwnat|cwodk|
                      cwfin|cwfiw|cwcud|cwbol|cwequ|cweqi|cwoff|cwpen|cwdeb|cwdeq|
                      cwboo|cwres|cwtoq|cwfie|cwfin|cwcud|gweal|gwnfa|gwhou|gwdwe|
                      gwlan|gwbus|gwagr|gwnat|gwodk|gwfin|gwfiw|gwcud|gwbol|gwequ|
                      gweqi|gwoff|gwpen|gwdeb|gwdec|gwfie|gwfin|gwcud")

Distr_wealth   <- c( "hweal|hwnfa|hwhou|hwdwe|hwlan|hwbus|hwagr|hwnat|hwodk|hwfin|
                      hwfiw|hwcud|hwbol|hwequ|hweqi|hwoff|hwpen|hwdeb|hwfie|hwfin|
                      hwcud") 

Indexes        <- c( "nyixx|lcusp|lceup|lcyup|lcusx|lceux|lcyux") #OnlyI
Populations    <- c( "npopul|npopem|ntaxto|ntaxma|ntaxad|ntaxre")
Ratios         <- c( "labsh|capsh|wealn|wealp|wealh|weali|wealc|wealg") #Only W
Inequality_Idx <- c( "quali") #Only I
Agg_Carbon     <- c( "ntghg|ntcar|ntgho|ntghg|hfghd|ntgna|ntcna|ntona|nighg|nicar|
                      nigho|nighg|highg|hicar|higho|gighg|gicar|gigho|iighg|iicar|
                      iigho|oighg|oicar|oigho|neghg|necar|negho|nnghg|nncar|nngho|
                      nfghg|nfcar|nfgho|nfghg|hfghg|hfghd|hfghn|hfcar|hfgho|gfghg|
                      gfcar|gfgho|ifghg|ifcar|ifgho|ofghg|ofcar|ofgho")

Distr_Carbon   <- c( "ptghg|ptcar|ptgho|pfghg|pfcar|pfgho")




widcodes_hm <- unique(data$widcode[grepl("popul|nninc", data$widcode)]) #iso<- core
widcodes_cc <- unique(data$widcode[!grepl("popul|nninc|ptinc|diinc", data$widcode)]) #iso all
widcodes_cds <- unique(data$widcode[grepl("ptinc", data$widcode)]) # iso<- core
widcodes_ds <- unique(data$widcode[grepl("ptinc|diinc", data$widcode)]) #iso all

widcode_macro_base <- unique(data$widcode[grepl("confc|gdpro|nninc", data$widcode)]) 

widcodes_Agg_Income_Macro <- unique(data$widcode[grepl(Agg_Income_Macro, data$widcode)])
widcodes_Agg_Income_H_NPISH <- unique(data$widcode[grepl(Agg_Income_H_NPISH, data$widcode)])
widcodes_Agg_Income_Corp <- unique(data$widcode[grepl(Agg_Income_Corp, data$widcode)])
widcodes_Agg_Income_Govr <- unique(data$widcode[grepl(Agg_Income_Govr, data$widcode)])
widcodes_Agg_Income_CA <- unique(data$widcode[grepl(Agg_Income_CA, data$widcode)])
widcodes_Distr_Income <- unique(data$widcode[grepl(Distr_Income, data$widcode)])
widcodes_Agg_Wealth <- unique(data$widcode[grepl(Agg_Wealth, data$widcode)])
widcodes_Distr_wealth <- unique(data$widcode[grepl(Distr_wealth, data$widcode)])
widcodes_Indexes <- unique(data$widcode[grepl(Indexes, data$widcode)])
widcodes_Populations <- unique(data$widcode[grepl(Populations, data$widcode)])
widcodes_Ratios <- unique(data$widcode[grepl(Ratios, data$widcode)])
widcodes_Inequality_Idx <- unique(data$widcode[grepl(Inequality_Idx, data$widcode)])
widcodes_Agg_Carbon <- unique(data$widcode[grepl(Agg_Carbon, data$widcode)])
widcodes_Distr_Carbon <- unique(data$widcode[grepl(Distr_Carbon, data$widcode)])

#--------  Fractiles -----------------------------------------------------------

## 1. Groupped
group <- c("p0p50", "p50p90", "p99p100", "p99.99p100")
## 2. Deciles
ranges <- seq(0, 90, by = 10)
deciles <- paste0("p", ranges, "p", ranges + 10)
## 3. Pcercentiles
ranges <- seq(0, 99, by = 1)
percentiles_0 <- paste0("p", ranges, "p", ranges + 1)

ranges <- seq(99, 99.9, by = 0.1)
top1 <- paste0("p", ranges, "p", ranges + 0.1)

ranges <- seq(99.9, 99.99, by = 0.01)
top01 <- paste0("p", ranges, "p", ranges + 0.01)

ranges <- seq(99.99, 99.999, by = 0.001)
top001 <- paste0("p", ranges, "p", ranges + 0.001)

percentiles <- setdiff(unique(c(percentiles_0, top1, top01, top001)), c("p99p100", "p99.9p100", "p99.99p100"))
rm(percentiles_0, top1, top01, top001)

## 4. Top
ranges <- seq(0, 99, by = 1)
top <- paste0("p", ranges, "p", 100)

## 5. Bottom
ranges <- seq(0, 99, by = 1)
bottom <- paste0("p",0, "p", ranges + 1)

## 6. ALL
p0p100=c("p0p100")

## 6. todos
any_p <- unique(c(group, deciles, percentiles, top, bottom, p0p100))

### 6.1 generating order for 
p_orders <- data.frame(p = any_p)
#p_orders$first_number <- gsub("p([0-9\\.]+)p.*", "\\1", p_orders$p)
#p_orders$second_number <- gsub(".*p([0-9\\.]+)$", "\\1", p_orders$p)
p_orders$order_p <- seq_len(nrow(p_orders))

#--------  Regions  ------------------------------------------------------------
# Define  REGIONS
corecountries <- c("AD", "AE", "AF", "AG", "AI", "AL", "AM", "AO", "AR", "AT", "AU", 
                   "AW", "AZ", "BA", "BB", "BD", "BE", "BF", "BG", "BH", "BI", "BJ",
                   "BM", "BN", "BO", "BQ", "BR", "BS", "BT", "BW", "BY", "BZ", "CA", 
                   "CD", "CF", "CG", "CH", "CI", "CL", "CM", "CN", "CO", "CR", "CU", 
                   "CV", "CW", "CY", "CZ", "DE", "DJ", "DK", "DM", "DO", "DZ", "EC", 
                   "EE", "EG", "ER", "ES", "ET", "FI", "FJ", "FM", "FR", "GA", "GB", 
                   "GD", "GE", "GG", "GH", "GI", "GL", "GM", "GN", "GQ", "GR", "GT", 
                   "GW", "GY", "HK", "HN", "HR", "HT", "HU", "ID", "IE", "IL", "IM", 
                   "IN", "IQ", "IR", "IS", "IT", "JE", "JM", "JO", "JP", "KE", "KG", 
                   "KH", "KI", "KM", "KN", "KP", "KR", "KS", "KW", "KY", "KZ", "LA", 
                   "LB", "LC", "LI", "LK", "LR", "LS", "LT", "LU", "LV", "LY", "MA", 
                   "MC", "MD", "ME", "MG", "MH", "MK", "ML", "MM", "MN", "MO", "MR", 
                   "MS", "MT", "MU", "MV", "MW", "MX", "MY", "MZ", "NA", "NC", "NE", 
                   "NG", "NI", "NL", "NO", "NP", "NR", "NZ", "OM", "PA", "PE", "PF", 
                   "PG", "PH", "PK", "PL", "PR", "PS", "PT", "PW", "PY", "QA", "RO",
                   "RS", "RU", "RW", "SA", "SB", "SC", "SD", "SE", "SG", "SI", "SK",
                   "SL", "SM", "SN", "SO", "SR", "SS", "ST", "SV", "SX", "SY", "SZ",
                   "TC", "TD", "TG", "TH", "TJ", "TL", "TM", "TN", "TO", "TR", "TT",
                   "TV", "TW", "TZ", "UA", "UG", "US", "UY", "UZ", "VC", "VE", "VG", 
                   "VN", "VU", "WS", "YE", "ZA", "ZM", "ZW")

not_corecountries <- c("AS", "CK", "CS", "DD", "FO", "GU", "MF", "MP", "SL", "SU", "VI", "XI", "YU", "ZZ")

coreterritories <- c("RU", "OA", "CN", "JP", "OB", "DE", "ES", "FR", "GB", "IT", "SE", "OC", 
                     "QM", "AR", "BR", "CL", "CO", "MX", "OD", "DZ", "EG", "TR", "OE", "CA", 
                     "US", "AU", "NZ", "OH", "IN", "ID", "OI", "ZA", "OJ")


coreterritoriesmer <- c("RU", "OA-MER", "CN", "JP", "OB-MER", "DE", "ES", "FR", "GB", "IT", "SE", 
                        "OC-MER", "QM-MER", "AR", "BR", "CL", "CO", "MX", "OD-MER", "DZ", "EG", 
                        "TR", "OE-MER", "CA", "US", "AU", "NZ", "OH-MER", "IN", "ID", "OI-MER", 
                        "ZA", "OJ-MER")

EURO <- c("AD", "AL", "AT", "BA", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", 
          "FR", "GB", "GG", "GI", "GR", "HR", "HU", "IE", "IM", "IS", "IT", "JE", "KS", "LI", 
          "LT", "LU", "LV", "MC", "MD", "ME", "MK", "MT", "NL", "NO", "PL", "PT", "RO", "RS", 
          "SE", "SI", "SK", "SM")

NAOC <- c("AU", "BM", "CA", "FJ", "FM", "GL", "KI", "MH", "NC", "NR", "NZ", "PF", "PG", "PW", 
          "SB", "TO", "TV", "US", "VU", "WS")

LATA <- c("AG", "AI", "AR", "AW", "BB", "BO", "BQ", "BR", "BS", "BZ", "CL", "CO", "CR", "CU", 
          "CW", "DM", "DO", "EC", "GD", "GT", "GY", "HN", "HT", "JM", "KN", "KY", "LC", "MS", 
          "MX", "NI", "PA", "PE", "PR", "PY", "SR", "SV", "SX", "TC", "TT", "UY", "VC", "VE", 
          "VG")

MENA <- c("AE", "BH", "DZ", "EG", "IL", "IQ", "IR", "JO", "KW", "LB", "LY", "MA", "OM", "PS", 
          "QA", "SA", "SY", "TN", "TR", "YE")

SSAF <- c("AO", "BF", "BI", "BJ", "BW", "CD", "CF", "CG", "CI", "CM", "CV", "DJ", "ER", "ET", 
          "GA", "GH", "GM", "GN", "GQ", "GW", "KE", "KM", "LR", "LS", "MG", "ML", "MR", "MU", 
          "MW", "MZ", "NA", "NE", "NG", "RW", "SC", "SD", "SL", "SN", "SO", "SS", "ST", "SZ", 
          "TD", "TG", "TZ", "UG", "ZA", "ZM", "ZW")

RUCA <- c("AM", "AZ", "BY", "GE", "KG", "KZ", "RU", "TJ", "TM", "UA", "UZ")

EASA <- c("CN", "HK", "JP", "KP", "KR", "MN", "MO", "TW")

SSEA <- c("AF", "BD", "BN", "BT", "ID", "IN", "KH", "LA", "LK", "MM", "MV", "MY", "NP", "PH", 
          "PK", "SG", "TH", "TL", "VN")

oil <- c("AE", "BH", "IQ", "IR", "KW", "OM", "QA", "SA", "YE")

regions  <- c("XX", "QB", "QC", "QD", "QE", "QF", "QG", "QH", "QI", "QJ", "QK", "QL", "QM", "QN",
              "QO", "QP", "QQ", "QR", "QS", "QT", "QU", "QV", "QW", "QX", "QY", "WO", "XA", "XB",
              "XF", "XL", "XM", "XN", "XR", "XS", "OA", "OB", "OC", "OD", "OE", "OH", "OI", "OJ") 

regionsMER <- c("XX-MER", "QB-MER", "QC-MER", "QD-MER", "QE-MER", "QF-MER", "QG-MER", "QH-MER", 
                "QI-MER", "QJ-MER", "QK-MER", "QL-MER", "QM-MER", "QN-MER", "QO-MER", "QP-MER", 
                "QQ-MER", "QR-MER", "QS-MER", "QT-MER", "QU-MER", "QV-MER", "QW-MER", "QX-MER", 
                "QY-MER", "WO-MER", "XA-MER", "XB-MER", "XF-MER", "XL-MER", "XM-MER", "XN-MER", 
                "XR-MER", "XS-MER", "OA-MER", "OB-MER", "OC-MER", "OD-MER", "OE-MER", "OH-MER", 
                "OI-MER", "OJ-MER") 

subcountries <- c("CN-RU", "CN-UR", "DE-BD", "DE-BY", "DE-HB", "DE-HE", "DE-HH", "DE-PR", "DE-SN",
                   "DE-WU", "US-AK", "US-AL", "US-AR", "US-AZ", "US-CA", "US-CO", "US-CT", "US-DC",
                   "US-DE", "US-FL", "US-GA", "US-HI", "US-IA", "US-ID", "US-IL", "US-IN", "US-KS",
                   "US-KY", "US-LA", "US-MA", "US-MD", "US-ME", "US-MI", "US-MN", "US-MO", "US-MS",
                   "US-MT", "US-NC", "US-ND", "US-NE", "US-NH", "US-NJ", "US-NM", "US-NV", "US-NY",
                   "US-OH", "US-OK", "US-OR", "US-PA", "US-RI", "US-SC", "US-SD", "US-TN", "US-TX",
                   "US-UT", "US-VA", "US-VT", "US-WA", "US-WI", "US-WV", "US-WY")



# ------------------------------------------------------------------------------
# ---------------------- Data Preparation --------------------------------------
# ------------------------------------------------------------------------------
# Create a grid of all possible combinations of isos and years
grid_1 <- expand.grid(iso = coreterritories, widcode = widcodes_hm, 
                      year=c(1820, 1850, 1880, 1900:last_year) ,  p=p0p100)
grid_2 <- expand.grid(iso = corecountries,   widcode = widcodes_cc,  
                      year=c(1970:last_year),  p=p0p100)
grid_3 <- expand.grid(iso = coreterritories, widcode = widcodes_cds, 
                      year=c(1820, 1850, 1880, 1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980:last_year),
                      p=any_p)
grid_4 <- expand.grid(iso = corecountries,   widcode = widcodes_ds, 
                      year=c(1980:last_year),  p=any_p)
grid <- rbind(grid_1, grid_2, grid_3, grid_4)
rm(grid_1,grid_2,grid_3,grid_4) 

# grid Agg_Inc, Agg wealth, Agg-carbon
list_of_vectors <- list(corecountries,regions,regionsMER)
main_countries <- Reduce(union, list_of_vectors)
list_of_vectors <- list(widcodes_Agg_Income_Macro, widcodes_Agg_Income_H_NPISH, widcodes_Agg_Income_Corp, 
                        widcodes_Agg_Income_Govr, widcodes_Agg_Income_CA, widcodes_Agg_Wealth, widcodes_Agg_Carbon)
widcodes_agg_income <- Reduce(union, list_of_vectors)

grid_5<- expand.grid(iso=main_countries, 
                     widcode = widcodes_agg_income, year=c(1970:last_year),  p=p0p100)
grid_51<- expand.grid(iso=not_corecountries, 
                      widcode = widcode_macro_base, year=c(1950:last_year),  p=p0p100)
grid <- rbind(grid, grid_5, grid_51) 
rm(grid_5,grid_51, list_of_vectors,main_countries, widcodes_agg_income) 


#Grid Distibutions
list_of_vectors <- list(corecountries,regions,regionsMER,subcountries)
main_countries <- Reduce(union, list_of_vectors)
list_of_vectors <- list(widcodes_Distr_Income, widcodes_Distr_wealth, widcodes_Distr_Carbon)
widcodes_distr <- Reduce(union, list_of_vectors)
#list_of_vectors <- list(p0p100,percentiles)
#p_distr <- Reduce(union, list_of_vectors)

grid_6<- expand.grid(iso=main_countries, 
                     widcode = widcodes_distr, year=c(1995:last_year),  p=p0p100)
grid <- rbind(grid, grid_6) 
rm(grid_6, list_of_vectors,main_countries, widcodes_distr, p_dist) 

#Indexes, trasnparency
list_of_vectors <- list(corecountries,not_corecountries)
main_countries <- Reduce(union, list_of_vectors)
list_of_vectors <- list(widcodes_Indexes,widcodes_Inequality_Idx)
widcodes_indx <- Reduce(union, list_of_vectors)

grid_7<- expand.grid(iso=main_countries, 
                     widcode = widcodes_indx, year=c(1960:last_year),  p=p0p100)
grid <- rbind(grid, grid_7) 
rm(grid_7, list_of_vectors,main_countries, widcodes_indx) 

#Populations
list_of_vectors <- list(corecountries,not_corecountries, subcountries)
main_countries <- Reduce(union, list_of_vectors)
list_of_vectors <- list(widcodes_Populations)
widcodes_pop<- Reduce(union, list_of_vectors)

grid_8<- expand.grid(iso=main_countries, 
                     widcode = widcodes_pop, year=c(1950:last_year),  p=p0p100)
grid <- rbind(grid, grid_8) 
rm(grid_8, list_of_vectors,main_countries, widcodes_pop) 


## Ratios
list_of_vectors <- list(corecountries,regions,regionsMER)
main_countries <- Reduce(union, list_of_vectors)
list_of_vectors <- list(widcodes_Ratios)
widcodes_rqt<- Reduce(union, list_of_vectors)

grid_9<- expand.grid(iso=main_countries, 
                     widcode = widcodes_rqt, year=c(1995:last_year),  p=p0p100)
grid <- rbind(grid, grid_9) 
rm(grid_9, list_of_vectors,main_countries, widcodes_rat) 

rm(widcodes_hm ,widcodes_cc, widcodes_cds, widcodes_ds, widcode_macro_base, widcodes_Agg_Income_Macro,
   widcodes_Agg_Income_H_NPISH,widcodes_Agg_Income_Corp,widcodes_Agg_Income_Govr,widcodes_Agg_Income_CA,
   widcodes_Distr_Income,widcodes_Agg_Wealth,widcodes_Distr_wealth,widcodes_Indexes,
   widcodes_Populations,widcodes_Ratios,widcodes_Inequality_Idx,widcodes_Agg_Carbon,widcodes_Distr_Carbon)

gridy <- grid %>%
  distinct(iso, year) 
gridp <-grid %>%
  distinct(iso, p )
gridw <-grid %>%
  distinct(iso, widcode)
#-------------------------------------------------------------------------------------------------------------------
# Add Country groups
data <- add_country_groups(data)
gridy <- add_country_groups(gridy)
gridp <- add_country_groups(gridp)
gridw <- add_country_groups(gridw)

# Add P groups
data <- add_p_filters(data)
gridp <- add_p_filters(gridp)

# Add a column for each vector
data <- add_widcodes_filters(data)
gridw <- add_widcodes_filters(gridw)

# Generate a result table
data <- data %>%arrange(iso, widcode, year,  p)


# Run the app
#runApp(app_path)
