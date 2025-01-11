################################################################################
################################################################################
##################  Compleate Heatmaps  ########################################
################################################################################
################################################################################

# ------------------------------------------------------------------------------
# -------------------- Libraries -----------------------------------------------
# ------------------------------------------------------------------------------
#library(readxl)
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shiny)
library(openxlsx)
library(stringr) 

# ------------------------------------------------------------------------------
# -------------------- Path setting --------------------------------------------
# ------------------------------------------------------------------------------
data_path <- "/Users/manuelestebanarias/Documents/GitHub/wid-world/work-data_updated/aggregate-regions-output.dta"
export_path <- "/Users/manuelestebanarias/Downloads/"

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
      Non_corecountries = ifelse(grepl("^[A-NP-Z][A-Z]$", iso) & 
                                   !grepl("^[OQX]", iso) & 
                                   (iso %in% not_corecountries), 1, 0),
      Regions_PPP = ifelse(grepl("^[OQX][A-Z]$", iso), 1, 0),
      Regions_MER = ifelse(grepl("^[A-Z]{2}-MER$", iso), 1, 0),
      Subcountries = ifelse(grepl("^[A-Z]{2}-[A-Z&]+$", iso), 1, 0)
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
# 4. Function to Set the height of the heatmap with variating widcodes or p
get_plot_height <- function(data, base_height = 200, row_height = 10) {
  # Define the possible columns
  possible_columns <- c("widcode", "p", "year")
  # Find which column exists in the data
  available_column <- possible_columns[possible_columns %in% colnames(data)]
  # Ensure exactly one column is available
  if (length(available_column) != 1) {
    stop("Exactly one of 'widcode', 'p', or 'year' must be present in the data.")
  }
  # Extract unique values from the available column
  n <- length(unique(data[[available_column]]))
  # Calculate and return the height
  return(base_height + n * row_height)
}

# 5. Function to Set the height of the heatmap with variating isos
get_plot_height2 <- function(data, base_height = 300, row_height = 10) {
  n <- nrow(unique(data[, "iso", drop = FALSE]))  # Count unique ISO codes
  return(base_height + n *  row_height)
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
# Years historical macro
years_hm  <- c(1820, 1850, 1880, 1900:last_year) 
years_cc  <- 1970:last_year
years_cds <- c(1820, 1850, 1880, 1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 1980:last_year)
years_ds  <- 1980:last_year

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
                          nsrhn|nmxho|ptxgo|labsh|capsh")

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
#widcodes_dis <- unique(data$widcode[grepl( distr, data$widcode)]) #iso all
#widcodes_mac <- unique(data$widcode[grepl( macro, data$widcode)]) #iso all

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


# ------------------------------------------------------------------------------
# ---------------------- Data Preparation --------------------------------------
# ------------------------------------------------------------------------------
# Create a grid of all possible combinations of isos and years
grid_1 <- expand.grid(iso = coreterritories, widcode = widcodes_hm,  year=years_hm,  p=p0p100)
grid_2 <- expand.grid(iso = corecountries,   widcode = widcodes_cc,  year=years_cc,  p=p0p100)
grid_3 <- expand.grid(iso = coreterritories, widcode = widcodes_cds, year=years_cds, p=any_p)
grid_4 <- expand.grid(iso = corecountries,   widcode = widcodes_ds,  year=years_ds,  p=any_p)
#grid_5 <- expand.grid(iso = corecountries,   widcode = widcodes_dis,  year=years_cc,  p=any_p)
#grid_6 <- expand.grid(iso = corecountries,   widcode = widcodes_mac,  year=years_ds,  p=p0p100)
# Combine all grids into a single dataset
grid <- rbind(grid_1, grid_2, grid_3, grid_4) #, grid_5, grid_6)
rm(grid_1,grid_2,grid_3,grid_4) #, grid_5, grid_6)

gridy <- grid %>%
  distinct(iso, year) 
gridp <-grid %>%
  distinct(iso, p, )
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
data <- data %>%arrange(iso, year, widcode, p)

# Generate options for the menus of the APP
region_columns <- c("Corecountries",  "Not_corecountries", "Coreterritories", 
                    "Subcountries", "Europe", "NorthAmerica_Oceania", "LatinAmerica", "MiddleEast_NorthAfrica", "SubSaharanAfrica", 
                    "Russia_CentralAsia", "EastAsia", "South_EastAsia", "Oil","Coreterritoriesmer",
                    "Regions_PPP", "Regions_MER")

p_columns <- c("p0p100","Percentiles","Groupped","Deciles","Top","Bottom")

w_columns <- c("Average", "Pareto_Lrnz", "Fem_pop", "Gini","Idx_Xrate", "Pop","Share", 
               "Threshold", "Total", "Fem_por", "Weal_Inc", "Top_bot", "Ex_rate", 
               "Emissions", "Emssons_pc","Emsns_Avg_g")
f_columns <- c("Agg_Income_Macro","Agg_Income_H_NPISH","Agg_Income_Corp",
               "Agg_Income_Govr","Agg_Income_CA","Distr_Income","Agg_Wealth",
               "Distr_wealth","Indexes","Populations","Ratios","Inequality_Idx",
               "Agg_Carbon","Distr_Carbon")




# ------------------------------------------------------------------------------
# --------------------- Graph --------------------------------------------------
# ------------------------------------------------------------------------------

ui <- fluidPage(
  tags$style(HTML("
    .sidebar { 
      background-color: #28a745; /* Green background */
      color: white; /* White text for better contrast */
    }
    .sidebar .form-group, .sidebar label {
      color: white; /* Ensure text within the sidebar is white */
    }
    .top-controls {
      margin-bottom: 20px; /* Add spacing between controls and heatmaps */
    }
  ")),
  
  titlePanel(paste("Observation count in", file_name, ".dta")),
  
  
  # Sidebar layout with input and main panel
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      width = 3,
      
      # Shared input for Country Group
      selectInput("Country_group", "Country Group:", choices = region_columns),
      
      # Shared input for Widcode group
      selectInput("F_group", "Fivelet Group for Widcodes:", choices = f_columns),
      
      # Shared input for P group
      selectInput("P_group", "P Group:", choices = p_columns)
    ),
    
    mainPanel(
      width = 9,
      div(
        class = "top-controls",
        # Top controls outside of sidebar layout
        fluidRow(
          column(
            width = 9, 
            sliderInput("year_range", "Select Year Range:",
                        min = min(data$year), max = max(data$year),
                        value = c(min(data$year), max(data$year)),
                        step = 10)
          ),
          column(
            width = 3, 
            selectInput("W_group", "Index Group for Widcodes:", choices = w_columns)
          )
          
        ),
        fluidRow(
          plotlyOutput("heatmap_year", height = "auto", width = "100%")
        ),
        fluidRow(
          plotlyOutput("heatmap_p", height = "auto", width = "100%")
        ),
        fluidRow(
          plotlyOutput("heatmap_w", height = "auto", width = "100%")
        )
      )
    )
  )
)


# Server function
server <- function(input, output) {
  
  # Data filteres
  filtered_data <- reactive({
    col_name1 <- input$Country_group
    col_name2 <- input$P_group
    col_name3 <- input$W_group
    col_name4 <- input$F_group
    subset(data, year >= input$year_range[1] & year <= input$year_range[2] 
           & get(col_name1) == 1 & get(col_name2) == 1 &
             get(col_name3) == 1 & get(col_name4) == 1)
  })
  
  
  # Reactive function for filtered gridy
  # 1. grid Year
  filtered_gridy <- reactive({
    col_name1 <- input$Country_group
    subset(gridy, year >= input$year_range[1] & year <= input$year_range[2] & 
             get(col_name1) == 1 )
  })
  # 1. grid p
  filtered_gridp <- reactive({
    col_name1 <- input$Country_group
    col_name2 <- input$P_group
    subset(gridp, get(col_name1) == 1  & get(col_name2) == 1)
  })
  # 1. grid widcode
  filtered_gridw <- reactive({
    col_name1 <- input$Country_group
    col_name3 <- input$W_group
    col_name4 <- input$F_group
    subset(gridw, get(col_name1) == 1  & get(col_name3) == 1 & get(col_name4) == 1)
  })
  
  # Count the observations in the dataset for each combination of iso and year
  countsy <- reactive({
    filtered_data() %>%
      group_by(iso, year) %>%
      summarise(count = n(), .groups = 'drop')
  })
  
  countsp <- reactive({
    filtered_data() %>%
      group_by(iso, p) %>%
      summarise(count = n(), .groups = 'drop')
  })
  
  countsw <- reactive({
    filtered_data() %>%
      group_by(iso, widcode) %>%
      summarise(count = n(), .groups = 'drop')
  })
  
  # Merge the grid with the counts to ensure all combinations are included
  resulty <- reactive({
    filtered_gridy() %>%
      left_join(countsy(), by = c("iso", "year")) %>%
      replace_na(list(count = NA))
  })
  
  resultp <- reactive({
    filtered_gridp() %>%
      left_join(countsp(), by = c("iso", "p")) %>%
      replace_na(list(count = NA))
  })
  
  resultw <- reactive({
    filtered_gridw() %>%
      left_join(countsw(), by = c("iso", "widcode")) %>%
      replace_na(list(count = NA))
  })

  # Render the first heatmap
  output$heatmap_year <- renderPlotly({
    datay <- resulty()
    plot_height <- get_plot_height2(datay)
    plot_ly(
      data = datay,
      x = ~year,
      y = ~iso,
      z = ~count,
      type = "heatmap",
      colors = c( "steelblue1", "dodgerblue4"),
      hovertemplate = paste(
        "Year: %{x}<br>",
        "ISO: %{y}<br>",
        "Count: %{z}<br>",
        "<extra></extra>"
      )
    ) %>% 
      layout(
        title = "Count Country-Year",
        yaxis = list(title = "ISO Alpha-2"),
        plot_bgcolor = "crimson",       # Set the plot background color to red
        height = plot_height  # Apply dynamic height
      )
  })
  
  # Render the second heatmap
  output$heatmap_p <- renderPlotly({
    datap <- resultp()
    plot_height <- get_plot_height(datap)
    plot_ly(
      data = datap,
      x = ~iso,
      y = ~p,
      z = ~count,
      type = "heatmap",
      colors = c("steelblue1", "dodgerblue4"),
      hovertemplate = paste(
        "Country: %{x}<br>",
        "P: %{y}<br>",
        "Count: %{z}<br>",
        "<extra></extra>"
      )
    ) %>% 
      layout(
        title = "Count Country-Fractile_Group",
        xaxis = list(title = "ISO Alpha-2"),
        plot_bgcolor = "crimson", 
        height = plot_height  # Apply dynamic height
      )
  })
  
  # Render the third heatmap
  output$heatmap_w <- renderPlotly({
    dataw <- resultw()
    plot_height <- get_plot_height(dataw)
    plot_ly(
      data = dataw,
      x = ~iso,
      y = ~widcode,
      z = ~count,
      type = "heatmap",
      colors = c("steelblue1", "dodgerblue4"),
      hovertemplate = paste(
        "Country: %{x}<br>",
        "Widcode: %{y}<br>",
        "Count: %{z}<br>",
        "<extra></extra>"
      )
    ) %>% 
      layout(
        title = "Count Country-Widcode_Group",
        yaxis = list(title = NULL),
        plot_bgcolor = "crimson", 
        height = plot_height  # Apply dynamic height
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)