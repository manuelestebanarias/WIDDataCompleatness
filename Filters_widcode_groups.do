use "/Users/manuelestebanarias/Documents/GitHub/wid-world/work-data_updated/wid-long.dta", clear

gen match = 0 // Initialize a flag variable
foreach substr in nninc ndpro gdpro confc nnfin finrx flcir comrx pinrx fdirx ///
                  ptfrx ptdrx pterx ptrrx ptfrr fsubx fpsub fosub finpx flcip ///
                  compx pinpx fdipx ptfpx ptdpx ptepx ptfrp ftaxx fptax fotax ///
                  flcin pinnx fdinx ptfnx ptdnx ptenx ptfrn taxnx prtxn optxn ///
                  nwnxa fdixn fdixa fdixd ptfxn ptfxa ptexa ptdxa ptrxa ptfxd ///
                  ptexd ptdxd prigo prihn priho prinp prico prinf prifc secgo ///
                  sechn secho secnp secco secnf secfc prphn prpco prpgo taxgo ///
                  tiwhn taxco sschn sscgo sscco ssbhn ssbgo ssbco savin savhn ///
                  savho savnp savco savig saghn sagho sagnp sagco segnf segfc ///
                  saggo cfchn cfcho cfcnp cfcco cfcnf cfcfc cfcgo comhn fkpin ///
                  nsrhn nmxho ptxgo labsh capsh {
    replace match = 1 if strpos(widcode, "`substr'") > 0
}
tab year widcode if match & iso=="CS"


gen match = 0 // Initialize a flag variable
foreach substr in prihn comhn prphn nsmhn nsrhn nmxhn prghn gsmhn gsrhn  ///
                          gmxhn sechn taxhn sschn tiwhn ssbhn seghn savhn conhn ///
                          saghn cfchn ccshn ccmhn priho comho prpho nsmho nsrho ///
                          nmxho prgho gsmho gsrho gmxho secho taxho sscho tiwho ///
                          ssbho segho savho conho sagho cfcho ccsho ccmho prinp ///
                          comnp prpnp nsrnp prgnp gsrnp secnp taxnp sscnp tiwnp ///
                          ssbnp segnp {
	replace match = 1 if strpos(widcode, "`substr'") > 0
}

tab iso if match 




gen match = 0 // Initialize a flag variable
foreach substr in prico prpco nsrco prgco prpco gsrco secco prico taxco ///
                         sscco ssbco segco prgco taxco sscco ssbco prgco prico ///
                         cfcco segco secco cfcco prinf prpnf nsrnf prgnf prpnf ///
                         gsrnf secnf prinf taxnf sscnf ssbnf segnf prgnf taxnf ///
                         sscnf ssbnf prgnf prinf cfcnf segnf secnf cfcnf prifc ///
                         prpfc nsrfc prgfc prpfc gsrfc secfc prifc taxfc sscfc ///
                         ssbfc segfc prgfc taxfc sscfc ssbfc prgfc prifc cfcfc ///
                         segfc secfc cfcfc  {
	replace match = 1 if strpos(widcode, "`substr'") > 0
}

tab p if match 






gen match = 0 // Initialize a flag variable
foreach substr in prigo ptxgo tpigo tprgo otpgp spigo sprgo ospgo prpgo ///
                         nsrgo prggo ptxgo tpigo tprgo otpgp spigo sprgo ospgo ///
                         prpgo gsrgo secgo prigo taxgo tiwgo sscgo ssbgo seggo ///
                         prggo taxgo tiwgo sscgo ssbgo savgo secgo congo indgo ///
                         colgo saggo seggo congo indgo colgo congo gpsgo defgo ///
                         polgo ecogo envgo hougo heago recgo edugo sopgo othgo ///
                         expgo gpsge defge polge ecoge envge houge heage recge ///
                         eduge edpge edsge edtge sopge spige sacge sakge revgo ///
                         pitgr citgr scogr pwtgr intgr ottgr ntrgr retgo revgo ///
                         ntrgr psugo revgo expgo ssugo psugo inpgo {
	replace match = 1 if strpos(widcode, "`substr'") > 0
}

tab p if match 



gen match = 0 // Initialize a flag variable
foreach substr in ncanx pinnx pinrx pinpx comnx comrx compx tbnnx tbxrx ///
                         tbmpx taxnx fsubx ftaxx scgnx scgrx scgpx scrnx scrrx ///
                         scrpx sconx scorx scopx tbnnx tgnnx tgxrx tgmpx tsnnx ///
                         tsxrx tsmpx fkanx fkarx fkapx {
	replace match = 1 if strpos(widcode, "`substr'") > 0
}

tab p if match 



gen match = 0 // Initialize a flag variable
foreach substr in fiinc filin fiwag fimil ficap firen fiint fidiv fikgi ///
                         fimik fimix fimil fimik ptinc ptlin ptkin ptinc pllin ///
                         pkkin diinc cainc fainc flinc {
	replace match = 1 if strpos(widcode, "`substr'") > 0
}

tab iso  if match 

gen match = 0 // Initialize a flag variable
foreach substr in hweal hwnfa hwhou hwdwe hwlan hwbus hwagr hwnat hwodk hwfin ///
                      hwfiw hwcud hwbol hwequ hweqi hwoff hwpen hwdeb hwfie hwfin ///
                      hwcud {
	replace match = 1 if strpos(widcode, "`substr'") > 0
}

tab  year if match 




gen match = 0 // Initialize a flag variable
foreach substr in nweal  nwnfa  nwhou  nwdwe  nwlan  nwbus  nwagr  nwnat  nwodk  nwnxa  ///
                      nwgxa  nwgxd  nwboo  nweal  cwres  nwdka  nweal  nwnxa  pweal  pwnfa ///  
                      pwhou  pwdwe  pwlan  pwbus  pwagr  pwnat  pwodk  pwfin  pwfiw  pwcud  ///
                      pwbol  pwequ  pweqi  pwoff  pwpen  pwdeb  pwfie  pwfin  pwcud  hweal  ///
                      hwnfa  hwhou  hwdwe  hwlan  hwbus  hwagr  hwnat  hwodk  hwfin  hwfiw  ///
                      hwcud  hwbol  hwequ  hweqi  hwoff  hwpen  hwdeb  hwfie  hwfin  hwcud  ///
                      iweal  iwnfa  iwhou  iwdwe  iwlan  iwbus  iwagr  iwnat  iwodk  iwfin  ///
                      iwfiw  iwcud  iwbol  iwequ  iweqi  iwoff  iwpen  iwdeb  iwfie  iwfin  ///
                      iwcud  cwboo  cwnfa  cwhou  cwdwe  cwlan  cwbus  cwagr  cwnat  cwodk  ///
                      cwfin  cwfiw  cwcud  cwbol  cwequ  cweqi  cwoff  cwpen  cwdeb  cwdeq  ///
                      cwboo  cwres  cwtoq  cwfie  cwfin  cwcud  gweal  gwnfa  gwhou  gwdwe  ///
                      gwlan  gwbus  gwagr  gwnat  gwodk  gwfin  gwfiw  gwcud  gwbol  gwequ  ///
                      gweqi  gwoff  gwpen  gwdeb  gwdec  gwfie  gwfin  gwcud {
	replace match = 1 if strpos(widcode, "`substr'") > 0
}

tab  p if match 

gen match = 0 // Initialize a flag variable
foreach substr in npopul npopem ntaxto ntaxma ntaxad ntaxre {
	replace match = 1 if strpos(widcode, "`substr'") > 0
}

tab  p if match 



gen match = 0 // Initialize a flag variable
foreach substr in labsh  capsh  wealn  wealp  wealh  weali  wealc  wealg {
	replace match = 1 if strpos(widcode, "`substr'") > 0
}

tab  iso if match 





