$Title StarGen Lite Stochastic Daily Unit Commitment of Thermal and Hydro Units (SDUC)

$OnText
                    GNU GENERAL PUBLIC LICENSE
                       Version 3, 29 June 2007

 Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>
 Everyone is permitted to copy and distribute verbatim copies
 of this license document, but changing it is not allowed.

                            Preamble

  The GNU General Public License is a free, copyleft license for
software and other kinds of works.

  The licenses for most software and other practical works are designed
to take away your freedom to share and change the works.  By contrast,
the GNU General Public License is intended to guarantee your freedom to
share and change all versions of a program--to make sure it remains free
software for all its users.  We, the Free Software Foundation, use the
GNU General Public License for most of our software; it applies also to
any other work released this way by its authors.  You can apply it to
your programs, too.

  When we speak of free software, we are referring to freedom, not
price.  Our General Public Licenses are designed to make sure that you
have the freedom to distribute copies of free software (and charge for
them if you wish), that you receive source code or can get it if you
want it, that you can change the software or use pieces of it in new
free programs, and that you know you can do these things.

  To protect your rights, we need to prevent others from denying you
these rights or asking you to surrender the rights.  Therefore, you have
certain responsibilities if you distribute copies of the software, or if
you modify it: responsibilities to respect the freedom of others.

  For example, if you distribute copies of such a program, whether
gratis or for a fee, you must pass on to the recipients the same
freedoms that you received.  You must make sure that they, too, receive
or can get the source code.  And you must show them these terms so they
know their rights.

  Developers that use the GNU GPL protect your rights with two steps:
(1) assert copyright on the software, and (2) offer you this License
giving you legal permission to copy, distribute and/or modify it.

  For the developers' and authors' protection, the GPL clearly explains
that there is no warranty for this free software.  For both users' and
authors' sake, the GPL requires that modified versions be marked as
changed, so that their problems will not be attributed erroneously to
authors of previous versions.

  Some devices are designed to deny users access to install or run
modified versions of the software inside them, although the manufacturer
can do so.  This is fundamentally incompatible with the aim of
protecting users' freedom to change the software.  The systematic
pattern of such abuse occurs in the area of products for individuals to
use, which is precisely where it is most unacceptable.  Therefore, we
have designed this version of the GPL to prohibit the practice for those
products.  If such problems arise substantially in other domains, we
stand ready to extend this provision to those domains in future versions
of the GPL, as needed to protect the freedom of users.

  Finally, every program is threatened constantly by software patents.
States should not allow patents to restrict development and use of
software on general-purpose computers, but in those that do, we wish to
avoid the special danger that patents applied to a free program could
make it effectively proprietary.  To prevent this, the GPL assures that
patents cannot be used to render the program non-free.

Developed by

   Andrés Ramos
   Instituto de Investigacion Tecnologica
   Escuela Tecnica Superior de Ingenieria - ICAI
   UNIVERSIDAD PONTIFICIA COMILLAS
   Alberto Aguilera 23
   28015 Madrid, Spain
   Andres.Ramos@comillas.edu
   https://pascua.iit.comillas.edu/aramos/Ramos_CV.htm

   January 28, 2023

$OffText

$OnEmpty OnMulti OffListing

* options to skip or not the Excel input/output
* if you want to skip it put these values to 1
* in such a case input files must be already in the directory created by any other means
* output file will be the tmp.gdx that can be exported to Excel manually
$ifthen.OptSkipExcelInput       %gams.user2% == ""
$  setglobal OptSkipExcelInput  0
$else.OptSkipExcelInput
$  setglobal OptSkipExcelInput  %gams.user2%
$endif.OptSkipExcelInput

$ifthen.OptSkipExcelOutput      %gams.user3% == ""
$  setglobal OptSkipExcelOutput 0
$else.OptSkipExcelOutput
$  setglobal OptSkipExcelOutput %gams.user3%
$endif.OptSkipExcelOutput

* solve the optimization problems until optimality
option OptcR = 0

* definitions

sets
   n      hour
   n1(n)  first hour of the day
   sc     scenario

   g      generating unit
   t (g)  thermal    unit
   h (g)  hydro      plant ;

alias (n,nn)

parameters
   pDemand        (n) hourly load                    [GW]
   pOperReserve   (n) hourly operating reserve       [GW]
   pOperReserveUp (n) hourly operating reserve up    [GW]
   pOperReserveDw (n) hourly operating reserve down  [GW]
   pIntermGen  (n,sc) stochastic IG generation       [GW]
   pScenProb     (sc) probability of scenarios       [p.u.]
   pCommitt  (   g,n) commitment of the unit         [0-1]
   pProduct  (sc,g,n) output     of the unit         [GW]
   pIG       (sc,  n) output     of IG generation    [GW]
   pSRMC     (sc,  n) short run marginal cost        [ EUR per MWh]

   pMaxProd       (g) maximum output                 [GW]
   pMinProd       (g) minimum output                 [GW]
   pMaxCons       (g) maximum consumption            [GW]
   pIniOut        (g) initial output > min load      [GW]
   pIniUC         (g) initial commitment             [0-1]
   pRampUp        (g) ramp up                        [GW per   h]
   pRampDw        (g) ramp down                      [GW per   h]
   pMinTU         (g) minimum up   time              [h]
   pMinTD         (g) minimum down time              [h]
   pSlopeVarCost  (g) slope     variable cost        [MEUR per GWh]
   pInterVarCost  (g) intercept variable cost        [MEUR per   h]
   pEmissionCost  (g) emission           cost        [MEUR per GWh]
   pStartUpCost   (g) startup            cost        [MEUR]
   pShutDownCost  (g) shutdown           cost        [MEUR]
   pMaxReserve    (g) maximum reserve                [GWh]
   pMinReserve    (g) minimum reserve                [GWh]
   pIniReserve    (g) initial reserve                [GWh]
   pEffic         (g) pumping efficiency             [p.u.]
   pInflows     (g,n) inflows                        [GWh]
   pENSCost           energy not served  cost        [MEUR per GWh]
   pCO2Cost           CO2 emission       cost        [ EUR per tCO2]

variables
   vTotalVCost        total system variable cost     [MEUR]

binary   variables
   vCommitment(  n,g) commitment of the unit         [0-1]
   vStartup   (  n,g) startup    of the unit         [0-1]
   vShutdown  (  n,g) shutdown   of the unit         [0-1]

positive variables
   vOutput   (sc,n,g) output of the unit             [GW]
   vOutput2nd(sc,n,g) output of the unit > min load  [GW]
   vConsump  (sc,n,g) consumption of the unit        [GW]
   vENS      (sc,n  ) energy not served              [GW]
   vIG       (sc,n  ) intermittent generation        [GW]
   vWtReserve(sc,n,g) water reserve at end of period [GWh]
   vSpillage (sc,n,g) spillage                       [GWh]

equations
   eTotalVCost        total system variable cost     [MEUR]
   eBalance  (sc,n  ) load generation balance        [GW]
   eOpReserve(   n  ) operating reserve              [GW]
   eReserveUp(sc,n  ) operating reserve upwards      [GW]
   eReserveDw(sc,n  ) operating reserve downwards    [GW]
   eMaxOutput(sc,n,g) max output of a committed unit [GW]
   eMinOutput(sc,n,g) min output of a committed unit [GW]
   eTotOutput(sc,n,g) tot output of a committed unit [GW]
   eRampUp   (sc,n,g) bound on ramp up               [GW]
   eRampDw   (sc,n,g) bound on ramp down             [GW]
   eUCStrShut(   n,g) relation among commitment startup and shutdown
   eMinTUp   (   n,g) minimum up   time (    committed)
   eMinTDw   (   n,g) minimum down time (not committed)
   eWtReserve(sc,n,g) water reserve                  [GWh] ;

* mathematical formulation

eTotalVCost        .. vTotalVCost =e= sum[(sc,n  ), pENSCost        *vENS       (sc,n  )*pScenProb(sc)] +
                                      sum[(sc,n,t), pSlopeVarCost(t)*vOutput    (sc,n,t)*pScenProb(sc)] +
                                      sum[(sc,n,t), pEmissionCost(t)*vOutput    (sc,n,t)*pScenProb(sc)] +
                                      sum[(   n,t), pInterVarCost(t)*vCommitment(   n,t)] +
                                      sum[(   n,t), pStartUpCost (t)*vStartup   (   n,t)] +
                                      sum[(   n,t), pShutDownCost(t)*vShutdown  (   n,t)] ;

eBalance  (sc,n  ) $ pScenProb(sc) .. sum[t, vOutput(sc,n,t)] + sum[h, vOutput(sc,n,h)] - sum[h, vConsump(sc,n,h)] + vIG(sc,n) + vENS(sc,n) =e= pDemand(n) ;

eOpReserve(   n  )                 .. sum[t, pMaxProd(t) * vCommitment(n,t)] + sum[h, pMaxProd(h)] =g=   pOperReserve  (n) + pDemand(n) ;
eReserveUp(sc,n  ) $ pScenProb(sc) .. sum[t, pMaxProd(t) * vCommitment(n,t)  - vOutput(sc,n,t)]    =g=   pOperReserveUp(n) ;
eReserveDw(sc,n  ) $ pScenProb(sc) .. sum[t, pMinProd(t) * vCommitment(n,t)  - vOutput(sc,n,t)]    =l= - pOperReserveDw(n) ;

eMaxOutput(sc,n,t) $[pScenProb(sc) and pMaxProd(t)] .. vOutput(sc,n,t) / pMaxProd(t) =l= vCommitment(n,t) ;
eMinOutput(sc,n,t) $[pScenProb(sc) and pMinProd(t)] .. vOutput(sc,n,t) / pMinProd(t) =g= vCommitment(n,t) ;

eTotOutput(sc,n,t) $ pScenProb(sc)                  .. vOutput(sc,n,t) =e= pMinProd(t)*vCommitment(n,t) + vOutput2nd(sc,n,t) ;

eRampUp   (sc,n,t) $ pScenProb(sc) .. vOutput2nd(sc,n,t) - vOutput2nd(sc,n-1,t) - max[pIniOut(t)-pMinProd(t),0] $n1(n) =l=   pRampUp(t) ;
eRampDw   (sc,n,t) $ pScenProb(sc) .. vOutput2nd(sc,n,t) - vOutput2nd(sc,n-1,t) - max[pIniOut(t)-pMinProd(t),0] $n1(n) =g= - pRampDw(t) ;

eUCStrShut(   n,t)                 .. vCommitment(n,t) - vCommitment(n-1,t) - pIniUC(t) $n1(n) =e= vStartup(n,t) - vShutdown(n,t) ;

eMinTUp   (   n,t) $[pMinTU(t) > 1 and ord(n) >= pMinTU(t)] .. sum[nn $(ord(nn) >= ord(n)+1-pMinTU(t) and ord(nn) <= ord(n)), vStartup (nn,t)] =l=     vCommitment(n,t) ;
eMinTDw   (   n,t) $[pMinTD(t) > 1 and ord(n) >= pMinTD(t)] .. sum[nn $(ord(nn) >= ord(n)+1-pMinTD(t) and ord(nn) <= ord(n)), vShutdown(nn,t)] =l= 1 - vCommitment(n,t) ;

eWtReserve(sc,n,h) $ pScenProb(sc) .. vWtReserve(sc,n-1,h) + pIniReserve(h) $n1(n) + pInflows(h,n) - vSpillage(sc,n,h) - vOutput(sc,n,h) + vConsump(sc,n,h)*pEffic(h) =e= vWtReserve(sc,n,h) ;

model mSDUC / all / ;
mSDUC.SolPrint = 1 ; mSDUC.HoldFixed = 1 ;

* read input data from Excel and include into the model

file TMP / tmp_%gams.user1%.txt /
$OnEcho  > tmp_%gams.user1%.txt
   r1=    indices
   o1=tmp_indices.txt
   r2=    param
   o2=tmp_param.txt
   r3=    demand
   o3=tmp_demand.txt
   r4=    oprres
   o4=tmp_oprres.txt
   r5=    oprresup
   o5=tmp_oprresup.txt
   r6=    oprresdw
   o6=tmp_oprresdw.txt
   r7=    IGgen
   o7=tmp_IGgen.txt
   r8=    thermalgen
   o8=tmp_thermalgen.txt
   r9=    hydrogen
   o9=tmp_hydrogen.txt
   r10=    inflows
   o10=tmp_inflows.txt
$OffEcho
$ifthen.OptSkipExcelInput '%OptSkipExcelInput%' == '0'
* MacOS and Linux users must comment the following call and copy and paste the named ranges of the Excel interface into the txt files
$call =xls2gms m i="%gams.user1%.xlsm" @"tmp_%gams.user1%.txt"
$else.OptSkipExcelInput
$  log Excel input skipped
$endif.OptSkipExcelInput

sets
$include  tmp_indices.txt
;
$include  tmp_param.txt
parameter pDemand(n)        hourly load              [MW] /
$include  tmp_demand.txt
                                                          /
parameter pOperReserve(n)   hourly operating reserve [MW] /
$include  tmp_oprres.txt
                                                          /
parameter pOperReserveUp(n) hourly operating reserve [MW] /
$include  tmp_oprresup.txt
                                                          /
parameter pOperReserveDw(n) hourly operating reserve [MW] /
$include  tmp_oprresdw.txt
                                                          /
table     pIntermGen(n,sc)  stochastic IG generation [MW]
$include  tmp_IGgen.txt
table     pThermalGen(g,*)
$include  tmp_thermalgen.txt
table     pHydroGen  (g,*)
$include  tmp_hydrogen.txt
table     pInflows   (g,n)
$include  tmp_inflows.txt
;

* MacOS and Linux users must comment the following execute
*execute 'del tmp_"%gams.user1%".txt tmp_indices.txt tmp_param.txt tmp_demand.txt tmp_oprres.txt tmp_oprresup.txt tmp_oprresdw.txt tmp_IGgen.txt tmp_thermalgen.txt tmp_hydrogen.txt tmp_inflows.txt' ;

* determine the first hour of the day

n1(n) $[ord(n) = 1] = yes ;

* assignment of thermal units, storage hydro and pumped storage hydro plants

t (g) $[pThermalGen(g,'MaxProd') and pThermalGen(g,'FuelCost')] = yes ;
h (g) $[pHydroGen  (g,'MaxProd')                              ] = yes ;

* scaling of parameters to GW and MEUR

pDemand       (n   )                = pDemand       (n   ) * 1e-3 ;
pOperReserve  (n   )                = pOperReserve  (n   ) * 1e-3 ;
pOperReserveUp(n   )                = pOperReserveUp(n   ) * 1e-3 ;
pOperReserveDw(n   )                = pOperReserveDw(n   ) * 1e-3 ;
pIntermGen    (n,sc) $pScenProb(sc) = pIntermGen    (n,sc) * 1e-3 ;

pENSCost         = pENSCost                          * 1e-3 ;
pMaxProd     (t) = pThermalGen(t,'MaxProd'         ) * 1e-3 ;
pMinProd     (t) = pThermalGen(t,'MinProd'         ) * 1e-3 ;
pIniOut      (t) = pThermalGen(t,'IniProd'         ) * 1e-3 ;
pRampUp      (t) = pThermalGen(t,'RampUp'          ) * 1e-3 ;
pRampDw      (t) = pThermalGen(t,'RampDown'        ) * 1e-3 ;
pMinTU       (t) = pThermalGen(t,'MinUptime'       )        ;
pMinTD       (t) = pThermalGen(t,'MinDowntime'     )        ;
pSlopeVarCost(t) = pThermalGen(t,'OMVarCost'       ) * 1e-3 +
                   pThermalGen(t,'SlopeVarCost'    ) * 1e-3 * pThermalGen(t,'FuelCost') ;
pEmissionCost(t) = pThermalGen(t,'EmissionRate'    ) * 1e-3 * pCO2Cost ;
pInterVarCost(t) = pThermalGen(t,'InterceptVarCost') * 1e-6 * pThermalGen(t,'FuelCost') ;
pStartUpCost (t) = pThermalGen(t,'StartUpCost'     ) * 1e-6 * pThermalGen(t,'FuelCost') ;
pShutDownCost(t) = pThermalGen(t,'ShutDownCost'    ) * 1e-6 * pThermalGen(t,'FuelCost') ;

pMaxProd     (h) = pHydroGen  (h,'MaxProd'         ) * 1e-3 ;
pMinProd     (h) = pHydroGen  (h,'MinProd'         ) * 1e-3 ;
pMaxCons     (h) = pHydroGen  (h,'MaxCons'         ) * 1e-3 ;
pEffic       (h) = pHydroGen  (h,'Efficiency'      )        ;
pMaxReserve  (h) = pHydroGen  (h,'MaxReserve'      ) * 1e-3 ;
pMinReserve  (h) = pHydroGen  (h,'MinReserve'      ) * 1e-3 ;
pIniReserve  (h) = pHydroGen  (h,'IniReserve'      ) * 1e-3 ;

* if the initial output of the unit is above its minimum load then the unit is committed, otherwise it is not committed
pIniUC       (g) = 1 $[pIniOut(g) >= pMinProd(g)] ;

* if the efficiency of a hydro plant is 0, it is changed to 1
pEffic    (h) $[pEffic    (h) = 0] =   1 ;

* if the minimum up or down times are 0, they are changed to 1
pMinTU    (t) $[pMinTU    (t) = 0] =   1 ;
pMinTD    (t) $[pMinTD    (t) = 0] =   1 ;

* bounds on variables

vOutput.up   (sc,n,g) $pScenProb(sc) = pMaxProd   (g   ) ;
vConsump.up  (sc,n,g) $pScenProb(sc) = pMaxCons   (g   ) ;
vOutput2nd.up(sc,n,t) $pScenProb(sc) = pMaxProd   (t   ) - pMinProd(t) ;
vIG.up       (sc,n  ) $pScenProb(sc) = pIntermGen (n,sc) ;
vENS.up      (sc,n  ) $pScenProb(sc) = pDemand    (n   ) ;
vWtReserve.up(sc,n,g) $pScenProb(sc) = pMaxReserve(g   ) ;
vWtReserve.lo(sc,n,g) $pScenProb(sc) = pMinReserve(g   ) ;

vCommitment.up(n,g) = 1 ;
vStartup.up   (n,g) = 1 ;
vShutdown.up  (n,g) = 1 ;

* solve stochastic daily unit commitment model

solve mSDUC using MIP minimizing vTotalVCost ;

* scaling of the results

pCommitt(   t,n)                = vCommitment.l(   n,t)                   + eps ;
pProduct(sc,g,n) $pScenProb(sc) = vOutput.l    (sc,n,g)*1e3               + eps ;
pIG     (sc,  n) $pScenProb(sc) = vIG.l        (sc,n  )*1e3               + eps ;
pSRMC   (sc,  n) $pScenProb(sc) = eBalance.m   (sc,n  )*1e3/pScenProb(sc) + eps ;

* data output to xls file

put TMP putclose 'par=pCommitt rdim=1 rng=UC!a1' / 'par=pProduct rdim=2 rng=Output!a1' / 'par=pIG    rdim=1 rng=IG!a1' / 'par=pSRMC rdim=1 rng=SRMC!a1' /
                 'text="Unit"         rng=UC!a1' / 'text="Scen"         rng=Output!a1' / 'text="Scen"       rng=IG!a1' / 'text="Scen"      rng=SRMC!a1' /
                                                   'text="Unit"         rng=Output!b1'
execute_unload   'tmp_%gams.user1%.gdx' pProduct pCommitt pIG pSRMC
$ifthen.OptSkipExcelOutput '%OptSkipExcelOutput%' == '0'
* MacOS and Linux users must comment the following execute
execute          'gdxxrw tmp_"%gams.user1%".gdx SQ=n EpsOut=0 O=tmp_"%gams.user1%".xlsx @tmp_"%gams.user1%".txt'
$else.OptSkipExcelOutput
$  log Excel output skipped
$endif.OptSkipExcelOutput
* execute          'del                                                                    tmp_"%gams.user1%".txt'

$OnListing
