$Title StarGen Lite Long Term Generation Expansion Planning Model (GEPM)

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

   Andr�s Ramos
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
option OptcR = 0.01

* definitions

sets
   y              year
   z(y)           year
   p                        period
   p1(p)          first     period
   pn(p)          last      period
   s                     subperiod
   s1(s)          first  subperiod
   n                    load level
   n1(n)          first load level
   sc             scenario
   sca  (sc     ) scenario
   scp  (sc,p   ) tree defined as scenario and period
   scscp(sc,p,sc) ancestor        sc2    of node (sc1 p)
   scsch(sc,sc,p) descendant     (sc2 p) of node  sc1
   scscr(sc,p,sc) representative  sc2    of node (sc1 p)
   spsn(sc,p,s,n) active load levels for each scenario
   psn (   p,s,n) active load levels

   g              generating unit
   t (g)          thermal    unit
   h (g)          hydro      plant
   r                         reservoir
   rs(r)          storage    reservoir
   ruh(r,g)              reservoir   upstream of        hydro plant
   rph(r,g)              reservoir   upstream of pumped hydro plant
   hur(g,r)              hydro plant upstream of reservoir
   hpr(g,r)       pumped hydro plant upstream of reservoir
   rur(r,r)              reservoir 1 upstream of reservoir 2 ;

alias (sc,scc), (r,rr)

parameters
   pDemand     (  sc,  p,s,n) hourly load                    [GW]
   pOperReserve(  sc,  p,s,n) hourly operating reserve       [GW]
   pDuration   (       p,s,n) duration                       [h]
   pCommitt    (y,sc,g,p,s  ) commitment of the unit         [0-1]
   pProduct    (y,sc,g,p,s,n) production of the unit         [MW]
   pEnergy     (y,sc,g,p,s,n) energy     of the unit         [MWh]
   pLRMC       (y,sc,  p,s,n) long run marginal cost         [EUR per MWh]
   pReserve    (y,sc,r,p    ) reservoir level                [hm3]
   pWValue     (y,sc,r,p    ) water value                    [MEUR per hm3]

   pInstalCapG (g,y)          gen installed capacity         [MW]
   pMxExisUnY  (g)            max existing  units            [0-N]
   pMxInstUnY  (g)            max installed units            [0-N]

   pDemIncr     (y)                yearly demand increase    [p.u.]
   pCumDemIncr  (y)           cumm yearly demand increase    [p.u.]
   pOrder       (y)           ordinal of the year

   pEFOR        (g)           EFOR                           [p.u.]
   pMaxProd     (g)           maximum output                 [GW]
   pMinProd     (g)           minimum output                 [GW]
   pMaxCons     (g)           maximum consumption            [GW]
   pSlopeVarCost(g)           slope     variable cost        [MEUR per GWh]
   pInterVarCost(g)           intercept variable cost        [MEUR per   h]
   pStartUpCost (g)           startup            cost        [MEUR]
   pFixedCost   (g)                     fixed    cost        [MEUR per GW ]
   pMaxReserve  (r)           maximum reserve                [km3]
   pMinReserve  (r)           minimum reserve                [km3]
   pIniReserve  (r)           initial reserve                [km3]
   pProdFunct   (g)           production function            [GWh per km3]
   pEffic       (g)           pumping efficiency             [p.u.]
   pInflows     (r,sc,p)      inflows                        [km3]
   pENSCost                   energy non-served cost         [MEUR per GWh]
   pPNSCost                   power  non-served cost         [MEUR per GW ]

   pProbsc        (sc,p)      probability of a given node

   lag(p)                     backward counting of period
   scaux                      scenario number
   pIntSol                    boolean for integer investment and UC

variables
   vTotalTCost                total system          cost     [MEUR]
   vTotalFCost                total system fixed    cost     [MEUR]
   vTotalVCost                total system variable cost     [MEUR]

integer   variables
   vCommitment(y,sc,p,s,  g)   commitment of the units       [0-N]
   vStartup   (y,sc,p,s,  g)   startup    of the units       [0-N]
   vShutdown  (y,sc,p,s,  g)   shutdown   of the units       [0-N]
   vInstUnits (y,         g)   new installed     units       [0-N]

positive variables
   vOutput   (y,sc,p,s,n,g)   production  of the unit        [GW]
   vConsump  (y,sc,p,s,n,g)   consumption of the unit        [GW]
   vENS      (y,sc,p,s,n  )   energy non served              [GW]
   vPNS      (y,sc,p,s    )   power  non served              [GW]
   vWtReserve(y,sc,p,    r)   water reserve at end of period [km3]
   vSpillage (y,sc,p,    r)   spillage                       [km3]

   vMaxProd  (y,g)                installed capacity         [GW]

equations
   eTotalTCost                total system          cost     [MEUR]
   eTotalFCost                total system fixed    cost     [MEUR]
   eTotalVCost                total system variable cost     [MEUR]
   eCommitIns(y,sc,p,s,  g)   relation between commitment and investment decisions [p.u.]
   eInstlCapc(y,         g)   installed capacity             [GW]
   eProdInstl(y,sc,p,s,n,g)   output lower than installed    [GW]
   eOpReserve(y,sc,p,s,n  )   operating reserve              [GW]
   eBalance  (y,sc,p,s,n  )   load generation balance        [GW]
   eMaxOutput(y,sc,p,s,n,g)   max output of a committed unit [GW]
   eMinOutput(y,sc,p,s,n,g)   min output of a committed unit [GW]
   eProdctPer(y,sc,p,s,n,g)   unit production in same period [GW]
*  eStrtUpPer(y,sc,p,s,  g)   unit startup    in same period
*  eStrtUpNxt(y,sc,p,s,  g)   unit startup    in next period
   eWtReserve(y,sc,p,    r)   water reserve                  [km3] ;

* mathematical formulation

eTotalTCost    .. vTotalTCost =e= vTotalFCost + vTotalVCost ;

eTotalFCost    .. vTotalFCost =e= sum[(y,g), (card(y)-ord(y)+1)*pFixedCost(g)*pMaxProd(g)*vInstUnits(y,g) / [1-pEFOR(g)]] ;

eTotalVCost    .. vTotalVCost =e= sum[(y,spsn(sc,p ,s,n)  ), pProbSc(sc,p)*pDuration(p,s,n)*pENSCost        *vENS       (y,sc,p,s,n  )] +
                                  sum[(y,scp (sc,p),s     ), pProbSc(sc,p)                 *pPNSCost        *vPNS       (y,sc,p,s    )] +
*                                 sum[(y,scp (sc,p),s,   t), pProbSc(sc,p)                 *pStartUpCost (t)*vStartup   (y,sc,p,s,  t)] +
*                                 sum[(y,spsn(sc,p ,s,n),t), pProbSc(sc,p)*pDuration(p,s,n)*pInterVarCost(t)*vCommitment(y,sc,p,s,  t)] +
                                  sum[(y,spsn(sc,p ,s,n),t), pProbSc(sc,p)*pDuration(p,s,n)*pSlopeVarCost(t)*vOutput    (y,sc,p,s,n,t)] ;


* relation between commitment of thermal units and cumulative investment
eCommitIns(y,scp(sc,p),s,   t)   .. vCommitment(y,sc,p,s,t) =l=                pMxExisUnY(t) + sum[z $[pOrder(z) <= ord(y)], vInstUnits(z,t)]  ;
eInstlCapc(y,               g)   .. vMaxProd   (y,       g) =e= pMaxProd(g) * [pMxExisUnY(g) + sum[z $[pOrder(z) <= ord(y)], vInstUnits(z,g)]] ;

eProdInstl(y,spsn(sc,p,s,n),g)   .. vOutput(y,sc,p,s,n,g) =l= vMaxProd(y,g) ;

eOpReserve(y,spsn(sc,p,s,n1(n))) .. sum[t,     vMaxProd(y,         t)] + sum[h, vMaxProd(y,         h)] + vPNS(y,sc,p,s  ) =g= [pDemand(sc,p,s,n) + pOperReserve(sc,p,s,n)] * pCumDemIncr(y) ;
eBalance  (y,spsn(sc,p,s,   n )) .. sum[g,     vOutput (y,sc,p,s,n,g)] - sum[h, vConsump(y,sc,p,s,n,h)] + vENS(y,sc,p,s,n) =e=  pDemand(sc,p,s,n)                           * pCumDemIncr(y) ;

eMaxOutput(y,spsn(sc,p,s,n),t) $pMaxProd(t) .. vOutput (y,sc,p,s,n,t)  / pMaxProd(t) =l= vCommitment(y,sc,p,s,t) ;
eMinOutput(y,spsn(sc,p,s,n),t) $pMinProd(t) .. vOutput (y,sc,p,s,n,t)  / pMinProd(t) =g= vCommitment(y,sc,p,s,t) ;

eProdctPer(y,spsn(sc,p,s1(s),n),g) .. vOutput(y,sc,p,s+1,n,g) =l= vOutput(y,sc,p,s,n,g) ;

*eStrtUpPer(y,scp(sc,p),s1(s),t) $[card(s) > 1              ] .. vCommitment(y,sc,p,s+1,t) =g=                      vCommitment(y,sc ,p  ,s  ,t)  + vStartup(y,sc,p,s+1,t) - vShutdown(y,sc,p,s+1,t) ;
*eStrtUpNxt(y,scp(sc,p),s1(s),t) $[card(s) > 1 and not p1(p)] .. vCommitment(y,sc,p,s  ,t) =g= sum[scscp(sc,p,scc), vCommitment(y,scc,p-1,s+1,t)] + vStartup(y,sc,p,s  ,t) - vShutdown(y,sc,p,s  ,t) ;

eWtReserve(y,scp(sc,p),      r) .. sum[scscp(sc,p,scc), vWtReserve(y,scc,p-1,r)] + pIniReserve(r) $p1(p) - vWtReserve(y,sc,p,r) +
                                   pInflows(r,sc,p) - vSpillage(y,sc,p,r) + sum[rur(rr,r), vSpillage(y,sc,p,rr)]  +
                                   sum{(s,n), pDuration(p,s,n)*sum[hur(h,r), vOutput (y,sc,p,s,n,h)/pProdFunct(h)]} -
                                   sum{(s,n), pDuration(p,s,n)*sum[ruh(r,h), vOutput (y,sc,p,s,n,h)/pProdFunct(h)]} +
                                   sum{(s,n), pDuration(p,s,n)*sum[hpr(h,r), vConsump(y,sc,p,s,n,h)/pProdFunct(h)*pEffic(h)]} -
                                   sum{(s,n), pDuration(p,s,n)*sum[rph(r,h), vConsump(y,sc,p,s,n,h)/pProdFunct(h)*pEffic(h)]} =e= 0 ;

model mGEPM / all / ;
mGEPM.SolPrint = 1 ; mGEPM.HoldFixed = 1 ;

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
   r5=    duration
   o5=tmp_duration.txt
   r6=    thermalgen
   o6=tmp_thermalgen.txt
   r7=    hydrogen
   o7=tmp_hydrogen.txt
   r8=    reservoir
   o8=tmp_reservoir.txt
   r9=    inflows
   o9=tmp_inflows.txt
   r10=    tree
   o10=tmp_tree.txt
$OffEcho
* MacOS and Linux users must comment the following call and copy and paste the named ranges of the Excel interface into the txt files
$ifthen.OptSkipExcelInput '%OptSkipExcelInput%' == '0'
$call =xls2gms m i="%gams.user1%.xlsm" @"tmp_%gams.user1%.txt"
$else.OptSkipExcelInput
$  log Excel input skipped
$endif.OptSkipExcelInput

sets
$include tmp_indices.txt
;
$include tmp_param.txt
table    pDemand(sc,p,s,n)
$include tmp_demand.txt
table    pOperReserve(sc,p,s,n)
$include tmp_oprres.txt
table    pDuration(p,s,n)
$include tmp_duration.txt
table    pThermalGen(g,*)
$include tmp_thermalgen.txt
table    pHydroGen(g,*)
$include tmp_hydrogen.txt
table    pReservoir(r,*)
$include tmp_reservoir.txt
table    pInflows(r,sc,p)
$include tmp_inflows.txt
table    pScnTree(sc,*)
$include tmp_tree.txt
;

* MacOS and Linux users must comment the following execute
*execute 'del tmp_"%gams.user1%".txt tmp_indices.txt tmp_param.txt tmp_demand.txt tmp_oprres.txt tmp_duration.txt tmp_thermalgen.txt tmp_hydrogen.txt tmp_reservoir.txt tmp_inflows.txt tmp_tree.txt' ;

* determine the first and last period and the first subperiod

p1(p)      $[ord(p) =       1] = yes ;
s1(s)      $[ord(s) =       1] = yes ;
n1(n)      $[ord(n) =       1] = yes ;
pn(p)      $[ord(p) = card(p)] = yes ;
psn(p,s,n) $pDuration(p,s,n)   = yes ;
lag(p) = card(p) - 2*ord(p) + 1 ;

* assignment of thermal units, storage hydro and pumped storage hydro plants

t (g) $[pThermalGen(g,'MaxProd'   ) and pThermalGen(g,'FuelCost')] = yes ;
h (g) $[pHydroGen  (g,'MaxProd'   )                              ] = yes ;
rs(r) $[pReservoir (r,'MaxReserve') > 0                          ] = yes ;

* compute the cummulative yearly demand growth

z          (y) = yes ;
pOrder     (y) = ord(y) ;
pCumDemIncr(y) = prod[z $[pOrder(z) <= ord(y)], 1+pDemIncr(z)] ;

* scaling of parameters

pDemand     (sc,p,s,n) = pDemand     (sc,p,s,n)      * 1e-3 ;
pOperReserve(sc,p,s,n) = pOperReserve(sc,p,s,n)      * 1e-3 ;
pENSCost               = pENSCost                    * 1e-3 ;
pPNSCost               = pPNSCost                    * 1e-3 ;

pEFOR        (t) = pThermalGen(t,'EFOR'            )        ;
pMaxProd     (t) = pThermalGen(t,'MaxProd'         ) * 1e-3 * [1-pEFOR(t)] ;
pMinProd     (t) = pThermalGen(t,'MinProd'         ) * 1e-3 * [1-pEFOR(t)] ;
pSlopeVarCost(t) = pThermalGen(t,'OMVarCost'       ) * 1e-3 +
                   pThermalGen(t,'SlopeVarCost'    ) * 1e-3 * pThermalGen(t,'FuelCost') ;
pInterVarCost(t) = pThermalGen(t,'InterceptVarCost') * 1e-6 * pThermalGen(t,'FuelCost') ;
pStartUpCost (t) = pThermalGen(t,'StartUpCost'     ) * 1e-6 * pThermalGen(t,'FuelCost') ;

pFixedCost   (t) = pThermalGen(t,'FixedCost'       )        * pThermalGen(t,'FxChargeRate') ;
pMxExisUnY   (t) = pThermalGen(t,'MxExisUnY'       )        ;
pMxInstUnY   (t) = pThermalGen(t,'MxInstUnY'       )        ;

pMaxProd     (h) = pHydroGen  (h,'MaxProd'         ) * 1e-3 ;
pMinProd     (h) = pHydroGen  (h,'MinProd'         ) * 1e-3 ;
pMaxCons     (h) = pHydroGen  (h,'MaxCons'         ) * 1e-3 ;
pProdFunct   (h) = pHydroGen  (h,'ProdFunct'       ) * 1e+3 ;
pEffic       (h) = pHydroGen  (h,'Efficiency'      )        ;
pMxExisUnY   (h) = pHydroGen  (h,'MxExisUnY'       )        ;
pMxInstUnY   (h) = pHydroGen  (h,'MxInstUnY'       )        ;
pMaxReserve  (r) = pReservoir (r,'MaxReserve'      ) * 1e-3 ;
pMinReserve  (r) = pReservoir (r,'MinReserve'      ) * 1e-3 ;
pIniReserve  (r) = pReservoir (r,'IniReserve'      ) * 1e-3 ;

pFixedCost   (h) = pHydroGen  (h,'FixedCost'       )        * pHydroGen  (h,'FxChargeRate') ;

pInflows(r,sc,p) = pInflows   (r,sc,p              ) * 1e-6 * 3.6*sum[(s,n), pDuration(p,s,n)] ;

* if the production function of a hydro plant is 0, it is changed to 1 and scaled to 1000
* if the efficiency          of a hydro plant is 0, it is changed to 1

pProdFunct(h) $[pProdFunct(h) = 0] = 1e3 ;
pEffic    (h) $[pEffic    (h) = 0] =   1 ;

* bounds on variables

vInstUnits.up(y,g)             =                             pMxInstUnY(g)  ;
vOutput.up   (y,sc,p,s,n,g)    = pMaxProd(g)*[pMxExisUnY(g)+ pMxInstUnY(g)] ;
vConsump.up  (y,sc,p,s,n,g)    = pMaxCons(g)                                ;

vENS.up      (y,sc,p,s,n  )    = pDemand(sc,p,s,n) * pCumDemIncr(y) ;

vWtReserve.up(y,sc,p,r)        = pMaxReserve(r) ;
vWtReserve.lo(y,sc,p,r)        = pMinReserve(r) ;
vWtReserve.fx(y,sc,p,r) $pn(p) = pIniReserve(r) ;

* define the nodes of the scenario tree and determine ancestor sc2 of node (sc1 p) and descendant (sc2 p) of node sc1

scp (     sc,p     ) $[ord(p) >= pScnTree(sc,'FirstPeriod')                                       ] = yes ;
scscp(scp(sc,p),scc) $[ord(p) >  pScnTree(sc,'FirstPeriod') and ord(scc) = ord(sc)                ] = yes ;
scscp(scp(sc,p),scc) $[ord(p)  = pScnTree(sc,'FirstPeriod') and ord(scc) = pScnTree(sc,'Ancestor')] = yes ;
scsch(sc,scp(scc,p)) $scscp(scc,p,sc)                                                               = yes ;

pProbSc(sc,pn(p)) = pScnTree(sc,'Prob')/sum[scc, pScnTree(scc,'Prob')] ;
loop (p $[not p1(p)],
   pProbSc(scp(sc,p+lag(p))) = sum[scsch(sc,scc,p+(lag(p)+1)), pProbSc(scc,p+(lag(p)+1))] ;
) ;

* delete branches with probability 0 and define the active load levels

scp  (    sc,p     ) $[pProbSc(sc,p) = 0                        ] =  no ;
scscp(    sc,p ,scc) $[pProbSc(sc,p) = 0 or pProbSc(scc,p-1) = 0] =  no ;
scsch(sc,scc,p     )                                              = yes $scscp(scc,p,sc) ;
spsn (scp(sc,p),s,n) $psn      (p,s,n)                            = yes ;

* determine the representative sc2 of node (sc1 p) for non-existing scenarios in the tree

loop (sc $sum[p, pProbSc(sc,p)],
   scaux = ord(sc) ;
   loop (p,
      scscr(sc,p+lag(p),scc) $[ord(scc) = scaux] = yes ;
      SCA(scc)               $[ord(scc) = scaux] = yes ;
      scaux = sum[scscp(sca,p+lag(p),scc), ord(scc)] ;
      SCA(scc)                                   =  no ;
   ) ;
) ;
SCA(sc) $sum[p, pProbSc(sc,p)] = yes ;

* solve generation expansion planning model
if(pIntSol,
    solve mGEPM using  MIP minimizing vTotalTCost ;
else
    solve mGEPM using RMIP minimizing vTotalTCost ;
);

* scaling of the results

pInstalCapG(g,y)             =                       pMaxProd(g)   * vInstUnits.l (y,          g) / [1-pEFOR(g)]                                   *1e3 + eps ;

pCommitt(y,sca,t,    p,s   ) = sum[scscr(sca,p,scc)                , vCommitment.l(y,scc,p,s,  t)                                                 ]     + eps ;
pProduct(y,sca,g,psn(p,s,n)) = sum[scscr(sca,p,scc)                , vOutput.l    (y,scc,p,s,n,g)                                                 ]*1e3 + eps ;
pEnergy (y,sca,g,psn(p,s,n)) = sum[scscr(sca,p,scc)                , vOutput.l    (y,scc,p,s,n,g)                *pDuration(p,s,n)                ]*1e3 + eps ;
pReserve(y,sca,rs(r),p     ) = sum[scscr(sca,p,scc)                , vWtReserve.l (y,scc,p,    r)                                                 ]*1e3 + eps ;
pWValue (y,sca,rs(r),p     ) = sum[scscr(sca,p,scc) $pProbSc(scc,p), eWtReserve.m (y,scc,p,    r)/sum[psn(p,s,n), pDuration(p,s,n)]/pProbSc(scc,p)]*1e3 + eps ;
pLRMC   (y,sca,  psn(p,s,n)) = sum[scscr(sca,p,scc) $pProbSc(scc,p), eBalance.m   (y,scc,p,s,n  )                /pDuration(p,s,n) /pProbSc(scc,p)]*1e3 + eps ;

* data output to xls file

put TMP putclose 'par=pProduct rdim=3 rng=Output!a1' / 'par=pEnergy rdim=3 rng=Energy!a1' / 'par=pReserve rdim=3 rng=WtrReserve!a1' / 'par=pWValue rdim=3 rng=WtrValue!a1' / 'par=pLRMC rdim=2 rng=LRMC!a1' / 'par=pCommitt rdim=3 rng=UC!a1' / 'par=pInstalCapG rdim=1 rng=InstalCapG!a1' /
                 'text="Year"         rng=Output!a1' / 'text="Year"        rng=Energy!a1' / 'text="Year"         rng=WtrReserve!a1' / 'text="Year"        rng=WtrValue!a1' / 'text="Year"      rng=LRMC!a1' / 'text="Year"         rng=UC!a1' / 'text="Unit"            rng=InstalCapG!a1' /
                 'text="Scen"         rng=Output!b1' / 'text="Scen"        rng=Energy!b1' / 'text="Scen"         rng=WtrReserve!b1' / 'text="Scen"        rng=WtrValue!b1' / 'text="Scen"      rng=LRMC!b1' / 'text="Scen"         rng=UC!b1' /
                 'text="Unit"         rng=Output!c1' / 'text="Unit"        rng=Energy!c1' / 'text="Reservoir"    rng=WtrReserve!c1' / 'text="Reservoir"   rng=WtrValue!c1' /                                  'text="Unit"         rng=UC!c1' /
execute_unload   'tmp_%gams.user1%.gdx' pProduct pEnergy pReserve pWValue pLRMC pCommitt pInstalCapG
$ifthen.OptSkipExcelOutput '%OptSkipExcelOutput%' == '0'
* MacOS and Linux users must comment the following execute
execute          'gdxxrw tmp_"%gams.user1%".gdx SQ=n EpsOut=0 O=tmp_"%gams.user1%".xlsx @tmp_"%gams.user1%".txt'
$else.OptSkipExcelOutput
$  log Excel output skipped
$endif.OptSkipExcelOutput
* execute          'del                                                                    tmp_"%gams.user1%".txt'

$OnListing
