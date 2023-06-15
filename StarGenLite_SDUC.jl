# StarGen Lite Stochastic Daily Unit Commitment of Thermal and Hydro Units (SDUC)

#                     GNU GENERAL PUBLIC LICENSE
#                        Version 3, 29 June 2007
#
#  Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>
#  Everyone is permitted to copy and distribute verbatim copies
#  of this license document, but changing it is not allowed.
#
#                             Preamble
#
#   The GNU General Public License is a free, copyleft license for
# software and other kinds of works.
#
#   The licenses for most software and other practical works are designed
# to take away your freedom to share and change the works.  By contrast,
# the GNU General Public License is intended to guarantee your freedom to
# share and change all versions of a program--to make sure it remains free
# software for all its users.  We, the Free Software Foundation, use the
# GNU General Public License for most of our software; it applies also to
# any other work released this way by its authors.  You can apply it to
# your programs, too.
#
#   When we speak of free software, we are referring to freedom, not
# price.  Our General Public Licenses are designed to make sure that you
# have the freedom to distribute copies of free software (and charge for
# them if you wish), that you receive source code or can get it if you
# want it, that you can change the software or use pieces of it in new
# free programs, and that you know you can do these things.
#
#   To protect your rights, we need to prevent others from denying you
# these rights or asking you to surrender the rights.  Therefore, you have
# certain responsibilities if you distribute copies of the software, or if
# you modify it: responsibilities to respect the freedom of others.
#
#   For example, if you distribute copies of such a program, whether
# gratis or for a fee, you must pass on to the recipients the same
# freedoms that you received.  You must make sure that they, too, receive
# or can get the source code.  And you must show them these terms so they
# know their rights.
#
#   Developers that use the GNU GPL protect your rights with two steps:
# (1) assert copyright on the software, and (2) offer you this License
# giving you legal permission to copy, distribute and/or modify it.
#
#   For the developers' and authors' protection, the GPL clearly explains
# that there is no warranty for this free software.  For both users' and
# authors' sake, the GPL requires that modified versions be marked as
# changed, so that their problems will not be attributed erroneously to
# authors of previous versions.
#
#   Some devices are designed to deny users access to install or run
# modified versions of the software inside them, although the manufacturer
# can do so.  This is fundamentally incompatible with the aim of
# protecting users' freedom to change the software.  The systematic
# pattern of such abuse occurs in the area of products for individuals to
# use, which is precisely where it is most unacceptable.  Therefore, we
# have designed this version of the GPL to prohibit the practice for those
# products.  If such problems arise substantially in other domains, we
# stand ready to extend this provision to those domains in future versions
# of the GPL, as needed to protect the freedom of users.
#
#   Finally, every program is threatened constantly by software patents.
# States should not allow patents to restrict development and use of
# software on general-purpose computers, but in those that do, we wish to
# avoid the special danger that patents applied to a free program could
# make it effectively proprietary.  To prevent this, the GPL assures that
# patents cannot be used to render the program non-free.
#
# Developed by

#    Andres Ramos
#    Instituto de Investigacion Tecnologica
#    Escuela Tecnica Superior de Ingenieria - ICAI
#    UNIVERSIDAD PONTIFICIA COMILLAS
#    Alberto Aguilera 23
#    28015 Madrid, Spain
#    Andres.Ramos@comillas.edu

#    MIT Energy Initiative
#    Massachusetts Institute of Technology
#    arght@mit.edu

#    July 9, 2019

# Define the packages
using JuMP          # used for mathematical programming
using ExcelReaders  # used for data input from Excel
using DataFrames    # used for data frames
using Gurobi        # used as the solver
using CSV           # used for writting csv files

# system dimensions
N  = 24  # hours
SC =  3  # scenarios
G  = 13  # thermal and hydro generating units

# reading data from Excel
InputFile = openxl("StarGenLite_SDUC.xlsm")
dfDemand        = readxl(InputFile, "DemandReserveIG!D6:D29")
dfOperReserve   = readxl(InputFile, "DemandReserveIG!I6:I29")
dfOperReserveUp = readxl(InputFile, "DemandReserveIG!N6:N29")
dfOperReserveDw = readxl(InputFile, "DemandReserveIG!S6:S29")
dfIntermGen     = readxl(InputFile, "DemandReserveIG!X6:Z29")
dfThermalGen    = readxl(InputFile, "Generation!D7:R19"     )

#  parameters
pScenProb      = [0.3, 0.5, 0.2]            # probability of scenarios      [p.u.]
pENSCost       = 10                         # energy not served  cost       [MEUR per GWh]
pCO2Cost       =  5                         # CO2 emission       cost       [ EUR per tCO2]
# scaling of parameters to GW and MEUR
pDemand        =        dfDemand[1:N     ] * 1e-3  # hourly load                   [GW]
pOperReserve   =   dfOperReserve[1:N     ] * 1e-3  # hourly operating reserve      [GW]
pOperReserveUp = dfOperReserveUp[1:N     ] * 1e-3  # hourly operating reserve up   [GW]
pOperReserveDw = dfOperReserveDw[1:N     ] * 1e-3  # hourly operating reserve down [GW]
pIntermGen     =     dfIntermGen[1:N,1:SC] * 1e-3  # stochastic IG generation      [GW]

pMaxProd       = dfThermalGen[1:G, 1] * 1e-3                                                       # maximum output            [GW]
pMinProd       = dfThermalGen[1:G, 2] * 1e-3                                                       # minimum output            [GW]
pIniOut        = dfThermalGen[1:G, 3] * 1e-3                                                       # initial output > min load [GW]
pRampUp        = dfThermalGen[1:G, 4] * 1e-3                                                       # ramp up                   [GW per   h]
pRampDw        = dfThermalGen[1:G, 5] * 1e-3                                                       # ramp down                 [GW per   h]
pSlopeVarCost  = dfThermalGen[1:G, 7] * 1e-3 .* dfThermalGen[1:G,6] .+ dfThermalGen[1:G,9] * 1e-3  # slope     variable cost   [MEUR per GWh]
pInterVarCost  = dfThermalGen[1:G, 8] * 1e-6 .* dfThermalGen[1:G,6]                                # intercept variable cost   [MEUR per   h]
pMinTU0        = dfThermalGen[1:G,10]                                                              # minimum time up           [h]
pMinTD0        = dfThermalGen[1:G,11]                                                              # minimum time down         [h]
pEmissionCost  = dfThermalGen[1:G,12] * 1e-3  * pCO2Cost                                           # emission           cost   [MEUR per GWh]
pStartUpCost   = dfThermalGen[1:G,13] * 1e-6 .* dfThermalGen[1:G,6]                                # startup            cost   [MEUR]
pShutDownCost  = dfThermalGen[1:G,14] * 1e-6 .* dfThermalGen[1:G,6]                                # shutdown           cost   [MEUR]

pIniUC = zeros(Int64,G)
pMinTU = zeros(Int64,G)
pMinTD = zeros(Int64,G)

for g in 1:G
  # if the initial output of the unit is above its minimum load then the unit is committed, otherwise it is not committed
  if   pIniOut[g] >= pMinProd[g]
       pIniUC[g]   = 1
  else pIniOut[g] <  pMinProd[g]
       pIniUC[g]   = 0
  end
  # if the minimum up or down times are 0, they are changed to 1
  if pMinTU0[g] == 0
     pMinTU[g]  =  1
  end
  if pMinTD0[g] == 0
     pMinTD[g]  =  1
  end
  # round these integer parameters to integer numbers
  pMinTU[g] = round(Int,pMinTU0[g])
  pMinTD[g] = round(Int,pMinTD0[g])
end

function solve_mSDUC(UC, N, SC, G, pMaxProd, pMinProd, pIntermGen, pDemand, pOperReserve, pOperReserveUp, pOperReserveDw, pSlopeVarCost, pEmissionCost, pInterVarCost, pStartUpCost, pShutDownCost, pIniUC, pIniOut, pMinTU, pMinTD, pCommitt_Lo, pCommitt_Up, pStartup_Lo, pStartup_Up, pShutdown_Lo, pShutdown_Up)

  # stochastic daily unit commitment (UC) model
  mSDUC = Model(solver=GurobiSolver(MIPGap=0.0))

  # decision variables
  @variable(mSDUC, 0 <=  vOutput[sc=1:SC,n=1:N,g=1:G] <=   pMaxProd[g ]            )  # output of the unit             [GW]
  @variable(mSDUC, 0 <= vOutput2nd[sc=1:SC,n=1:N,g=1:G] <=   pMaxProd[g ]-pMinProd[g])  # output of the unit > min load  [GW]
  @variable(mSDUC, 0 <=       vIG[sc=1:SC,n=1:N      ] <= pIntermGen[n,sc]          )  # intermittent generation        [GW]
  @variable(mSDUC, 0 <=      vENS[sc=1:SC,n=1:N      ] <=    pDemand[n ]            )  # energy not served              [GW]

  @variable(mSDUC,       vCommitment[        n=1:N,g=1:G], Bin                         )  # binary commitment of the unit  {0,1}
  @variable(mSDUC,       vStartup[        n=1:N,g=1:G], Bin                         )  # binary startup    of the unit  {0,1}
  @variable(mSDUC,      vShutdown[        n=1:N,g=1:G], Bin                         )  # binary shutdown   of the unit  {0,1}

  # fix values of binary variables to get the dual variables of the relaxed problem
  setlowerbound.(vCommitment , pCommitt_Lo )
  setupperbound.(vCommitment , pCommitt_Up )
  setlowerbound.(vStartup , pStartup_Lo )
  setupperbound.(vStartup , pStartup_Up )
  setlowerbound.(vShutdown, pShutdown_Lo)
  setupperbound.(vShutdown, pShutdown_Up)

  # objective function total system variable cost     [MEUR]
  @objective(mSDUC, :Min, sum(     pENSCost    *      vENS[sc,n  ] * pScenProb[sc] for sc=1:SC,n=1:N      ) +
                          sum(pSlopeVarCost[g] *  vOutput[sc,n,g] * pScenProb[sc] for sc=1:SC,n=1:N,g=1:G) +
                          sum(pEmissionCost[g] *  vOutput[sc,n,g] * pScenProb[sc] for sc=1:SC,n=1:N,g=1:G) +
                          sum(pInterVarCost[g] *  vCommitment[   n,g]                 for         n=1:N,g=1:G) +
                          sum( pStartUpCost[g] *  vStartup[   n,g]                 for         n=1:N,g=1:G) +
                          sum(pShutDownCost[g] * vShutdown[   n,g]                 for         n=1:N,g=1:G) )

  # load generation balance        [GW]
  @constraint(mSDUC, eBalance[sc=1:SC,n=1:N], sum(vOutput[sc,n,g] for g=1:G) + vIG[sc,n] + vENS[sc,n] == pDemand[n])

  # operating reserve              [GW]
  @constraint(mSDUC, eOpReserve[n=1:N], sum(pMaxProd[g] * vCommitment[n,g] for g=1:G) >= pOperReserve[n] + pDemand[n])

  # operating reserve upwards and downwards    [GW]
  @constraint(mSDUC, eReserveUp[sc=1:SC,n=1:N], sum(pMaxProd[g] * vCommitment[n,g] - vOutput[sc,n,g] for g=1:G) >=   pOperReserveUp[n])
  @constraint(mSDUC, eReserveDw[sc=1:SC,n=1:N], sum(pMinProd[g] * vCommitment[n,g] - vOutput[sc,n,g] for g=1:G) <= - pOperReserveDw[n])

  # maximum and minimun output of a committed unit [GW]
  @constraint(mSDUC, eMaxOutput[sc=1:SC,n=1:N,g=1:G], vOutput[sc,n,g] / pMaxProd[g] <= vCommitment[n,g])
  @constraint(mSDUC, eMinOutput[sc=1:SC,n=1:N,g=1:G], vOutput[sc,n,g] / pMinProd[g] >= vCommitment[n,g])

  # total output of a committed unit [GW]
  @constraint(mSDUC, eTotOutput[sc=1:SC,n=1:N,g=1:G], vOutput[sc,n,g] == pMinProd[g]*vCommitment[n,g] + vOutput2nd[sc,n,g])

  # bounds on up and down ramps      [GW]
  @constraint(mSDUC, eRampUp[sc=1:SC,n=  1,g=1:G], vOutput2nd[sc,n,g] -      max(pIniOut[g] - pMinProd[g],0) <=   pRampUp[g])
  @constraint(mSDUC, eRampDw[sc=1:SC,n=  1,g=1:G], vOutput2nd[sc,n,g] -      max(pIniOut[g] - pMinProd[g],0) >= - pRampDw[g])
  @constraint(mSDUC, eRampUp[sc=1:SC,n=2:N,g=1:G], vOutput2nd[sc,n,g] - vOutput2nd[sc,n-1,g]                  <=   pRampUp[g])
  @constraint(mSDUC, eRampDw[sc=1:SC,n=2:N,g=1:G], vOutput2nd[sc,n,g] - vOutput2nd[sc,n-1,g]                  >= - pRampDw[g])

  # relation among commitment startup and shutdown
  @constraint(mSDUC, eUCStrShut[n=1  ,g=1:G], vCommitment[n,g] - pIniUC[      g] == vStartup[n,g] - vShutdown[n,g])
  @constraint(mSDUC, eUCStrShut[n=2:N,g=1:G], vCommitment[n,g] - vCommitment[n-1,g] == vStartup[n,g] - vShutdown[n,g])

  # minimum up (committed) and down (not committed) time       [GW]
  @constraint(mSDUC, eMinTUp[g=1:G,n=pMinTU[g]:N], sum( vStartup[nn,g] for nn=n+1-pMinTU[g]:n) <=     vCommitment[n,g])
  @constraint(mSDUC, eMinTDw[g=1:G,n=pMinTD[g]:N], sum(vShutdown[nn,g] for nn=n+1-pMinTD[g]:n) <= 1 - vCommitment[n,g])

  writeLP(mSDUC, "modelJuMP.lp", genericnames=false)

  # Solve statement
  if   UC == 1
    status = solve(mSDUC; relaxation=false)
    return status, getobjectivevalue(mSDUC), getvalue(vOutput), getvalue(vIG), getvalue(vCommitment), getvalue(vStartup), getvalue(vShutdown)
  else UC == 0
    status = solve(mSDUC; relaxation=true)
    return status, getobjectivevalue(mSDUC), getvalue(vOutput), getvalue(vIG), getvalue(vCommitment), getvalue(vStartup), getvalue(vShutdown), getdual(eBalance)
  end

end

# solve the stochastic daily unit commitment problem
UC = 1
pCommitt_Lo  = zeros(Int64,N,G)
pCommitt_Up  =  ones(Int64,N,G)
pStartup_Lo  = zeros(Int64,N,G)
pStartup_Up  =  ones(Int64,N,G)
pShutdown_Lo = zeros(Int64,N,G)
pShutdown_Up =  ones(Int64,N,G)
(status_opt, obj, pProduct_opt, pIG_opt, pCommitt_opt, pStartup_opt, pShutdown_opt) =
solve_mSDUC(UC, N, SC, G, pMaxProd, pMinProd, pIntermGen, pDemand, pOperReserve, pOperReserveUp, pOperReserveDw, pSlopeVarCost, pEmissionCost, pInterVarCost, pStartUpCost, pShutDownCost, pIniUC, pIniOut, pMinTU, pMinTD, pCommitt_Lo, pCommitt_Up, pStartup_Lo, pStartup_Up, pShutdown_Lo, pShutdown_Up)

# solve the economic dispatch problem with binary variables fixed to their binary values
UC = 0
pCommitt_Lo  =  pCommitt_opt
pCommitt_Up  =  pCommitt_opt
pStartup_Lo  =  pStartup_opt
pStartup_Up  =  pStartup_opt
pShutdown_Lo = pShutdown_opt
pShutdown_Up = pShutdown_opt
(status_opt, obj, pProduct_opt, pIG_opt, pCommitt_opt, pStartup_opt, pShutdown_opt, pSRMC_opt) =
solve_mSDUC(UC, N, SC, G, pMaxProd, pMinProd, pIntermGen, pDemand, pOperReserve, pOperReserveUp, pOperReserveDw, pSlopeVarCost, pEmissionCost, pInterVarCost, pStartUpCost, pShutDownCost, pIniUC, pIniOut, pMinTU, pMinTD, pCommitt_Lo, pCommitt_Up, pStartup_Lo, pStartup_Up, pShutdown_Lo, pShutdown_Up)

# scaling of the results
pProduct_opt = pProduct_opt * 1e3
for sc in 1:SC
  for n in 1:N
      pSRMC_opt[sc,n]    = pSRMC_opt[sc,n]    * 1e3 / pScenProb[sc]
  end
end

# output of the results
dfProduct = DataFrame(vcat(hcat("Scenario", "Generator", reshape(1:N,1,N)), hcat(sort!(repeat(1:SC,G)), repeat(1:G,SC), reshape(pProduct_opt,SC*G,N))))
dfIG      = DataFrame(vcat(hcat("Scenario",              reshape(1:N,1,N)), hcat(             1:SC,                     reshape(pIG_opt     ,SC,  N))))
dfUC      = DataFrame(vcat(hcat(            "Generator", reshape(1:N,1,N)), hcat(                              1:G,     reshape(pCommitt_opt,   G,N))))
dfSRMC    = DataFrame(vcat(hcat("Scenario",              reshape(1:N,1,N)), hcat(             1:SC,                     reshape(pSRMC_opt   ,SC,  N))))
CSV.write("Output.csv", dfProduct, delim = ";")
CSV.write("IG.csv",     dfIG,      delim = ";")
CSV.write("UC.csv",     dfUC,      delim = ";")
CSV.write("SRMC.csv",   dfSRMC,    delim = ";")
