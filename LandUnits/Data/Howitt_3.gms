
$TITLE  POSITIVE PROGRAMMING EXAMPLE
* ILLUSTRATIVE POSITIVE PROGRAMMING MODEL- LIVESTOCK- CROP MIX
* R E HOWITT ,  JULY 1997
* DEPT OF AGRICULTURAL ECONOMICS
* U C DAVIS. CA
*  e-mail   rehowitt.ucdavis.edu


*##################################################################

$OFFSYMLIST OFFSYMXREF
 OPTION LIMROW = 0
 OPTION LIMCOL = 0
 option iterlim =1500
*******************************************************************

 SETS  I      ACTIVITIES               /COT,BAR,RI,HAY,DAIRY,BEEF,GRAZE,SILAGE,
                                        FEEDB,FEEDH,SELLB, SELLH /
       II(I)  PRODUCTION PROCESSES     /COT,BAR,RI,DAIRY,BEEF,GRAZE,HAY,SILAGE /
       IM(I)  INTERMEDIATE PROCESS     / FEEDB, FEEDH, SELLB, SELLH /
       CR(I)  CROPPING ACTIVITIES      /COT,BAR,RI,GRAZE, HAY,SILAGE /
       GR(I)  GRASS ACTIVITIES        / GRAZE, SILAGE /
       SL(I)  FODDER SELLING CROPS     / BAR, HAY /
       FD(IM)  FODDER FEEDING          / FEEDB, FEEDH /
       IL(I)  LIVESTOCK ACTIVITIES     / DAIRY, BEEF /
       FO(I)  FODDER ACTIVITIES        / GRAZE,SILAGE, FEEDB, FEEDH /
       J      INPUTS                   /LAND,WATER,CAPITAL, HEAD, TONS /
       VAR(J) VARIABLE INPUTS          / CAPITAL, HEAD/


       ALIAS (I,K)
       ALIAS (J,L)
*##################################################################
*     DATA - NOTE THE MINIMAL LP DATA SET
*##################################################################

   SCALAR   PER  PMP PERTURBATION   / 0.0001 /

 PARAMETER V(I)    PRICE PER UNIT PRODUCED
     /COT         292.4
      BAR          0.0
      RI           7.09
      HAY          0.0
      DAIRY        1.25
      BEEF         1.10
      GRAZE        0.0
      BEEF         0.0
      FEEDB         0.0
      FEEDH         0.0
      SELLB        2.98
      SELLH        80.0
       /


 PARAMETER YB(I)   REGIONAL AVERAGE YIELDS
      /COT        2.20
       BAR        95.0
       RI         70.1
       HAY         4.5
       DAIRY     1000.0
       BEEF       800.0
       GRAZE       4.0
       SILAGE       2.0
       FEEDB        0.06
       FEEDH        0.067
       SELLB        1.0
       SELLH        1.0  /


 TABLE C(I,J)  RESOURCE VARIABLE COSTS
                     LAND       WATER      CAPITAL     HEAD   TONS
       COT           264.3       25.6        0.1        0.0
       BAR            60.0       25.6        0.1        0.0
       RI            196.9       25.6        0.1        0.0
       HAY           158.2       28.4        0.1        0.0
       DAIRY           0.0        0.0        0.1      350.0
       BEEF            0.0        0.0        0.1      300.0
       GRAZE          20.0        5.0        0.1        0.0
       SILAGE         20.0        3.0        0.1        0.0
       FEEDB          0.0        0.0        0.0        0.0     110
       FEEDH          0.0        0.0        0.0        0.0     11.0
       SELLB
       SELLH

 TABLE X(I,J)  BASE RESOURCE USE
                     LAND       WATER      CAPITAL     HEAD      TONS
       COT            1.49       4.47        610        0.0
       BAR            0.62       1.14        350        0.0
       RI             0.54       3.08        520        0.0
       HAY            2.74       7.95        480        0.0
       DAIRY          0.0        0.0         500        3.5
       BEEF           0.0        0.0         450        5.5
       GRAZE          0.8        2.5         120        0.0
       SILAGE         1.75       2.0         250        0.0
       FEEDB          0.0        0.0           0.0      0.0       33.0
       FEEDH          0.0        0.0           0.0      0.0       10.0
       SELLB
       SELLH

  TABLE  XFED(IL, FD)   QUANTITIES FED
                      FEEDB       FEEDH
           DAIRY       18.0        6.0
           BEEF        15.0        4.0

 TABLE  MAP1(I,J)  MAPPING FOR KEY INPUTS
                     LAND       WATER      CAPITAL     HEAD      TONS
       COT            1.0
       BAR            1.0
       RI             1.0
       HAY            1.0
       DAIRY          0.0        0.0         0.0        1.0
       BEEF           0.0        0.0         0.0        1.0
       GRAZE          1.0
       SILAGE         1.0
       FEEDB          0.0        0.0           0.0      0.0        0.0
       FEEDH          0.0        0.0           0.0      0.0        0.0
       SELLB
       SELLH

  TABLE MAP2(SL,IM)    MAPPING FOR JOINT ACTIVITIES

              FEEDB     FEEDH     SELLB     SELLH
       BAR     1                    1
       HAY                1                   1

 TABLE MAP3(SL,IM)    MAPPING FOR PRICING JOINT ACTIVITIES

              FEEDB     FEEDH     SELLB     SELLH
       BAR                          1
       HAY                                    1


 PARAMETER
         RR(I,J) REGIONAL LEONTIEFF COEFFICIENTS
         CL(I) LINEAR COST
         NET(I) NET CASH RETURN
         R(J)    RHS CALCULATION ;

  RR(IM,J)  = 0.0 ;
  RR(II,J) =  X(II,J) / SUM(L,  MAP1(II,L)* X(II,L) );

  CL(I) = SUM(J, C(I,J)*RR(I,J) ) ;

  R(J)   = SUM(I, X(I,J) ) ;

*  SET THE RHS SO THE VARIABLE INPUTS ARE NOT CONSTRAINING
  R(VAR) = R(VAR) * 1.25 ;
  R("LAND") = R("LAND") * 0.95 ;

*   DISPLAY  CL, RR, R ;


*##################################################################
* LINEAR PROGRAM TO CALCULATE RESOURCE AND PMP DUALS
*##################################################################

 VARIABLES  LX(I)   ACTIVITY
            LXFD(IL,FD)  FEED ALLOCATION
            LINPROF   LP PROFIT

 POSITIVE VARIABLE LX, LXFD;

 EQUATIONS RESOURCE(J)   CONSTRAINED RESOURCES
           CALIBU(I)     UPPER CALIBRATION CONSTRAINTS
           CALIBU2(I)    LIVESTOCK CALIBRATION CONSTRAINTS
           CALIBF(IL,FD)  FEEDING CALIBRATION
           FEEDING          FODDER REQUIREMENT
           LFEED1(FD)   FEED ALLOCATION
           ALLOC(SL)        ALLOCATION OF JOINT ACTIVITIES
           LPROFIT         LP OBJECTIVE FUNCTION;

 RESOURCE(J)..                 SUM(I,RR(I,J)*LX(I))   =L= R(J);

 CALIBU(CR)..                    LX(CR) =L= X(CR,"LAND")* (1+PER);

 CALIBU2(IL)..                    LX(IL) =L= X(IL,"HEAD")* (1+PER);

 CALIBF(IL,FD)..            LXFD(IL,FD) =G= XFED(IL,FD)* (1+PER);

 FEEDING..        SUM(IL, LX(IL)) =L= SUM(FO, LX(FO) * YB(FO)) ;

 LFEED1(FD)..    LX(FD) =G= SUM(IL, LXFD(IL,FD) ) ;

 ALLOC(SL)..         YB(SL) * LX(SL) =G= SUM(IM, MAP2(SL,IM) * LX(IM)) ;

 LPROFIT..     SUM((I),((V(I)*YB(I))-CL(I))*LX(I)) =E= LINPROF;

 MODEL  CALIBRATE /RESOURCE, CALIBU,CALIBU2,
                  CALIBF, FEEDING, LFEED1, ALLOC, LPROFIT /;

 SOLVE CALIBRATE   USING LP MAXIMIZING LINPROF;

* DISPLAY LX.L, X, LX.M, LXFD.L, XFED;

*#########################################################################
*   CALCULATING THE COEFFICIENTS FOR THE PMP QUADRATIC YIELD FUNCTION
*#########################################################################

    PARAMETER
               OP       OPPORTUNITY COST OF LAND
               ADJ      ADJUSTMENT TO MARGINAL CROP DUALS
               VV(I)     ADJUSTED OUTPUT VALUES FOR FODDER ACTIVITIES
               LAMU(I)   UPPER PMP DUAL VALUE
               FLAG(I)   MARGINAL CROP INDICATOR
               MYLD(I)   MARGINAL YIELD PARAMETER
               XYLD(I)   CALIBRATED MAXIMUM YIELD
               FALPH(IL,FD)  FEEDING VALUE INTERCEPT
               FGAM(IL,FD)  FEEDING VALUE SLOPE
                                                    ;


*  ADJUSTING THE OPPORTUNITY COST OF LAND FOR THE MARGINAL YIELD
*  OF THE MARGINAL CROP IN EACH REGION

    ADJ = RESOURCE.M("LAND") * 0.2 ;

*#################################################################
*   DEFINING THE IMPUTED VALUES OF GRAZING AND FODDER SELLING ACTIVITIES

     VV(I)  = V(I) ;
     VV(SL) = SUM(IM, MAP3(SL,IM)* V(IM)) ;
     VV(GR)  = FEEDING.M ;

    OP = RESOURCE.M("LAND") - ADJ ;

    NET(I) = (YB(I) * VV(I) ) - CL(I) ;

    FLAG(II) = 0;
    FLAG(II)$((X(II,"LAND") NE 0) AND (CALIBU.M(II) EQ 0)) = 1;

    LAMU(CR)  = CALIBU.M(CR) + ADJ ;
    LAMU(IL)  = CALIBU2.M(IL) ;

*#################################################################
*  SPECIFICATION OF THE YIELD FUNCTION FOR THE MARGINAL CROPS
*  IN EACH REGION USING TWENTY PERCENT REDUCTION IN OPP COST

    MYLD(II)$(FLAG(II) AND X(II,"LAND") AND LX.L(II) )
            =  ADJ /( VV(II) * LX.L(II)) ;

    XYLD(II)$(FLAG(II) AND X(II,"LAND") )
             = YB(II) + (MYLD(II) * LX.L(II)) ;

*#####################################################################

* CALCULATION OF YIELD FUNCTION FOR NON-MARGINAL PRODUCTION ACTIVITIES


    MYLD(II)$( (FLAG(II) EQ 0) AND LX.L(II) )
                       =  LAMU(II) / ( VV(II) * LX.L(II)) ;

    XYLD(II)$ LX.L(II)
                       = YB(II) + (MYLD(II) * LX.L(II)) ;

* FIXED YIELD FOR INTERMEDIATE ACTIVITIES

    XYLD(IM) = YB(IM) ;

* DISPLAY  MYLD, XYLD ;

*  CALCULATING THE IMPLICIT VALUES FOR FEEDING

    FALPH(IL,FD)  = - 2 * CALIBF.M(IL,FD) ;
    FGAM(IL,FD)$XFED(IL,FD)   =  CALIBF.M(IL,FD) / XFED(IL,FD) ;

*###########################################################
*    POLICY CHANGES ARE PUT IN HERE  (shut off for now)

*   EXAMPLE FOR CHANGE IN PRICE OR CHANGE IN QUANTITY OF RESOURCE
*     10% increase in dairy prices in all regions
*     V(IL) = V(IL) * 1.10 ;

*     20% increase in BARLEY prices in all regions
*     V("SELLB") = V("SELLB") * 1.20 ;

*     20% increase in COTTON prices in all regions
*     V("COT") = V("COT") * 1.20 ;

*    10% reduction in water available  in both regions
*   R("WATER") = R("WATER") * 0.90 ;


*################################################################

*  PMP MODEL SOLUTION FOR BASE YEAR

*################################################################

 VARIABLES   NX(I)     NONLINEAR ACTIVITY
             NLPROF  NONLINEAR PROFIT
             NXFD(IL,FD)  FEED ALLOCATION  ;

 POSITIVE VARIABLE NX, NXFD ;

 EQUATIONS RESOURCEN(J)   CONSTRAINED RESOURCES
           FEED1(FD)         FEED ALLOCATION
           FEED2        FODDER REQUIREMENT
           ALLOCN(SL)   JOINT ACTIVITY ALLOCATION
           NPROFIT         NLP OBJECTIVE FUNCTION;

  RESOURCEN(J)..    SUM(I,RR(I,J)*NX(I))   =L= R(J);

  FEED2..          SUM(IL, NX(IL)) =L=
               SUM(FO, (XYLD(FO) - MYLD(FO)* NX(FO)) * NX(FO) ) ;

  FEED1(FD)..    NX(FD) =E= SUM(IL, NXFD(IL,FD) ) ;

  ALLOCN(SL)..   (XYLD(SL) - MYLD(SL)* NX(SL)) * NX(SL)
               =G= SUM(IM, MAP2(SL,IM) * NX(IM)) ;

  NPROFIT..    SUM((I), ( V(I) * (XYLD(I) - MYLD(I)* NX(I))
               - CL(I) ) * NX(I) )
   + SUM((IL,FD), (FALPH(IL,FD) + 0.5* FGAM(IL,FD)*NXFD(IL,FD))*NXFD(IL,FD))
                       =E= NLPROF;

 MODEL  PRIMAL  / RESOURCEN, FEED1, FEED2, ALLOCN, NPROFIT / ;

    NX.L(I) = LX.L(I)* 0.95 ;

 SOLVE  PRIMAL   USING NLP MAXIMIZING NLPROF;

   PARAMETER    TEST1(IL,FD)   FEED VALUE TEST ;

       TEST1(IL,FD) = FALPH(IL,FD) +  FGAM(IL,FD)* XFED(IL,FD) ;

* DISPLAY LX.L, NX.L, X, LX.M, LXFD.L, XFED, NXFD.L, TEST1, CALIBF.M,
*         FALPH,  FGAM ;


*#####################################################################
*  SHOW RESULTS
*#####################################################################

 PARAMETER
               MARGYLD(I)   MARGINAL YIELD
               AVYLD(I)     AVERAGE YIELD AT BASE LP S
               PERDIF(I)    PERCENT DIFFERENCE IN ACTIVITY
               feedpro(sl)  total feed production
               feeduse(SL)   TOTAL USE
                                         ;

           PERDIF(I)$LX.L(I) =
                  (NX.L(I) - LX.L(I)) * 100 / LX.L(I) ;

           MARGYLD(I) = XYLD(I) -(2* MYLD(I)* NX.L(I)) ;
           MARGYLD(IL) = XYLD(IL) -(2* MYLD(IL)* NX.L(IL)) ;

            AVYLD(I) = XYLD(I) -( MYLD(I)* LX.L(I)) ;
            FEEDPRO(SL) = (XYLD(SL) - MYLD(SL)* NX.L(SL)) * NX.L(SL) ;
            FEEDUSE(SL)   = SUM( IM, MAP2(SL,IM)* NX.L(IM) ) ;

 DISPLAY
           XYLD,MYLD, MARGYLD, YB, AVYLD, LINPROF.L, NLPROF.L,
           LX.L, NX.L, PERDIF, RESOURCE.M, RESOURCEN.M, ADJ, NET,
           LINPROF.L, NLPROF.L, FEEDPRO, FEEDUSE ;
*####################################################################
