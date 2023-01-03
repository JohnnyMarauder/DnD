10  COM I$[20],N$[20],N1$[25],W[10],K[34],P[20,20],R[30],M0$[240]
20  COM R,C,S2,G,R1,A2,T,S1,H1,H3,N7,L,C9
30  COM T$[8],T0$[72],U$[8],U0$[160],O$[5],A0$[210],W$[130],D$[5]
40  COM M$[9],M[10,30],F[10,30]
50  COM A[10],E[10],S[30],G1,V,X,W1,P2,A$[21]
70  DIM Q$[25],N0$[25],Q1$[25],G1$[7]
100  REM *************************************************************
110  REM                  CHAIN Subroutines from *DND
120  REM
200  REM NOTE: Any variables used in these subroutines MUST exist in
210  REM       the above COM statements so they carry forward and back
220  REM       using the CHAIN to return!  Failure is not an option!
230  REM *************************************************************
4200  V=INT(P[R,C]*10)
4210  X=INT((INT(P[R,C]*1000)-V*100)/3)
4220  M$=M0$[8*X+1,8*X+8]
4221  IF POS(M$," ")=0 THEN 4230
4222  M$=M$[1,POS(M$," ")-1]
4230  IF X=1 THEN 4250
4250  RETURN
8000  REM
8020  G1=INT(P[R,C]*100)
8025  H1=INT(H1/3+RND(5)*5)+1
8030  GOSUB 4200
8050  T$=T0$[8*G1+1,8*G1+8]
8055  T$=T$[1,POS(T$," ")-1]
8060  E4=INT(RND(5)*10)+1
8070  GOTO E4 OF 8090,8120,8150,8180,8210,8240,8270,8300,8330,8360
8090  PRINT "Under a clump of refuse in the north corner, ";N1$
8100  PRINT "finds a pile of ";T$;" worth ";H1;" gold."
8110  GOTO 8400
8120  PRINT "Beside the body of one of the dead ";M$;" in the room,"
8130  PRINT N1$;" finds some ";T$;" worth about ";H1;" gold."
8140  GOTO 8400
8150  PRINT N1$;" finds ";H1;" gold worth of ";T$;" in a small"
8160  PRINT "hole in the wall of the south east corner of the room."
8170  GOTO 8400
8180  PRINT N1$;" notices something strange about one of the dead ";M$
8190  PRINT "in the room.  Cutting the beast open reveals some ";T$;"."
8195  PRINT H1;" gold worth of ";T$
8200  GOTO 8400
8210  PRINT N1$;" finds some ";T$;" in a small wooden chest in the"
8220  PRINT "north west corner of the room.  Worth :";H1;" gold."
8230  GOTO 8400
8240  PRINT "The ";M$;" were hoarding ";T$;"!  ";N1$;" picks up"
8250  PRINT H1;" gold worth of ";T$;"."
8260  GOTO 8400
8270  PRINT "Behind a secret panel in the south wall, ";N1$;" finds"
8280  PRINT H1;" gold worth of ";T$;"."
8290  GOTO 8400
8300  PRINT "Scattered among the bones in the room, ";N1$;" finds"
8310  PRINT H1;" gold worth of ";T$;"."
8320  GOTO 8400
8330  PRINT N1$;" finds ";H1;" gold worth of ";T$;" uneder a basin"
8340  PRINT "at the base of the west wall of the room."
8350  GOTO 8400
8360  PRINT "On a natural shelf, just above the east door, ";N1$
8370  PRINT "discovered a small sack containing ";H1;" gold worth of ";T$;"."
8400  G=G+H1
8410  IF RND(5)<.15*L/(R1+4) THEN 9100
8420  IF H1>11*L THEN 8440
8425  P[R,C]=0
8430  CHAIN Z9,"DND.A100",3350
8440  CHAIN Z9,"DND.A100",8440
9100  FOR J=1 TO INT(S[X+1]/6/L)+1
9110  I=INT(RND(5)*12)+1
9120  IF I=11 THEN 9210
9130  IF I=12 THEN 9280
9140  IF W[I]=0 THEN 9400
9143  W8=RND(5)
9145  IF W8>.15 THEN 9150
9147  W8=.0001
9150  W[I]=W8*L MAX 1
9160  IF E[I]/5=INT(E[I]*W[I]/5) THEN 9400
9170  PRINT LIN(1)"Then ";N1$;" finds a magic scroll that enchants his ";W$[I*13-12,I*13]
9180  PRINT "It will inflict ";INT(E[I]*W[I]/5);" blows/hit until the end.."
9190  GOTO 9400
9210  G5=INT(RND(5)*(250+10*L))
9220  IF RND(5)>.5 THEN 9240
9230  G5=-G5
9240  G=G+G5 MAX 0
9250  PRINT LIN(1)"Then ";N1$;" finds a magic scroll that affects his gold!"
9260  PRINT "He now has a total of ";G;" gold."
9270  CHAIN Z9,"DND.A100",2170
9280  R5=INT(RND(5)*R1*300*L/(L+1))/1000
9290  IF RND(5)>.5 THEN 9330
9300  IF R1<5*L THEN 9400
9310  R5=-R5
9330  R1=R1+R5
9340  PRINT LIN(1);SPA(29)"*** POOF ***"
9350  PRINT "There was a Rank spell in this room.  ";N1$;" is now rank ";R1
9400  NEXT J
9410  GOTO 8420
9500  PRINT LIN(1);SPA(29);"*** ZAP! ***"
9510  PRINT SPA(17)"Automatic Transporter Activated"
9515  L=L+1 MAX L+INT(RND(5)*R1/11)
9520  CHAIN Z9,"DND.A100",1290
9999  END
