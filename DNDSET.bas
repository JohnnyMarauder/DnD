10  COM I$[20],N$[20],N1$[25],W[10],K[34],P[20,20],R[30],M0$[240]
20  COM R,C,S2,G,R1,A2,T,S1,H1,H3,N7,L,C9
30  COM T$[8],T0$[72],U$[8],U0$[160],O$[5],A$[210],W$[130],D$[5]
40  COM M$[8],M[10,30],F[10,30]
50  COM A[10],E[10],S[30],G1,V,X
60  PRINT "[System Setup]"
70  DIM Z0$[12]
75  DIM P$[30],P1$[30],Y$[30],Y1$[30]
80  Z0$="ST2"
90  CREATE W5,Z0$,12
100  PRINT "Create:";W5
110  FILES *
120  ASSIGN "ST2",1,W4
130  PRINT "assign:";W4
140  IF W5=0 THEN 150
141  MAT  READ #1;S,E,A,M,F
142  READ #1;A$,W$,T0$,M0$,U0$
145  GOTO 370
150  FOR X=1 TO 30
160  S[X]=X
170  NEXT X
180  FOR X=1 TO 10
190  E[X]=X
200  A[X]=X
210  NEXT X
220  FOR X=1 TO 10
230  FOR Y=1 TO 30
240  M[X,Y]=X*Y
250  F[X,Y]=X*Y
260  NEXT Y
270  NEXT X
280  A$="stuffff"
290  W$="stuff"
300  T0$="skdfskfnvcskjdfskdjf"
310  M0$="monster monster "
320  U0$="sdjfjsjsdf"
330  Z=1
340  MAT  PRINT #1;S,E,A,M,F
350  PRINT #1;A$,W$,T0$,M0$,U0$
360  STOP
370  PRINT "Edit Dnd ST2 File Defaults"
375  PRINT
380  PRINT "Edit: [A]rmor, [W]eapons, [T]reasure, [M]onsters, [F]ightSounds, [MP], [AD]";
390  INPUT O$
400  O$=UPS$(O$)
410  IF O$="A" THEN 1000
412  IF O$="W" THEN 2000
430  IF O$="T" THEN 3000
440  IF O$="M" THEN 4000
450  IF O$="F" THEN 5000
460  IF O$="MP" THEN 6000
470  IF O$="AD" THEN 7000
480  IF O$="SAVE" THEN 9000
490  IF O$[1,1]="Q" THEN 9999
500  PRINT "Eh?"
510  GOTO 380
600  REM Trim Str function  Set Y$ to the string to be trimmed, Y1$ will be set trimmed
610  Y1$=""
620  FOR X1=LEN(Y$) TO 1 STEP -1
630  IF Y$[X1,X1]#" " THEN 650
640  NEXT X1
650  Y1$=Y$[1,X1]
699  RETURN
1000  REM Armor to edit
1005  PRINT LIN(1)"EDIT ARMOR"LIN(1)
1010  FOR X=1 TO 10
1020  PRINT X;"Name [";A$[X*21-20,X*21];"]   ";X+10;"Cost [";A[X];"]"
1030  NEXT X
1040  PRINT "Edit # (0=done) ";
1050  INPUT X
1060  IF X <= 0 THEN 375
1070  IF X<11 THEN 1100
1080  P$=A$[(X-10)*21-20,(X-10)*21]
1090  GOTO 1130
1095  P$=A$[X*21-20,X*21]
1100  PRINT "Enter the description for Armor #";X;" ('.' to not change) ";
1110  INPUT P$
1115  IF P1$="." THEN 1000
1120  A$[X*21-20,X*21]=P$[1,21]
1125  GOTO 1000
1130  PRINT "[";Y$;"] [";P$;"]"
1131  GOSUB 600
1133  Y$=P$
1134  GOSUB 600
1135  PRINT "Gold to purchase the ";Y1$;
1140  INPUT Y
1150  A[X-10]=Y
1999  GOTO 1000
2000  REM Weapon to edit
2005  PRINT LIN(1)"EDIT WEAPONS"LIN(1)
2010  FOR X=1 TO 10
2020  PRINT X;"Name [";W$[X*13-12,X*13];"]   ";X+10;"Cost [";E[X];"]"
2030  NEXT X
2040  PRINT "Edit # (0=done) ";
2050  INPUT X
2060  IF X <= 0 THEN 375
2070  IF X<11 THEN 2095
2080  P$=W$[(X-10)*13-12,(X-10)*13]
2090  GOTO 2130
2095  P$=W$[X*13-12,X*13]
2100  PRINT "Enter the description for Weapon #";X;" ('.' to not change) ";
2104  P1$=P$
2110  INPUT P1$
2115  IF P1$="." THEN 2000
2117  P$=P1$
2120  W$[X*13-12,X*13]=P1$[1,21]
2125  GOTO 2000
2130  Y$=P$
2134  GOSUB 600
2135  PRINT "Gold to purchase the ";Y1$;
2140  INPUT Y
2150  E[X-10]=Y
2999  GOTO 2000
3000  REM Treasure to edit
3005  PRINT LIN(1)"EDIT TREASURES"LIN(1)
3010  FOR X=1 TO 9
3020  PRINT X;"Name [";T0$[X*8-7,X*8];"]"
3030  NEXT X
3040  PRINT "Edit # (0=done) ";
3050  INPUT X
3060  IF X <= 0 THEN 375
3070  IF X<10 THEN 3095
3095  P$=T0$[X*8-7,X*8]
3100  PRINT "Enter the description for Treasure #";X;" ('.' to not change) ";
3104  P1$=P$
3110  INPUT P1$
3115  IF P1$="." THEN 3000
3117  P$=P1$
3120  T0$[X*8-7,X*8]=P1$[1,8]
3999  GOTO 3000
4000  REM Edit the Monsters
4005  PRINT LIN(1)"EDIT MONSTERS"LIN(1)
4010  FOR X=1 TO 30
4020  PRINT X;"Name [";M0$[X*8-7,X*8];"]"
4030  NEXT X
4040  PRINT "Edit # (0=done) ";
4050  INPUT X
4060  IF X <= 0 THEN 375
4070  IF X<31 THEN 4095
4080  GOTO 4000
4095  P$=M0$[X*8-7,X*8]
4100  PRINT "Enter the description for Monster #";X;" ('.' to not change) ";
4104  P1$=P$
4110  INPUT P1$
4115  IF P1$="." THEN 4000
4117  P$=P1$
4120  M0$[X*8-7,X*8]=P1$[1,8]
4999  GOTO 4000
5000  REM Fight Sounds to edit
5005  PRINT LIN(1)"EDIT FIGHTING SOUNDS"LIN(1)
5008  PRINT "Sounds YOU make when hitting monsters..."
5010  FOR X=1 TO 20
5012  IF X#11 THEN 5020
5015  PRINT LIN(1)"Sounds the MONSTERS make hitting you!"
5020  PRINT X;"Name [";U0$[X*8-7,X*8];"]"
5030  NEXT X
5040  PRINT "Edit # (0=done) ";
5050  INPUT X
5060  IF X <= 0 THEN 375
5070  IF X<21 THEN 5095
5080  GOTO 5000
5095  P$=U0$[X*8-7,X*8]
5100  PRINT "Enter the description for Sound #";X;" ('.' to not change) ";
5104  P1$=P$
5110  INPUT P1$
5115  IF P1$="." THEN 5000
5117  P$=P1$
5120  U0$[X*8-7,X*8]=P1$[1,8]
5999  GOTO 5000
6000  REM Probability of Weapon[X] hitting Monster[Y]
6005  PRINT LIN(1)"EDIT WEAPON HITTING MONSTER"LIN(1)
6008  PRINT "Probability of Weapon[X] hitting Monster[Y]"
6010  PRINT "Enter Weapon #, 0 to quit: ";
6015  INPUT W9
6017  IF W9#0 THEN 6020
6018  IF W9>10 THEN 6010
6019  GOTO 375
6020  PRINT "Weapon ";W$[W9*13-12,W9*13]
6021  PRINT " #     - Monster     Hit Probability"
6022  FOR X=1 TO 30
6024  PRINT X;" - ";M0$[X*8-7,X*8];"   ";100-M[W9,X]
6030  NEXT X
6040  PRINT "Edit # (0=done) ";
6050  INPUT X4
6060  IF X4 <= 0 THEN 6010
6070  IF X4<31 THEN 6095
6080  GOTO 6000
6095  PRINT "Enter Probability:";
6100  INPUT P9
6110  IF P9<1 THEN 6040
6115  IF P9<1 THEN 6040
6120  IF P9>100 THEN 6040
6130  M[W9,X4]=100-P9
6140  GOTO 6040
6999  GOTO 6000
7000  REM Probability of Monster[X] hitting against Armor[Y]
7005  PRINT LIN(1)"EDIT MONSTER HITTING ARMOR"LIN(1)
7008  PRINT "Probability of Monster[Y] hitting Armor[X]"
7010  PRINT "Enter Armor #, 0 to quit: ";
7015  INPUT W9
7017  IF W9#0 THEN 7020
7018  IF W9>10 THEN 7010
7019  GOTO 375
7020  PRINT "Armor ";A$[W9*21-20,W9*21]
7021  PRINT " #     - Monster     Hit Probability"
7022  FOR X=1 TO 30
7024  PRINT X;" - ";M0$[X*8-7,X*8];"   ";100-F[W9,X]
7030  NEXT X
7040  PRINT "Edit # (0=done) ";
7050  INPUT X4
7060  IF X4 <= 0 THEN 7010
7070  IF X4<31 THEN 7095
7080  GOTO 7000
7095  PRINT "Enter Probability:";
7100  INPUT P9
7110  IF P9<1 THEN 7040
7120  IF P9>100 THEN 7040
7130  F[W9,X4]=100-P9
7140  GOTO 7040
9000  REM save
9010  ASSIGN "ST2",1,W4
9015  PRINT "Assign:";W4
9020  MAT  PRINT #1;S,E,A,M,F
9030  PRINT #1;A$,W$,T0$,M0$,U0$
9040  PRINT LIN(1)"Data saved."
9999  END
