1  COM I$[20],N$[20],N1$[25],W[10],K[34],P[20,20],R[30],M0$[240]
2  COM R,C,S2,G,R1,A2,T,S1,H1,H3,N7,L,C9
20  COM T$[8],T0$[72],U$[8],U0$[160],O$[5],A0$[210],W$[130],D$[5]
30  COM M$[9],M[10,30],F[10,30]
40  COM A[10],E[10],S[30],G1,V,X,W1,P2,A$[21]
50  DIM Q$[21],Q1$[21]
55  REM b=brk(0)
80  FILES *,ST2.A100
90  MAT  READ #2;S,E,A,M,F
92  P2=0
100  READ #2;A0$,W$,T0$,M0$,U0$
110  PRINT LIN(1)"Welcome to Dragons and Dungeons!"
120  PRINT LIN(1)"Do you need instructions";
121  INPUT O$
122  O$=UPS$(O$)
123  IF O$[1]#"Y" THEN 290
124  PRINT
125  PRINT "This is the game of Dungeons and Dragons, as written back in the"
126  PRINT "late '70s by a pair of University of Northern Iowa students.  I"
127  PRINT "printed a copy of two of the 3 modules; DND and LORDS from back"
128  PRINT "then, and have recreated the missing DRAGON module.  I do not"
129  PRINT "have the names of the original authors and would like to give"
130  PRINT "credit where credit is due. If you have any information on where"
131  PRINT "they can be found, please email me at john@frieltek.com"
132  PRINT "Help me find the authors!"
140  PRINT LIN(1)"The dungeon is a 20x20 matrix of rooms and many, many levels"
141  PRINT "deep. You will encounter all sorts of monsters and treasures"
142  PRINT "during your travels.  Locked doors, enchanted scrolls, magic"
143  PRINT "rings, and maybe a Mage or two..."
150  SYSTEM R,"PAU-1"
200  REM setup the variables, such as monsters, weapons, and how this all interacts
290  PRINT LIN(1)
300  PRINT "Enter your adventurer's password, or type 'NEW' to create"
310  PRINT "a new account ";
320  INPUT I$
330  I$=UPS$(I$)
340  IF I$="NEW" THEN 400
350  IF LEN(I$)#6 THEN 370
360  GOTO 2000
370  PRINT "The password must be 6 characters long and can contain only"
372  PRINT "letters and numbers!"'7
380  GOTO 300
400  PRINT LIN(1)"Enter a 6 character password, letters and numbers only!"
410  INPUT I$
420  IF LEN(I$)#6 THEN 400
425  REM
430  REM check to see if the file exists, and load it so we can compare the
440  REM adventurer name in the file to what they say it is.  If it doesn't match
450  REM then say the file is already in use and to try again.
455  REM
460  I$[7]=".A199"
470  ASSIGN I$[1,11],1,W4
475  REM PRINT "<*> Assign W4:";W4
480  IF W4=3 THEN 510
490  PRINT "That account already exists, try a new password";
500  GOTO 400
509  REM
510  REM password / filename is unique, so try to create and continue
511  REM
530  CREATE W3,I$,4
540  REM PRINT "<*> Create:";W3;" File: ";I$
550  ASSIGN I$[1,11],1,W4
560  REM PRINT "<*> Assign W4:";W4
570  REM W3:0=File Created Successfully, 1: File already exists  2: Invalid filename
580  REM GOTO W3+1 OF 1150,970,600
581  IF W4#3 THEN 600
590  PRINT "Only letters and numbers allowed!"
595  GOTO 300
600  REM OK, its a new user and we created the file!
1160  PRINT "What is your name  ";
1170  INPUT N$
1180  PRINT "What is the name of your adventurer  ";
1190  MAT K=ZER
1200  INPUT N1$
1201  IF LEN(N1$) <= 20 THEN 1205
1202  PRINT "Don't they have a shorter name?  Try that."
1203  GOTO 1180
1205  W1=A2=S1=N7=H=H1=H2=H3=0
1206  L=R1=C9=1
1207  MAT W=ZER
1208  MAT R=ZER
1209  MAT P=ZER
1210  G=55+5*INT(RND(5)*10)
1220  S2=INT(20+5*R1)
1230  T=TIM(0)+60*TIM(1)
1240  R=C=10
1241  GOSUB 1293
1245  REM PRINT "<*> Chaining to the Store!"
1250  CHAIN Z9,"STORE.A100",1000
1290  GOSUB 1293
1291  CHAIN Z4,"DND.A100"
1293  MAT P=ZER
1295  GOSUB 4300
1300  FOR J=1 TO 6
1310  R4=INT(RND(5)*20)+1
1320  C4=INT(RND(5)*20)+1
1323  IF L>1 THEN 1330
1325  IF R4>5 AND R4<15 AND C4>5 AND C4<15 THEN 1310
1330  P[R4,C4]=-5
1340  IF ABS(R4-C4)<3 THEN 1360
1350  P[C4,R4]=-7
1360  NEXT J
1400  FOR I=1 TO INT(RND(5)*3)+1
1410  P[INT(RND(5)*20+1),INT(RND(5)*20+1)]=-9
1430  NEXT I
1432  P[INT(RND(5)*20+1),INT(RND(5)*20+1)]=-11
1440  P[R,C]=.00013
1450  MAT  READ #2,1;S
1460  FOR I=1 TO 30
1470  S[I]=INT((L ** 1.25)*S[I])
1480  NEXT I
1490  RETURN
2000  REM *****************************************************************************
2002  REM
2003  REM check to see if the file exists, and load it so we can compare the
2004  REM adventurer name in the file to what they say it is.  If it doesn't match
2005  REM then say the file is already in use and to try again.
2006  REM ******************************************************************************
2008  I$[7]=".A199"
2010  ASSIGN I$[1,11],1,W4
2020  REM PRINT "<*> File: ";I$;"  Assign W4:";W4
2030  REM W4 3=File does not exist
2040  IF W4=3 THEN 300
2100  READ #1;N$,N1$,T,R1,R,C,S2,G,A2,S1,H1,H3,N7,L,C9
2110  MAT  READ #1;W,R,K,P
2120  PRINT "What is the name of your adventurer";
2130  INPUT Q$
2133  Q$=UPS$(Q$)
2135  Q1$=UPS$(N1$)
2140  IF Q$=Q1$ THEN 2170
2150  PRINT LIN(1)"Either the password or adventurer does not match."
2160  GOTO 300
2170  REM READ #1;N$,N1$,T,R1,R,C,S2,G,A2,S1,H1,H3,N7,L,C9
2180  REM MAT  READ #1;W,R,K,P
2190  PRINT LIN(1)"Well, if it isn`t ";N$;"!  Good to see you!"
2200  CHAIN "DND.A100"
4297  REM **************************************
4298  REM Save the file data so far
4299  REM **************************************
4300  ASSIGN I$[1,11],1,W4
4320  PRINT #1,1;N$,N1$,T,R1,R,C,S2,G,A2,S1,H1,H3,N7,L,C9
4330  MAT  PRINT #1;W,R,K,P
4340  RETURN
8900  PRINT "I'm sorry, but there is no more room to play this game."
8910  PRINT "Let John know he needs to prune abandoned players."
8960  SYSTEM R,"BYE"
8970  STOP
9000  REM
9010  CHAIN Z4,"DND.A100"
9020  PRINT "Chain Error:";Z4
9999  END
