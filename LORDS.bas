10  COM I$[20],N$[20],N1$[25],W[10],K[34],P[20,20],R[30],M0$[240]
20  COM R,C,S2,G,R1,A2,T,S1,H1,H3,N7,L,C9
20  COM T$[8],T0$[72],U$[8],U0$[160],O$[5],A0$[210],W$[130],D$[5]
30  COM M$[9],M[10,30],F[10,30]
40  COM A[10],E[10],S[30],G1,V,X,W1,P2,A$[21]
59  IF  ERROR  THEN 9998
60  FILES HROLL.A100,HROLL.A100
70  DIM Q$[25],N0$[25],Q1$[25],G1$[7]
100  LOCK #1,A
110  IF A#0 THEN 100
120  GOSUB 870
130  Q1$=N1$
150  READ #2,1
160  IF  END #1 THEN 210
170  READ #1;Q$,R3,S3,L3
180  IF Q$=N1$ THEN 170
190  PRINT #2;Q$,R3,S3,L3
200  GOTO 170
210  PRINT #2; END
212  ASSIGN "HROLL.A100",1,A
215  ASSIGN "HROLL.A100",2,A
220  IF S2<0 THEN 360
225  REM IF R1*L<8 THEN 360
230  IF  END #2 THEN 340
232  GOTO 240
235  ADVANCE #1;4,A
240  READ #2;Q$,R3,S3,L3
250  IF R3*L3>R1*L THEN 235
280  IF  END #1 THEN 350
290  PRINT #1;N1$,R1,S2,L
300  PRINT #1;Q$,R3,S3,L3
320  READ #1;Q$,R3,S3,L3
330  GOTO 300
340  PRINT #1;N1$,R1,S2,L
350  PRINT #1; END
360  PRINT LIN(2);"Do you want to see the Honor Roll";
365  ASSIGN "HROLL.A100",2,A
366  ASSIGN "HROLL.A100",1,A
370  INPUT O$
380  O$=UPS$(O$)
390  READ #1,1
400  IF TYP(1)=3 THEN 930
410  IF O$[1,1]#"Y" THEN 441
420  PRINT LIN(2);SPA(30);"Honor Roll"
430  PRINT LIN(1);SPA(19);"High Lords of the Blue Mountains"
440  PRINT LIN(1)"NAME"TAB(27)"RANK";SPA(12)"STRENGTH";SPA(8)"LEVEL"
441  IF TYP(1)=3 THEN 930
442  READ #1;Q$,R3,S3,L3
443  N0$[1,5]="King "
444  N0$[6]=Q$
445  IF O$[1,1]#"Y" THEN 447
446  PRINT LIN(1),N0$;TAB(26);R3;TAB(42);S3,L3
447  IF Q$#N1$ THEN 450
448  Q1$=N0$
450  FOR I=1 TO 4
460  IF TYP(1)=3 THEN 930
470  READ #1;Q$,R3,S3,L3
480  N0$[1,5]="Lord "
490  N0$[6]=Q$
500  IF O$[1,1]#"Y" THEN 530
510  PRINT
520  PRINT N0$;TAB(26);R3;TAB(42);S3,L3
530  IF Q$#N1$ THEN 560
550  Q1$=N0$
560  NEXT I
570  IF TYP(1)=3 THEN 930
575  B=0
580  IF O$[1,1]#"Y" THEN 620
585  ENTER 30,W3,W3
590  PRINT LIN(2);SPA(10);"Knights of the Blue Mountains (rank * level > 120)"
600  PRINT LIN(1)"NAME"TAB(27)"RANK";SPA(12)"STRENGTH";SPA(8)"LEVEL"
620  IF TYP(1)=3 THEN 930
630  READ #1;Q$,R3,S3,L3
640  IF R3*L3<120 THEN 670
650  N0$[1,5]=" Sir "
660  GOTO 730
670  N0$[1,5]="Page "
680  IF O$[1,1]#"Y" THEN 730
690  IF B THEN 730
695  ENTER 30,W3,W3
700  PRINT LIN(2);SPA(10);"Apprentice Knights of the Blue Mountain (rank * Level)"
710  PRINT LIN(1);"NAME"TAB(27)"RANK";SPA(12)"STRENGTH";SPA(8)"LEVEL"
720  B=1
730  N0$[6]=Q$
740  IF O$[1,1]#"Y" THEN 770
750  PRINT
760  PRINT N0$;TAB(26);R3;TAB(42);S3,L3
770  IF Q$#N1$ THEN 800
790  Q1$=Q$[6]
800  GOTO 620
810  IF Q$[1,5]="Lord " THEN 850
815  IF Q$[1,5]="KING " THEN 850
820  IF Q$[1,5]=" Sir " THEN 850
830  IF Q$[1,5]="Page " THEN 850
840  GOTO 860
850  Q$=Q$[6]
860  RETURN
870  IF N1$[1,5]="Lord " THEN 910
875  IF N1$[1,5]="KING " THEN 910
880  IF N1$[1,5]=" Sir " THEN 910
890  IF N1$[1,5]="Page " THEN 910
900  GOTO 920
910  N1$=N1$[6]
920  RETURN
930  PRINT
940  UNLOCK #1,A
950  REM N1$=Q1$
960  CHAIN "DND.A100",4100
2000  P[R,C]=-8.1
2001  GOTO INT(RND(5)*10+1) OF 4000,5000
2700  PRINT N1$;" finds a treasure chest in the room."
2710  PRINT "It could be booby-trapped. Does he open it";
2720  INPUT O$
2730  O$=UPS$(O$)
2740  IF O$[1,1]#"Y" THEN 9900
2750  IF RND(5)<.4 THEN 2840
2760  S2=S2-INT(RND(5)*10*L+1)
2770  PRINT "BOOM!"'7
2780  IF S2 >= 0 THEN 2830
2800  PURGE W4,I$
2810  PRINT N1$;"has been killed!"'7'7'7'7
2820  GOTO 100
2830  PRINT N1$;" has ";S2;" blows left!"
2840  PRINT LIN(1)"Inside the chest ";N1$;" finds"
2850  P9=INT(RND(5)*3+1)
2860  GOTO INT(RND(5)*10+1) OF 3020,3020,3020,3300,3300,3200
2870  G5=INT(RND(5)*25*L+P9*100)
2880  PRINT "A Mithril ingot worth";G5;" gold."
2890  G=G+G5
2900  GOTO 9900
3020  FOR I=1 TO P9
3030  X=INT(RND(5)*30+1)
3040  R[X]=INT(RND(5)*20+40)+R[X]
3042  Q1$ = M0$[8*X-7,8*X]
3043  IF POS(Q1$," ") = 0 then 3060 
3044  Q1$ = Q1$[1,POS(Q1$," ")-1]
3060  PRINT "A ";Q1$;" Ring of Power!"
3070  NEXT I
3080  PRINT "Rings of Power reduce their monster's prob. of hitting you."
3090  PRINT "However, they have a limited number of uses.  Use the"
3100  PRINT "Action : Rings  to see how many uses you have left!"
3110  GOTO 9900
3200  N7=1+INT(RND(5)*10)/10
3210  G=G+40+INT(20*R1)
3220  PRINT "A brilliant Gem of Light!  it is worth";40+INT(20*R1);" gold."
3230  PRINT "However, it is so bright that ";N1$;" is blinded!"
3240  GOTO 9900
3300  PRINT "A magic potion!  Does ";N1$;" drink it";
3310  INPUT O$
3320  O$=UPS$(O$)
3330  IF O$[1,1]#"Y" THEN 9900
3335  PRINT
3340  GOTO INT(RND(5)*10+1) OF 3350,3370,3400,3440,3480,3510,3550,3600,3640,3680
3350  PRINT "Water.  Nothing happens."
3360  GOTO 9900
3370  N7=1+INT(RND(5)*10)/10
3380  PRINT "Blindness potion!  ";N1$;" can't see at all!"
3390  GOTO 9900
3400  N7=2+INT(RND(5)*10)/10
3410  PRINT "Berserk potion!  Until the potion wears off, ";N1$
3420  PRINT "will have almost superhuman speed in fighting.  But it will"
3430  PRINT "cost him 4% of his rank for each round of attack."
3435  GOTO 9900
3440  N7=3+INT(RND(5)*10)/10
3450  PRINT "Invulnerability potion!  2/3 of every monster's attack"
3460  PRINT "will bounce off ";N1$;" without effect."
3470  GOTO 9900
3480  N7=4+INT(RND(5)*10)/10
3490  PRINT "X-ray Vision potion!  ";N1$;" can see through locked doors."
3500  GOTO 9900
3510  S2=-INT(20+5*R1)
3530  PRINT "Plague potion!  ";N1$;" collapses in agony!"
3540  CHAIN "DND.A100",4100
3550  R=INT(RND(5)*20+1)
3560  C=INT(RND(5)*20+1)
3570  PRINT "Bottled grey mist!  When ";N1$;" opens the bottle"
3580  PRINT "he is WARPED to Room # (";R;",";C;")"
3590  GOTO 9900
3600  S2=INT(S2+24+5*RND(5))
3610  PRINT "Healing potion!  It is so potent that ";N1$
3620  PRINT "has his strength brought up to";S2;"blows."
3630  GOTO 9900
3640  C9=C9+INT((RND(5)-.53)*4+1)
3650  PRINT "Hypnotic potion!  ";N1$;" goes into a trance."
3660  PRINT "He changes his constitution to";C9
3670  GOTO 9900
3680  N7=5+INT(RND(5)*10)/10
3690  PRINT "Detection potion!  ";N1$;" can see stairways and"
3700  PRINT "special rooms at a distance."
3710  GOTO 9900
4000  PRINT N1$;" walks into the room and sees what appears to be"
4010  PRINT "a large hobbit in a wizard's cloak";
4020  GOTO INT(RND(5)*6+1) OF 4070,4110,4150,4190,4230
4030  PRINT " doing sit-ups"
4040  PRINT "in mid-air!  Spying you in mid-sit, he stops, does a double"
4050  PRINT "back flip and lands in a wastepaper basket."
4060  GOTO 4260
4070  PRINT " sitting at a desk."
4080  PRINT "The unusual thing is that the desk is on the ceiling!"
4090  PRINT "He looks down at you casually from his perch."
4100  GOTO 4260
4110  PRINT ", he is engaged"
4120  PRINT "in changing rabbits into canaries and vice-versa.  With a "
4130  PRINT "wave of his hand the all disappear in a puff of smoke."
4140  GOTO 4260
4150  PRINT " mixing a potion in"
4160  PRINT "test-tubes, behind a large lab bench filled with oddly"
4170  PRINT "shaped glassware.  He lets the test-tubes hover and steps forward."
4180  GOTO 4260
4190  PRINT " standing by a pentagram."
4200  PRINT "Within the pentagram is a blue-white flame 12 feet tall,"
4210  PRINT "which disappears as ";N1$;" enters."
4220  GOTO 4260
4230  PRINT " sitting on the"
4240  PRINT "west wall polishing silver.  When he sees you, he hops"
4250  PRINT "down and places the silver in a drawer."
4260  GOTO INT(RND(5)*6+1) OF 4290,4320,4350,4390,4410
4270  PRINT "He says: You are ";N1$;", right?  I knew it!"
4280  GOTO 4420
4290  PRINT "Looking at the scroll that has materialized in fron of him,"
4300  PRINT "the wizard says: Alright ";N1$;" let's do business."
4310  GOTO 4420
4320  PRINT "He snaps his fingers and '";N1$;"' appears in"
4330  PRINT "glowing letters.  The wizard mutters: Silly name.  Oh well!"
4340  GOTO 4420
4350  PRINT "He wistles and an imp runs into the room, hops on his"
4360  PRINT "shoulder and whispers.  The wizard straightens and says:"
4370  PRINT "Ah, ";N1$;"!  Shall we begin?"
4380  GOTO 4420
4390  PRINT "Catching your gaze, the wizard states:"
4400  GOTO 4420
4410  PRINT "In a friendly voice with a ring of steel, the wizard says:"
4420  PRINT "I am Mage Stone.  Just what is it you want?"
4430  PRINT " 1 - Nothing.  Just stopped in to say 'Hi!'"
4440  PRINT " 2 - To buy a weapon enchantment"
4450  PRINT " 3 - To buy some Rings of Power"
4460  PRINT " 4 - To buy a magic potion";LIN(1)
4470  INPUT W4
4480  IF W4 <= 4 AND W4 >= 1 THEN 4500
4490  GOTO 4430
4500  GOTO W4 OF 4510,4540,4630,4760
4510  PRINT LIN(3);SPA(30);"Hi!";LIN(3)
4520  C9=2
4530  GOTO 9900
4540  PRINT "My price is";L;"in rank for one weapon enchantment."
4545  IF R1<L+1 THEN 4410
4550  PRINT "Do you accept my price";
4560  INPUT O$
4570  O$=UPS$(O$)
4580  IF O$[1,1]#"Y" THEN 9900
4590  W[RND(5)*10+1]=(RND(5)*(L+1)) MAX ((L/2)+.84)
4600  R1=R1-L
4610  PRINT "FLASH!  You now have your enchantment!"
4620  GOTO 9900
4630  PRINT "I see that you brought";G;"gold with you."
4635  PRINT "My price is 100 gold per ring.  Only 10 to a customer."
4640  PRINT "How many do you want";
4650  INPUT P9
4660  IF P9 >= 0 AND P9 <= 10 THEN 4700
4670  PRINT LIN(1)"Smart Alec!"'7;LIN(1)
4680  R1=(R1-R1/3) MAX 1
4685  C9=0
4690  GOTO 9900
4700  IF G<P9*100 THEN 4670
4705  G=G-P9*100
4710  IF P9>0 THEN 3020
4715  IF RND(5)>.3 THEN 9900
4720  PRINT "Oh please!  Have at least one!"
4730  G=(G-100) MAX 0
4740  P9=1
4750  GOTO 3020
4760  PRINT "My price is 200 gold for one potion."
4770  PRINT "Do you want a potion";
4780  INPUT O$
4790  O$=UPS$(O$)
4800  IF O$[1,1]#"Y" THEN 9900
4810  IF G<200 THEN 4670
4820  G=G-200
4830  GOTO 3300
5000  PRINT N1$;" walks ino the room and sees what appears to be"
5010  PRINT "a tall, dark figure in a wizard's cloak";
5020  GOTO INT(RND(5)*6+1) OF 5060,5090,5130,5160,5190
5030  PRINT " flipping a"
5040  PRINT "two-headed gold piece.  He smiles and floats toward you."
5050  GOTO 5210
5060  PRINT " standing by"
5070  PRINT "a roulette wheel.  He gives it an idle spin as you approach."
5080  GOTO 5210
5090  PRINT " practicing"
5100  PRINT "fire-writing on the far wall.  The burning runes fade as he"
5110  PRINT "turns towards you, but his eyes still blaze with inner fire."
5120  GOTO 5210
5130  PRINT " shuffling"
5140  PRINT "six decks of cards simultaneously.  He lets the cards hover."
5150  GOTO 5210
5160  PRINT ".  He is"
5170  PRINT "sitting 30 feet in the air, reading a racing form."
5180  GOTO 5210
5190  PRINT " reading from"
5200  PRINT "an ancient book of enchantments, written in human blood."
5210  GOTO INT(RND(5)*6+1) OF 5240,5270,5300,5320,5350
5220  PRINT "The wizard addresses you with a voice of amusement:"
5230  GOTO 5370
5240  PRINT "Glancing at the card that has appeared in his hand, the"
5250  PRINT "wizard says: Well ";N1$;", how do you do?"
5260  GOTO 5370
5270  PRINT "The wizard rubs his ring, and ' ";N1$;" '"
5280  PRINT "forms in smoke over your head.  He nods knowingly."
5290  GOTO 5370
5300  PRINT "Ah!  ";N1$;", you are finally here.  Good!"
5310  GOTO 5370
5320  PRINT "Placing his hands to his temples, the wizard states: You"
5330  PRINT "are ";N1$;".  And you crave adventure."
5340  GOTO 5370
5350  PRINT "The wizard cocks his head to one side, as if listening,"
5360  PRINT "and then says: Greetings ";N1$
5370  PRINT "I am Mage Lauterbach.  Are you interested in a little"
5380  PRINT "game of chance";
5390  INPUT O$
5400  O$=UPS$(O$)
5410  IF O$[1,1]#"Y" THEN 5440
5420  IF RND(5)<.8 THEN 9900
5430  PRINT "Oh please!  I insist!"
5440  PRINT "Which would you like to wager?"
5450  PRINT " 1 - Rank"
5460  PRINT " 2 - Strength"
5470  PRINT " 3 - Constitution"
5480  PRINT " 4 - Gold";LIN(1)
5490  INPUT W4
5500  IF W4 >= 1 AND W4 <= 4 THEN 5540
5510  PRINT LIN(1);"Wise guy!"'7'7
5520  P9=R1/3
5530  GOTO 5600
5540  PRINT "The rules are double or nothing.  Minimum bet:";
5550  GOTO W4 OF 5560,5730,5830,5940
5560  PRINT R1/20
5570  PRINT "How much Rank do you wager";
5580  INPUT P9
5590  IF P9<R1/20 OR P9>R1 THEN 5510
5595  CONVERT P9 TO G1$
5600  PRINT "You bet ";G1$;" in Rank.  ";
5605  SYSTEM W4,"PAU-3"
5610  GOTO INT(RND(5)*2+1) OF 5620,5670
5620  R1=R1+P9
5625  CONVERT R1 TO G1$
5630  PRINT "And WON!"
5640  PRINT "You are now rank ";G1$
5650  PRINT "Mage Lauterbach says:  Good-bye!  Come again."
5660  GOTO 9900
5670  R1=(R1-P9) MAX 1
5675  CONVERT R1 TO G1$
5680  PRINT "And lost."
5690  PRINT "Your Rank is now ";G1$
5700  IF RND(5)>.8 THEN 5650
5710  PRINT "Mage Lauterbach says:  Would you like to try again";
5720  GOTO 5390
5730  PRINT INT(S2/20)
5740  PRINT "What is your wager";
5750  INPUT P9
5760  IF P9<INT(S2/20) OR P9>S2 THEN 5510
5770  SYSTEM W4,"PAU-3"
5780  GOTO INT(RND(5)*2+1) OF 5785,5805
5785  S2=S2+P9
5787  CONVERT S2 TO G1$
5790  PRINT "You WIN!  Your strength is ";G1$;" blows."
5800  GOTO 5650
5805  S2=S2-P9
5807  CONVERT S2 TO G1$
5810  PRINT "You lose.  Your strength is ";G1$;" blows."
5820  GOTO 5700
5830  PRINT " .5"
5840  PRINT "How much constitution do you wager";
5850  INPUT P9
5852  IF P9 <= 5 OR P9 <= C9 THEN 5860
5854  PRINT "Thought you were smart didn't you!!!  HA!    HA!"
5856  C9=C9-P9
5858  GOTO 9900
5860  IF P9<.5 THEN 5510
5870  SYSTEM W4,"PAU-3"
5880  GOTO INT(RND(5)*2+1) OF 5890,5915
5890  C9=C9+P9
5895  CONVERT C9 to G1$
5900  PRINT "You WIN!  Your constitution is now ";G1$
5910  GOTO 5650
5915  C9=C9-P9
5917  CONVERT C9 to G1$
5920  PRINT "You lose.  Your constitution is now ";G1$
5930  GOTO 5700
5940  PRINT INT(G/10)
5950  PRINT "What is your wager";
5960  INPUT P9
5970  IF P9<INT(G/10) THEN 5510
5980  SYSTEM W4,"PAU-3"
5990  GOTO INT(RND(5)*2+1) OF 5995,6015
5995  G=G+P9
5996  CONVERT G TO G1$
6000  PRINT "You WIN!  You now have ";G1$;" gold."
6010  GOTO 5650
6015  G=G-P9
6017  CONVERT G TO G1$
6020  PRINT "You lose.  You now have ";G1$;" gold."
6030  GOTO 5700
9900  CHAIN "DND.A100",2170
9998  CHAIN "DND.A100",4100
9999  END
