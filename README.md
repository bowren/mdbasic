# MDBASIC
MDBASIC is an extension to the Commodore 64 BASIC V2.<br>
MDBASIC version 2022.12.27<br>
<br>
Download the MS Word document mdbasic.pdf for details about each command.<br>
<br>
Download Options:<br>
<ol>
<li>mdbasic.d64 - Contains mdbasic.prg and a few testing/example programs & games</li>
<li>mdbasic.prg - Only the compiled prg to be copied to your media format</li>
<li>mdbasic.asm - Compile it yourself (see below)</li>
</ol>
Download Turbo Assembler to compile source:<br>
https://style64.org/file/TMPx_v1.1.0-STYLE.zip<br>
<br>
See the shell script "compile.sh" for an example of how to compile using Turbo Assembler and execute on the VICE emulator.
<br>
Download the VICE emulator for the Commodore 64:<br>
http://vice-emu.sourceforge.net/<br>
<br>
<u><b>Load & Run MDBASIC:</b></u><br>
<pre style="font-family:'Courier New'">
READY.
<b>LOAD"MDBASIC",8,1</b>
&nbsp;
SEARCHING FOR MDBASIC.PRG
LOADING
READY.
<b>SYS64738</b>
</pre>
<u><b>General Features:</b></u><br>
<pre style="font-family:'Courier New'">
* Displays memory address range after LOADing programs
* LOAD/SAVE directly to/from the text screen, bitmap or character definition memory
* Binary, Hexadecimal and Octal values in expressions
* 8 assignable function keys each with up to 31 characters
* Freeze LISTing by holding down the shift key
</pre>
<u><b>Various Examples of statements (not complete list; commands have many optional parameters):</b></u><br>
<br>
<i>Immediate Mode:</i><br>
<pre style="font-family:'Courier New'">
RUN "MYPRG",8             :REM LOAD AND RUN MYPRG FROM DISK
RENUM 10,10               :REM RENUMBER PRG START AT 10 INC BY 10
FILES                     :REM LIST ALL FILES TO SCREEN
FILES"M*"                 :REM LIST ALL FILES STARTING WITH M
KEY LIST                  :REM DISPLAY FUNCTION KEY ASSIGNMENTS
KEY 2,"RUN"+CHR$(13)      :REM ASSIGN FUNCTION KEY 2
DUMP LIST                 :REM PRINT BASIC PROGRAM ON PRINTER
AUTO 10                   :REM AUTO LINE NUMBERING INCREMENT BY 10
AUTO OFF                  :REM TURN AUTO LINE NUMBERING OFF
DELETE 150-170            :REM DELETE PROGRAM LINES 150 TO 170 INCLUSIVELY
TRACE                     :REM RUN PROGRAM WITH TRACE ENABLED
</pre>
<i>Program Mode:</i><br>
<pre style="font-family:'Courier New'">
0 SCREEN CLR              :REM CLEAR TEXT SCREEN
1 BITMAP CLR              :REM CLEAR BITMAP
2 VOICE CLR               :REM CLEAR SID REGISTERS
5 ON ERROR GOTO 100       :REM WHEN ERROR OCCURS GOTO LINE 100
6 ON ERROR RESUME NEXT    :REM WHEN ERROR OCCURS SKIP TO NEXT STATEMENT
7 DISK"S0:MYPRG"          :REM ERASE FILE MYPRG - ALL DOS CMD SUPPORTED
8 POKE 1024 TO 2023,1     :REM POKE ADDRESS RANGE WITH VALUE 1
9 DUMP"HELLO THERE"       :REM PRINT EXPRESSION TO PRINTER
10 SPRITE 0,1             :REM MAKE SPRITE 0 VISIBLE
11 SPRITE 1,1,6,1,13      :REM SPRITE 1 VISIBLE, BLUE, ABOVE FOREGND, DTPTR 13
12 MOVE 0,50,24           :REM LOCATE SPRITE 0 AT COORD 50,24
13 MOVE1 TO 159,99,50     :REM SLIDE SPRITE 1 TO CENTER SCREEN AT SPEED 50
14 MOVE0,0,0TO511,255     :REM MOVE SPRITE 0 FROM TOP LEFT TO BOTTOM RIGHT, FAST 
15 X = $C000: B=%10101010 :REM HEX $, BINARY % AND OCTAL @ IN EXPRESSIONS
20 BITMAP 1, 0            :REM SHOW BITMAP IN MULTICOLOR MODE WITH BLACK BKGD
21 MAPCOL 2, 1, 6         :REM SET MULTICOLOR REG 1,2,3
22 PLOT 16,10,1,1         :REM PLOT A POINT TO START DRAW LOCATION
25 DRAW "R60,D55,L60,U55" :REM DRAW A RECT
30 CIRCLE 48,37,23,18     :REM DRAW A CIRCLE CTR 48,37 SIZEX 23, SIZEY 18
34 PAINT 48,27,1,1        :REM PAINT INSIDE CIRCLE
35 TEXT 0,0,"MARK"        :REM TEXT ON BITMAP W/DEFAULT CHRSET & SIZING
36 DUMP SCREEN            :REM PRINT TEXT SCREEN TO PRINTER
38 DUMP BITMAP            :REM PRINT BITMAP TO PRINTER
39 TEXT                   :REM RETURN TO TEXT MODE
40 PLAY "AA#BCC#DD#FF#"   :REM PLAY NOTES IN BKGRND
42 VOICE 1,1000           :REM SET VOICE 1 PITCH TO 1000HZ
43 WAIT 60                :REM PAUSE FOR 60 JIFFIES (1 SEC)
45 ENVELOPE 1,0,0,15,0    :REM SET VOICE 1 ATTACK/DECAY/SUSTAIN/RELEASE
48 FILTER 500, 15, 1      :REM LOW PASS FILTER, CUTOFF=500Hz
50 X1 = ROUND(0.125, 2)   :REM X1 = 0.13
51 X2 = ROUND(121, -3)    :REM X2 = 100
55 PRINT PTR(X1)          :REM DISPLAY MEM LOC OF VAR X1
56 J = JOY(1)             :REM VALUE OF JOYSTICK 1
57 P = POT(1)             :REM VALUE OF PADDLE 1
58 SWAP J,P               :REM SWAP VALUES IN VARS J,P
60 LOCATE 10,5: PRINT"*"; :REM POSITION CRSR FOR PRINT
61 C% = CSR(0)            :REM LOGICAL COLUMN OF CURSOR
62 REM CSR(N) WHERE N IS 0 TO 7 AS FOLLOWS:
63 REM 0=LOGICAL COLUMN, 1=PHYSICAL LINE, 2=CURSOR BLINK ON/OFF
64 REM 3=MAX COLUMNS, 4=CHAR UNDER CURSOR
65 REM 5=PHYSICAL COLUMN, 6=COLOR UNDER CURSOR, 7=ADDRESS OF CURSOR LINE
70 PRINT INSTR("MARK D. BOWREN",".") :REM PRINT INDEX OF PERIOD IN STR
75 PRINT INSTR(2,"TEST THIS INSTR","IS") :REM INDEX OF "IS" START AT IDX 2 
80 LINE INPUT A$          :REM TAKE INPUT FROM KEYBOARD TILL ENTER KEY PRESSED
81 LINE INPUT#1,A$        :REM READ LINE FROM FILE 1 TERMINATED BY CR
85 SCROLL 0,0 TO 39,24    :REM SCROLL WHOLE SCREEN UP 1 CHAR, NO WRAPPING (DEFAULTS)
90 DESIGN NEW             :REM COPIES CHARACTER SET FOR DESIGN MODE
91 DESIGN ON              :REM SWITCH TO REDEFINED CHAR MODE ON
92 DESIGN 1,0, 0,0,0,8,8,0,0,0 :REM CHR A NOW LOOKS LIKE A DOT
93 DESIGN OFF             :REM RETURN TO STD TEXT MODE
95 RUN "PROG2",8          :REM LOAD AND RUN ANOTHER PROGRAM
96 SYS $FFD2,ASC("A")     :REM CALL ML SUBROUTINE WITH OPTIONAL A,X,Y,P REGISTER VALUES
99 END
100 PRINT "ERROR #";ERROR(0);" LINE #";ERROR(1)
105 IF ERROR(0) = 20 THEN RESUME NEXT :REM DIV BY 0, SKIP ERRORED STMT
110 IF ERROR(0) = 29 THEN RESUME 65   :REM LOAD ERR, CONT AT LINE 65
115 IF ERROR(0) = 15 THEN RESUME      :REM OVERFLOW, TRY STMT AGAIN
120 ERROR 11                          :REM RAISE SYNTAX ERROR
</pre>
<br>
<br>
<div>Legal:</div>
<div>
This program is free software; you can redistribute it and/or
modify it under the terms of the Apache Public License 2.0 as
published by the Apache Software Foundation; either version 2
of the License, or (at your option) any later version.<br>
<br>
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Apache General Public License 2.0 for more details.
</div>
