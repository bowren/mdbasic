# MDBASIC
MDBASIC is an extension to the Commodore 64 BASIC V2.<br>
MDBASIC version 2022.06.10<br>
<br>
Download the MS Word document mdbasic.odt for details about each command.<br>
<br>
Download the VICE emulator for the Commodore 64:<br>
http://vice-emu.sourceforge.net/<br>
<br>
Download Turbo Assembler to compile source:<br>
https://style64.org/file/TMPx_v1.1.0-STYLE.zip<br>
<br>
<br>
See the shell script "compile.sh" for an example of how to compile using Turbo Assembler and execute with Vice.
<br>
<br>
<u><b>Example BASIC lines of code:</b></u><br>
<br>
<i>Immediate Mode:</i><br>
<div style="font-family:'Courier New'">
RUN "MYPRG",8  :REM LOAD AND RUN MYPRG FROM DISK<br>
RENUM 10,10    :REM RENUMBER PRG START AT 10 INC BY 10<br>
DISK"S0:MYPRG" :REM ERASE FILE MYPRG - ALL DOS CMD SUPPORTED<br>
FILES          :REM LIST ALL FILES<br>
FILES"M*"      :REM LIST ALL FILES STARTING WITH M<br>
AUTO 0,10      :REM AUTO LINE NUMBERING START 0 INC 10<br>
TRACE          :RUN PROGRAM WITH TRACE ENABLED<br>
</div>
<br>
<i>Program Mode:</i><br>
<br>
<div style="font-family:'Courier New'">
0 SCREEN CLR              :REM CLEAR TEXT SCREEN<br>
1 BITMAP CLR              :REM CLEAR BITMAP<br>
5 ON ERROR GOTO 100       :REM JUST LIKE GWBASIC<br>
6 REM ON ERROR RESUME NEXT<br>
10 BITMAP 1, 0            :REM SHOW BITMAP IN MULTICOLOR MODE WITH BLACK BKGD<br>
15 MAPCOL 2, 1, 6         :REM SET MULTICOLOR REG 1,2,3<br>
20 PLOT 16,10,1,1         :REM PLOT A POINT TO START DRAW LOCATION<br>
25 DRAW "R60,D55,L60,U55" :REM DRAW A RECT<br>
30 CIRCLE 48,37,23,18,1,1 :REM DRAW A CIRCLE<br>
35 PAINT48,27,1,1         :REM PAINT INSIDE CIRCLE<br>
40 PLAY "AA#BCC#DD#FF#"   :REM PLAY NOTES IN BKGRND<br>
50 X1 = ROUND(0.125, 2)   :REM X1 = 0.13<br>
51 X2 = ROUND(121, -3)    :REM X2 = 100<br>
55 PRINT PTR(X1)          :REM DISPLAY MEM LOC OF VAR X1<br>
60 LOCATE 10,5: PRINT"*"; :REM POSITION CRSR FOR PRINT<br>
61 C% = CSR(0)            :REM LOGICAL COLUMN OF CURSOR<br>
62 REM CSR(N) WHERE N IS 0 TO 7 AS FOLLOWS:<br>
63 REM 0=LOGICAL COLUMN, 1=PHYSICAL LINE, 2=CURSOR BLINK ON/OFF<br>
64 REM 3=MAX COLUMNS, 4=CHAR UNDER CURSOR<br>
65 REM 5=PHYSICAL COLUMN, 6=COLOR UNDER CURSOR, 7=ADDRESS OF CURSOR LINE<br>
70 PRINT INSTR("MARK D. BOWREN",".") :REM PRINT INDEX OF PERIOD IN STR<br>
95 RUN "PROG2",8          :REM LOAD AND RUN ANOTHER PROGRAM<br>
99 END<br>
100 PRINT "ERROR #";ERR(0);" LINE #";ERR(1)<br>
105 IF ERROR(0) = 20 THEN RESUME NEXT :REM DIV BY 0, SKIP ERRORED STMT<br>
110 IF ERROR(0) = 29 THEN RESUME 65   :REM LOAD ERR, CONT AT LINE 65<br>
115 IF ERROR(0) = 15 THEN RESUME      :REM OVERFLOW, TRY STMT AGAIN<br>
120 ERROR 11                          :REM RAISE SYNTAX ERROR
</div>
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
