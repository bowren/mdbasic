# MDBASIC
MDBASIC is an extension to the Commodore 64 BASIC V2.<br>
MDBASIC version 24.10.30<br>
<br>
Download the documentation file <b>mdbasic.pdf</b> for details.<br>
<br>
Download Options:<br>
<ol>
<li>mdbasic.d64 - A disk image that contains mdbasic.prg and a few testing/example programs & games</li>
<li>mdbasic.prg - The compiled executable program</li>
<li>mdbasic.asm - The source code</li>
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
<b>LOAD"MDBASIC.PRG",8,1</b>
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
* SAVE custom address range using existing SAVE command
* Binary, Hexadecimal and Octal contant values in expressions and VAL strings
* 8 assignable function keys each with up to 15 characters
* Exit quote and insert mode using key combination CTRL-Commodore
* Freeze LISTing by holding down the shift key
</pre>
<u><b>Various Examples of statements (not a complete list; commands have many optional parameters):</b></u><br>
<br>
<i>Immediate Mode:</i><br>
<pre style="font-family:'Courier New'">
SAVE49152,53247,"HIRAM",8 :' BINARY SAVE OF 4K HIRAM
RUN "MYPRG",8             :' LOAD AND RUN MYPRG FROM DISK
RENUM 100,10              :' RENUMBER PRG START AT 100 INC BY 10
FILES                     :' LIST ALL FILES ON DEVICE 8
FILES"M*",9               :' LIST FILES STARTING WITH "M" ON DEVICE 9
KEY LIST                  :' DISPLAY FUNCTION KEY ASSIGNMENTS
KEY 2,"RUN"+CHR$(13)      :' ASSIGN FUNCTION KEY 2
DUMP LIST                 :' PRINT BASIC PROGRAM ON PRINTER
AUTO 10                   :' AUTO LINE NUMBERING INC BY 10
DELETE 150-170            :' DELETE PROGRAM LINES 150 TO 170 INCLUSIVELY
TRACE                     :' RUN PROGRAM WITH TRACE ENABLED
</pre>
<i>Program Mode:</i><br>
<pre style="font-family:'Courier New'">
0 SCREEN CLR              :' CLEAR TEXT SCREEN
1 BITMAP CLR              :' CLEAR BITMAP
2 VOICE CLR               :' CLEAR SID REGISTERS
5 ON ERROR GOTO 100       :' WHEN ERROR OCCURS GOTO LINE 100
7 DISK"S0:MYPRG"          :' ERASE FILE MYPRG
8 POKE 1024 TO 2023,1     :' POKE ADDRESS RANGE WITH VALUE 1
9 DUMP"HELLO THERE"       :' PRINT EXPRESSION TO PRINTER
10 SPRITE 0,1             :' MAKE SPRITE 0 VISIBLE
11 SPRITE 1,1,6,0,13,1,3  :' SPRITE 1 VISIBLE, BLUE, MC OFF, PTR 13, ABOVE FOREGND, FULL EXPAND
12 MOVE 0,50,24           :' PUT SPRITE 0 AT COORD 50,24
13 MOVE1 TO 159,99,50     :' SLIDE SPRITE 1 TO CENTER SCREEN AT SPEED 50
14 MOVE0,0,0TO511,255     :' MOVE SPRITE 0 FROM TOP LEFT TO BOTTOM RIGHT, SLOWEST SPEED
15 PLAYSPRITE0,92,99,5    :' ANIMATE SPRITE 0 WITH PTRS 92 TO 99 WITH 5 JIFFIES BETWEEN FRAMES
19 X = $C000: B=%10101010 :' HEX $, BINARY % AND OCTAL @ IN EXPRESSIONS
20 BITMAP 0,15            :' SHOW BITMAP IN HIRES MODE WITH LIGHT GRAY BKGD
21 BITMAP 1, 0            :' SHOW BITMAP IN MULTICOLOR MODE WITH BLACK BKGD
22 MAPCOL 2, 1, 6         :' SELECT COLORS FOR BITMAP PLOTING
23 PLOT 16,10,1,1         :' PLOT A POINT
25 DRAW "R60,D55,L60,U55" :' DRAW A 60x55 RECTANGLE RELATIVE TO LAST PLOTTED POINT
30 CIRCLE 48,37,23,18     :' DRAW A CIRCLE CTR 48,37 X-SIZE 23, Y-SIZE 18
34 PAINT 48,27,1,1        :' PAINT INSIDE CIRCLE
35 TEXT 0,0,"MARK"        :' TEXT ON BITMAP W/DEFAULT CHRSET & SIZING
36 DUMP SCREEN            :' PRINT TEXT SCREEN TO PRINTER
38 DUMP BITMAP            :' PRINT BITMAP TO PRINTER
39 TEXT                   :' RESTORE NORMAL TEXT MODE
40 VOL 15                 :' SET VOLUME TO MAX
41 PLAY "AA#BB-CC#DD#F@"  :' PLAY NOTES IN BACKGROUND REPEATEDLY
42 PLAY OFF               :' TURN OFF BACKGROUND PLAY
43 PLAY "!ABC>ABC>ABC"    :' PLAY NOTES IN FOREGROUND GOING UP IN OCTAVE
44 ENVELOPE 1,0,0,15,0    :' SET VOICE 1 ATTACK/DECAY/SUSTAIN/RELEASE
45 FILTER 500, 15, 1      :' LOW PASS FILTER, CUTOFF=500Hz
46 VOICE 1,1000           :' SET VOICE 1 PITCH TO 1000HZ
47 WAVE 1,1,1             :' START ATTACK/DECAY/SUSTAIN CYCLE FOR VOICE 1 WITH TRIANGE WAVEFORM
48 WAIT 60                :' PAUSE FOR 60 JIFFIES (1 SEC)
49 WAVE 1,1,0             :' START RELEASE CYCLE FOR VOICE 1, TRIANGLE WAVEFORM
50 X1 = ROUND(0.125, 2)   :' X1 = 0.13
51 X2 = ROUND(121, -3)    :' X2 = 100
52 IF A THEN 100 ELSE 200 :' ELSE TO HANDLE FALSE CONDITION
55 PRINT PTR(X1)          :' DISPLAY MEM LOC OF VAR X1
56 J = JOY(1)             :' VALUE OF JOYSTICK 1 OF 2
57 P = POT(1)             :' VALUE OF PADDLE 1 OF 4
58 N = PEN(0);PEN(1)      :' DISPLAY THE LIGHT PEN X & Y COORDINATES
59 SWAP J,P               :' SWAP VALUES IN VARS J,P
60 CURSOR 10,5: PRINT"*"; :' POSITION CURSOR FOR PRINT
61 C% = INF(0)            :' PHYSICAL COLUMN OF CURSOR
62 TIME$="00:00:00"       :' SET REAL-TIME CLOCK TO 12AM
63 PRINT TIME$, TIME      :' DISPLAY TIME STRING AND NUM SECONDS SINCE MIDNIGHT
70 PRINT INSTR("MARK D. BOWREN",".") :' PRINT INDEX OF PERIOD IN STR
75 PRINT INSTR(2,"TEST THIS INSTR","IS") :' INDEX OF "IS" START AT IDX 2
80 KEY CLR                :' CLEAR KEYBOARD BUFFER
81 KEY WAIT S$            :' WAIT FOR KEY IN BUFFER THEN GET AS STRING
82 KEY WAIT I%            ;' WAIT FOR KEY IN BUFFER THEN GET AS INT (ASCII)
83 KEY WAIT F             ; 'WAIT FOR KEY IN BUFFER THEN GET AS FLOAT (ASCII)
84 KEY"OK"                :' PUT KEYS IN KEYBOARD BUFFER
86 LINE INPUT A$          :' TAKE INPUT FROM KEYBOARD TILL ENTER KEY PRESSED
87 LINE INPUT#1,A$        :' READ LINE FROM FILE 1 TERMINATED BY CR
88 SCROLL 0,0 TO 39,24    :' SCROLL WHOLE SCREEN UP 1 CHAR, NO WRAPPING (DEFAULTS)
90 DESIGN NEW             :' COPIES CHARACTER SET FOR DESIGN MODE
91 DESIGN 1,0, 0,0,0,8,8,0,0,0 :' CHANGE "A" TO LOOK LIKE A CENTERED DOT
92 SCREEN 1               :' PAGES 1-4 CAN USE THE REDEFINED CHARS
93 PRINT"A":WAIT 120      :' DISPLAY CHAR THEN WAIT 200 JIFFIES
94 SCREEN 0               :' PAGE 0 IS NORMAL C64 TEXT MODE
96 SYS $FFD2,ASC("A")     :' CALL ML SUBROUTINE WITH OPTIONAL A,X,Y,P REGISTER VALUES
99 RUN "PROG2",8          :' LOAD AND RUN ANOTHER PROGRAM
100 PRINT "ERROR #";ERR;" LINE #";ERRL
105 IF ERR = 20 THEN RESUME NEXT :' DIV BY 0, SKIP ERRORED STMT
110 IF ERR = 29 THEN RESUME 65   :' LOAD ERR, CONT AT LINE 65
115 IF ERR = 15 THEN RESUME      :' OVERFLOW, TRY STMT AGAIN
120 ERR 11                       :' INVOKE SYNTAX ERROR
</pre>
<br>
<i>Other Features:</i><br>
MDBASIC supports numeric constants of base 2 (binary %), 8 (octal @) or 16 (hexadecimal $).<br>
The functions VALB, VALO and VALH (respectively) convert strings of these number bases.
The NOT expression short-hand is the exclamation point. REM (remark) short-hand is the 
apostrophe. See examples below:<br>
<pre style="font-family:'Courier New'">
B1 = %00001111     :'DECIMAL VALUE OF 15 FROM BINARY CONSTANT %00001111
B2 = VALB("1111")  :'DECIMAL VALUE OF 15 FROM BINARY STRING "1111"
SYS $C000,0        :'CALL TO ADDRESS 49152 WITH ACCUMULATOR LOADED WITH ZERO
H = VALH("C000")   :'DECIMAL VALUE OF 49152 FROM HEX STRING "C000"
O = @20            :'DECIMAL VALUE 16 FROM OCTAL CONSTANT @20
O = VALO("10")     :'DECIMAL VALUE 8 FROM OCTAL STRING "10"
X = !X             :'SAME AS X = NOT X
</pre>
<br>
<i>LOAD/SAVE Text Screen, Charset or Bitmap</i><br>
MDBASIC has a predefined binary format for storing the text screen, redefined character set 
and bitmap graphics. For text and bitmap screens all color settings are included in the 
saved file. On load of a text or bitmap screen the change will automatically be displayed. 
After loading a character set the new font is shown by using the DESIGN ON command which then 
should be followed by SCREEN CLR since it is a different page that may not have been initialized. 
See examples below:<br>
<pre style="font-family:'Courier New'">
SAVE"SCREEN",8,16   :'SAVES ALL TEXT WITH COLOR FROM CURRENT TEXT SCREEN
SAVE"CHRSET",8,17   :'SAVES THE CURRENT DESIGN OF CHARACTERS ASSUMING DESIGN IS APPLIED
SAVE"BITMAP",8,18   :'SAVES CURRENT BITMAP IMAGE WITH COLOR MODE

LOAD"SCREEN",8,16   :'LOADS SAVED SCREEN TEXT AND COLOR INTO CURRENT SCREEN
LOAD"CHRSET",8,17   :'LOADS CUSTOM DEFINED CHARSET, NEEDS DESIGN ON TO SEE
LOAD"BITMAP",8,18   :'LOADS BITMAP IMAGE AND DISPLAYS IT
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
