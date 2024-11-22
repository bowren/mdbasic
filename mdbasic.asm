; ***MDBASIC***
; by Mark D Bowren
; (c)1985-2024 Bowren Consulting, Inc. (www.bowren.com)
;
;zero-page registers
R6510  = $01 ;LORAM/HIRAM/CHAREN RAM/ROM selection and cassette control register
VERCK  = $0a ;flag for LOAD or VERIFY
COUNT  = $0b ;general counter
VALTYP = $0d ;Type of Data (255=String, 0=Numeric)
INTFLG = $0e ;Type of Numeric Data (128=Integer, 0=Floating Point)
GARBFL = $0f ;Flag for LIST, Garbage Collection, and Program Tokenization
CHANNL = $13 ;Current I/O Channel (CMD Logical File) Number
VARTAB = $2d ;Pointer to the Start of the BASIC Variable Storage Area
TXTTAB = $2b ;Pointer to the Start of BASIC Program Text
ARYTAB = $2f ;Pointer to the Start of the BASIC Array Storage Area
STREND = $31 ;Pointer to Start of Free BASIC RAM
CURLIN = $39 ;Current BASIC Line Number (lobyte)
OLDLIN = $3b ;Previous BASIC Line Number (lobyte)
OLDTXT = $3d ;Pointer to the Address of the Current BASIC Statement
DATPTR = $41 ;Pointer to the Address of the Current DATA Item
TXTPTR = $7a ;Pointer to the Address of the Current BASIC text char
STATUS = $90 ;Kernal I/O Status Word (ST)
XSAV   = $97 ;Temporary .X Register Save Area
LDTND  = $98 ;Number of Open I/O Files/Index to the End of File Tables
MSGFLG = $9d ;Msg Ctrl: 0=none, $40=Kernal, $80=CTRL, $C0=Kernal & CTRL
EAL    = $ae ;vector to I/O end address of last loaded file
FNLEN  = $b7 ;Length of Current Filename
LA     = $b8 ;Current Logical File Number
SA     = $b9 ;Current Secondary Address
FA     = $ba ;Current Device Number
             ;0=keybrd,1=tape,2=rs232,3=screen,4-5=printer,8=11=disk
STAL   = $c1 ;vector to I/O start address
LSTX   = $c5 ;Matrix Coordinate of Last Key Pressed, 64=None Pressed
NDX    = $c6 ;number of keys in keyboard buffer
RVS    = $c7 ;Flag: Print Reverse Characters? 0=No
BLNSW  = $cc ;Cursor Blink Disabled: 0=No, 1=Yes
SFDX   = $cb ;Matrix Coordinate of Current Key Pressed
BLNCT  = $cd ;Timer: Countdown to Blink Cursor
GDBLN  = $ce ;ASCII value of char under csr (when blinking)
BLNON  = $cf ;Flag: Was Last Cursor Blink on or off?
PNTR   = $d3 ;Logical Cursor Column on Current Line
QTSW   = $d4 ;Flag: Editor in Quote Mode? 0=No
LNMX   = $d5 ;Maximum Length of Physical Screen Line (39 or 79)
TBLX   = $d6 ;Current Cursor Physical Line Number
TMPASC = $d7 ;ASCII value of last character printed to screen
INSRT  = $d8 ;Editor Current Insert Character Count
KEYTAB = $f5 ;vector to keyboard decode table

;BASIC and Kernal work registers
BAD    = $0100 ;Tape Input Error Log and string work area
BUF    = $0200 ;BASIC Line Editor Input Buffer
COLOR  = $0286 ;Current Foreground Color for Text
GDCOL  = $0287 ;Color of Character under Cursor
HIBASE = $0288 ;Top Page of Screen Memory
RPTFLAG= $028a ;which keys repeat
               ;0=cursor, insert, delete and spacebar, 64=none, 128=all
SHFLAG = $028d ;SHIFT/CTRL/Logo Keypress flags
               ;bit0 SHIFT, bit1 Commodore Logo Key, bit2 Ctrl Key
PALNTSC= $02a6 ;PAL/NTSC TV Standard Flag: 0=NTSC, 1=PAL

;Kernal Tables for File Management
LAT    = $0259 ;Table of Logical Active File Numbers
FAT    = $0263 ;Table of Device Numbers for Each Logical File
SAT    = $026d ;Table of Secondary Addresses for Each Logical File

;RS-232 Mock 6551 Registers
M51CTR = $0293 ;CTR
M51CDR = $0294 ;CDR
M51AJB = $0295 ;Non-Std Bit Timing lobyte, $0296 hibyte
RSSTAT = $0297 ;Status
BITNUM = $0298 ;Number of Bits Left to be Sent/Received
RIDBE  = $029b ;Index to End of Receive Buffer
RIDBS  = $029c ;Index to Start of Receive Buffer
RODBS  = $029d ;Index to Start of Transmit Buffer
RODBE  = $029e ;Index to End of Transmit Buffer
ENABL  = $02a1 ;IRQ Statuses, 1=Transmittinging, 2=Receiving, 16=Waiting

;SYS command mock registers
SAREG  = $030c ;Storage Area for processor registers A,X,Y,P

;Memory-Mapped I/O Registers
SP0X   = $d000 ;Sprite 0 Horizontal Position
SP0Y   = $d001 ;Sprite 0 Vertical Position
MSIGX  = $d010 ;Most Significant Bits of Sprites 0-7 Horizontal Position

SCROLY = $d011 ;Vertical Fine Scrolling and Control Register
;Bits 0-2 Fine scroll display vertically by X scan lines (0-7)
;Bit 3 Select a 24-row or 25-row text display (1=25 rows, 0=24 rows)
;Bit 4 Blank the entire screen to the same color as the background (0=blank)
;Bit 5 Enable bitmap graphics mode (1=enable)
;Bit 6 Enable extended color text mode (1=enable)
;Bit 7 High bit (Bit 8) of raster compare register at 53266 ($D012)
;
LPENX  = $d013 ;Light Pen Horizontal Position (0-160) must by multiplied by 2
SPENA  = $d015 ;Sprites 0-7 Enable Register (1=on, 0=off)

SCROLX = $d016 ;Horizontal Fine Scrolling and Multicolor Control Register
;Bits 0-2 Fine scroll display horizontally by X dot positions (0-7)
;Bit 3 Select a 38-column or 40-column text display (1=40 columns, 0=38 cols)
;Bit 4 Enable multicolor text or bitmap mode (1=on, 0=off)
;Bit 5 Video chip reset (0=normal operations, 1=video completely off)
;Bits 6-7 Unused
;

YXPAND = $d017 ;Sprites 0-7 Vertical Expansion Register (1=2x, 0=normal)

VMCSB  = $d018 ;VIC-II Chip Memory Control Register
;Bit 0 Unused
;Bits 1-3 Text char dot-data base address in VIC-II 16K addressable memory.
;   The default is %100 (4) = 4 * 1K = $1000 (4096) the address of Dot-Data.
;   Uppercase chars are the first 2K. The alternate charset which contains both
;   upper and lowercase chars are in the second 2K. To shift to the alternate
;   charset, change this nybble to %110 (6) = 6 * 1K = $1800 (6144).
;
;Bits 4-7 Video matrix base address within VIC-II 16K addressable memory
;   The default is %0001 (1) = 1 * 1K = $0400 (1024).
;   Select which 1K area of memory will have screen codes for text on screen.
;   The last 8 bytes of this 1K area are used as pointers to select the 64-byte
;   block of memory for each sprite.
;
SPBGPR = $d01b ;Sprite to Foreground Display Priority Register
SPMC   = $d01c ;Sprite Multicolor Registers
XXPAND = $d01d ;Sprite Horizontal Expansion Register
EXTCOL = $d020 ;Border Color Register
BGCOL0 = $d021 ;Bkgnd Color 0 for text, sprites and mc bitmap (default 6=blue)
BGCOL1 = $d022 ;Bkgnd Color 1 mc mode bits 6,7 bit-pair 01 (default 1=white)
               ;for screen codes  64-127
BGCOL2 = $d023 ;Bkgnd Color 2 mc mode bits 6,7 bit-pair 10 (default 2=red)
               ;for screen codes 128-191
BGCOL3 = $d024 ;Bkgnd Color 3 mc mode bits 6,7 bit-pair 11 (default 3=cyan)
               ;for screen codes 192-255
SPMC0  = $d025 ;Sprite Multicolor Register 0 (01 bit-pair)
SPMC1  = $d026 ;Sprite Multicolor Register 1 (11 bit-pair)
SP0COL = $d027 ;Sprite 0 Color Register (the default color value is 1, white)

;SID registers
FRELO1 = $d400 ;Voice 1 Frequency Control (low byte
FREHI1 = $d401 ;Voice 1 Frequency Control (high byte)
PWLO1  = $d402 ;Voice 1 Pulse Waveform Width (low byte)
PWHI1  = $d403 ;Voice 1 Pulse Waveform Width (high nybble)
VCREG1 = $d404 ;Voice 1 Control Register
ATDCY1 = $d405 ;Voice 1 Attack/Decay Register
SUREL1 = $d406 ;Voice 1 Sustain/Release Control Register
CUTLO  = $d415 ;Filter Cutoff Frequency (lo byte)
CUTHI  = $d416 ;Filter Cutoff Frequency (high byte)
RESON  = $d417 ;Filter Resonance Control Register
SIGVOL = $d418 ;Volume and Filter Select Register
POTX   = $d419 ;Read Game Paddle 1 or 3, POTX+1 ($d41a) = Paddle 2 or 4

;Complex Interface Adapter (CIA) #1 Registers ($DC00-$DC0F)
CIAPRA = $dc00 ;Data Port Register A
CIAPRB = $dc01 ;Data Port Register B
CIDDRA = $dc02 ;Data Direction Register A
CIACRA = $dc0e ;Control Register A

;Complex Interface Adapter (CIA) #2 Registers ($DD00-$DD0F)
CI2PRA = $dd00 ;Data Port Register A
;Bits 0-1 Select VIC-II 16K addressable memory bank (0-3)
;  00 Bank 3 ($C000-$FFFF) 16K RAM / Memory mapped I/O, char ROM, 4K Kernal
;  01 Bank 2 ($8000-$BFFF) 16K RAM / BASIC text, 8K BASIC interpreter ROM
;  10 Bank 1 ($4000-$7FFF) 16K RAM / BASIC text
;  11 Bank 0 ($0000-$3FFF) 16K RAM / system variables, screen RAM, BASIC text
;*See zero-page memory location $01 bits 0 & 1 for RAM/ROM switching
CI2PRB = $dd01 ;Data Port Register B
C2DDRA = $dd02 ;Data Direction Register for port A (CI2PRA)

;CIA #2 Timer A
TI2ALO = $dd04 ;Timer A (lo byte)
TI2AHI = $dd05 ;Timer A (hi byte)
;
;TOD #2 Registers
TO2TEN = $dd08 ;TOD clock tenths of a seconds in BCD format (lower nybble)
TO2SEC = $dd09 ;TOD clock seconds in BCD format
TO2MIN = $dd0a ;TOD clock minutes in BCD format
TO2HRS = $dd0b ;TOD clock hours in BCD format

CI2ICR = $dd0d ;Interrupt Control Register
CI2CRA = $dd0e ;Control Register A
CI2CRB = $dd0f ;Control Register B
;Bit7: Select TOD or Alarm Time
; 0=writing to TOD registers sets clock
; 1=writing to ROD registers sets alarm

;CBM command line functions
CHRGET = $0073 ;Get Next BASIC Text Character
CHRGOT = $0079 ;Get Current BASIC Text Character
KEYD   = $0277 ;Keyboard Buffer (Queue)

;Vectors to various subroutines
KEYLOG = $028f ;Keyboard Table Setup Routine
IERROR = $0300 ;Print BASIC Error Message Routine
IMAIN  = $0302 ;Main BASIC Program Loop
ICRNCH = $0304 ;Crunches ASCII to Tokens
IQPLOP = $0306 ;Lists BASIC Program Token as ASCII Text
IGONE  = $0308 ;Executes the Next BASIC Program Token
IEVAL  = $030a ;Evaluates a Single-Term Arithmetic Expression
CINV   = $0314 ;IRQ Interrupt Routine
CBINV  = $0316 ;BRK Instruction Interrupt
ILOAD  = $0330 ;Kernal LOAD Routine
ISAVE  = $0332 ;Kernal SAVE Routine

;MDBASIC IRQ management
MDBIRQ = $0313 ;MDBASIC IRQ Control Register
               ;bit0=play, bit1=playsprite, bit2=key, bit3=spritecolor16
TMPIRQ = $0334 ;2-byte temp storage for original IRQ vector
;misc vectors (2-bytes each)
TMPERR = $0336 ;original error handling vector
TMPERRP= $0338 ;TXTPTR of statement for ON ERR GOTO line#
TMPKEYP= $033a ;TXTPTR of statement for ON KEY GOSUB line#

;CBM BASIC subroutines
GETSTK = $a3fb ;check for space on stack
REASON = $a408 ;check for space in memory
READY  = $a474 ;print READY
LINKPRG= $a533 ;relink lines of tokenized program text
INLIN  = $a560 ;input a line to buffer from keyboard (max 88 chars)
FINDLN = $a613 ;search for line number using ptr TXTTAB
NEW    = $a642 ;perform NEW
CLEAR  = $a65e ;perform CLR
RUNC   = $a68e ;reset ptr of current text char to the beginning of prg text
LIST   = $a69c ;perform LIST
FOR    = $a742 ;perform FOR
NEWSTT = $a7ae ;setup next statement for execution
RESTORE= $a81d ;perform RESTORE
END    = $a831 ;perform END
CONT   = $a857 ;perform CONT
RUN    = $a871 ;perform RUN
GOSUB  = $a883 ;perform GOSUB
GOTO   = $a8a0 ;perform GOTO
RETURN = $a8d2 ;perform RETURN
DATA   = $a8f8 ;perform DATA
DATAN  = $a906 ;search BASIC text for the end of the current statement
REM    = $a93b ;perform REM
ONGOTO = $a94b ;perform ON
LINGET = $a96b ;convert an ASCII decimal number to a 2-byte binary line number
LET    = $a9a5 ;perform LET
PRINTN = $aa80 ;perform PRINT#
CMD    = $aa86 ;perform CMD
PRINT  = $aaa0 ;perform PRINT
STROUT = $ab1e ;print msg from ptr in Y (hi byte) and A (lo byte) registers
GET    = $ab7b ;perform GET
INPUTN = $aba5 ;perform INPUT#
INPUT  = $abbf ;perform INPUT
READ   = $ac06 ;perform READ
NEXT   = $ad1e ;perform NEXT
FRMNUM = $ad8a ;evaluate numeric expression and check data type, result in FAC1
TESTNUM= $ad8d ;test last expression was numeric, if not, type mismatch error
FRMEVL = $ad9e ;evaluate expression
PARCHK = $aef1 ;eval expr inside parentheses
CHKCLS = $aef7 ;check for and skip closing parentheses
CHKOPN = $aefa ;check for and skip opening parentheses 
CHKCOM = $aefd ;check for and skip comma
ISVAR  = $af28 ;get the value of a variable into FAC1
DIM    = $b081 ;perform DIM
PTRGET = $b08b ;search for a variable & setup if not found
GIVAYF = $b391 ;convert 16-bit signed int to floating-point (a=hibyte y=lobyte)
ERRDIR = $b3a6 ;check if prg is running in direct mode/cause error
DEF    = $b3b3 ;perform DEF
STRLIT = $b487 ;scan and setup ptrs to a str in memory ptr A lobyte, Y hibyte
GETSPA = $b4f4 ;alloc space in mem for string
FRESTR = $b6a3 ;discard a temporary string
GETBYTC= $b79b ;input a parameter whose value is between 0 and 255
VAL    = $b7ad ;perform VAL
GETADR = $b7f7 ;convert FAC1 to unsigned 16-bit integer in ($14,$15)
POKE   = $b824 ;perform POKE
WAIT   = $b82d ;perform WAIT
FADDH  = $b849 ;add 0.5 to FAC1
DIV10  = $bafe ;divide FAC1 by 10
MUL10  = $bae2 ;multiply FAC1 by 10
FMULT  = $ba28 ;copy mem pointed by Y(hi),A(lo) to FAC2 then FAC1=FAC1*FAC2
FDIVT  = $bb12 ;divide FAC2 by FAC1 FAC1 = (FAC2/FAC1)
MOVFM  = $bba2 ;copy a 5-byte float from memory to FAC1, A=lobyte, Y=hibyte
MOV2F  = $bbc7 ;copy a 5-byte float from FAC1 to memory, $57-$5B
MOVEF  = $bc0f ;copy FAC1 to FAC2 without rounding
ROUND  = $bc1b ;round FAC1 by adjusting the rounding byte
SIGN   = $bc2b ;put the sign of FAC1 into accumulator
ABS    = $bc58 ;perform ABS
QINT   = $bc9b ;convert FAC1 into a 4-byte (32-bit) signed integer within FAC1
INT    = $bccc ;perform INT
FIN    = $bcf3 ;convert ASCII string to a float in FAC1
FINLOG = $bd7e ;add signed integer to FAC1
INPRT  = $bdc2 ;print IN followed by a line number
LINPRT = $bdcd ;print 2-byte number stored in A (hibyte), X (lobyte)
FOUT   = $bddd ;convert contents of FAC1 to ASCII String

;CBM BASIC routines to raise a specific error
NOGOSUB= $a8e0 ;return without gosub
UNDEFST= $a8e3 ;undef'd statement error
TMERR  = $ad99 ;type mismatch error
SNERR  = $af08 ;syntax error
BSERR  = $b245 ;bad subscript error
FCERR  = $b248 ;illegal quanity error
OVERR  = $b97e ;overflow error
LODERR = $e19c ;load error

;Commodore 64 Kernal functions
SYS    = $e12a ;perform SYS
VERIFY = $e165 ;perform VERIFY
HALT   = $e386 ;halt program and return to main BASIC loop
INIT   = $e3bf ;initialize BASIC
LP2    = $e5b4 ;get a character from the keyboard buffer
CLS    = $e544 ;init screen line link table and clear the screen

;Commodore 64 Kernal API
CINT   = $ff81 ;initialize screen editor and video chip
RAMTAS = $ff87 ;initialize RAM, tape buffer, screen
RESTOR = $ff8a ;restore default I/O vectors
IOINIT = $fda3 ;initialize CIA I/O devices
SETMSG = $ff90 ;set kernal msg ctrl flag
TKSA   = $ff96 ;send secondary address after TALK
ACPTR  = $ffa5 ;input byte from serial bus
TALK   = $ffb4 ;command serial bus device to TALK
READST = $ffb7 ;read I/O status word
SETLFS = $ffba ;set logical file parameters
SETNAM = $ffbd ;set file name parameters
OPEN   = $ffc0 ;open a logical file
CLOSE  = $ffc3 ;close a logical file
CHKIN  = $ffc6 ;define an input channel
CHKOUT = $ffc9 ;define an output channel
CLRCHN = $ffcc ;restore default devices
CHRIN  = $ffcf ;input a character
CHROUT = $ffd2 ;output a char to the current output device
LOAD   = $ffd5 ;load from a device
STOP   = $ffe1 ;test the stop key
CLALL  = $ffe7 ;close all files
PLOT   = $fff0 ;Read/Set Location of the Cursor

RESLST = $a09e ;$A09E-$A19D List of Keywords

TOKEN_NEXT    = $82
TOKEN_DATA    = $83
TOKEN_INPUTN  = $84
TOKEN_INPUT   = $85
TOKEN_DIM     = $86
TOKEN_READ    = $87
TOKEN_GOTO    = $89
TOKEN_RUN     = $8a
TOKEN_IF      = $8b
TOKEN_RESTORE = $8c
TOKEN_GOSUB   = $8d
TOKEN_RETURN  = $8e
TOKEN_ON      = $91
TOKEN_WAIT    = $92
TOKEN_OPEN    = $9f
TOKEN_CLOSE   = $a0
TOKEN_PRINT   = $99
TOKEN_LIST    = $9b
TOKEN_CLR     = $9c
TOKEN_SYS     = $9e
TOKEN_GET     = $a1
TOKEN_NEW     = $a2
TOKEN_TO      = $a4
TOKEN_THEN    = $a7
TOKEN_AND     = $af
TOKEN_EQUAL   = $b2
TOKEN_EXP     = $bd
TOKEN_VAL     = $c5

FIRST_CMD_TOK = $cb  ;first MDBASIC token
TOKEN_OFF     = $cb  ;OFF keyword token
TOKEN_ELSE    = $cc
TOKEN_VARS    = $cf
TOKEN_FILL    = $d1
TOKEN_DELETE  = $d6
TOKEN_FILES   = $d7
TOKEN_COLOR   = $d8
TOKEN_SPRITE  = $da
TOKEN_BITMAP  = $df
TOKEN_TEXT    = $e6
TOKEN_SCREEN  = $e7
TOKEN_RESUME  = $e8
TOKEN_VOICE   = $eb
TOKEN_TRACE   = $f2
TOKEN_TIME    = $f4
FIRST_FUN_TOK = $f3  ;first MDBASIC function
TOKEN_KEY     = $f6
TOKEN_ERR     = $f7
TOKEN_PI      = $ff  ;PI symbol token

*=$8000 ;"MDBASIC RAM Memory Block"

;cartridge identifier
.word resvec,runstp        ;new reset and runstop vectors
.byte $c3,$c2,$cd,$38,$30  ;necessary for cartridge indicator
;
mesge .byte 147
.text "mdbasic 24.11.21"
.byte 13,0
;
;Text for New Commands
;string values of command w/ last chr having bit 7 = on
newcmd
;keywords
.shift "off"  ;OFF keyword token $CB
.shift "else"

;commands
.shift "merge"
.shift "dump"
.shift "vars"
.shift "circle"
.shift "fill"
.shift "scroll"
.shift "swap"
.shift "cursor"
.shift "disk"
.shift "delete"
.shift "files"
.shift "color"
.shift "move"
.shift "sprite"
.shift "multi"
.shift "find"
.shift "serial"
.shift "design"
.shift "bitmap"
.shift "mapcol"
.shift "plot"
.shift "line"
.shift "paint"
.shift "draw"
.shift "renum"
.shift "text"
.shift "screen"
.shift "resume"
.shift "envelope"
.shift "wave"
.shift "voice"
.shift "pulse"
.shift "vol"
.shift "filter"
.shift "play"
.shift "auto"
.shift "old"
.shift "trace"
;functions
.shift "fix"
.shift "time"
.shift "round"
;statement & function
keystr .shift "key"
.shift "err"
;functions only
.shift "ptr"
.shift "inf"
.shift "pen"
.shift "joy"
.shift "pot"
.shift "hex$"
.shift "instr"
.byte 0 ;needed terminator
;
;Command Dispatch Table
cmdtab
;CBM BASIC
.rta END    ;$80
.rta FOR    ;$81
.rta NEXT   ;$82
.rta DATA   ;$83
.rta INPUTN ;$84 INPUT#
.rta INPUT  ;$85
.rta DIM    ;$86
.rta READ   ;$87
.rta LET    ;$88
.rta GOTO   ;$89
.rta newrun ;$8a RUN augmented
.rta if     ;$8b IF augmented
.rta restor ;$8c RESTORE augmented
.rta GOSUB  ;$8d
.rta return ;$8e RETURN augmented
.rta REM    ;$8f
.rta $a82f  ;$90 STOP
.rta on     ;$91 ON augmented
.rta wait   ;$92 WAIT augmented
.rta $e168  ;$93 LOAD
.rta bsave  ;$94 SAVE augmented
.rta VERIFY ;$95
.rta DEF    ;$96
.rta poke   ;$97 POKE augmented
.rta PRINTN ;$98 PRINT#
.rta PRINT  ;$99
.rta CONT   ;$9a
.rta LIST   ;$9b
.rta CLEAR  ;$9c CLR
.rta CMD    ;$9d
.rta sys    ;$9e SYS augmented
.rta $e1be  ;$9f OPEN
.rta close  ;$a0 CLOSE augmented
.rta GET    ;$a1
.rta new    ;$a2 NEW augmented
;Commodore 64 BASIC Keyword Tokens
;$a3  TAB(
;$a4  TO
;$a5  FN
;$a6  SPC(
;$a7  THEN
;$a8  NOT
;$a9  STEP
;Commodore 64 BASIC Operator Tokens
;$aa  +       $b86a ADD
;$ab  -       $b853 SUBTRACT
;$ac  *       $ba2b MULTIPLY
;$ad  /       $bb12 DIVIDE
;$ae  ^       $bf7b EXPONENTIATE
;$af  AND     $afe9 LOGICAL AND
;$b0  OR      $afe6 LOGICAL OR
;$b1  >       $bfb4 GREATER THAN
;$b2  =       $aed4 EQUAL TO
;$b3  <       $b016 LESS THAN
;Commodore 64 BASIC Function Tokens
;$b4  SGN     $bc39
;$b5  INT     $bccc
;$b6  ABS     $bc58
;$b7  USR     $0310
;$b8  FRE     $b37d
;$b9  POS     $b39e
;$ba  SQR     $bf71
;$bb  RND     $e097
;$bc  LOG     $b9ea
;$bd  EXP     $bfed
;$be  COS     $e264
;$bf  SIN     $e26b
;$c0  TAN     $e2b4
;$c1  ATN     $e30e
;$c2  PEEK    $b80d
;$c3  LEN     $b77c
;$c4  STR$    $b465
;$c5  VAL     $b7ad
;$c6  ASC     $b78b
;$c7  CHR$    $b6ec
;$c8  LEFT$   $b700
;$c9  RIGHT$  $b72c
;$ca  MID$    $b737
;$cb  GO      *This is a keyword used to support syntax GO TO instead of GOTO
;MDBASIC Keyword Tokens
;$cb token was GO now OFF (keyword only)
.rta REM     ;$cc ELSE token - using this cmd by itself behaves like REM
.rta merge   ;$cd
.rta dump    ;$ce
.rta vars    ;$cf
.rta circle  ;$d0
.rta fill    ;$d1
.rta scroll  ;$d2
.rta swap    ;$d3
.rta cursor  ;$d4
.rta disk    ;$d5
.rta delete  ;$d6
.rta files   ;$d7
.rta color   ;$d8
.rta move    ;$d9
.rta sprite  ;$da
.rta multi   ;$db
.rta find    ;$dc
.rta serial  ;$dd
.rta design  ;$de
.rta bitmap  ;$df
.rta mapcol  ;$e0
.rta plot    ;$e1
.rta line    ;$e2
.rta paint   ;$e3
.rta draw    ;$e4
.rta renum   ;$e5
.rta text    ;$e6
.rta screen  ;$e7
.rta resume  ;$e8
.rta adsr    ;$e9
.rta wave    ;$ea
.rta voice   ;$eb
.rta pulse   ;$ec
.rta vol     ;$ed
.rta filter  ;$ee
.rta play    ;$ef
.rta auto    ;$f0
.rta old     ;$f1
.rta trace   ;$f2
.rta SNERR   ;$f3 placeholder for fix (not a command, func only)
.rta time    ;$f4 cmd and func
.rta SNERR   ;$f5 placeholder for round (not a command, func only)
.rta key     ;$f6 cmd & func
.rta error   ;$f7 cmd & func
;
;MDBASIC Function Dispatch Table
funtab
.rta fix,fntime, fnround          ;$f3,$f4,$f5
.rta fnkey, fnerr                 ;$f6,$f7 are both a command and a function
.rta ptr, inf, pen, joy, pot, hex ;$f8,$f9,$fa,$fb,$fc,$fd
.rta instr, $ae9e                 ;$fe,$ff (PI Constant)
;
;*** error messages ***
;To invoke, load x register with error# then jmp ($0300)
;CBM BASIC ERROR MESSAGES:
; 1 TOO MANY FILES
; 2 FILE OPEN
; 3 FILE NOT OPEN
; 4 FILE NOT FOUND
; 5 DEVICE NOT PRESENT
; 6 NOT INPUT FILE
; 7 NOT OUTPUT FILE
; 8 MISSING FILENAME
; 9 ILLEGAL DEVICE NUMBER
;10 NEXT WITHOUT FOR
;11 SYNTAX
;12 RETURN WITHOUT GOSUB
;13 OUT OF DATA
;14 ILLEGAL QUANTITY
;15 OVERFLOW
;16 OUT OF MEMORY
;17 UNDEF'D STATEMENT
;18 BAD SUBSCRIPT
;19 REDIM'D ARRAY
;20 DIVISION BY ZERO
;21 ILLEGAL DIRECT
;22 TYPE MISMATCH
;23 STRING TOO LONG
;24 FILE DATA
;25 FORMULA TOO COMPLEX
;26 CAN'T CONTINUE
;27 UNDEF'D FUNCTION
;28 VERIFY
;29 LOAD
;30 BREAK
;
;MDBASIC ERROR MESSAGES:
misop  .shift "missing operand"       ;31
ilvne  .shift "illegal voice number"  ;32
illspr .shift "illegal sprite number" ;33
ilcoor .shift "illegal coordinate"    ;34
cantre .shift "can't resume"          ;35
usrerr .shift "user defined"          ;36 to 127 and 0
;
erradd .word misop, ilvne, illspr, ilcoor, cantre, usrerr
;
;Program Tokenization process - text to tokens via vector ICRNCH ($0304)
;
toknew ldx TXTPTR
 ldy #$04
 sty GARBFL
nxtchr lda BUF,x
 bpl norma      ;bit 7 indicates a token value
 cmp #TOKEN_PI  ;pi token?
 beq takchr
 inx
 bne nxtchr
norma cmp #" "  ;space character?
 beq takchr
 sta $08        ;search char for statement terminator or quote
 cmp #"""
 beq getchr
 bit GARBFL     ;variable used by Program Tokenization process
 bvs takchr
 cmp #"?"
 bne skip
 lda #TOKEN_PRINT
 bne takchr
skip
 cmp #"'"
 bne skipp
 lda #0
 beq rem
skipp
 cmp #"0"
 bcc skip1
 cmp #"<"
 bcc takchr
skip1 sty $71
 lda #FIRST_CMD_TOK
 sta COUNT      ;current token
 ldy #$ff
 stx TXTPTR
 dex
cmplop iny
 inx
tstnxt lda BUF,x
 sec
 sbc newcmd,y
 beq cmplop
 cmp #$80      ;last letter?
 bne nxtcmd
 ora COUNT
tachr1 ldy $71
takchr inx
 iny
 sta BUF-5,y
 cmp #0
 beq endln
 sbc #":"       ;ascii $3a
 beq skip2
 cmp #TOKEN_DATA-":" ;data token $83 minus $3a equals $49
 bne skip3
skip2 sta GARBFL
skip3 sec
 sbc #$55       ;rem token $8f minus $3a equals $55
 bne nxtchr
rem
 sta $08
remlop lda BUF,x
 beq takchr
 cmp $08
 beq takchr
getchr iny
 sta BUF-5,y
 inx
 bne remlop
nxtcmd ldx TXTPTR
 inc COUNT      ;next token
contin iny
 lda newcmd-1,y
 bpl contin
 lda newcmd,y
 bne tstnxt
 beq oldtok
notfou lda BUF,x
 bpl tachr1
endln sta BUF-3,y
 dec TXTPTR+1
 lda #$ff
 sta TXTPTR
 rts
oldtok ldy #0
 sty COUNT
 lda RESLST,y  ;list of cbm keywords
 bne oldtst
oldcmp iny
 inx
oldtst lda BUF,x
 sec
 sbc RESLST,y  ;list of cbm keywords
 beq oldcmp
 cmp #$80
 bne nxtold
 ora COUNT
 bne tachr1
nxtold ldx TXTPTR
 inc COUNT
cont1 iny
 lda RESLST-1,y
 bpl cont1
 lda RESLST,y
 bne oldtst
 beq notfou
;
;Evaluate tokens via vector IGONE ($0308)
;
exccmd
 lda traceflag
 beq execut
 jsr trace1
execut
 jsr CHRGET
 beq nocmd     ;occurs when line ends with a colon
xcmd jsr tstcmd
;if key trapping is enabled then proccess it
 ldy keyflag   ;0=off,1=on,2=pause
 beq nocmd     ;key trapping is off
 dey
 bne nocmd     ;key trapping is paused
 lda NDX       ;num chars in keyboard buffer
 beq nocmd
 sei
 jsr LP2       ;get char in keyboard buffer
 sta keyentry  ;use K=KEY to get value
 inc keyflag   ;pause key trapping
 lda #3        ;actually is 5 since this jsr counts for 2
 jsr GETSTK    ;check for space on stack
 lda #>onkey1-1
 pha
 lda #<onkey1-1
 pha
 lda TXTPTR+1  ;save basic text ptr
 pha           ;of the statement to execute 
 lda TXTPTR    ;after returning from subroutine
 pha           ;and it's
 lda CURLIN+1  ;basic line# to RETURN
 pha
 lda CURLIN
 pha
 lda #TOKEN_GOSUB
 pha
 lda TMPKEYP
 sta TXTPTR
 lda TMPKEYP+1
 sta TXTPTR+1
 lda keyline
 ldy keyline+1
setline
 sta CURLIN
 sty CURLIN+1
nocmd
 jmp NEWSTT     ;find beginning of next statement and execute
;
;after ON KEY RETURN re-enable key trapping
onkey1
 lda keyflag    ;if key trapping turned off manually during subroutine
 beq nocmd      ;then no need to switch pause to on
 dec keyflag    ;otherwise switch from paused (2) to on (1)
 jmp NEWSTT     ;find beginning of next statement and execute
;
remm jmp REM
let cmp #"'"-$80
 beq remm
 jmp LET        ;perform LET
tstcmd
 sbc #$80
 bcc let
 cmp #$a3-$80   ;lower than TAB( token $A3?
 bcc oldcmd     ;normal CBM BASIC cmd
 cmp #FIRST_CMD_TOK+1-$80 ;token $cc is first executable
 bcc badtok
 sbc #FIRST_CMD_TOK-$80-$22 ;index of first executable token $cc is 35
oldcmd
 asl            ;index * 2 for word pointer indexing
 tax
 lda cmdtab+1,x ;hibyte
 pha
 lda cmdtab,x   ;lobyte
 pha
 jmp CHRGET
;
vall
 jsr CHRGET
valll
 jsr PARCHK       ;Evaluate Expression Within Parentheses
 jmp VAL          ;perform VAL
;
valfn
 jsr chrget       ;advance TXTPTR one byte
 cmp #"b"
 bcc valll
 sta XSAV
 jsr vall         ;use VAL to determine TXTPTR
 ldy $23          ;result of VAL is not used
 ldx $22          ;set TXTPTR to first char in string
 bne back1        ;backup 1 char to prepare for CHRGET
 dey
back1
 dex
 stx TXTPTR
 sty TXTPTR+1
;push onto stack the byte after the string
 ldy #0
 lda ($24),y
 pha
;null-terminate the string for val calculation
 tya
 sta ($24),y
;push onto stack the return address $b7dd-1 to restore byte and TXTPTR
 lda #$b7
 pha
 lda #$dc
 pha
 lda XSAV
;determine VAL function
 cmp #"h"         ;hexadecimal
 beq hexaa
 cmp #"b"         ;binary
 beq binary
 cmp #"o"         ;octal
 beq octal
badtok
 jmp SNERR
;
;Evalutate functions via vector IEVAL ($030A) originally pointing to EVAL $AE86
newfun lda #$00   ;0=number, 255=string - all funcs take one numeric param
 sta VALTYP       ;Flag Type of Data (String or Numeric) to enforce data type
 jsr CHRGET
 bcc xbcf3        ;numeric digit 0 to 9
 jsr $b113        ;Check If .A Register Holds Alphabetic ASCII Character
 bcs isvari       ;is alpha
 cmp #"@"
 beq octal        ;octal value
 bcs funtok       ;probably token
 cmp #"%"         ;binary value literal?
 beq binary
 bcs oldfun       ;probably a parenthesis or decimal point
 cmp #"$"         ;hex value literal?
hexaa beq hexa
 cmp #"!"         ;NOT short-hand
 beq not          ;otherwise probably a quote
oldfun jmp $aead  ;execute CBM BASIC function
not    jmp $aed0  ;perform NOT
isvari jmp ISVAR  ;get value of variable
xbcf3  jmp FIN    ;convert ASCII numerals into float with result in FAC1
funtok
 cmp #TOKEN_VAL
 beq valfn
 cmp #FIRST_FUN_TOK ;CBM basic max token for functions?
 bcc oldfun       ;bad func token - will raise error
 sbc #FIRST_FUN_TOK
 asl              ;index * 2 for word pointer indexing
 tay              ;prepare for direct indexing
 lda funtab+1,y   ;lobyte value of address for function
 pha              ;lobyte for indirect addressing
 lda funtab,y     ;hibyte value of address for function
 pha              ;hibyte for indirect addressing
 rts              ;execute function
;
;evaluate inline octal value denoted by @
octal jsr clrfac
nexto jsr CHRGET
 sec
 sbc #"0"
 bmi end1
 cmp #8
 bcs end1
 tax
 lda $61    ;exponent
 beq zero3
 adc #3     ;increase by 2^3 = 8
 sta $61
 beq over
zero3 txa
 beq nexto
 jsr FINLOG
 jmp nexto
;
over jmp OVERR
;
;evaluate inline binary value denoted by %
binary jsr clrfac
nextb jsr CHRGET
 sec
 sbc #"0"
 bmi end1
 cmp #2
 bcs end1
 tax
 lda $61    ;exponent
 beq zero2
 inc $61    ;increase by 2^1 = 2
 beq over
zero2 txa
 beq nextb
 jsr FINLOG
 jmp nextb
;
;evaluate inline hex value denoted by $
hexa jsr clrfac
nexth jsr CHRGET
 bcc digit  ;numeric digits
 cmp #"a"   ;ensure chars A thru F
 bcc end1
 cmp #"f"+1
 bcs end1   ;bad hex value
 sec        ;prepare char A-F for index conversion
 sbc #7     ;'0'=0,'1'=1,...'A'=10,...'F'=15
digit sec
 sbc #"0"
 tax
 lda $61    ;exponent
 beq zero
 clc
 adc #4     ;increase by 2^4 = 16
 bcs over
 sta $61
zero txa
 beq nexth
 jsr FINLOG ;add signed int to FAC1
 jmp nexth
;
;clear num work area $5d-$60, FAC1 $61-$68 and FAC2 $69-$6E
clrfac ldx #11
 lda #0
loop sta $5d,x
 dex
 bpl loop
end1 rts
;
;----------------
; TIME CLR    reset time to all zeros
; TIME$ = T$  set the time start, format="00:00:00"
time
 cmp #TOKEN_CLR
 beq timeclr
 cmp #"$"
 beq settime
badtime jmp SNERR
badtime2 jmp TMERR
;set the clock
settime
 jsr CHRGET
 cmp #TOKEN_EQUAL
 bne badtime
 jsr getstr
 dec R6510
 jsr settimee
 inc R6510
 bcs badtime2
 rts
;reset clock to 12AM
timeclr
 jsr CHRGET      ;skip over CLR token
 lda #%10010010  ;BCD 12 (am/pm flag=1 since it flips on write when hour is 12)
 sta TO2HRS      ;writing this reg stops time reg updates
 lda #0
 sta TO2MIN
 sta TO2SEC
 sta TO2TEN      ;writing this reg resumes time reg updates
 rts
;
; T$ = TIME$  get current time as string value
; T  = TIME   get current time as float number of seconds since start
fntime
 jsr chrget   ;advance TXTPTR 1 position and get the char
 dec R6510
 cmp #"$"
 bne time2
;get time as a string
 jsr getimstr
 inc R6510
 ldy #>BUF
 lda #<BUF+$50
 jsr STRLIT
 jmp CHRGET
;get time in seconds since midnight
time2
 jsr dotime
 inc R6510
 lda TXTPTR
 pha
 lda TXTPTR+1
 pha
 stx TXTPTR
 sty TXTPTR+1
 jsr FRMNUM
pultxtptr
 pla
 sta TXTPTR+1
 pla
 sta TXTPTR
 rts
;
;******************************************
;LIST command re-write to decode new tokens via vector IQPLOP ($0306)
;Supports freezing the listing while holding down the shift key.
;This routine is called repeatedly until the entire list range is complete.
;******************************************
list
 bpl out        ;less than 128 is non token so just output char as-is
 ldx #$01       ;only shift key flag
lstpause
 cpx SHFLAG     ;0=none, 1=shift key, 2=logo key, 4=ctrl key
 beq lstpause   ;bit pattern 001=shift, 010=commodore, 100=ctrl (any combo)
 bit GARBFL     ;quote mode enabled?
 bmi out        ;bit7 set means yes so just output char as-is
 cmp #TOKEN_PI  ;pi token?
 beq out        ;just output pi symbol as-is
 cmp #FIRST_CMD_TOK ;first MDBASIC command token?
 bcs newlst     ;decode MDBASIC token to text
 jmp $a724      ;perform part of CLR
out jmp $a6f3   ;output byte as it is on cmd line
newlst
 sbc #FIRST_CMD_TOK-1
 tax            ;index to x reg soon to subtract 1 so 0-based index
 sty $49
 ldy #$ff
nextt dex       ;next token index
 beq found      ;if we are on first token index then it must be a match
loop1 iny
 lda newcmd,y   ;get command's next text char from table
 bpl loop1      ;the last character has bit 7 on as a flag of end-of-string
 bmi nextt      ;reached end of string with no match so try next command
found iny       ;found a command match for every chr in string
 lda newcmd,y   ;get current char in command string
 bmi oldend     ;if on last chr of command then continue with old list function
 jsr CHROUT
 jmp found      ;next char
oldend
 jmp $a6ef      ;original LIST for CBM-BASIC tokens
;
;**************************************
; IF statement re-write to support ELSE
;**************************************
if
 jsr FRMEVL
 jsr CHRGOT
 cmp #TOKEN_GOTO ;GOTO token? syntax IF X=1 GOTO 10
 beq condit
 lda #TOKEN_THEN ;THEN token
 jsr CHKCOM+2    ;check for and skip over THEN, error if not there
condit
 lda $61         ;expression result 0=false, otherwise true
 beq isfalse     ;non-zero means expression is true
 jsr CHRGOT      ;check current char is numeric digit
 bcs endlin      ;clear carry means ASCII numerials
dogoto
 jmp GOTO        ;preform goto
isfalse
 ldx #TOKEN_ELSE
 jsr DATAN+5     ;scan for end of line terminator (byte 0) or ELSE token
 tax             ;a reg holds byte found, either 0 or ELSE token
 beq nxtline     ;end of line, IF statement finished, go to next line
 sty XSAV        ;save current offset for ELSE token
 ldx #TOKEN_IF
 jsr DATAN+5     ;scan for end of line terminator (byte 0) or IF token
 tax
 beq doelse      ;nested IF not found, use ELSE
 cpy XSAV        ;ELSE before IF?
 bcs doelse      ;yes, found nearest ELSE for IF
 ldy XSAV        ;no, scan for next ELSE
 iny             ;skip over ELSE token
 jsr DATA+3      ;add y reg num bytes to advance TXTPTR
 jmp isfalse     ;scan for ELSE again
doelse
 ldy XSAV        ;get offset for ELSE token
 jsr DATA+3      ;add y reg num bytes to advance TXTPTR
 jsr CHRGET      ;skip over ELSE token to next char or token
 bcc dogoto      ;ascii numerials indicate line number for GOTO
endlin
 jmp tstcmd      ;process statements on the rest of current line
nxtline
 jmp REM         ;perform REM to advance TXTPTR to next line
;
vars dec R6510
 jmp varss
;
;*******************
;DISK dos$ [,device] [,out$]]
;"S0:myfile.bas"                 - delete a file
;"N0:label,id"                   - full format disk with a label and id
;"N0:label"                      - soft format (BAM only) with label
;"I0:"                           - init disk-clr err, move head to trk 0 sec 0
;"C0:sourceFile=destinationFile" - copy a file
;"R0:newfileName=oldfileName"    - rename a file
;"V0:"                           - validate (defragment) disk
disk
 jsr getstr0    ;get DOS string
 jsr SETNAM
 jsr getdskdev  ;get disk device num in x reg
 jsr getfilnum  ;get file num
 sta mdbin
 ldy #$0f       ;secondary channel 15 = DOS channel
 jsr SETLFS
 jsr OPEN       ;perform OPEN
 bcs errmdb
;read line from current channel and device
 ldx mdbin
inpstr
 dec R6510
 lda MSGFLG     ;display message if not in prg mode
 jsr bufio      ;read response and print if needed
 inc R6510
 bcs errmdb     ;carry indicates i/o error
 jsr clsmdb
 jsr CHRGOT     ;check another param exists
 beq donehere   ;if not stop now, otherwise
;copy output to specified string var
outstr
 tya            ;y reg holds bytes to allocate
 sta $fd
 beq gstr
 jsr GETSPA     ;alloc space in mem for string returning address ptr in $35,$36
 sta $fd        ;actual length allocated
 jsr buf2str
gstr
 jsr CHRGET
 jsr PTRGET     ;search for a var & setup if not found
 sta $49        ;var address is returned in a (lo byte), y (hi byte) registers
 sty $4a
 lda $fd        ;string length
setstrptr
 ldy #0
 sta ($49),y    ;save it to the variable's string length byte
 iny
 lda $35        ;get lo byte of str ptr
 sta ($49),y    ;save it to variable's str ptr info
 iny
 lda $36        ;get hi byte of str ptr
 sta ($49),y    ;save it to variable's str ptr info
donehere rts
;
filemax
 lda #1         ;TOO MANY FILES error
errmdb
 pha
 jsr clsmdb
 pla
 tax
 jmp (IERROR)
;
;*******************
;FILES [volume$], [device]
;volume$ is an optional string for filtering directory results
;the string can include the drive num prefix, ie: "0:DEMO*"
files
 beq onechar    ;no param, just use $ as param
 cmp #","
 beq onechar
 jsr getstr1    ;get volume$ (should never be more than 18 chars)
 clc
 adc #1         ;one more char for $ symbol
 cmp #19        ;DOS volume$ string max length is 18
 bcc prepstr    ;always branches (unless len was 255)
 lda #18        ;truncate string at 18 chars
.byte $2c       ;ignore next lda
onechar lda #1
prepstr
 sta $63        ;save length of string
 lda #"$"
 ldy #$ff
copystr
 iny
 sta BAD,y      ;create tmp str in buffer
 lda ($50),y    ;source param str
 cpy $63        ;reached tmp str len?
 bne copystr    ;no, keep copying
;prepare file params
 tya
 ldx #<BAD
 ldy #>BAD
 jsr SETNAM
 jsr getdskdev  ;get disk device num in x reg
 jsr getfilnum  ;get file num in accumulator
 sta mdbin      ;current mdbasic input channel
 ldy #$00       ;secondary 0 in y reg
 jsr SETLFS
 jsr OPEN       ;perform OPEN
 bcs errmdb     ;handle error
 dec R6510
 jsr filess
 inc R6510
 bcs errmdb
 jsr LINPRT     ;print 2-byte binary number in x and y regs
 lda #$a6       ;$a1a6 ROM value for string " FILES"
 ldy #$a1
 jsr printstr   ;print str whose addr is in y reg (hi byte) and a reg (lo byte)
 jmp clsmdb
;
;get an unused (not open) file number, start with 127 down to 0
;file number is returned in accumulator, preserves x and y regs
getfilnum
;enforce max open file handles
 lda LDTND      ;number of open i/o files/index to the end of file tables
 cmp #10        ;limit 10
 bcs filemax
;determine available file number
 stx $14
 sty $15
 ldy #128
nextf
 dey
 tya
 jsr $f314      ;check if file num is in use
 beq nextf      ;file is in use, try next
 ldx $14
 ldy $15
 rts
;
;get and validate the disk device number 8-11, default 8
getdskdev
 ldx #8         ;default device 8
 jsr comchk
 bne devdone
 jsr getvalg    ;get single byte value in x reg
 cmp #8         ;valid serial device numbers 8 to 12
 bcc illdev
 cmp #12
 bcs illdev
 tax
devdone
 rts
illdev
 ldx #9         ;illegal device number error
 jmp (IERROR)
;
;Open MDBASIC file handle for printer
opnprt0
 ldy #$00       ;secondary parameter $FF=not used, 5=binary graphic
opnprt          ;7=upper/lower case chars, 0=Upper case and symbol chars
 jsr getfilnum  ;get file num in accumulator
 sta mdbout     ;current mdbasic input channel
 ldx #$04       ;device 4
 jsr SETLFS     ;set current logical file
 lda #$00       ;zero byte file name length (no name)
 jsr SETNAM     ;set file name
 jsr OPEN       ;perform OPEN 127,4,0,""
 bcc prtopen    ;clear carry flag means success
errout
 jmp errmdb
prtopen
 ldx mdbout     ;current MDBASIC output channel
 jsr CHKOUT     ;set as the current I/O file number
 bcs errout
 rts
;
;*******************
;DUMP LIST [start]-[end]
;DUMP SCREEN
;DUMP BITMAP
;DUMP VARS
;DUMP FILES
;DUMP {expression}
dump
 cmp #TOKEN_LIST
 beq dumplist
 cmp #TOKEN_SCREEN
 beq dumpscreen
 cmp #TOKEN_BITMAP
 beq dumpbitmap
 cmp #TOKEN_VARS
 beq dumpvars
 cmp #TOKEN_FILES
 beq dumpfiles
dumpexpr
 jsr opnprt0
 jsr $aa9d   ;perform print of expression
 jmp clsmdb
dumpvars
 jsr opnprt0
 jsr vars
 jmp closer+2
dumplist
 jsr opnprt0
 jsr lstrap
 jsr opget   ;get LIST parameters
 jsr $a6c9   ;perform list
 jsr printcr ;print carriage return
 jmp clsmdb
dumpscreen
 dec R6510
 jsr dumpscreen2
closer
 inc R6510
 jsr clsmdb
 jmp CHRGET
dumpbitmap
 dec R6510
 jsr dumpbitmap2
 jmp closer
dumpfiles
 jsr opnprt0
 lda mdbout  ;current mdbasic output channel
 sta CHANNL  ;redirect stdout
 jsr CHRGET  ;skip over FILES token
 jsr files
;ensure MDBASIC file handles are closed and restore std io channels
clsmdb
 lda mdbin
 beq zzz
 jsr CLOSE
zzz
 lda mdbout
 beq clsclr
 jsr CLOSE
clscmd
 lda #0
 sta CHANNL
 sta mdbout
clsclr
 sta mdbin
 jmp CLRCHN  ;restore default devices as current I/O channels
;
;*******************
; FILL x1,y1 TO x2,y2, [scanCode], [color]
fill
 jsr getcoords
 jsr ckcom2     ;check for and skip over comma, misop err if missing
 cmp #","       ;another comma?
 beq srncol
 jsr getval     ;get scan code
 jsr filler
srncol
 jsr chkcomm    ;check and skip over comma, quit if not found
 jsr getval15   ;get fill color
 ldx $fd
 ldy $fe
 stx $fb
 sty $fc
filler
 sta XSAV
 ldx $bf        ;line count
nxtc ldy $be    ;column count
 lda XSAV       ;poke code
nxtcol
 sta ($fb),y
 dey
 bpl nxtcol
 lda #40
 jsr addfb
 dex
 bpl nxtc
done4 rts
;
tokopn
 dec R6510
 jsr getfilnum ;get available file number
 jsr openrs232
 inc R6510
 bcc done4     ;clear carry indicates success
 jmp (IERROR)  ;otherwise x reg has error number
;
tokclse
 lda mdbio    ;logical file number
 jsr $f314    ;find the index of an opened logical file number to X reg
 bne clsd232  ;zero flag indicates not found
 jsr $f31f    ;set current logical file, device and seconday address
 txa
 jsr $f2f2    ;remove from table of open files
 jsr $f483    ;Initialize IRQ Timers and data direction registers
clsd232
 lda #0       ;clear hibytes of I/O buffers for RS-232 to indicate not used
 sta $f8      ;hibyte ptr to RS-232 input buffer
 sta $fa      ;hibyte ptr to RS-232 output buffer
 sta mdbio    ;clear filenum used by RS-232
 jmp CHRGET   ;finally, skip over token
;
tokprt
 ldx mdbio
 stx CHANNL   ;set current I/O channel (logical file) number
 jsr $e118    ;BASIC wrapper for CHKOUT with error handling
 jsr CHRGET   ;position TXTPTR on first char of expression
 jsr PRINT    ;perform CBM BASIC PRINT
waitout
 jsr STOP
 beq end232   ;STOP key pressed, abort print
 lda ENABL    ;transmitting when bit0 is 1
 and #1
 bne waitout
end232
 jmp $abb5    ;send UNLSN & UNTALK to serial device and restore default devices
;
;*******************
;SERIAL OPEN [baud],[databits],[stopbits],[duplex],[parity],[handshake]
;SERIAL [WAIT timeout] READ s$ | f | i% [TO byte]
;SERIAL PRINT expression
;SERIAL CLOSE
serial
 cmp #TOKEN_PRINT
 beq tokprt
 cmp #TOKEN_OPEN
 beq tokopn
 cmp #TOKEN_CLOSE
 beq tokclse
;prepare for READ
 ldx #0
 stx $fe       ;wait flag: 0 is no wait else wait
 stx $50       ;timeout lobyte
 stx $51       ;timeout hibyte
 inx           ;1=timeout disabled
 stx $52       ;timeout disabled by default 0=enabled, 1=disabled
 cmp #TOKEN_WAIT
 bne tokread
 lda #8        ;bit 3 same as status bit of empty buffer
 sta $fe
 jsr CHRGET
 cmp #TOKEN_READ
 beq tokread   ;no timeout supplied
 jsr getvaluez ;get timeout 0-65535
 stx $50
 sty $51
 txa
 ora $51
 beq tokread   ;zero timeout is a disabled timeout
 dec $52       ;enable timeout
tokread
 lda #TOKEN_READ
 jsr CHKCOM+2  ;skip over READ token otherwise SYNTAX ERROR
 ldx mdbio     ;file number
 stx CHANNL    ;current I/O channel (cmd logical file) number
 jsr $e11e     ;BASIC wrapper for CHKIN with error error handling
 jsr getvar    ;get pointer to target variable
;check if sentinel param supplied
 lda #0
 sta $fb       ;flag for sentinel check
 jsr CHRGOT
 cmp #TOKEN_TO
 bne savesb
 jsr getvalg   ;get sentinel byte param value
 sta $fc       ;sentinel byte to check
 inc $fb       ;flag for sentinel check
savesb
 lda $a9       ;start bit received flag
 sta $62       ;remember it before first read
;begin read loop
goread
 lda #0
 sta RSSTAT    ;clear status
;reset timeout
 lda $50
 sta $14
 lda $51
 sta $15
;read next byte with timeout (if enabled)
 jsr waitread
 bcs rdone
;save the byte
 lda $61       ;last byte read
 ldy $fd       ;offset to store result
 sta ($35),y   ;store to variable
;check for critical error
 lda RSSTAT    ;get status without clearing it
 and #%11110111 ;errors other than empty buffer?
 bne strdone   ;yes, stop now and return status
;if provided, check if last read byte is sentinel byte
 lda $fb       ;flag to use sentinel
 beq nxtbyte   ;zero means disabled
 lda $61       ;last read byte
 cmp $fc       ;sentinel reached?
 beq strdone   ;yes, stop reading
 lda $ff       ;string type
 bne goread    ;numeric variable use only 1 byte
nxtbyte
 inc $fd       ;next index in string
 dec XSAV      ;reduce byte count for read
 beq rdone     ;stop reading
 bne goread    ;keep reading if more room in string

;include byte in string length
strdone
 inc $fd       ;string length = index+1

;return result based on data type
rdone
 jsr $abb5    ;send UNLSN, UNTALK to serial device then restore default devices

;apply value to variable
setval
 ldy $ff       ;type 0=string, 1=float, 2=int
 beq setstr
 dey
 beq setflt
 lda #0        ;make hibyte zero
 tay
 sta ($35),y   ;store directly to int value memory
valdone
 rts
setflt
 ldy $61       ;byte read is lobyte
 lda #0        ;zero hibyte
 jsr GIVAYF    ;convert binary int to float with result in FAC1
 ldx $35       ;copy the result in FAC1
 ldy $36       ;to the variable memory
 jmp $bbd7     ;copy FAC1 to memory
setstr
 lda $fd       ;length of string read
 sta ($49),y   ;string length byte
 beq valdone   ;zero-length string
 jsr GETSPA    ;alloc new str return ptr in $35,$36 and length in A reg
 jsr setstrptr ;set pointer to newly allocated string
buf2str
 dec R6510
 jmp buf2strg
;
;read a byte with timeout (if enabled)
waitread
 jsr $f086     ;GETIN for RS-232 device
 sta $61
 lda RSSTAT    ;get status without clearing it
 beq byter     ;no errors then accept byte
 bit $fe       ;empty buffer and wait requested
 beq tstbyte   ;no, test for framing error
 jsr chktimo   ;count down timer, pause 1 jiffy
 bne waitread  ;timeout not reached (or enabled)
readquit sec   ;return flag in carry to stop reading
 rts
tstbyte
;check for framing error, adjust only if start bit just received
 bit bitweights+1 ;bit1, framing error?
 beq byter     ;no, accept byte
 ldy $62       ;start bit saved before first read
 bne readquit  ;already received then return error status
 lsr $61       ;shift bit frame to correct first byte read
 and #%11111101 ;clear the framing error bit
 sta RSSTAT    ;and keep remaining status info
 bne readquit  ;other errors present
byter clc      ;return with success flag in carry
 rts
;
chktimo
 jsr STOP      ;STOP key?
 beq endtimer
 lda $52       ;timer flag 0=timeout enabled, else disabled
 bne endtimer  ;not enabled, return with zero flag clear
 lda $14
 bne dec14
 lda $15
 beq endtimer  ;timer at 0, return with zero flag set
 dec $15
dec14
 dec $14
;make this entire process take about 1 jiffy
 lda $a2       ;jiffy clock updated 60 times per sec.
topause
 cmp $a2
 beq topause
chktimer
 lda $14       ;zero flag indicates timeout reached
 ora $15
endtimer rts
;
;get or create pointer to string pointer provided as param
getvar
 jsr PTRGET    ;search for a var & setup if not found
 sta $49       ;var address is returned in a (lo byte) y (hi byte) registers
 sty $4a       ;sting descriptor is 3 bytes, 2 for ptr, 1 for length
;determine data type to read
 ldx VALTYP    ;0=numeric, 255=string
 inx
 stx $ff       ;0=string,1=float
 stx $fd       ;offset to store bytes
 beq rdstr     ;string read 255 bytes at a time
 stx XSAV      ;numeric binary read 1 byte at a time
 ldx INTFLG    ;0=float, 128=int
 beq rdnum     ;float stores 5 bytes
 inc $ff       ;2=int
rdnum
 sta $35       ;ptr of a numeric variable
 sty $36       ;is the ptr of the value
 rts
;allocate space for new string
rdstr
 dex           ;max string length is 255
 stx XSAV      ;buffer size
 lda #<paintbuf1
 sta $35
 lda #>paintbuf1
 sta $36
 rts
;
;*******************
; SYS address [,a [,x [,y [,p]]]]
sys
 jsr getvaluez ;get 2-byte int into $14 lobyte, $15 hibyte
 ldy #252      ;prepare for loop of 4 registers
regloop
 sty XSAV      ;current register index offset
 jsr CHRGOT    ;another param?
 beq sysend
 jsr GETADR-6  ;skip over comma then eval expr to a single byte into x reg
 txa
 ldy XSAV
 sta SAREG-252,y
 iny
 bne regloop
sysend
 jmp SYS+6     ;perform remainder of SYS
;
;*******************
; WAIT location, mask1 [,mask2]
; WAIT jiffies
wait
 jsr getvaluez ;get 2-byte int into $14 lobyte, $15 hibyte
 jsr CHRGOT    ;another param?
 beq delay2    ;no, do jiffy wait
 jsr GETADR-6  ;skip over comma then eval expr to a single byte into x reg
 jmp WAIT+3    ;continue with original WAIT cmd
delay2         ;entry point for internal use; set x and y reg accordingly
 clc           ;flag for STOP key
 txa
 bne decx
 tya
 beq stopnow
 dey
decx dex
 lda $a2       ;jiffy clock updated 60 times per sec.
dlay2 cmp $a2
 beq dlay2
 lda LSTX      ;matrix coordinate of last key pressed, 64=none
 cmp #$3f      ;STOP key?
 bne delay2    ;carry set indicates STOP key pressed
stopnow rts
;
;*******************
; AUTO            :uses last used increment setting, default is 10
; AUTO increment  :valid increment values 0 to 1023 with 0 = AUTO OFF
; AUTO OFF        :turn off auto numbering
auto
 beq applyauto
 ldy #1
 cmp #TOKEN_ON
 beq autotok
 dey
 cmp #TOKEN_OFF
 bne getinc
autotok
 jsr CHRGET   ;skip over token
 tya          ;on/off flag
setaflg
 sta autoflag
 rts
getinc
 jsr getvalue ;get increment param
 cpy #4       ;max auto-line number value is 1023
 bcs badauto
 tya
 ora $14
 beq setaflg  ;0 increment same as AUTO OFF
 stx autonum
 sty autonum+1
applyauto
 lda #1
 bne setaflg  ;always branches
badauto
 jmp FCERR
;
;******************
; MDBASIC immediate mode input via vector IMAIN ($0302)
;******************
immed
 jsr detrap     ;ensure error trapping is off
;if screen is off or in bitmap mode then switch to normal text
 lda SCROLY
 eor #%00010000
 and #%00110000 ;is screen off or bitmap mode on?
 beq aline
 jsr norm       ;restore normal display
;input a BASIC line of text
aline
 jsr INLIN      ;input a BASIC line into buffer
 stx TXTPTR
 sty TXTPTR+1
 jsr CHRGET     ;carry clear when numeral
 tax
 beq aline
 ldx #$ff       ;indicate immediate mode by setting
 stx CURLIN+1   ;line number hi byte to invalid value
 bcc doauto     ;input begins with numeric value
 jmp $a496      ;non-line number input, tokenize then execute
doauto
 jsr LINGET
 lda autoflag
 beq eauto
 jsr CHRGOT
 beq eauto
;determine next line number
 lda $14
 clc
 adc autonum
 sta $63
 lda $15
 adc autonum+1
 sta $62
;convert binary value of line number to string
 ldx #$90       ;FAC1 exponent
 sec            ;flag for unsigned int
 jsr $bc49      ;convert 2-byte int in FAC1 to float
 jsr FOUT       ;convert FAC1 to ASCII String at BAD ($0100)
;put line number in keyboard buffer
 ldy #1         ;start at 1 to skip sign char (space)
bad2buf
 lda BAD,y      ;get each digit in the
 beq endnum     ;zero-byte terminated string
 sta KEYD-1,y   ;put char in keyboard buffer
 iny            ;max number would be 5 digits
 bne bad2buf    ;so this will always branch
endnum lda #32  ;a space char
 sta KEYD-1,y   ;is the last char
 sty NDX        ;set num chars in keyboard buffer
eauto jmp $a49f ;continue with main loop for direct mode
;
;*******************
; TRACE line#
;runs the program with trace mode enabled
trace
 php     ;save flags from CHRGET
 lda #1
 ldx #<exccmd
 ldy #>exccmd
 jsr settrace
 plp     ;restore flags from CHARGET
 jmp RUN
;**trace subroutine during prg execution
trace1
 ldy CURLIN+1 ;when current line num hibyte is $FF (invalid)
 iny          ;then immediate mode
 bne trace2
;turn off trace mode
troff
 lda #$00
 ldx #<execut
 ldy #>execut
settrace
 sta traceflag
 stx IGONE
 sty IGONE+1
 rts
trace2
 lda CURLIN
 sta $14
 lda CURLIN+1
 sta $15
;clear top 2 lines
 ldx #0
 jsr $e9ff
 inx
 jsr $e9ff
;save current cursor position on stack
 lda PNTR
 pha
 lda TBLX
 pha
 jsr weglst ;find and display line number in $14,$15
 pla        ;restore original cursor position
 tax
;restore cursor position from stack
 pla
 tay
 clc
 jsr PLOT
shftky
 jsr STOP   ;check stop key
 beq etrace ;stop now
 lda #$01
 bit SHFLAG ;is the shift key pressed?
 beq shftky ;wait for it to be pressed
etrace rts
;
initmdb
 lda #0         ;clear all MDBASIC IRQ flags
 sta MDBIRQ     ;if MDBASIC IRQ is active it will restore original irq
 sta SPENA      ;turn off all sprites
 jsr troff      ;turn off tracing
 jsr detrap     ;turn off error trapping in case it was enabled in previous run
 jsr clearerr   ;clear last error info
 jsr keyoff     ;ensure key trapping is off
 jmp sidclr     ;clear SID registers
;
;*******************
; RUN
; RUN linenum
; RUN filename$,[device],[secondary]
oldrun jmp RUN  ;CBM BASIC - perform RUN
newrun
 php
 jsr initmdb    ;initialize MDBASIC
 plp
 beq oldrun     ;RUN without params
 bcc oldrun     ;RUN with line num param
 lda #$00       ;RUN with file params
 sta VERCK      ;load or verify? 0=load, 1=verify
 jsr $e1d4      ;process file parameters
 jsr RUNC       ;reset TXTPTR to start of program text
 jsr $e16f      ;perform load
 jsr old        ;set BASIC prg ptrs
 lda #0         ;run without line number
 jsr RUN        ;set run mode and clear vars
 jmp NEWSTT     ;enter loop for BASIC program processing
;
;*******************
; MERGE filename$   appends file to end of current BASIC program
merge
 lda #$00
 sta VERCK
 jsr $e1d4   ;set params for load, verify and save
 lda VARTAB
 sec
 sbc #$02
 tax
 lda VARTAB+1
 sbc #$00
 tay
 lda #$00
 jsr LOAD    ;load from a device
 bcs ioerr   ;carry set indicates error
 jsr READST  ;read i/o status word
 and #%10111111 ;did an error occur other than EOF/EOI (bit6)?
 beq okmerg  ;no error
 jmp LODERR  ;raise LOAD ERROR
okmerg stx VARTAB
 sty VARTAB+1
 jsr CLEAR-5 ;reset TXTPTR to beginning of prg then perform CLR
 jsr LINKPRG ;relink lines of tokenized prg text
 jmp READY   ;main basic loop
ioerr jmp $e0f9 ;handle i/o error
;
;*******************
;secondary address 16=SCREEN, 17=CHAREN, 18=BITMAP
newlod
 cpx #19
 bcs oldload
 pla            ;do not return to calling subroutine
 pla            ;to prevent adjusting BASIC memory pointers
 dec R6510      ;switch out BASIC ROM for RAM ($a000-$bfff)
 jmp loadd      ;continue under rom with the rest of the new load routine
romin
 jsr norm2
 lda LA         ;close current open file
 jsr CLOSE
 jmp CLRCHN     ;restore default I/O devices
;
; Vector to Kernal LOAD Routine ILOAD ($0330)
newload
 sta $93     ;flag for load routine 0=Load, 1=Verify
 ldx SA      ;secondary address
 stx $fe     ;used after load to determine if mem ptrs need to be restored
 cpx #16     ;mdbasic secondary address 16,17,18
 bcs newlod  ;indicates MDBASIC load
oldload
;CBM code from original vector location $f4a5 to perform load
 lda #0
 sta STATUS ;kernal I/O status
 lda FA     ;get current device number
 bne xf4b2  ;0=keyboard
xf4af
 jmp $f713  ;load from keyboard or screen
xf4b2
 cmp #3
 beq xf4af  ;3=screen
 bcs xf4b8  ;4=printer,8-9=disk
 jsr $f533  ;1=dataset or 2=User Port RS-232 (illegal device)
 bcs lodfin ;error
 bcc oklod2 ;success
xf4b8
 ldy FNLEN  ;length of current filename
 bne xf4bf
 jmp $f710  ;handle error #8 - MISSING FILE NAME ERROR
xf4bf
 ldx SA     ;current secondary address
 jsr $f5af  ;print SEARCHING FOR filename
 lda #$60   ;DOS secondary device NCTE: SA = $60 + N
 sta SA     ;current secondary address
 jsr $f3d5  ;send filename to device using kernal LISTEN, CIOUT and UNLSN
 lda FA     ;current device number
 jsr TALK   ;send talk to a device on the serial bus
 lda SA     ;current secondary address
 jsr TKSA   ;send a secondary address to a device on the serial bus after talk
 jsr ACPTR  ;receive a byte of data from a device on the serial bus
 sta EAL    ;lobyte of address for load which will increment to the end address
 sta STAL   ;remember start address
 lda STATUS ;kernal I/O status
 lsr        ;shift bit 1 (serial read timeout)
 lsr        ;right into carry
 bcc oklod  ;timeout? no, read byte
 jmp $f704  ;yes, handle error #4 - FILE NOT FOUND
oklod
 jsr ACPTR  ;receive a byte of data from a device on the serial bus
 sta STAL+1 ;remember start address
 jsr $f4e3  ;continue with original LOAD subroutine
 bcc oklod2 ;carry set indicates error
 jmp $e0f9  ;handle i/o error
oklod2
 jsr chkio  ;check the I/O status
 lda MSGFLG ;display message if not in prg mode
 bpl lodone ;don't display load addresses
 jsr $ab3f  ;print space
 ldx TXTTAB ;assume BASIC mem load
 lda TXTTAB+1
 ldy $fe    ;secondary address: 0=BASIC load, 1=binary
 beq prtmem
 ldx STAL   ;print mem ptr from file
 lda STAL+1
prtmem
 jsr LINPRT ;print 2-byte binary value
 lda #"-"
 jsr CHROUT
 ldx EAL    ;ptr to end addr of loaded file
 lda EAL+1
 jsr LINPRT ;print 2-byte binary value
lodone
 lda VERCK  ;load=0 or 1=verify
 bne lodbas
 lda $fe    ;secondary address
 bne lodbin ;binary load does not need to adjust BASIC mem ptrs
lodbas
 ldx EAL    ;restore x,y ptr to end of prg from load subroutine
 ldy EAL+1
 clc        ;no error
lodfin
 rts
lodbin
 lda TXTTAB+1 ;check if binary load was actually a BASIC prg
 cmp STAL+1   ;by comparing the start address of loaded binary
 bne isbin    ;with the start address of BASIC prg mem
 lda STAL     ;if it was loaded exactly in BASIC mem
 cmp TXTTAB   ;then finish load as usual to init mem ptrs
 beq lodbas   ;this will kill the current running BASIC prg
isbin         ;otherwise do not return to calling subroutine
 pla          ;to prevent adjusting BASIC memory pointers
 pla          ;this way no corruption of BASIC mem will occur
 rts          ;and the running BASIC program can continue
;
chkio
 jsr READST     ;read the I/O status
 and #%10111111 ;did an error occur other than EOF/EOI (bit6)?
 beq lodfin     ;no error
 jsr romin      ;ensure BASIC ROM is available
 jmp LODERR     ;raise LOAD ERROR

;*******************
; Vector to Kernal SAVE Routine ISAVE ($0332)
;secondary address 16=SCREEN, 17=CHAREN, 18=BITMAP
newsave
 lda SA       ;secondary address
 cmp #16
 bcs newsav   ;file handles >=128 are MDBASIC file handles
oldsav
 jmp $f5ed    ;perform normal save
newsav
 cmp #19
 bcs oldsav
 dec R6510
 jmp savee
;
;MDBASIC binary save
bsave
 beq osave
 jsr FRMEVL   ;eval expression
;set file defaults
 lda #$00     ;default file#
 ldx #$01     ;default device 1 (tape)
 ldy #$00     ;default secondary 0
 jsr SETLFS   ;set logical file params, A=file#,X=device,Y=secondary
;check last expression evaluated data type
 lda VALTYP   ;numeric or string?
 beq bsaver
;continue as if normal subroutine
 jsr $e25a    ;FRESTR and SETNAM
 jsr $e1e6    ;get remaining save params
 jmp $e159    ;save BASIC prg to device
osave
 jmp $e156
bsaver
 jsr getvalue+3 ;start address
 stx STAL
 sty STAL+1
 jsr getvalueg ;end address
 stx EAL
 sty EAL+1
 jsr CHRGET   ;advance to next param or end of statement
 jsr $e1d4    ;set parms for LOAD, VERIFY, SAVE
 ldx EAL      ;ptr to end address
 ldy EAL+1    ;in x and y reg
 lda #$c1     ;first byte in zero-page used as ptr to start address
 jmp $e15f    ;save RAM to device - finish save
;
;*******************
; FIND cmd - tokenized search, ie: FIND FOR
; FIND"chars - text search, ie: FIND"FOR
find
 dec R6510
 jsr findd
 inc R6510
 jsr printcr
 jmp DATA
findlnr
 inc R6510
 jsr FINDLN   ;search for line#
 jsr $a6c9    ;perform list (print line on screen)
 dec R6510
 rts
;
;*******************
; NEW [SYS]
new beq oldnew
 cmp #TOKEN_SYS
 bne oldnew
 jmp ($fffc)
oldnew jmp NEW
;
;;*******************
; OLD takes no params
old lda #$08
 ldy #1
 sta (TXTTAB),y ;should be $0802
old2
 jsr LINKPRG
 lda $22   ;apply calculated end-of-prg pointer
 ldx $23
 clc
 adc #$02
 sta VARTAB ;Pointer to the Start of the BASIC Variable Storage Area
 sta ARYTAB ;Pointer to the Start of the BASIC Array Storage Area
 sta STREND ;Pointer to the Start of BASIC Free RAM
 bcc savex
 inx
savex stx VARTAB+1
 stx ARYTAB+1
 stx STREND+1
 rts
;
;*******************
; DELETE line  (delete one line)
; DELETE start-end  (delete all lines from start to end)
delete
 beq oldnew
 jsr opget2
 lda $5f
 sta $24
 lda $60
 sta $25
 jsr FINDLN
 lda $5f
 ldx $60
 bcc noline
 ldy #$01
 lda ($5f),y
 beq noline
 tax
 dey
 lda ($5f),y
noline
 sta TXTPTR
 stx TXTPTR+1
 lda $24
 sec
 sbc TXTPTR
 tax
 lda $25
 sbc TXTPTR+1
 tay
 bcs deldone
 txa
 clc
 adc VARTAB
 sta VARTAB
 tya
 adc VARTAB+1
 sta VARTAB+1
 ldy #$00
copy lda (TXTPTR),y
 sta ($24),y
 iny
 bne copy
 inc TXTPTR+1
 inc $25
 lda VARTAB+1
 cmp $25
 bcs copy
deldone
 jsr old2
relink
 jsr CLEAR-5  ;reset TXTPTR to beginning of prg then perform CLR
endprg
 jsr $a67a  ;empty the stack
 jmp HALT
;
weglst
 jsr FINDLN ;find BASIC line number in $14,$15
 bcc endprg ;line not found
 jsr $a82c  ;test STOP key for break in program
 lda #19    ;chr 19 = cursor home
 jsr CHROUT
 jsr lstrap
 ldy #1
 jmp $a6d7  ;perform LIST of current line
;
;******************
; RESTORE       - set first data line as next DATA READ
; RESTORE line# - set line# as next DATA READ
restor
 beq oldrst
 jsr getvalue
 jsr getlin      ;find the line specified
 stx DATPTR      ;set DATA ptr to the start of line
 sty DATPTR+1
 rts
oldrst
 jmp RESTORE     ;original CBM RESTORE takes no params
;
;*******************
; POKE mem, value
; POKE mem1 TO mem2, value, [operation]
;operation is optional (default 0): 0=SET,1=AND,2=OR,3=EOR
poke
 jsr getvaluez   ;get 2-byte int in $14,$15
 jsr CHRGOT
 cmp #","
 bne newpoke
 jsr GETBYTC     ;get single byte int in x reg
 jmp $b827       ;do single byte poke
newpoke
 stx $fb
 sty $fc
 lda #TOKEN_TO   ;token to skip over
 jsr CHKCOM+2    ;check for and skip over TO token, syntax error if not found
 jsr getvaluez   ;get 2-byte int in $14,$15
 jsr ckcom2      ;check for and skip over comma, misop err if missing
 jsr GETBYTC+3   ;get poke value
 stx $fe         ;set poke value
 lda #0
 sta $fd         ;set default poke type 0=SET,1=AND,2=OR,3=EOR
 jsr comchk      ;poke type param?
 bne gopoke
 jsr GETBYTC     ;get poke operation
 stx $fd
 cpx #4
 bcs baderr2
gopoke
 dec R6510
 jmp pokee
;
; ERR CLR    :clear last error data
; ERR OFF    :turn off error trapping
; ERR errnum :raise error (1-35)
error
 cmp #TOKEN_CLR
 beq errclr
 cmp #TOKEN_OFF
 bne raiseerr
erroff
 jsr detrap
errclr
 jsr CHRGET
clearerr
 ldy #0
 sty errnum
 dey           ;y is now #$FF
 sty errline   ;make last error line -1
 sty errline+1
 rts
raiseerr
 jsr getvalz  ;valid error number is 1-127
 bmi baderr2  ;128 and over is invalid
 tax
 jmp (IERROR)
;
baderr2 jmp FCERR   ;ILLEGAL QUANTITY
;
;*******************
; RENUM            :use defaults, start at 10 inc by 10
; RENUM start      :start line specified, default inc 10
; RENUM start, inc :use both start and inc specified
renum
 bne renumm   ;param1 specified
 lda #10      ;no params, set default
 sta $35      ;start at line 10, inc by 10
 sta $33
 lda #0
 sta $36
 jmp hiinc
renumm
 jsr LINGET   ;convert ascii to binary
 lda $14
 sta $35
 lda $15
 sta $36
 jsr comchk
 beq gaiv     ;increment specified
 lda $35
 sta $33
 lda $36
 jmp hiinc
gaiv jsr CHRGET
 jsr LINGET   ;get inc value (ascii to binary)
 lda $14
 sta $33
 ora $15
 beq baderr2  ;increment of 0 not allowed
okinc lda $15
hiinc sta $34
 jsr RUNC
 dec R6510
 jsr renumer
 inc R6510
;end of renum; list any go tokens with 65535 as line number (errors)
 jsr old2
 jsr find ;find all 65535
 jmp relink
;
evalnum
 inc R6510
 jsr FRMNUM
 jsr GETADR
 dec R6510
 rts
;
fltstr
 inc R6510
 ldx #$90
 sec          ;flag for unsinged int
 jsr $bc49    ;convert 2-byte int in FAC1 to float
 jsr FOUT+2   ;convert FAC1 to string without leading space
 dec R6510
 rts
;
tofac
 lda $35
 sta $63
 lda $36
 sta $62
runcc
 inc R6510
 jsr RUNC     ;reset TXTPTR to beginning of program
 dec R6510
 rts
;
chrget ldy #$00
 inc TXTPTR
 bne ne7a
 inc TXTPTR+1
ne7a lda (TXTPTR),y
 rts
;
;*******************
; SWAP A, B    SWAP A%, B%    SWAP A$, B$
swap
 beq mop4
 jsr PTRGET    ;get param1
 sta $14
 sty $15
 lda VALTYP    ;data type string or numeric
 sta $fd       ;save param1 data type
 lda INTFLG    ;numeric type 128=int, 0=float
 sta $fe       ;save param1 numeric type
 jsr ckcom2    ;check for and skip over comma, misop err if missing
 jsr PTRGET    ;get param2
 lda INTFLG    ;param2 numeric type, int or float
 cmp $fe       ;does param2 have the same numeric type as param1?
 bne nomtch    ;mismatch
 ldx $fd       ;param1 numeric type, int or float
 cpx VALTYP    ;does param2 have the same num/string type as param1
 bne nomtch    ;mismatch
 lda #1
 inx           ;$FF=string so $FF+1 = 0
 beq isstr     ;string uses 3 bytes 0-2
 ldx INTFLG    ;int or float?
 bne isint     ;int uses 2 bytes 0-1
 asl           ;float uses 5 bytes 0-4
isstr asl
isint tax      ;hold that value
 tay
cpyvar lda ($14),y ;save param1 in FAC2
 sta $0069,y
 dey
 bpl cpyvar
 txa
 tay
cpyvr2 lda ($47),y ;param2->param1
 sta ($14),y
 dey
 bpl cpyvr2
 txa
 tay
faccpy lda $0069,y ;param1->param2
 sta ($47),y
 dey
 bpl faccpy
 rts
mop4 jmp missop
nomtch jmp TMERR  ;TYPE MISMATCH ERROR
;
;*******************
; CLOSE filenum               -close a single file number (CBM BASIC)
; CLOSE filenum1 [,filenum2]. -close mutiple file numbers
; CLOSE FILES                 -close all open files
close
 beq mop4
 cmp #TOKEN_FILES
 beq clsfiles
gfn jsr getval    ;get file number
 beq nxtcls       ;ignore zero file number (invalid)
 pha              ;save file number
 jsr CLOSE        ;close file if open, ignore if not
 pla              ;recall file number
 cmp CHANNL       ;if closing the cmd i/o channel then
 bne nxtcls       ;clear the cmd i/o channel
 jsr clscmd       ;and restore default devices
nxtcls
 jsr chkcomm      ;check for comma, quit if none otherwise skip over it
 jmp gfn          ;process next file number
clsfiles
 jsr CLALL        ;close all open files and restore default devices
 lda #0           ;clear the cmd i/o channel
 sta CHANNL
 jmp CHRGET
;
;*******************
; ON ERR GOTO line
; ON ERR RESUME NEXT
; ON KEY GOSUB line
; ON i GOTO|GOSUB line1,line2,...
baderr jmp SNERR  ;syntax err
;
on
 beq mop4
 cmp #TOKEN_ERR
 beq onerror
 cmp #TOKEN_KEY
 beq onkey
 jmp ONGOTO   ;perform ON
onkey
 jsr CHRGET
 cmp #TOKEN_GOSUB
 beq onkeygo
 cmp #TOKEN_OFF
 bne baderr
 jmp onkeyoff
onkeygo
 jsr getline
 stx TMPKEYP  ;of the line# specified
 sty TMPKEYP+1
 lda $14
 sta keyline
 lda $15
 sta keyline+1
 lda #1
 sta keyflag  ;turn on key trapping
 rts
onerror
 jsr CHRGET
 cmp #TOKEN_GOTO
 beq errgoto
 cmp #TOKEN_RESUME
 beq onerres
 cmp #TOKEN_OFF
 bne baderr
 jmp erroff
onerres
 jsr CHRGET
 cmp #TOKEN_NEXT
 bne baderr
 ldx #<resumenext  ;apply ON ERR RESUME NEXT
 ldy #>resumenext  ;so that all errors will be ignored
 jsr seterrvec     ;and failed statement are skipped
 jmp CHRGET
errgoto jsr getline
 stx TMPERRP       ;of the line# specified
 sty TMPERRP+1
 lda $14
 sta errtrap
 lda $15
 sta errtrap+1
entrap         ;enable error trapping
 ldx #<trap
 ldy #>trap
 bne seterrvec ;hibyte of vector will always be non-zero
lstrap
 ldx IERROR
 ldy IERROR+1
 stx TMPERR
 sty TMPERR+1
 ldx #<retrap
 ldy #>retrap
 bne seterrvec ;hibyte of vector will always be non-zero
retrap
 ldx TMPERR
 ldy TMPERR+1
 bne seterrvec
detrap         ;disable error trapping
 ldx #<errors
 ldy #>errors
seterrvec
 stx IERROR
 sty IERROR+1
 rts
;
;*******************
;get BASIC line number ($14,$15) and text ptr-1 in X,Y
getline
 jsr CHRGET
 jsr LINGET        ;convert an ascii # to 2 byte int
getlin jsr FINDLN  ;search for line#
 bcc undef
 ldy $60
 ldx $5f
 bne dec5f2
 dey
dec5f2 dex
 rts
undef jmp UNDEFST   ;UNDEF'D STATEMENT
;*******************************************
; error trap routine for ON ERR RESUME NEXT
;*******************************************
resumenext
 txa
 bmi quitrun
 ldy CURLIN+1  ;when current line num hibyte is $FF (invalid)
 iny           ;then immediate mode
 beq quitrun
 stx errnum    ;update last error number
 lda CURLIN+1  ;update last BASIC line# causing error
 sta errline+1
 lda CURLIN
 sta errline
 jsr DATA      ;scan for start of next BASIC stmt
 jmp nxtstmt
;
quitrun
 jsr detrap    ;disable error trapping
 jmp READY     ;use normal error routine
;*******************************************
; general error trap routine
;*******************************************
trap
 txa           ;x holds the error num
 bmi quitrun   ;bit7, 0=error, 1=switch to immediate mode
 ldy CURLIN+1  ;when current line num hibyte is $FF (invalid)
 iny           ;then immediate mode
 beq quitrun
 stx errnum    ;set current error number
 jsr detrap    ;disable error trapping
 lda #3        ;3 plus the 2 for this jsr is 5 bytes
 jsr GETSTK    ;ensure space on stack, out of mem err if not
 lda OLDTXT+1  ;save the BASIC text ptr
 pha           ;of the beginning of the stmt
 lda OLDTXT    ;that caused the error
 pha           ;and save the BASIC
 lda CURLIN+1  ;line# for resume
 sta errline+1
 pha
 lda CURLIN
 sta errline
 pha
 lda #TOKEN_ERR
 pha
 lda TMPERRP
 sta TXTPTR
 lda TMPERRP+1
 sta TXTPTR+1
 lda errtrap
 ldy errtrap+1
 jmp setline
;
;*******************
; RESUME
; RESUME NEXT
; RESUME line#
resume
 pla          ;discard calling subroutine
 pla
 tsx
 lda BAD+1,x
 cmp #TOKEN_ERR
 beq okresu
 jsr detrap
 ldx #35      ;can't resume error
 jmp (IERROR)
okresu
 jsr clearerr ;clear last error info
 jsr CHRGOT
 beq resum    ;no token or digit? resume with statement that caused the error
 cmp #TOKEN_NEXT
 bne resume0
;perform RESUME NEXT - next statement after the one that caused the error
 pla          ;discard ERR token
 pla
 sta CURLIN   ;pull line number from stack and make current
 pla
 sta CURLIN+1
 pla
 sta TXTPTR   ;pull text ptr from stack and make current
 pla
 sta TXTPTR+1
 ldy #0       ;the first stmt on line will begin
 lda (TXTPTR),y  ;at the end marker of previous line.
 bne skpstmt  ;zero here indicates previous line is preceded by the 4-byte
 lda #4       ;line header which will be skipped over so that TXTPTR is on
 jsr DATA+4   ;the byte that began the stmt that caused the error
skpstmt
 jsr CHRGET   ;get current char at TXTPTR
 jsr DATA     ;scan for start of next BASIC stmt
 jmp nxtstmt0 ;setup next stmt for execution
;perform RESUME line#
resume0
 pla          ;discard ERR token
 pla          ;discard line number that caused the error
 pla
 pla          ;discard txt ptr of error
 pla
 jsr CHRGOT
 jsr GOTO     ;perform goto (adjust txt ptr to given line num)
nxtstmt0
 jsr entrap   ;re-enable error trapping
nxtstmt       ;prepare next stmt for execution
 ldy #0
 lda (TXTPTR),y
 bne _a807
 ldy #2
 lda (TXTPTR),y
 bne setlin
 jmp endprg    ;return control to main BASIC loop
setlin iny
 lda (TXTPTR),y
 sta CURLIN
 iny
 lda (TXTPTR),y
 sta CURLIN+1
 jsr DATA+3   ;advance TXTPTR using offset in y reg
;empty stack to where the base call was made
pullit
 pla
pullit2
 tsx
 cpx #$ff
 beq stoppull
 cmp #<xcmd+2 ;is the point where last command was executed
 bne pullit   ;in the main MDBASIC loop via jsr tstcmd
 pla          ;keep going till 2-byte ptr is found
 cmp #>xcmd+2
 bne pullit2
stoppull
 lda #$19     ;25=empty temp string index value
 sta $16      ;reset temp string stack
 lda #0       ;clear SUBFLG, 0=normal var else var is array or user FN
 sta $10
 jmp (IGONE)  ;read and execute the next statement
_a807 cmp #$3a
 beq pullit
 jmp REM
;
;perform RESUME - with statement that caused the error
resum
 pla          ;discard ERR token
 pla          ;pull line number from stack and make current
 sta CURLIN
 pla
 sta CURLIN+1
 pla          ;pull text ptr from stack and make current
 sta TXTPTR
 pla
 sta TXTPTR+1
findstart
 jsr CHRGOT
 beq nxtstmt0
 lda TXTPTR
 bne bbbb
 dec TXTPTR+1
bbbb dec TXTPTR
 jmp findstart
;
;*******************
;RETURN [line#]
return beq oldrtn
 pla          ;discard call to this subroutine
 pla
 tsx
 lda BAD+1,x
 cmp #TOKEN_GOSUB
 beq resume0
 jmp NOGOSUB  ;RETURN WITHOUT GOSUB
oldrtn jmp RETURN+2
;
;*******************
; MOVE sprite#, x1, y1 [TO x2, y2, speed]
; MOVE sprite# TO x2, y2, [speed]
move
 jsr sprnum    ;get sprite# and 2^sprite# ($bf)
 tya           ;sprite number 0-7
 asl           ;convert to 2-byte index for registers
 sta GARBFL    ;sprite# * 2
 jsr CHRGOT
 cmp #TOKEN_TO
 bne getfrom
;get current x and y coordiates for move starting point
 ldy GARBFL    ;sprite# * 2
 lda SP0Y,y    ;get current y coord for sprite
 sta $fd
 lda SP0X,y    ;get current x coord for sprite
 sta $fb
 lda $bf       ;2^sprite#
 and MSIGX     ;get msb of x coordinate
 beq msbx
 lda #1        ;hibyte for x coord in y reg
msbx sta $fc
 jmp moveto
getfrom
 jsr CHRGET
 cmp #","      ;comma means skip over x param
 beq gety
 jsr getvalue
 sty $fc       ;hibyte of x coordinate
 beq bitof     ;msb off
 cpy #2        ;x coordinate hibyte can only be 0 or 1
 bcs badxy
 lda MSIGX     ;Most Significant Bits of Sprites 0-7 Horizontal Position
 ora $bf       ;2^sprite#
 bne msb       ;always branches
bitof lda $bf  ;sprite register offset 2^sprite#
 eor #$ff
 and MSIGX
msb sta MSIGX  ;x coord hibyte
 ldy GARBFL    ;sprite# * 2
 lda $14
 sta SP0X,y    ;sprite x coord
 sta $fb
gety
 jsr chkcomm
 jsr getvalue  ;y2 coord
 bne badxy     ;hibyte must be 0
 txa
 ldy GARBFL    ;sprite# * 2
 sta SP0Y,y    ;sprite y coord
 sta $fd
 jsr CHRGOT
 cmp #TOKEN_TO
 beq moveto    ;TO token not present so we are done
 rts
badxy jmp hellno
moveto
 lda $bf       ;temp var holding 2^sprite# value
 sta $07       ;temp var for moving sprite on a line
 jsr getvalueg ;get x2 coordinate
 cpy #2        ;must be between 0 and 511
 bcs badxy     ;illegal coordinate error
 stx $50       ;point2 x coordinate lobyte
 sty $51       ;point2 x coordinate hibyte
 jsr ckcom2    ;check for and skip over comma, misop err if missing
 jsr getvalue  ;get y2 coordinate
 bne badxy     ;hibyte must be 0
 stx $52       ;point2 y coordinate
;getspeed
 lda #20
 sta $fe       ;default speed is 20
 jsr comchk
 bne nosped    ;no move speed specified?
 jsr getvalg   ;get the speed param 0-255
 sta $fe       ;temp storage for move speed
nosped lda #1  ;0=draw line, 1=move sprite
 jmp strtln    ;calculate line and move sprite along that line at given speed
;
spriteon
 dey            ;0 dec to 255 for all sprites
spriteoff
 sty SPENA
 jmp CHRGET
;
sprexp
 jsr chrget
 cmp #TOKEN_AND
 bne badspr
 lda #$ff    ;all sprites
 sta $bf
 jsr CHRGET
 jmp sprsize+3
;
badspr jmp SNERR
;
;*******************
;SPRITE ON | OFF
;SPRITE EXPAND ON | OFF
;SPRITE sprite# EXPAND size
;SPRITE sprite# DATA index
;SPRITE sprite#, [visibile] [,color] [,colormode] [,index] [,priority] [,size]
sprite
 cmp #TOKEN_EXP
 beq sprexp
 ldy #0
 cmp #TOKEN_OFF
 beq spriteoff
 cmp #TOKEN_ON
 beq spriteon
;get sprite number
 jsr sprnum     ;sprite# returned in $be and 2^sprite# in $bf
 jsr CHRGOT
 cmp #TOKEN_DATA
 bne chkexp
 jsr CHRGET
 jmp sprptr    ;get sprite data index
chkexp
 cmp #TOKEN_EXP
 bne sprvis
 jsr chrget
 cmp #TOKEN_AND
 bne badspr
 jsr CHRGET
 jmp sprsize+3  ;get sprite expansion size
sprvis
 jsr ckcom22
 cmp #","       ;get next char and compare to comma
 beq scr        ;another comma so skip param
;get sprite visibility 0=off, 1=on
 jsr getbool
 bne spron
 lda $bf        ;2^sprite#
 eor #$ff
 and SPENA
 jmp onoff
spron lda SPENA ;turn sprite on
 ora $bf
onoff sta SPENA
scr jsr chkcomm
 cmp #","
 beq smcr
;get sprite color 0 to 16
 jsr getval
 cmp #16        ;special color 16?
 bcc color15    ;no, regular colors
 bne illqty9    ;over 16 illegal
 lda #%00001000 ;enable special color 16
 jsr irqon      ;using IRQ
 lda $bf        ;2^sprite#
 ora sprcolopt
 jmp setspropt
color15
 ldy $be        ;sprite# 0-7
 sta SP0COL,y   ;sprite y's color
 lda $bf        ;ensure irq flag for sprite is off
 eor #$ff
 and sprcolopt
setspropt
 sta sprcolopt
smcr jsr chkcomm ;stop now if no more params
 cmp #","
 beq spntr
;get multicolor flag 0 or 1
 jsr getbool
 bne setm
 lda $bf        ;2^sprite#
 eor #$ff       ;sprite# bit off
 and SPMC       ;sprite multicolor flags
 jmp skipmc
illqty9 jmp FCERR
setm lda SPMC
 ora $bf        ;2^sprite#
skipmc sta SPMC
spntr
 jsr chkcomm    ;if no more params, quit now else check for and skip over comma
 cmp #","       ;if another comma then omit param
 beq prorty
;get sprite data ptr 0-255 (ptr*64)=start address
sprptr
 jsr getval

;determine VIC-II base addr
 jsr ptrhi      ;get hibyte of sprite ptr start address
 sta $62        ;sprite pointers are in the last 8 bytes of 1K screen RAM
 lda #$f8       ;lobyte of offset to first byte of sprite data ptrs
 sta $61        ;ptr to first sprite ptr, ie bank 0 with 1K offset is $07F8

;apply ptr param
 lda $14        ;sprite ptr from cmd param
 ldy $be        ;sprite# 0-7
 sta ($61),y    ;sprite y's data ptr

prorty
 jsr chkcomm    ;check for comma, if end of statement then do not return here
 cmp #","       ;if another comma then omit param
 beq sprsize
;get sprite to foreground graphics/text priority: 0=over, 1=under
 jsr getboolz
 bne okpri
 lda $bf        ;2^sprite#
 eor #$ff       ;prepare to turn off bit for sprite
 and SPBGPR     ;turn off bit for sprite
 sta SPBGPR     ;apply new value
 jmp sprsize
okpri lda $bf   ;2^sprite#
 ora SPBGPR     ;turn on bit for sprite
 sta SPBGPR     ;apply new value

sprsize
 jsr chkcomm    ;check for comma, if end of statement then do not return here
 jsr getvalz    ;size 0-3
 lsr
 tax
 bcs a1
 jsr clrx       ;values 0,2
 jmp a2
a1 jsr magx     ;values 1,3
a2 txa
 lsr
 bne illqty9    ;values more than 3
 bcs a3
 jmp clry       ;values 0,1
a3 jmp magy     ;values 2,3
;
magy ldy #0
 .byte $2c
magx ldy #6
;load y reg with xpand reg offset 0=Y, 6=X
 lda YXPAND,y
 ora $bf        ;2^sprite#
 bne setmag     ;always branches
clry ldy #0
 .byte $2c
clrx ldy #6
;load y reg with xpand reg offset 0=Y, 6=X
 lda $bf
 eor #$ff
 and YXPAND,y
setmag
 sta YXPAND,y
 rts
;
;*******************
;MULTI [TEXT] [cc1], [cc2]         - multicolor text mode
;MULTI COLOR  [eb1], [eb2], [eb3]  - extended background color mode
;MULTI SPRITE [sc1], [sc2]         - multicolor sprite mode, bit patterns 01,11
multi
 cmp #TOKEN_SPRITE
 beq mcspri
 cmp #TOKEN_COLOR
 bne chrmap
;MULTI COLOR [eb1], [eb2], [eb3]
 lda SCROLY        ;horiz fine scrolling and control reg
 ora #%01000000    ;bit 6=1 to enable extended background color mode for text
 sta SCROLY        ;Vertical Fine Scrolling and Control Register
 jsr CHRGET
 cmp #","
 beq geteb2
 jsr getval15      ;eb1
 sta BGCOL1        ;ext bkgnd color reg#1
geteb2
 jsr chkcomm       ;check for comma and don't return here if missing
 cmp #","
 beq geteb3
 jsr getval15      ;eb2
 sta BGCOL2        ;ext bkgnd color reg#2
geteb3
 jsr chkcomm
 jsr getval15      ;eb3
 sta BGCOL3        ;ext bkgnd color reg#3
 rts
;MULTI SPRITE [sc1], [sc2]
mcspri
 jsr CHRGET
 cmp #","
 beq getsc2
 jsr getval15      ;sc1
 sta SPMC0         ;mcspr reg#0
getsc2
 jsr chkcomm
 jsr getval15      ;sc2
 sta SPMC1         ;mcspr reg#1
 rts
;MULTI [TEXT] [cc1], [cc2]
chrmap
 cmp #TOKEN_TEXT
 bne getcc1
 jsr CHRGET     ;skip over TEXT token
getcc1
 pha
 lda SCROLX     ;horiz fine scrolling and control reg
 ora #%00010000 ;turn on bit 4 - enable multi color text or bitmap mode
 sta SCROLX
 pla
 cmp #","
 beq getcc2
 jsr getval15   ;cc1
 sta BGCOL1
getcc2
 jsr chkcomm    ;check for and skip over comma, if missing then exit cmd
 jsr getval15   ;cc2
 sta BGCOL2
 rts
;
;*****************
mop6 jmp missop    ;missing operand error
;
;*******************
; COLOR [foregndColor (0-31)], [backgndColor (0-15)], [borderColor (0-15)]
color
 beq mop6
 cmp #","
 beq nochar
 jsr getval15
 sta COLOR      ;current cursor foreground color
nochar
 jsr chkcomm    ;if no more params then stop now, otherwise get next char
 cmp #","       ;another comma?
 beq noback
 jsr getval15
 sta BGCOL0     ;background color
noback jsr chkcomm
 jsr getval15
 sta EXTCOL     ;border color
 rts
;
;*****************
designon
 jsr CHRGET
;select video matrix and dot data offsets
 lda #2         ;page 2, offset $0800
setpage         ;valid page index 0 to 3
;set hibyte ptr for Kernal prints
 asl            ;each page is 4 blocks of 256 bytes (1K)
 asl
 ora #%11000000 ;video matrix base is at $c000
 sta HIBASE     ;page 0=$c0, 1=$c4, 2=$c8, 3=$cc
;set video matrix and char dot data address offsets
 asl            ;discard bits 6,7
 asl            ;while moving bits 2,3 to positions 4,5
                ;bits 4-7 video matrix offset (4*page*1K)
 ora #%00001100 ;bits 1-3 char dot data offset (6*1K) = $1800
 sta VMCSB      ;bit 0 unused
;select VIC-II 16K mem bank 3 ($c000-$ffff)
 lda #%11111100 ;bits 0-1 mem bank, 00=bank3, 01=bank2, 10=bank1, 11=bank0
 and CI2PRA
 sta CI2PRA     ;base address is now $c000
;turn off multicolor text/bitmap mode
 lda #%11101111 ;bit 4 off disables multicolor text/bitmap mode
 and SCROLX
 sta SCROLX
;turn off bitmap mode
 lda #%11011111 ;bit 5 off disables bitmap mode
 and SCROLY
 sta SCROLY
 rts

;*****************
; DESIGN ON
; DESIGN OFF
; DESIGN NEW
; DESIGN scancode, charset, d0,d1,d2,d3,d4,d5,d6,d7
design
 beq mop6
 cmp #TOKEN_ON
 beq designon
 cmp #TOKEN_OFF
 beq designoff
 cmp #TOKEN_NEW
 bne dodesign
;copy CHAREN ROM into HIRAM (std c64 char dot data)
 lda #$f0    ;target location $f000-$ffff
 sta $bc
 lda #$00
 sta $bb
 sta $be
 lda #$d0    ;source location 4K CHAREN at $d000-$dfff
 sta $bf
 lda R6510
 pha
 and #%11111011 ;bit2=0 switch out device I/O for CHAREN ROM bank $d000-$dfff
 sei
 sta R6510
nex256 ldy #0
nexbyt lda ($be),y
 sta ($bb),y
 iny
 bne nexbyt
 inc $bf
 inc $bc
 bne nex256
 pla
 sta R6510     ;back to normal
 cli
 jmp CHRGET
;
dodesign
 jsr getval    ;get screen code
 jsr times8    ;multiply A reg value by 8 result in $be,$bf
 lda #$f0      ;charset 0 at $f000
 jsr getchrset
 clc
 adc $bf
 sta $bf
 ldy #$00      ;loop for all 8 bytes of data
gtdata
 sty GARBFL
 jsr ckcom2    ;check for and skip over comma, misop err if missing
 jsr getval
 ldy GARBFL
 sta ($be),y
 iny
 cpy #8
 bne gtdata
 rts

designoff jsr norm
 jmp CHRGET

;*******************
;
bitmapclr
 lda #$e0  ;8K bitmap $e000-$ffff
 sta $63
 lda #$00
 sta $62
 tay
clrbyt sta ($62),y
 iny
 bne clrbyt
 inc $63
 bne clrbyt
;apply background color in screen RAM bytes
 lda mapcolc1c2     ;last set background color
setbmcol
 ldy #0
pokcol sta $c800,y  ;fill color mem for entire screen
 sta $c900,y
 sta $ca00,y        ;1000 bytes, not 1024 bytes
 sta $cb00-24,y     ;this overlaps to prevent clearing sprite ptrs
 iny
 bne pokcol
 rts
;
;BITMAP CLR
;BITMAP FILL x1,y1 TO x2,y2, [plotType], [color]
;BITMAP ON
;BITMAP OFF
;BITMAP [colorMode], [bkgndColor], [clear]
;colorMode 0=hires, 1=multicolor (mc)
;bkgndColor is applied based on colorMode:
;clear 0=no clear, 1=clear
;hires mode sets color RAM in video matrix with init of all 1000 bytes
;mc mode sets the single bkgrnd color register BGCOL0
;MAPCOL c1,c2,c3 (c3 mc mode only) to change colors:
;hires c1 (0-15) dot color, c2 (0-15) 8x8 square bkgnd color
;multicolor uses dual plotted bit pattern to select the color:
;  00 from BGCOL0 (0-15)
;  01 from video matrix hi-nybble: c1 (0-15)
;  10 from video matrix lo-nybble: c2 (0-15)
;  11 from color RAM lo-nybble:    c3 (0-15)
;when in mc mode all graphics cmds use color index 1-3 instead of color 0-15
;as a color parameter to select the color from tri-color pallete
;
bitmap
 beq mop7
 cmp #TOKEN_CLR
 beq bmclr
 cmp #TOKEN_OFF
 beq designoff
 cmp #TOKEN_ON
 beq bmon
 cmp #TOKEN_FILL
 bne bitscr
; BITMAP FILL x1,y1 TO x2, y2, plotType, color
 jsr CHRGET
 jsr getxy          ;get point1
 jsr point2         ;get point2
 jsr types          ;get plot type and color
 dec R6510
 jmp bitfil         ;perform FILL on rect; put code under ROM
;
bmon jsr bitmapon
 jmp CHRGET
bmclr jsr bitmapclr
 jmp CHRGET
;
mop7 jmp missop
;
;BITMAP [colorMode], [bkgndColor], [clear]
bitscr
 jsr getbool        ;colorMode 0 or 1
 beq hiresmode
 lda SCROLX         ;horiz fine scrolling and control reg
 ora #%00010000     ;turn on bit 4 - enable multi color text or bitmap mode
 sta SCROLX
;get bkgndColor for mc mode
 jsr comchk
 bne bitmapon       ;no more params so turn bitmap mode on
 jsr getval15g
 sta BGCOL0         ;color for 00 bit pair
 jmp bclr
hiresmode
 lda SCROLX         ;turn off mulicolor mode
 and #%11101111     ;set bit#4 0=off, 1=on
 sta SCROLX         ;apply setting to control register
;get bkgndColor for hires mode
 jsr comchk
 bne bitmapon
 jsr CHRGET
 jsr getc2          ;background color for all 8x8 squares
 jsr setbmcol       ;apply to all 8x8 squares
;get clear param
bclr
 jsr comchk
 bne bitmapon       ;no more params so turn bitmap mode on
 jsr getboolg
 beq bitmapon
 jsr bitmapclr
;
bitmapon
 lda C2DDRA         ;data direction for port A (CI2PRA)
 ora #%00000011     ;bits 0-1 are set to 1=output (default)
 sta C2DDRA
 
 lda #%11000100     ;bits 0-1 VIC-II 16K memory bank 00=bank3 ($C000-$FFFF)
 sta CI2PRA         ;send bits out data port register

 lda SCROLY         ;turn on bitmap graphics mode
 ora #%00100000     ;bit#5 1=on, 0=off
 sta SCROLY         ;apply setting to control register
 lda #%00101100     ;bit 0 unused
                    ;bits 1-3 text dot-data base offset of 6K $c000+6K=$d800
                    ;bits 4-7 video matrix base offset of 2K $c000+2K=$c800
 sta VMCSB          ;apply setting to control register
 rts
;
;*******************
;
;MAPCOL changes the default colors to be used when plotting dots on a bitmap
;In hires mode (c1,c2):
; c1 plot color (0-15) of dot in 8x8 square (upper nybble from Video Matrix)
; c2 background color (0-15) of dot in 8x8 square (lower 4-bits in Color RAM)
;In multi color mode (c1,c2,c3):
;    00 Background Color Register 0 BGCOL0 ($D021)
; c1 01 Upper nybble of Video Matrix (scan code)
; c2 10 Lower nybble of Video Matrix (scan code)
; c3 11 Lower nybble of Color RAM for Video Matrix ($D800-$DBE8)
;hires example:
;mapcol 0,1  is dot color black, background (of 8x8 square of dot) white
;multicolor example:
;mapcol 0,1,2 is bit patterns 01=black, 10=white, 11=red (00 is BGCOL0)
mapcol
 beq mop7
 cmp #","
 beq getc2a
 jsr getc1
getc2a
 jsr chkcomm
 cmp #","
 beq getc3
 jsr getc2
getc3
 jsr chkcomm
 jsr getval15    ;c3 (0-15) is used in multicolor mode only bit pattern 11
 sta mapcolc3    ;this color can be in the same 8x8 square that c1 & c2 are in
 rts
getc1
 lda mapcolc1c2  ;last plot color plotted
 and #%00001111  ;erase hi nybble
 sta XSAV        ;tmp storage
 jsr getval15    ;c1 (0-15) changes the color of the plotting dots
 asl             ;move low nybble to high nybble
 asl
 asl
 asl
 ora XSAV        ;apply new high nybble while keeping original low nybble
 sta mapcolc1c2  ;replace global variable storage for c1 (plot color)
 rts
getc2
 lda mapcolc1c2  ;again, get global variable storage but for low nybble
 and #%11110000  ;erase lo nybble
 sta XSAV        ;temp var for final byte value calculation
 jsr getval15    ;c2 (0-15) changes the background of the 8 x 8 square
 ora XSAV
 sta mapcolc1c2  ;update global variable for colors
 rts
;
;*******************
; PULSE voc#(1-3), width%(0-100)
pulse
 jsr getvoc
 sta $fe      ;save voice SID register offset (voice#-1)*7
 jsr ckcom2   ;check for and skip over comma, misop err if missing
 jsr FRMNUM   ;get width% (0.00 to 100.00)
 lda #<m4095  ;REGVAL=ROUND(40.95*WIDTH%) result in range 0-4095
 ldy #>m4095  ;y=hi byte, a=lo byte pointer to 5-byte FAC value
 jsr FMULT    ;multiply FAC1 by a value in memory ptr A=lo byte, Y=hi byte
 jsr doround  ;round FAC1 to nearest whole number
 jsr GETADR   ;convert FAC1 to 2-byte integer in Y (lobyte), A (hibyte)
 cmp #$10     ;0-4095 only (12-bit value)
 bcs badwav
 ldx $fe      ;register offset for voice
 sta PWHI1,x  ;Pulse Waveform Width (hi nybble)
 tya
 sta PWLO1,x  ;Pulse Waveform Width (lo byte)
 rts
;
;******************
;Voice Control Register Voice 1 $D404, Voice 2 $D40B, Voice 3 $D412
; Bit 0 Gate Bit 1=Start attack/decay/sustain, 0 = start release
; Bit 1 Sync Bit 1=Sync Oscillator with Oscillator 3
; Bit 2 Ring Modulation 1=Ring modulate Oscillators 1 and 3
; Bit 3 Test Bit 1 = Disable Oscillator
; Waveform parameter value:
; Voice Control Register bits 4-7 so this value is multiplied by 16
; 0 none
; 1 triangle
; 2 saw tooth
; 3 saw tooth + triangle
; 4 pulse 
; 5 pulse + triangle
; 6 pulse + saw tooth 
; 7 pulse + saw tooth + triangle
; 8 noise
;WAVE voice#, waveform, [gate], [sync], [ring], [disable]
;
badwav
 jmp FCERR
wave
 jsr getvoc    ;get voice SID register offset (voice#-1)*7
 pha           ;save SID register offset
 jsr ckcom2    ;check for and skip over comma, misop err if missing
 jsr getval    ;get waveform single byte operand value into $14
 cmp #9        ;valid values 0 to 8
 bcs badwav
 asl           ;move low nybble to high nybble
 asl
 asl
 asl
 sta $fe       ;waveform in bits 4-7
 jsr comchk
 bne waveit
 jsr getboolg  ;gate
 ora $fe       ;position is bit 0
 sta $fe
 jsr comchk
 bne waveit
 jsr getboolg  ;sync
 asl           ;position is bit 1
 ora $fe
 sta $fe
 jsr comchk
 bne waveit
 jsr getboolg  ;ring
 asl           ;position is bit 2
 asl
 ora $fe
 sta $fe
 jsr comchk
 bne waveit
 jsr getboolg  ;disable
 asl           ;position is bit 3
 asl
 asl
 ora $fe
 sta $fe
waveit
 pla
 tax
 lda $fe
 sta VCREG1,x ;apply setting for voice
 rts
;
;*******************
; VOL n   where n=0 to 15
vol
 jsr getval15z  ;only values from 0-15 allowed
 lda md418      ;get current volume (lo nybble) and resonance (hi nybble)
 and #%11110000 ;clear volume bits only
 ora $14        ;apply new volume bits only
setvol
 sta SIGVOL     ;SID register volume (lo nybble) and resonance (hi nybble)
setvol2
 sta md418      ;mock register for $d418 Volume and Filter Select Register
 rts
;
;subroutine to clear SID
sidclr
 ldy #$18       ;clear all 24 SID registers
 lda #0
 sta md417      ;mock register for $d417 Filter Resonance Control Register
clrsid sta FRELO1,y
 dey
 bpl clrsid
 bmi setvol2
;
;*******************
;NTSC and PAL hold the value of 1Hz (based on clock speed)
;REG_VAL=FREQUENCY/NTSC
;VOICE CLR
;VOICE voice#, frequency(0 to 3994.997 for NTSC machines)
voice
 php
 cmp #TOKEN_CLR  ;clr token?
 bne getfreq
 plp
 jsr sidclr
 jmp CHRGET
getfreq
 plp
 jsr getvoc      ;returns SID register offset (voice#-1)*7 in accumulator
 sta $fe
 jsr CHRGET
 jsr FRMNUM      ;convert current expression to a number and store in FAC1
 jsr MOVEF       ;copy FAC1 to FAC2 (numerator) frequency value
 lda PALNTSC     ;clock type 0=NTSC, 1=PAL
 beq is_ntsc
 lda #<pal
 ldy #>pal
 bne memfac      ;always branches since hibyte of ptr could never be 0
is_ntsc lda #<ntsc
 ldy #>ntsc
memfac jsr MOVFM ;copy mem to FAC1 pointed by a & y
 jsr FDIVT       ;FAC1 = (FAC2/FAC1)
 jsr doround     ;round FAC1 to nearest whole number
 jsr GETADR      ;convert FAC1 to unsigned 16-bit int
 ldx $fe
 sta FREHI1,x
 tya
 sta FRELO1,x    ;store result in data control reg for voice
done5 rts
;
;*******************
; DRAW S$
;P plot type (0-3) 0=erase,1=plot,2=flip,3=none
;C plot color (0-15 in hires, 1-3 in mc mode)
;U up
;D down
;L left
;R right
;E up & left
;F up & right
;G down & left
;H down & right
;
draw
 jsr getstr0
 beq done5
;save current txt ptr
 lda TXTPTR
 pha
 lda TXTPTR+1
 pha
;set txt ptr to string start
 lda $50
 sta TXTPTR
 lda $51
 sta TXTPTR+1
 jsr getpoint  ;get last plot coordinates
 jsr CHRGOT    ;get first char
drawloop
 cmp #"c"
 bne chkplottype
 jsr CHRGET
 jsr getc1
 jmp nxtmov
chkplottype cmp #"p"
 bne godraw
 jsr getvalg
 cmp #4         ;0-3 only
 bcs baddraw
 sta lastplott
 jmp nxtmov
godraw
 sta XSAV      ;save draw direction
 jsr getvalueg ;draw size
 txa
 ora $15       ;is both hi and lo bytes 0
 beq nxtmov    ;skip draw
 lda XSAV
 dec R6510
 jsr godraww
 inc R6510
 bcs nxtmov    ;draw cmd was valid
baddraw jmp SNERR  ;syntax error in draw string
nxtmov
 jsr comchk    ;another draw cmd?
 beq drawloop2
 jsr savepoint ;save final x and y coordinates plotted
 jmp pultxtptr
drawloop2
 jsr CHRGET
 jmp drawloop
;
;*******************
; PLOT x,y, [type], [color]
plot jsr getpnt
 jsr types
plotit dec R6510
 jsr setdot
memnorm
 inc R6510
 rts
;
; LINE x1,y1 TO x2, y2, plotType, color
liner
 jsr getxy
 jsr point2
 jsr types
 lda #0          ;0=draw line, 1=move sprite move
 jsr strtln
 jmp savepoint   ;save last plotted point
;
;entry point for commands PLOT,CIRCLE,PAINT,TEXT
getpnt
 jsr getxy      ;get x,y, plot type
savepoint
 ldx #2
 lda $fb,x
 sta lastplotx,x
 dex
 bpl savepoint+2
 rts
;
;draw line or move sprite on line
strtln
 sta XSAV  ;0=draw line, 1=move sprite move
 dec R6510
 jsr linedraw
 inc R6510
 rts
;
getpoint
 ldx #2
 lda lastplotx,x
 sta $fb,x
 dex
 bpl getpoint+2
 rts
;
point2
 jsr savepoint  ;save point1
 lda #TOKEN_TO
 jsr CHKCOM+2   ;skip over TO token, syntax error if not there
 jsr getxy      ;get point2
;set point1 and point2
 ldx #2
pntswp
 lda $fb,x
 sta $50,x      ;point2
 lda lastplotx,x
 sta $fb,x      ;point1
 dex
 bpl pntswp
 rts
;
;*******************
; LINE INPUT ["prompt",] S$ [,S2$,...,S3$]
; LLINE INPUT# filenum, S1$ [,S2$,...,S3$]
; LINE x1,y1 TO x2, y2, plotType, color
line
 beq mop3
 cmp #TOKEN_INPUTN ;input# token (line input#)
 bcc liner         ;line x,y to a,z
 beq lineinput
 cmp #TOKEN_INPUT  ;input token (line input)
 bne baddraw       ;SYNTAX ERROR
 jsr ERRDIR        ;if not in prg mode, illegal direct error
;check next required txt char without advancing TXTPTR
 ldy #1            ;should be a quote symbol or str var name
peekop
 lda (TXTPTR),y
 beq mop3          ;end of line terminator
 iny
 cmp #" "          ;skip over spaces
 beq peekop
 cmp #":"          ;end of statement terminator?
 beq mop3          ;sorry, parameter is required
;check for quoted prompt string
 cmp #"""
 bne readline
 jsr getstr   ;string is returned in registers y=hi byte, x=lo byte, a=length
 txa          ;x reg has low byte of str ptr but next func needs it in a reg
 jsr STROUT   ;print str whose addr is in y reg (hi byte) and a reg (lo byte)
 jsr comchk
 bne mop3
readline
 jsr INLIN    ;input a line into buffer from keyboard (80 chars max)
 ldy #$ff     ;count number of chars
fndend iny
 lda BUF,y
 sta paintbuf1,y
 bne fndend
inpend
 jsr outstr
 jsr comchk
 beq readline
 rts
; LINE INPUT# filenum, S1$ [,S2$,...,S3$]
lineinput
 jsr getvalg   ;get single byte param in a reg, misop err if missing
 sta $24
lnin
 ldx $24
 jsr inpstr
 jsr comchk
 beq lnin
 rts
;
mop3 jmp missop
;
;*******************
; CURSOR CLR           - clear cursor line
; CURSOR ON | OFF      - cursor on/off
; CURSOR [col], [row]  - set cursor coordiantes
cursor
 beq mop3
 cmp #TOKEN_ON
 beq csron
 cmp #TOKEN_OFF
 beq csroff
 cmp #TOKEN_CLR
 beq csrclr
 pha
 sec          ;flag for read
 jsr PLOT     ;read current position
 sty $fb      ;column
 stx $fc      ;line
 pla
 cmp #","
 beq row
 jsr getvalue ;get value as int: x=lobyte, y=hibyte
 bne hellno   ;hibyte must be zero
 cpx #40      ;40 is max column number
 bcs hellno
 stx $fb      ;new column
 jsr comchk   ;if 2nd param then get it
 bne column   ;else assume end of statement
row
 jsr getvalueg ;get value as int: ;x=lobyte, y=hibyte
 bne hellno   ;hibyte must be zero
 cpx #25      ;25 is max line number
 bcs hellno
 stx $fc      ;new line
column
 ldy $fb      ;column
 ldx $fc      ;line
 clc          ;clear carry is flag to write new value
 jmp PLOT     ;read/set cursor position on screen
csron
 lda #0       ;0=enabled
.byte $2c
csroff
 lda #1       ;1=disabled
 ldy BLNSW    ;is cursor disabled? 0=no, else yes
 bne setcsr   ;if yes then set its new value now
 iny          ;1 jiffiy
 sty BLNCT    ;set cursor remaining blink delay to 1 jiffy
csrwait       ;otherwise wait for it to blink off
 ldy BLNON    ;current blink state, 0=norm, 1=rvs
 bne csrwait  ;wait for cursor to blink off
setcsr
 sta BLNSW    ;cursor: 0=enable, 1=disable
 jmp CHRGET
;
csrclr
 ldx TBLX     ;current cursor line
 jsr $e9ff    ;clear line
 jmp CHRGET
;
hellno
 ldx #34      ;illegal coordinate error
 jmp (IERROR)
;
;get x,y coordinates
getxy
 beq mop3
 jsr getvalue   ;get x coordinate, returns lobyte in x, hibyte in y
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 beq chkpnt     ;hires mode
 txa            ;adjust offset to read ptab2/ptab3 for multicolor plot bits
 asl            ;multiply x coordinate by 2
 tax
 tya
 rol
 tay
chkpnt          ;validate the x coordinate
 tya
 beq okvalu
 cpy #2         ;valid value is 0 or 1 for x coord hibyte
 bcs hellno
 cpx #$40       ;valid values 0 to 63
 bcs hellno
okvalu stx $fb
 sty $fc
 jsr ckcom2     ;check for and skip over comma, misop err if missing
 jsr getvalue   ;x=lobyte, y=hibyte
 bne hellno     ;y reg has hibyte for y coordinate and must be zero
 cpx #200       ;x reg has lobyte for y coordinate, must be between 0 and 199
 bcs hellno
 stx $fd        ;y coordinate
 rts
;
;*******************
; PAINT x,y, [plotType], [color]
paint jsr getpnt
 jsr types
 dec R6510
 jmp painter
;
;*******************
; CIRCLE xcenter, ycenter, xsize, ysize, [options], [plottype], [color]
; options are represented in 8 bits grouped by nybbles:
; bits0-3: quadrant visible 0=no,1=yes, bits4-7: radius line visible 0=no,1=yes
circle
 jsr getpnt     ;center point x,y
 jsr ckcom2     ;check for and skip over comma, misop err if missing
 jsr getval     ;get x radius size
 beq illqty8    ;enforce valid range of 1 to 127
 bmi illqty8
 sta $35        ;save x-radius size to temp variable
 jsr ckcom2     ;check for and skip over comma, misop err if missing
 jsr getval     ;y radius size
 beq illqty8    ;enforce valid range of 1 to 127
 bmi illqty8
 sta $36        ;y radius variable
 lda #%00001111 ;default options
 sta $2a        ;variable to hold value
 jsr CHRGOT
 beq docircle
 jsr CHRGET     ;position for options param
 cmp #","
 beq circlept   ;skip if comma found
 jsr getval     ;get options value
 sta $2a        ;circle options
circlept
 jsr types      ;get optional params for changing plot type and color
docircle
;get multicolor mode flag
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 beq hirez
 asl $35
hirez sta $29   ;0=hires mode, 1=multicolor mode
 dec R6510      ;switch LOROM to LORAM
 jsr circel
 jsr ciropts
 inc R6510
 jmp clrfac
;
;*******************
norm
 lda #%11001000  ;display on, multicolor text/bitmap mode off
 sta SCROLX      ;40 columns, 0 horizontal scroll scan lines
 lda #%00011011  ;extended color text mode off, bitmap mode off
 sta SCROLY      ;display on, 25 rows, 3 vertical scroll scan lines
norm3
 lda C2DDRA      ;data direction for port A (CI2PRA)
 ora #%00000011  ;bits 0-1 are set to 1=output (default)
 sta C2DDRA
 lda CI2PRA
 ora #%00000011  ;select VIC-II 16K mem bank 0 ($0000-$4000)
 sta CI2PRA
 lda #$04        ;text page for kernal prints
 sta HIBASE      ;top page of screen mem for Kernal prints
 lda #%00010101  ;bit0 always 1; bits1-3 text dot data base addr in 1K chunks
 sta VMCSB       ;bits 4-7 video matrix base address in 1K chunks
norm2
 lda R6510
 ora #%00000111  ;switch mem banks to normal
 sta R6510       ;mem mapped I/O RAM ($d000-$dfff)
 rts             ;HIROM ($e000-$ffff), LOROM ($a000-$bfff)
;
illqty8 jmp FCERR     ;illegal quantity error

; TEXT x,y "string", [charset], [sizeX], [sizeY], [plotType], [color]
text
 beq norm
 jsr getpnt   ;get plot x,y
 jsr ckcom2   ;check for and skip over comma, misop err if missing
 jsr getstr0  ;get text string returning ptr in vector ($50)
 lda #1       ;default size value
 sta $57      ;temp var of x size
 sta $58      ;temp var of y size
 lda #$d0     ;assume charset 0 at $d000
 sta $26      ;charset hibyte temp var
 jsr comchk
 bne ne
 jsr getchrset+2
;sizes
 jsr comchk
 bne ne
 jsr getvalg
 cmp #32      ;max size is 31
 bcs illqty8
 sta $57      ;user specified x size
 jsr comchk
 bne ne
 jsr getvalg
 cmp #32      ;max y size is 31
 bcs illqty8
 sta $58      ;user specified y size
 jsr comchk
 bne ne
 jsr types
ne dec R6510
 jmp texter
;
screenoffg
 jsr CHRGET
screenoff
 lda SCROLY
 and #%11101111  ;bit4 = 0 screen off
 sta SCROLY
 rts
;
;*******************
; SCREEN CLR
; SCREEN ON|OFF
; SCREEN [page], [xoffset], [yoffset]
; page (0-4, initially 0, 0=normal text page, 1-4=redefined char mode)
; xoffset (0-15, initially 8) 0-7 horiz offset with 38 cols, 8-15 is 40 cols
; yoffset (0-15, initially 11) 0-7 vert offset with 24 rows, 8-15 is 25 rows
screen
 beq mop
 cmp #TOKEN_ON
 beq screenong
 cmp #TOKEN_OFF
 beq screenoffg
 cmp #TOKEN_CLR
 beq docls
 cmp #","
 beq xbits
 jsr getval      ;page 0-4, 0=norm
 bne page
 jsr norm3       ;select normal text screen
 jmp xbits
page
 cpy #5
 bcs illqty8
 dey
 tya
 jsr setpage
xbits jsr chkcomm
 cmp #","
 beq ybits
 jsr getval15    ;x offset and cols 38/40
 lda SCROLX
 and #%11110000
 ora $14
 sta SCROLX
ybits jsr chkcomm
 jsr getval15    ;y offset and rows 24/25
 lda SCROLY
 and #%11110000
 ora $14
 sta SCROLY
 rts
;
screenong
 jsr CHRGET
screenon
 lda SCROLY
 ora #%00010000  ;bit4 = 1 screen on
 sta SCROLY
 rts
;
docls jsr CLS
 jmp CHRGET
;

mop jmp missop
;
keyclr
 lda #0
 sta NDX
 jmp CHRGET
keylist dec R6510
 jmp keylistt
;
keyeq
 jsr getvalg
 jmp setkey
;
;*******************
; KEY LIST        -list current function key assignments
; KEY OFF         -turn off key trapping (enabled by ON KEY)
; KEY CLR         -clear the keyboard buffer
; KEY WAIT [var]  -wait for keypress, optionally put result in var of any type
; KEY GET var     -get key (or null) from buffer into var of any type
; KEY n, "string" -assign a function key n (1 to 8)
key
 beq mop
 cmp #TOKEN_CLR
 beq keyclr
 cmp #TOKEN_GET
 beq keyget
 cmp #TOKEN_WAIT
 beq keywait
 cmp #TOKEN_OFF
 beq onkeyoff
 cmp #TOKEN_LIST
 beq keylist
 cmp #TOKEN_EQUAL
 beq keyeq
 jsr FRMEVL     ;eval expression
 lda VALTYP     ;number or string?
 beq keynum     ;number is key assign
 jsr getstr2    ;get source string as first param
 beq kdone      ;do nothing if zero-length string
 ldy #0
cpystr2
 lda ($50),y    ;source param str
 sta paintbuf3,y
 iny
 cpy $52
 bne cpystr2    ;no, keep copying
 lda #0
 sta paintbuf3,y
 sta keyidx
 sty keystrflg  ;flag for manual key string
 lda #%00000100 ;key pump irq flag
 jmp irqon
;
illqty3 jmp FCERR
;
keynum
 jsr GETBYTC+6    ;convert FAC1 to an unsigned byte value 0-255 into X reg
 txa
 beq illqty3
 cmp #9           ;only func keys 1-8
 bcs illqty3
;assign function key
 pha              ;save key#
 jsr ckcom2       ;check for and skip over comma, misop err if missing
 jsr getstr0
 dec R6510
 pla
 jmp keyy
;
onkeyoff
 jsr CHRGET
keyoff
 lda #0
 sta keyflag
setkey
 sta keyentry
kdone rts
;
keyget
 jsr CHRGET
getkey
 jsr getvar
 ldx #0
 lda NDX
 beq setvar
;get byte in keyboard buffer
 sei
 jsr LP2
 ldx #1
setvar
 sta $61     ;store byte as lobyte in FAC1 for later conversion to float
 ldy $fd     ;offset to store result, strings use 0, int use 1, float n/a
 sta ($35),y ;use buffer if string, variable memory for int and float (n/a)
 stx $fd     ;when string type length is 0 or 1, otherwise not used
 jmp setval
;
keywait
 lda NDX
 beq keywait
 jsr CHRGET
 bne getkey
 rts
;
;*******************
; SCROLL x1, y1 TO x2, y2, [direction 0-3], [wrap 0-1]
scroll
 jsr getcoords
 lda #0
 sta $ff         ;default direction 0=up
 sta XSAV        ;default wrap 0=no wrap
 jsr comchk
 bne okscroll
 jsr getvalg     ;direction 0-3
 cmp #4
 bcs illqty3
 sta $ff
 jsr comchk
 bne okscroll
 jsr getboolg    ;wrap: 0=no, 1=yes
 sta XSAV        ;wrap param temp var
okscroll
 lda $ff         ;direction temp var
 asl             ;convert to 2 byte offset
 tay
 dec R6510
 lda scrolls+1,y ;scroll direction vector hibyte
 pha
 lda scrolls,y   ;scroll direction vector lobyte
 pha
 rts
;-----------
badscroll jmp hellno ;illegal coordinate error
getcoords
 jsr getvaluez   ;x1
 bne badscroll   ;hibyte should be 0
 cpx #40         ;max columns
 bcs badscroll
 stx $fb
 stx $be         ;columns to scroll
 jsr ckcom2      ;check for and skip over comma, misop err if missing
 jsr getvalue    ;y1
 bne badscroll   ;hibyte must be 0
 cpx #25         ;max rows
 bcs badscroll
 stx $bf         ;rows to scroll
 lda #TOKEN_TO   ;token to skip over
 jsr CHKCOM+2    ;check for and skip over TO token, syntax error if not found
;calc screen text and color ptrs
 dec R6510
 jsr calcptr
 inc R6510
;get x2,y2
 jsr getvaluez   ;x2
 bne badscroll   ;hibyte must be 0
 cpx #40
 bcs badscroll
 txa
 sec
 sbc $be
 sta $be
 jsr ckcom2      ;check for and skip over comma, misop err if missing
 jsr getvalue    ;y2
 bne badscroll   ;hibyte must be 0
 cpx #25
 bcs badscroll
 txa
 sec
 sbc $bf
 sta $bf
 rts
;
;*******************
; ENVELOPE voice#, attack, decay
; ENVELOPE voice#, attack, decay, sustain, release
adsr
 jsr getvoc
 sta XSAV      ;SID register offset for voice
 jsr ckcom2    ;check for and skip over comma, misop err if missing
 jsr getval15  ;attack
 asl
 asl
 asl
 asl
 sta $fe       ;attack duration
 jsr getval15g ;decay duration
 ora $fe
 ldx XSAV
 sta ATDCY1,x  ;apply attack and decay
;sustain and release params
 jsr chkcomm   ;if end of statement quit now
 jsr getval15  ;sustain volume
 asl
 asl
 asl
 asl
 sta $fe
 jsr getval15g ;release duration
 ora $fe
 ldx XSAV
 sta SUREL1,x  ;apply sustain and release
 rts
;
;******************
;FILTER cutoff, [resonance], [type]
;FILTER VOICE voice#, [boolean]
;
;The cutoff frequency has an 11-bit range (which corresponds to the
;numbers 0 to 2047).  This is made up of a high-byte and three low
;bits. The range of cutoff freqnencies represented by these 2048 values
;stretches from 30 Hz to about 12,000 Hz.
;The exact frequency may be calculated with the formula:
;FREQ=(REGVAL*5.8)+30Hz
;since we need to convert a freq to regval, rewrite expression:
;REGVAL = (FREQ-30)/5.8
;to simplify the math:
;REGVAL = (FREQ/5.8)-(30/5.8)  note 30/5.8=5.172...or just 5 (close enough)
;REGVAL = (FREQ/5.8)-5
;Summary of steps in code:
;1. get freq in FAC format
;2. divide by 5.8 using FAC DIV subroutine
;3. round up the value to whole number
;4. convert FAC value to a 2-byte binary value using FAC subroutine
;5. subtract 5 from binary value
;6. range check to ensure not larger than 2047, error if so
;7. store the result in SID registers
;
novoice
 jmp illvoc     ;illegal voice number
filter
 beq mop2
 cmp #TOKEN_VOICE
 bne getfreq1
;FILTER VOICE voice# [,boolean]
 jsr getvalg    ;get voice 1-4
 beq novoice
 cpy #5         ;4=external input via pin 5 of audio/video port
 bcs novoice
 dey            ;voice 1 to 4 -> index 0 to 3
 lda bitweights,y ;bit pattern for RESON register
 sta XSAV       ;remember for later
 jsr comchk     ;check if they supplied a boolean?
 bne filteron   ;missing boolean assumes on, syntax FILTER VOICE voice#
 jsr getboolg   ;get on/off expression, 0=off, 1=on
 bne filteron
 eor XSAV       ;flip all bits to turn off voice# bit
 and md417      ;all other bits will remain as they were
 jmp setfilter
filteron
 lda XSAV
 ora md417      ;affect only bit for voice
setfilter
 sta md417      ;remember this new setting for this register
 sta RESON      ;make setting active in SID register
 rts
; FILTER cutoff [,resonance [,type]]
getfreq1
 cmp #","
 beq reson
 jsr FRMNUM     ;convert current expression to a number and store in FAC1
 jsr MOVEF      ;copy FAC1 to FAC2 (numerator) frequency value 
 lda #<five8
 ldy #>five8
 jsr MOVFM      ;copy mem to FAC1 pointed by a & y
 jsr FDIVT      ;FAC1 = (FAC2/FAC1)
 jsr doround    ;round FAC1 to nearest whole number
 jsr GETADR     ;convert FAC1 to unsigned 16-bit int
;now subtract 30/5.8 = 5.172...or just 5 (close enough)
 tax            ;hibyte
 tya            ;lobyte
 sec            ;subtract 5 from 2-byte binary value
 sbc #5         ;to complete freq conversion formula
 tay            ;save lobyte
 txa            ;hibyte
 sbc #0
 bmi illqty5    ;if bit 7 in hibyte set then we rolled over from 0
;range check register value max from 0-2047 allowed
 cmp #8         ;2048 or larger is above SID's range
 bcc okfilt
illqty5 jmp FCERR ;illegal qty err
mop2 jmp missop
;
;the SID filter frequency register is an 11-bit value stored in a wacky way
;  hibyte    lobyte   Frequency binary value from parameter
; *****111  11111111  Possible bits used, *=not used since out of range
;  $d416     $d415    Target Filter frequency registers
; 11111111  XXXXX111  Target bits to map from parameter value, X=not used
;
;shift hibyte left 5x to promote lower 3 bits to highest 3 bits, fill with 0
okfilt
 asl
 asl
 asl
 asl
 asl
 sta XSAV   ;save hibyte
;store lobyte in SID lower byte register (bits 3-7 will be ignored by SID)
 tya        ;lobyte
 sta CUTLO  ;since upper 5 bits are not used by SID, no need to set them to 0
;shift lobyte right 3 times to demote upper 5 bits
 lsr
 lsr
 lsr
;merge upper and lower bits as hibyte and store in SID upper byte register
 ora XSAV
 sta CUTHI
;
;get the resonance parameter (if present)
;set the filter resonance control register $d417
;bits 4-7 set filter resonance 0-15, 0=none, 15=max
reson
 jsr chkcomm  ;if no more params then quit otherwise continue
 jsr getval15 ;get resonance param
 asl          ;resonance is stored in the upper nybble
 asl          ;so this value must be shifted left
 asl
 asl
 ora md417    ;include voice number in lower nybble
 sta md417    ;store for future read (cannot read SID registers, only write)
 sta RESON    ;SID's Output Filter Resonance Control Register
;get the type 0-4 if present, stop otherwise
;Store in SID Register $d418 bits 4-6
;Bits 0-3 Select output volume (0-15)
;Bit4 Select low-pass filter, 1=low-pass on, 0=off
;Bit5 Select band-pass filter, 1=band-pass on, 0=off
;Bit6 Select high-pass filter, 1=high-pass on, 0=off
;Bit7 Disconnect output of voice 3, 1=disconnect, 0=connect
;since SID registers can only be written to, an alternate var will hold it
;0. None.
;1. A low-pass filter is available, which suppresses the volume of those
;frequency components that are above a designated cutoff level.
;2. The high-pass filter reduces the volume of frequency components that are
;below a certain level.
;3. The band-pass filter reduces the volume of frequency components on both
;sides of the chosen frequency.
;4. The high-pass and low-pass filters can be combined to form a 
;notch reject filter, which reduces the volume of the frequency components
;nearest the selected frequency.
;
;Bits 4-6 are the target
;0  = 000  none
;1  = 001  low pass
;2  = 010  band pass
;3  = 100  hi pass
;4  = 101  notch reject
;
 jsr chkcomm  ;if no more params then quit otherwise continue
 jsr getval   ;filter type 0-4
 cmp #5
 bcs illqty5
 cmp #3       ;3 and 4 need to add 1 to achive desired bit pattern
 bcc settype
 tay
 iny
 tya
settype
 asl          ;shift bits 0,1,2 to positions 4,5,6
 asl          ;ie 00000111 becomes 01110000
 asl
 asl
 sta XSAV     ;temp storage
 lda md418    ;current value using SID mock register
 and #%10001111 ;clear bits 4-6, keep the rest
 ora XSAV     ;apply new value
 jmp setvol
;
;*******************
; PLAY S$
; PLAY OFF
; PLAY SPRITE sprite#, startPtr, endPtr, speed
; PLAY SPRITE [sprite#] OFF
play
 beq mop2
 cmp #TOKEN_OFF
 bne playy
 jsr endplay
 jmp CHRGET
playy
 cmp #TOKEN_SPRITE
 bne playyy
;PLAY SPRITE sprite#, startPtr, endPtr, speed
;PLAY SPRITE [sprite#] OFF
plyspr
 jsr CHRGET
 cmp #TOKEN_OFF
 bne getani
 lda #0         ;all sprite animation off
 sta aniopt
plysprx
 jmp CHRGET
getani
 jsr sprnum     ;$be=sprite index, A reg and $bf=2^sprite#
;turn off animation for sprite specified
 eor #$ff
 and aniopt
 sta aniopt
;
 jsr CHRGOT
 cmp #TOKEN_OFF
 beq plysprx
 jsr getvalg    ;startPtr
 ldx $be        ;sprite index 0-7
 sta ptrbegin,x
 tay
 jsr ptrhi
 sta ptr3ref+2
 sta ptr2ref+2
 sta ptr1ref+2
 tya
ptr3ref sta $07f8,x
 jsr getvalg    ;endPtr
 ldx $be
 sta ptrend,x
 jsr getvalg    ;jiffies between images
 ldx $be
 sta aniwait,x
 sta anidly,x
;turn sprite animation on for sprite specified
 lda $bf        ;sprite bit weight
 ora aniopt
 sta aniopt
 lda #%00000010 ;sprite animation flag on
 jmp irqon      ;turn on sprite animation and ensure MDB IRQ is on
;
; PLAY S$
playyy
 jsr getstr0
 beq playend    ;quit now if string is empty
 ldy #0
cpystr lda ($50),y
 sta playbuf1,y
 iny
 cpy $52
 bne cpystr
 lda #0         ;default voice 1 (index 0)
 sta playbuf1,y ;zero-byte terminator
 sta playindex  ;start index of string
 jsr initvoice
 ldy #1
 sty playtime   ;initial wait
 dey
 lda ($50),y
 cmp #"!"       ;play in foreground?
 beq pfgnd
 lda #%00000001 ;play flag on
irqon
 ora MDBIRQ
 sta MDBIRQ
 sei
 jsr mdbirqon
 cli
playend rts
;
;play notes in foreground
pfgnd lda $a2
 cmp $a2
 beq pfgnd+2
 jsr STOP       ;stop key pressed?
 beq endplay
 jsr playit
 bcc pfgnd
 rts
;
; IRQ routine to play next note
playit
 dec playtime
 bne nxtply
 lda R6510
 pha
 and #%11111110
 sta R6510
 jsr notegot
 beq played
 jsr nextn
 pla
 sta R6510
nxtply
 clc
 rts
played
 pla
 sta R6510
endplay
 lda #%11111110 ;play irq flag off
 jsr irqoff
 ldx playvoice  ;SID reg offset
 lda playwave   ;select current waveform; start release cycle
 sta VCREG1,x   ;start decay cycle
 sec
plydone
 rts

initvoice
 ldx #30        ;default note length to 30 jiffies (approx 1/60 sec.)
 stx playlen    ;note duration 30/60=0.50 sec.

 ldx #4         ;default octave 4
 stx playoct

;init SID registers for selected voice
 tax
 lda #%01000000 ;select default waveform to pulse; start decay cycle

inivoc
 stx playvoice  ;voice register offset
 sta playwave   ;waveform
 sta VCREG1,x   ;apply waveform/start release cycle

 lda #0
 sta FRELO1,x
 sta FREHI1,x
 sta PWLO1,x    ;set pulse waveform to 50%
 lda #$08       ;duty cycle is a 12-bit value 0-4095, 50%=2048
 sta PWHI1,x    ;lower nybble only, upper nybble is not used

 lda #$10       ;set attack duraction to 8ms (hi nybble)
 sta ATDCY1,x   ;and decay duration to 6ms (lo nybble)
 lda #$F8       ;set sustain volume to 15 (hi nybble)
 sta SUREL1,x   ;and release duration to 300ms (lo nybble)

 lda md418      ;all voices volume register mirror
 bne plydone    ;if volume is 0 (off) turn it up!
 ora #%00001111 ;set volume to max
 jmp setvol
;
;******************************************************************************
;* MDBASIC Functions
;******************************************************************************
;
; I = INSTR(src$,find$)
; I = INSTR(offset,src$,find$)
;if first param is numeric then use as offset, otherwise offset=1 (default)
instr
 jsr CHRGET   ;process next cmd text
 jsr CHKOPN   ;Check for and Skip Opening Parentheses
 jsr FRMEVL   ;eval expression
 lda VALTYP   ;check if numeric (offset param) or string (source$ param)
 beq getoff
 lda #0       ;default zero index
 sta $fe
 jsr getstr2  ;get source string as first param
 jmp getsrc
badidx
 jmp BSERR    ;BAD SUBSCRIPT ERROR
getoff
 jsr GETBYTC+6 ;convert FAC1 to an unsigned byte value 0-255 into X reg
 txa
 beq badidx   ;BASIC string indexes are based at 1 (not 0)
 sta $fe      ;start index
 jsr ckcom2   ;check for and skip over comma, misop err if missing
 jsr getstr0  ;get source string as second param
getsrc
 stx $fb
 sty $fc
 sta $fd      ;len of src str
 jsr ckcom2   ;check for and skip over comma, misop err if missing
 jsr getstr0  ;find string a = len($52), x=lobyte($50) ptr, y=hibyte($51) ptr
 lda $fd      ;src len
 beq notfound ;zero length strings cannot be searched
 cmp $fe      ;start index
 bcc notfound ;start index beyond src len
 cmp $52      ;find len > src len
 bmi notfound ;find str cannot be found in a shorter src string
 dec $fe      ;convert start index to a zero-based index
tryagain
 ldy $fe      ;index of current char in source str
 ldx #0       ;index of first char in find str
 inc $fe      ;prepare index for return value or next position to compare
nextchr
 cpy $fd      ;did we reach source str length?
 beq notfound ;yes then stop trying
 stx $61
 sty $62
 lda ($fb),y
 ldy $61
 cmp ($50),y
 bne tryagain
 ldy $62
 iny
 inx
 cpx $52      ;did we reach find str length?
 bne nextchr  ;no, keep going
 ldy $fe      ;index of beginning of str found in source str
.byte $2c     ;defeat ldy #0 as BIT $00A0
notfound
 ldy #0
 lda #0       ;hibyte 0 since strings cannot be longer than 255
 jsr GIVAYF   ;convert 16-bit signed int in A,Y regs to 5-byte float in FAC1
 jmp CHKCLS   ;check for and skip closing parentheses
;
;*******************
; P=PTR(x) or P=PTR(x%) or P=PTR(x$) where x is the variable name
ptr
 jsr CHRGET
 jsr PARCHK     ;get term inside parentheses
 lda $48        ;pointer to variable
 ldy $47
 jmp GIVAYF     ;convert 16-bit signed int in A,Y regs to 5-byte float in FAC1
;
;*******************
; I = FIX(float)  truncate the fractional portion
fix
 jsr CHRGET
 jsr PARCHK     ;get term inside parentheses
 jsr TESTNUM    ;ensure expression was numeric, error if not
 lda $66        ;FAC1 sign byte
 pha            ;save sign byte
 jsr ABS        ;ensure positive number for floor calculation
 jmp trunca     ;truncate remainder

;round FAC1 to the nearest whole number
;add 0.5 to the absolute value then truncate remainder
doround
 lda $66        ;FAC1 sign byte
 pha            ;save sign byte
 jsr ABS        ;ensure positive number
 jsr FADDH      ;add .5 to value in FAC1
trunca
 jsr INT        ;convert FAC1 value to its lowest integer value (floor)
signit
 pla            ;recall original FAC1 sign byte
 tax            ;if rounding or fixing results in zero
 jsr SIGN       ;then leave the sign byte alone
 beq signed     ;otherwise restore it as it was
 stx $66        ;before rounding or fixing began
signed rts

;*******************
; V = ROUND(n)    -round to nearest whole number
; V = ROUND(n,d)  -round to specific precision
; n is the 32-bit floating point number to round, d is the precision
; d(-9 to +9) is the number of places left (-) or right (+) of decimal point
fnround
 jsr CHRGET
 jsr CHKOPN     ;check for and skip opening parentheses
 jsr FRMNUM     ;get numeric param1 - number to round
;save param1
 ldx #<BUF+$54  ;5-byte buffer pointer to unused
 ldy #>BUF+$54  ;memory area at end of line input buffer
 jsr MOV2F+16   ;copy a 5-byte floating point number from FAC1 to memory
;prepare param2 default values
 lda #0
 sta $fb        ;default 0 decimal places (round to whole number)
 sta $fc        ;default first move direction right
 jsr comchk
 bne round1
;get param2, move decimal direction
 jsr CHRGET
 jsr FRMNUM
 jsr SIGN       ;get FAC1 0=Zero, 1=Positive, 255=Negative
 beq round1     ;zero value, no move needed
 sta $fc        ;save sign, negative move left, positive move right
 jsr ABS        ;ensure FAC1 is a positive number
 jsr fac2int    ;convert FAC1 to 1-byte unsigned int
 cmp #10
 bcs illqty7    ;range is -9 to +9
 sta $fb        ;decimal places to round
round1
 jsr CHKCLS     ;check for and skip closing parentheses
;restore param1 to FAC1
 lda #<BUF+$54  ;5-byte buffer pointer to unused
 ldy #>BUF+$54  ;memory area at end of line input buffer
 jsr MOVFM      ;copy a 5-byte float from memory to FAC1 A=lo, Y=hi
 lda $fb        ;decimal places to round
 beq doround    ;zero will round to nearest whole number
;move decimal point to the right or left based on sign of num places
 jsr movedec    ;move decimal to left or right based on sign of param2
 lda $fc        ;move direction
 eor #$ff       ;toggle move direction for 2nd call
 sta $fc
 jsr doround    ;round FAC1 to nearest whole number
;move decimal point
 lda $fb        ;decimal places to round
movedec
 sta COUNT
 lda $66        ;save FAC1 sign byte
 pha            ;before moving decimal
 lda $fc        ;direction: 255=left else right
 bpl xmul10
xdiv10
 jsr DIV10      ;divide FAC1 by 10
 jsr ROUND      ;adjust FAC1 rounding byte
 dec COUNT
 bne xdiv10     ;keep moving till done
 beq signit     ;always branches
xmul10
 jsr MUL10      ;multiply FAC1 by 10
 jsr ROUND      ;adjust FAC1 rounding byte
 dec COUNT
 bne xmul10     ;keep moving till done
 beq signit     ;always branches
;
;*******************
; KEY AND KEY$ are used with ON KEY GOSUB to return key that caused the event
; K = KEY    -get ASCII value, no key = 0
; K$ = KEY$  -get string value of length 1, no key has ASC(KEY$)=0
fnkey
 jsr chrget
 ldy keyentry    ;y=lobyte
 cmp #"$"
 bne nobutt
 jsr CHRGET
 tya
 jsr $b6f0       ;perform chr$
;NOTE: JSR will not return here since rountine pulls address off stack
;
;*******************
; J = JOY(n) where n=joystick number 1 or 2
joy
 jsr getfnparam
 beq illqty7
 cmp #3
 bcs illqty7
 lsr             ;joy 1 or 2 becomes index 0 or 1
 eor #%00000001  ;index 0 or 1 becomes 1 or 0
 tay             ;CIAPRB has joy 1
 lda CIAPRA,y    ;CIAPRA has joy 2
 and #%00011111  ;bit 4=button, bits0-3=direction
 tax             ;remember full value
 and #%00001111  ;lower nybble holds position value
 eor #$ff        ;calc 2's compliment
 clc             ;and add 15
 adc #16         ;to complete calc of a=15-a
 cpx #16         ;bit4=button pressed? 1=no, 0=yes
 bcs nofire      ;bit4 must be 1 if carry set
 ora #%10000000  ;add button flag
nofire tay       ;y=lowbyte
nobutt lda #0    ;hibyte 0
 jmp GIVAYF      ;convert binary int to FAC then return
;
illqty7 jmp FCERR ;display illegal qty error
;
;*******************
; EN = ERR    -error number
; EL = ERRL   -error line number
fnerr
 jsr chrget      ;advance TXTPTR one byte
 ldy errnum      ;assume ERR (error number)
 cmp #"l"        ;ERRL?
 bne nobutt      ;return error number
 jsr CHRGET      ;skip over letter
 ldy errline     ;return error line number
 lda errline+1
 jmp GIVAYF      ;convert binary int to float in FAC1
;
;*******************
; P = PEN(n) where n: 0=x-coordinate, 1:y-coordinate
;For PENY there are 200 visible scan lines possible so value is exact.
;For PENX there are only 8 bits available for 320 possible horiz axis values.
;Therefore the value is accurate only to every second dot position.
;The number will range from 0 to 160 and must be multiplied by 2 in order to
;approximate the actual horizontal dot position of the light pen.
;This routine returns the average of 4 reads to improve accuracy.
;
pen
 jsr getfnparam
 cmp #2          ;valid values 0 or 1
 bcs illqty7
 tax             ;0=x axis, 1=y axis
;init sum for 4 reads
 lda #0          ;init counter
 sta $fb
 sta $fc
;take 4 readings on 4 consecutive frames
 ldy #3
readpen
 lda SCROLY      ;wait for raster to go off screen and
 bpl readpen     ;the latch to capture the pen axis then
 lda LPENX,x     ;read 8-bit axis value of pen and
 jsr addfb       ;add it to the total.
 dey
 bmi div4        ;if 4 frames are done then calc avg
rastwait
 lda SCROLY      ;wait for raster to start at top of screen
 bmi rastwait
 bpl readpen     ;process next frame
;calc average for 4 reads
div4 lsr $fc     ;divibe 2-byte binary value by 2
 ror $fb         ;twice to achive division by 4
 iny
 beq div4
;finalize return value
 dex             ;x or y axis?
 beq pendone     ;y axis value is exact
 asl $fb         ;x axis needs to be multipliied
 rol $fc         ;by 2 to apporimate actual value
pendone          ;return 2-byte int value
 ldy $fb         ;lobyte return value
 lda $fc         ;hibyte return value
 jmp GIVAYF      ;convert binary int to FAC then return
;
;*******************
; V = INF(n) where n = 0 to 67 to select info
inf
 jsr getfnparam
 cmp #68
 bcs illqty7
 dec R6510
 jsr inff
 inc R6510
 sta $65         ;lobyte
 sty $64         ;hibyte
 jmp $b8d7       ;convert unsigned 4-byte int in FAC1 to a 5-byte float in FAC1
;
;*******************
; P = POT(n) where n=potentiometer number (1-4)
;labeled "Port 1" (pots 1 and 2) are on CIA #1 data port B
;labeled "Port 2" (pots 3 and 4) are on CIA #1 data port A
pot
 jsr getfnparam  ;get pot number param
 beq illqty7     ;zero is invalid
 cmp #5          ;5 and over is invalid
 bcs illqty7     ;valid pot numbers are 1 to 4
;
 cmp #3          ;set carry flag if pot num 3 or 4
 ldx #%11000000  ;bits 6,7 set to output to select port 1 or 2 paddle read
                 ;bits 3,4 set to input paddle buttons, the rest are not needed
 ldy #1          ;port index 0=A (port 2), 1=B (port 1)
;keyboard off
 sei             ;prevent irq from using port for keyboard
 lda CIDDRA      ;get current data direction on port A
 pha             ;save for restoring later
;select data direction
 stx CIDDRA      ;write bits to select reading pots on ports A and B
;select port 1 or 2
 lda #%10000000  ;bit 7 & 6 pattern: 10=port1, 01=port2
 bcc setport     ;carry was set if pot num 3 or 4
 dey             ;offset for data port A (port 2)
 lsr             ;adjust bit pattern to select port 2
setport
 sta CIAPRA      ;apply port selection
;read buttons
 lda CIAPRA,y    ;read button flags (bits 3,4) from selected port A or B
 sta $61         ;save button bit flags, 0=pressed, 1=not pressed
;wait for data
 ldy #$7f        ;wait for POTX/POTY latches to ensure 8-bit data capture
waitl dey        ;from the A/D converters in the SID chip that measure pot
 bne waitl       ;voltage (0 to +5 Volts) on the pins of the selected port
;read the data
 lda $14         ;pot num 1-4
 lsr             ;even or odd pot num?
 bcs readpot     ;odd pot num (1,3) use POTX
 iny             ;even pot num (2,4) use POTY
readpot
 lda POTX,y      ;read pot value captured from the data port
 tay             ;lobyte of the return value
;keyboard on
 pla             ;restore the data direction
 sta CIDDRA      ;register settings for port A
 cli             ;enable irq to read keyboard
;prepare return value
 ldx #0          ;hibyte default value
 lda #%00000100  ;odd pot nums use bit 2 for button
 bcs btnmsk      ;odd pot num
 asl             ;even pot nums use bit 3 for button
btnmsk bit $61   ;test associated bit for button press
 bne nobutt2     ;0=pressed, 1=not pressed
 inx             ;hibyte = 1 to indicate button pressed
nobutt2          ;reg y holds the lobyte
 txa             ;reg a holds the hibyte
 jmp GIVAYF      ;convert 2-byte int into a 5-byte float with result in FAC1
;
;*******************
;H$ = HEX$(n) where n is a signed 32-bit signed integer
hex
 jsr fix      ;get fn param as 5-byte float truncated
 jsr QINT     ;convert FAC1 into a signed 32-bit int in FAC1
 dec R6510
 jsr hexx
 inc R6510
 jmp STRLIT
;
;**********************************************
;* new reset subroutine set by cartridge header
;**********************************************
resvec
 ldx #$ff    ;highest stack ptr offset (empty stack)
 sei         ;disable interrupts
 txs         ;clear the stack
 cld         ;ensure decimal mode is turned off
 stx SCROLX  ;reset video chip and show mc text mode for visual effect
 jsr IOINIT  ;init CIA i/o devices
 jsr RAMTAS  ;init RAM, tape buffer & screen
 jsr RESTOR  ;restore default I/O vectors
 jsr CINT    ;init screen editor and VIC-II chip
 cli
 jsr $e453   ;copy BASIC vectors to RAM
 jsr INIT    ;initialize BASIC
 lda R6510
 and #%11111110
 sta R6510   ;switch out 8K BASIC ROM for 8K RAM
 jsr newvec  ;set new vectors to MDBASIC routines
 jsr initclk ;init TOD clocks
 inc R6510   ;restore 8K BASIC ROM
 lda #<mesge
 ldy #>mesge
 jsr STROUT  ;display MDBASIC banner
 lda TXTTAB  ;ptr to start of BASIC program text
 ldy TXTTAB+1
 jsr REASON  ;check free mem
 jsr $e430   ;prints the BYTES FREE message
 jmp $e39d   ;to basic main loop
;
;*********************************************************
;* new RUN-STOP IRQ-driven routine set by cartridge header
;*********************************************************
runstp
 lda #$7f
 sta CI2ICR
 ldy CI2ICR
 bmi nothin
 jsr $f6bc       ;scan keyboard for STOP key with result in $91
 jsr STOP        ;determine if STOP key was pressed
 bne nothin      ;if not, continue with NMI handler
;***************************************************************
;* BREAK Instruction IRQ-driven routine via vector CBINV ($0316)
;***************************************************************
brkirq
 jsr norm
 jsr IOINIT
 jsr $e518       ;initialize screen and keyboard
 dec R6510       ;LORAM signal select RAM
 jsr newvec      ;init vectors
 inc R6510       ;LORAM signal select ROM (BASIC)
 jsr CLALL
 ldx $fd9f       ;original CBM IRQ vector ($ea31) for CIA #1
 ldy $fda0       ;driven by CIA #1 Timer B
 lda #0          ;disable all 5 NMI events
 sta CI2ICR      ;CIA #2 NMI control register
 stx CINV        ;restore orignal CBM IRQ vector
 sty CINV+1
 lda #%01111111  ;with bit7=0 irq event flags (bits0-6) will be cleared
 sta CI2ICR      ;clear NMI event flags and enable NMI events
 jmp ($a002)     ;warm start vector
nothin jmp $fe72 ;NMI RS-232 Handler
;
;*******************************************************
;* MDBASIC ERROR HANDLER
;* BASIC Error Handler Routine via vector IERROR ($0300)
;*******************************************************
errors
 txa
 bpl doerr    ;bit 7 off means error condition
redy
 jmp READY    ;print READY. then continue with BASIC main loop
doerr
 sta XSAV
 jsr $a67a    ;empty system and temp string stacks
 jsr clsmdb   ;close mdb file handles and set default I/O channels
 jsr norm     ;set text mode normal display and ensure BASIC ROM enabled
 ldy CURLIN+1 ;when current line num hibyte is $FF (invalid)
 iny          ;then immediate mode
 bne prgmode  ;otherwise program mode
 lda #13      ;cr
.byte $2c     ;defeat lda #147 by making it BIT $93A9
prgmode
 lda #147     ;clr screen
 jsr CHROUT
 lda #$80     ;only control messages - SEARCHING, SAVING, FOUND, etc.
 jsr SETMSG
 jsr $ab45    ;print question mark
 lda XSAV     ;current error num
 beq usererr  ;user defined error numbers
 cmp #36      ;are 0, 35-127
 bcc erridx   ;CBM BASIC error numbers 1-31
usererr
 lda #36      ;all user defined errors use same message
erridx asl    ;calc 2-byte index to message
 tax
 sec
 sbc #62
 bcc cbmerr   ;CBM errors 1-30 (index 2-60)
 tax
 lda erradd,x ;MDBASIC errors 31-35
 ldy erradd+1,x
 bne hibyer   ;always branches since hi-byte would not be 0
cbmerr
 lda $a326,x  ;$A328-$A364 Error Message Vector Table
 ldy $a327,x
hibyer
 jsr printstr
 lda #$69     ;address of the zero-terminated string ($a369) = "  ERROR"
 ldy #$a3
 jsr STROUT   ;print str whose addr is in y reg (hi byte) and a reg (lo byte)
 ldy CURLIN+1 ;hibyte of TXTPTR of beginning of line where error occured
 iny          ;a value of $ff indicates immediate mode
 beq redy     ;otherwise program mode
 jsr INPRT    ;display text IN {line#}
 lda CURLIN   ;put the current BASIC line number
 sta $14      ;where FINDLN expect to see it
 lda CURLIN+1
 sta $15
 jsr FINDLN   ;set position to line num causing error
 jsr printcr
 jsr lstrap
 jsr $a6c9    ;perform LIST line#
 ldx #$02     ;place cursor on first digit of line number
 ldy #$00     ;which would be line 2 column 0
 clc
 jsr PLOT
 jmp (IMAIN)  ;main BASIC loop
;
;*********************************************************
; Keyboard Table Setup Routine via vector KEYLOG ($028F)
; IRQ driven key decode overriden to support function keys
;*********************************************************
keychk
 ldy SHFLAG     ;0=none, 1=shift key, 2=logo key, 4=ctrl key
 cpy #%00000110 ;ctrl and logo key combo exactly?
 bne keychk2
 lda #0         ;turn off editor quote/edit/rvs mode
 sta QTSW       ;quote mode flag
 sta INSRT      ;insert char count
 sta RVS        ;reverse char flag
 beq nokey      ;always branches
;start of the original mdbasic routine
keychk2
 lda MSGFLG     ;control messages enabled?
 beq nokey      ;no, use original routine
 lda QTSW       ;editor in quote mode?
 bne nokey      ;yes, use original routine
 lda #%00000100 ;keypump irq flag
 bit MDBIRQ     ;is keypump irq already enabled?
 bne nokey      ;yes, use normal decode routine
 ldx #$81       ;standard keyboard matrix decode table $eb81
 tya            ;SHFLAG: 0=none, 1=shift key, 2=logo key, 4=ctrl key
 beq stdkey
 dey            ;zero flag set if only shift key pressed
 bne nokey      ;unsupported key combo for function keys
 ldx #$c2       ;shifted keyboard matrix decode table $ebc2
stdkey stx KEYTAB
 lda #$eb
 sta KEYTAB+1
 ldy SFDX       ;matrix coordinate of current key pressed
 lda (KEYTAB),y
 tax
 cpy LSTX       ;matrix coordinate of last key pressed, 64=None Pressed
 bne norep      ;not a repeat keypress
 jmp $eaf0      ;resume CBM func to decode keystroke
norep sec
 sbc #$85       ;first func key?
 bcc nokey      ;F1=$85, F3=$86, F5=$87, F7=$88, F2=$89, F4=$8A, F6=$9B, F8=$9C
 cmp #8         ;valid function key index is 0-7
 bcc fkey
nokey jmp $eb48 ;setup proper keyboard decode table
fkey
 asl            ;key index*16  (16 chars per function key)
 asl
 asl
 asl
 sta keyidx
 lda #%00000100 ;key pump irq flag on
 ora MDBIRQ
 sta MDBIRQ
 jsr mdbirqon
 ldx #$ff
 jmp $eae9      ;continue regular key decode func
;
;*****************************************
; IRQ driven key pump into keyboard buffer
;*****************************************
keypump
 ldx NDX        ;if keyboard buffer is empty then put next char
 bne irqdone2   ;otherwise forward to original IRQ vector
 lda R6510
 pha
 and #%11111110
 sta R6510      ;switch LOROM to LORAM
 ldx keyidx     ;offset to next char to process
 ldy keybuf,x   ;get next char
 lda keystrflg  ;manual key string flag?
 beq nostrflg
 ldy paintbuf3,x
nostrflg
 pla
 sta R6510      ;switch LORAM to LOROM
 tya
 beq alldone    ;zero-terminated string
 sta KEYD       ;place char in keyboard buffer
 inc NDX        ;indicate 1 char waiting in buffer
 inc keyidx     ;advance index to next char
 bne irqdone2   ;continue original IRQ vector
alldone
 lda #0
 sta keystrflg
 lda #%11111011 ;key pump irq flag off
 jmp irqoff
irqdone2
 rts
;
;*************************
;MDBASIC IRQ Handler
;*************************
mdbirqhdl
 lda MDBIRQ
 beq mdbirqoff2
 bit bitweights
 beq irq1
 jsr playit
 lda MDBIRQ
irq1
 bit bitweights+1
 beq irq2
 jsr sprani
 lda MDBIRQ
irq2
 bit bitweights+2
 beq irq3
 jsr keypump
 lda MDBIRQ
irq3
 bit bitweights+3
 beq irqnorm
 jsr sprcolchg
irqnorm
 jmp (TMPIRQ)   ;orgininal irq vector
mdbirqoff2
 jsr mdbirqoff
 jmp (TMPIRQ)   ;orgininal irq vector
;
;****************************
;Sprite animation IRQ routine
;****************************
sprani
 lda aniopt
 beq anioff     ;all flags off so turn off irq
 ldx #7         ;prepare to process all 7 bits
chkani asl      ;bit7 to carry
 bcc nxtani     ;not set then next bit
 dec anidly,x   ;sprite x wait timeout
 bne nxtani
 tay            ;preserve bit pattern
 lda aniwait,x  ;reset ani wait time for sprite x
 sta anidly,x
ptr1ref         ;this ptr ref is used to change
 lda $07f8,x    ;the sprite ptr base address
 cmp ptrend,x   ;did we reach the last image index?
 bcc setptr     ;no, advance image index
 lda ptrbegin,x ;yes, reset to first image index
.byte $2c       ;prevent adc #1
setptr adc #1
ptr2ref         ;this ptr ref is used to change
 sta $07f8,x    ;the sprite ptr base address
 tya            ;restore bit pattern
nxtani
 dex
 bpl chkani
 rts
anioff
 lda #%11111101
.byte $2c
sprirqoff
 lda #%11110111
irqoff
 and MDBIRQ
 sta MDBIRQ
 rts
;
;Sprite color 16 IRQ routine
sprcolchg
 lda sprcolopt
 beq sprirqoff  ;all flags off so turn off irq option
 ldx #7         ;prepare to process all 8 bits
chkspr asl      ;bit7 to carry
 bcc nxtspr     ;not set then next bit
 inc SP0COL,x
nxtspr
 dex
 bpl chkspr
sprcolx rts
;
;ensure MDBASIC IRQ handler is enabled
;this subroutine assumes IRQs are already disabled
;caller should be an IRQ subroutine or use sei/cli
mdbirqon
 ldy CINV+1
 cpy #>mdbirqhdl
 beq irqset
 ldx CINV
 stx TMPIRQ
 sty TMPIRQ+1
 ldx #<mdbirqhdl
 ldy #>mdbirqhdl
setirq
 stx CINV
 sty CINV+1
irqset rts
mdbirqoff
 ldx TMPIRQ   ;restore IRQ vector
 ldy TMPIRQ+1 ;back to original $EA31 or user-defined address
 jmp setirq
;
;*******************************
;* general purpose subroutines *
;*******************************

;multiply the value in accumulator by 8 returning word in $be,$bf
times8
 ldx #0
 stx $be    ;result lobyte
 stx $bf    ;result hibyte
 tax        ;num to multiply by 8
 beq end40
 asl
 rol $bf
 asl
 rol $bf
 asl
 rol $bf
 sta $be
end40 rts
;
;add value in accumulator to value in $fb,$fc
addfb
 clc
 adc $fb
 sta $fb
 lda $fc
 adc #0
 sta $fc
 rts
;
;*******************
;get parameters for LIST and DELETE
opget jsr CHRGET
opget2 bcc okopge  ;text found (not a token)
 beq okopge        ;end of line found
 cmp #$ab          ;subtract token? (- is a token)
 beq okopge
sytxer jmp SNERR   ;print syntax error
okopge jsr LINGET  ;convert asci decimal number to a 2-byte binary line number
 jsr FINDLN        ;search for start line number
 jsr CHRGOT        ;get current char on line
 beq linnul
 cmp #$ab          ;subtract token? (- is a token)
 beq okopg2
okopg2 jsr CHRGET
 jsr LINGET        ;convert asci decimal number to a 2-byte binary line number
 bne sytxer
linnul lda $14     ;if no line num given
 ora $15           ;then default to 65535
 bne opgot         ;to indicate last line
 lda #$ff
 sta $14
 sta $15
opgot rts
;*******************
comchk
 ldy #0            ;quickly check
 lda (TXTPTR),y    ;if current TXTPTR
 cmp #","          ;is on a comma
 rts
;*******************
chkcomm
 ldy #0            ;quickly check
 lda (TXTPTR),y    ;if current TXTPTR
 cmp #","          ;is on a comma
 beq comma         ;if so skip over it
 pla               ;otherwise do not
 pla               ;return to caller
 rts
;*******************
;check for and skip over comma, misop err if missing
ckcom2
 ldy #0            ;quickly check
 lda (TXTPTR),y    ;if current TXTPTR
ckcom22
 cmp #","          ;is on a comma
 bne missop        ;error if not
comma jmp CHRGET   ;advance TXTPTR
missop ldx #31     ;missing operand error
 jmp (IERROR)      ;vector to print basic error message
;*******************
getstr jsr CHRGET  ;get next basic text chr
getstr0 beq missop
getstr1 jsr FRMEVL ;evaluate expression
getstr2 jsr FRESTR ;discard temp string
 stx $50           ;lowbyte
 sty $51           ;hibyte
 sta $52           ;length
 rts
;*******************
;get charset parameter 0-3
;0=Upper-case and symbols, 1=Reverse of set 0
;2=Lower-case and symbols  3=Reverse of set 2
;accumulator holds the hibyte of the base address of CHAREN
getchrset
 sta $26      ;either $d0 (BITMAP mode) or $f0 (DESIGN mode)
 jsr getvalg
 cmp #4       ;0 to 3 only
 bcs illqty4
 asl          ;calc hibyte offset
 asl          ;0=0, 1=4, 2=8, 3=12
 adc $26      ;carry is already clear
 sta $26      ;charset 0=$d000,1=$d400,2=$d800,3=$dc00
 rts
;*******************
getboolg jsr CHRGET
getboolz beq missop
getbool  jsr getval
 cmp #2
 bcs illqty4
 tax               ;sets zero flag if a reg is zero
 rts               ;result is in both a and x reg
;*******************
getval15g jsr CHRGET
getval15z beq missop
getval15  jsr getval
 cmp #16           ;enforce 0-15 range
 bcs illqty4       ;branch not taken saves 1 cycle
 rts               ;faster for non error condition
illqty4 jmp FCERR  ;illegal quanity error
;*******************
;get a single byte int (0-255) throw error if bad data type or range
getvalg jsr CHRGET
getvalz beq missop
getval  jsr FRMNUM ;eval numeric expr & type, store result in FAC1
fac2int jsr GETADR ;convert FAC1 to unsigned 2 byte int in $14,$15
 tax               ;if hi byte is not zero then
 bne illqty4       ;throw ill qty err
 tya               ;also return the result in the accumulator
 rts               ;processor zero flag set based on lobyte
;*******************
;get a single-byte numeric parameter (0-255) inside parentheses
getfnparam
 jsr fix
 jmp fac2int
;*******************
;get a 2-byte int parameter (0-65535)
getvalueg jsr CHRGET
getvaluez beq missop
getvalue  jsr FRMNUM ;eval numeric expr & type
 jsr GETADR          ;convert FAC1 to unsigned 2 byte int in $14,$15
 ldx $14             ;return lobyte in x reg
 ldy $15             ;return hibyte in y reg
 rts                 ;processor zero flag set based on hibyte
;*******************
; get plot type and color for graphics statements
types
 jsr chkcomm     ;if current char is comman skip over it
 cmp #","
 beq noparam     ;another comma found so skip plot type param
 jsr getval      ;get plot type value
 cmp #4          ;plot type 0=erase, 1=plot, 2=toggle, 3=none (locate only)
 bcs illqty4
 sta lastplott
noparam
 jsr chkcomm     ;if current char is not a comma do not return here
 lda SCROLX
 and #%00010000  ;check if multicolor mode on or off
 bne mcplot
 jmp getc1       ;get hires plot color 0-15
mcplot
 jsr getval      ;get color selection, multicolor selection index 1-3
 beq illqty4
 cmp #4          ;mc mode color index selection is 1,2 or 3
 bcs illqty4
;convert index to ptab offset 1=8, 2=16, 3=24
 asl
 asl
 asl
 sta mapcolbits  ;offset = index * 8 where index in (1,2,3)
 rts
;*******************
sprnum
 jsr getvalz
 cmp #8          ;valid sprite numbers 0-7
 bcc less8
 ldx #33         ;illegal sprite number
 jmp (IERROR)
less8 sta $be
 tay
 lda bitweights,y
 sta $bf         ;2^sprite#
 rts
;*******************
ptrhi           ;determine hibyte for address of sprite pointers
 lda CI2PRA     ;which VIC2 16K memory bank?
 and #%00000011 ;00=bank3, 01=bank2, 10=bank1, 11=bank0
 eor #%00000011 ;11=bank3, 10=bank2, 01=bank1, 00=bank0
 lsr            ;move bits 0-1 to position 6-7 via carry
 ror
 ror
 sta $62        ;VIC-II Base Address hibyte 0=$00, 1=$40, 2=$80 ,3=$C0
;determine location of sprite data pointers
 lda VMCSB      ;screen RAM page offset (n*1K) 1=$0400, 2=$0800, 3=$0C00, etc.
 and #%11110000 ;upper nybble holds the number of 1K chunks
 lsr            ;convert to offset for hibyte, 1K=(4*256), 1=4, 2=8, 3=12, etc.
 lsr            ;zero shifted into carry by lsr so it is clear
 adc $62        ;base+offset
 adc #3         ;the end of screen RAM is 1K more
 rts
;*******************
getvoc
 jsr getvalz
 beq illvoc
 cmp #4         ;valid voice numbers 1,2,3
 bcs illvoc
getvoff
 tax
 dex            ;voice index 0,1,2
 lda sidoff,x   ;register offset 0,7,14
 rts
illvoc ldx #32  ;illegal voice number
 jmp (IERROR)
;*******************
;these routines are used while BASIC ROM is switched out
rom1 inc R6510   ;switch to rom (a000-bfff)
 jsr GIVAYF      ;convert 16-bit signed int to float (a=hibyte y=lobyte)
 jmp prtnum
rom2 inc R6510   ;switch to LOROM (a000-bfff)
 jsr MOVFM       ;move a float from memory to fac1
prtnum jsr FOUT  ;convert fac1 to ascii with str ptr in a,y registers
 jsr STROUT      ;print string ptr (a=lobyte y=hibyte)
 dec R6510       ;switch to LORAM (a000-bfff)
 rts
rom3 inc R6510
 jsr getvalue
 php             ;save zero flag indicating hibyte non-zero
 txa             ;lobyte also in A reg for convenience
 dec R6510
 plp
 rts
rom4
 inc R6510
 jsr LINPRT+4    ;print 2-byte binary number in FAC1
 dec R6510
 rts
;*******************
;print string that ends with either a zero-byte or an ascii > 127
printstr
 sta $22
 sty $23
 ldy #0          ;loop print all chars in err msg
printer lda ($22),y
 beq prtdone
 bmi prtchr
 jsr CHROUT
 iny
 bne printer
prtdone rts
printeq lda #"="
.byte $2c
printqt lda #"""
.byte $2c
printcr lda #$0d
prtchr and #$7f
 jmp CHROUT
;
;********************************************************************
;* Global Constant Storage
;********************************************************************
;
;table for calculating 2^n where n=0-7
bitweights .byte 1,2,4,8,16,32,64,128

;SID voice register offsets
sidoff .byte 0,7,14
;
;VOICE command use VOICE voc#, frequency
;REGVAL=FREQ/(CLOCK/16777216)
;FREQ=REGVAL*(CLOCK/16777216)Hz
;where CLOCK NTSC=1022727.143, PAL=985248.611
;NTSC 1Hz Freq Value = 1022727.143/16777216 = 0.060959288
;PAL  1Hz Freq Value =  985248.611/16777216 = 0.058725393
;below are the FAC values for 1 unit in Hz for both CLOCK speeds
;
ntsc .byte $7c,$79,$b0,$72,$44
pal  .byte $7c,$70,$8a,$09,$a8
;
;PULSE command use PULSE voc#, width%
;used for converting register value to frequency in Hz
;Formula REGVAL=40.95*width%
m4095 .byte $86,$23,$cc,$cc,$cd ;FAC binary representation of 40.95
;FILTER command use FILTER frequency, resonance, type
five8 .byte $83,$39,$99,$99,$9a  ;FAC binary representation of 5.8
;

;********************************************************************
;* Global Variable Storage
;********************************************************************

autonum    .word 10     ;last auto line numbering increment
autoflag   .byte 0      ;auto line numbering flag: 0=off, 1=on

traceflag  .byte 0      ;trace flag: 0=off, 1=on

;MDBASIC file handles
mdbin      .byte 0      ;current mdbasic input channel/file num
mdbout     .byte 0      ;current mdbasic output channel/file num
mdbio      .byte 0      ;current mdbasic i/o channel (used by SERIAL command)

;SID mock registers
md417      .byte 0      ;current filter control and resonance
md418      .byte 0      ;current volume (lo nybble) and filter type (hi nybble)

errnum     .byte 0      ;last error number that occured, default 0 (no error)
errline    .word $ffff  ;last line number causing error, (default -1 for none)
errtrap    .word 0      ;error handler line number

keyflag    .byte 0      ;key capture mode 0=disabled, 1=enabled, 2=paused
keyentry   .byte 0      ;scan code of the last key captured with ON KEY stmt
keyline    .word $ffff  ;line number for ON KEY subroutine
keyidx     .byte 0      ;current index of char to put in keyboard buf via IRQ
keystrflg  .byte 0      ;flag to indicate key string from KEY cmd

;sprite animation
aniopt     .byte 0      ;enables animation for 8 sprites with 8 bit flags
anidly     .repeat 8,0  ;IRQ driven count-down timer for each sprite
aniwait    .repeat 8,0  ;cmd param for each sprite for count-down begin
ptrbegin   .repeat 8,0  ;param for each sprite starting data pointer
ptrend     .repeat 8,0  ;param for each sprite ending data pointer

;sprite color 16
sprcolopt  .byte 0      ;bit flags for which sprites to change color

;GENERAL GRAPHICS COMMANDS (Shared):
lastplotx  .word 0      ;last plot x coord
lastploty  .byte 0      ;last plot y coord
lastplott  .byte 1      ;last plot type used
mapcolc1c2 .byte $15    ;last plot color, hinybble c1=white, lonybble c2=green
mapcolc3   .byte 7      ;last plot multicol, c3 (1-3) for bit pattern 11=yellow
mapcolbits .byte 8      ;(8,16,24) used for plotting a dot on a multicol bitmap

;PLAY command use only - used during IRQ
playtime   .byte 0      ;current jiffies till next note
playvoice  .byte 0      ;SID register offset for play voice
playwave   .byte 0      ;waveform for play notes


;********************************************************************
;* ROM barrier at A000-BFFF switch to RAM using R6510 before entry  *
;********************************************************************
;
*=$a000 ;"MDBASIC RAM under ROM Memory Block"

;temp storage for PAINT and SCROLL command
paintbuf1 .repeat 256,0
paintbuf2 .repeat 256,0
paintbuf3 .repeat 256,0

playbuf1 .repeat 256,0

;function key assignments, 8 keys, 16 bytes each
keybuf
.text "list"
.byte 13
.repeat 16-5,0   ;F1
.text "load"
.byte 34
.repeat 16-5,0   ;F3
.text "files"
.byte 13
.repeat 16-6,0   ;F5
.text "keylist"
.byte 13
.repeat 16-8,0   ;F7
.text "run"
.byte 13
.repeat 16-4,0   ;F2
.text "save"
.byte 34
.repeat 16-5,0   ;F4
.text "text"
.byte 13
.repeat 16-5,0   ;F6
.text "screenclr"
.byte 13
.repeat 16-10,0  ;F8

;table for printing a bitmap screen
bmdt
.byte $00,$03,$0c,$0f,$30,$33,$3c,$3f,$c0,$c3,$cc,$cf,$f0,$f3,$fc,$ff
.byte $f2,$1a,$02,$12,$97,$20,$20,$20,$f2,$1a,$03,$20,$20,$20,$f2,$1b
.byte $04,$20,$f2,$09,$05,$a1,$20,$f2,$17,$05,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$f2,$08,$06,$92,$a2,$a2,$12,$bc,$92,$a2,$bb,$f2,$1b
.byte $06,$12,$20,$f2,$0a,$07,$92,$a1,$f2,$1b,$07,$12,$20,$f2,$09,$08
.byte $a1,$bb,$f2,$1a,$08,$20,$20,$20,$f2,$09,$09,$be,$a1,$92,$bb,$f2
.byte $1a,$09,$12,$20,$92,$20,$12,$20,$f2,$1a,$0a,$20,$92,$20,$12,$20
.byte $f2,$19,$0b,$20,$20,$92,$20,$12,$20,$20,$f2,$05,$10,$92,$98,$00

;tables for plotting dots on a bitmap per line 0-24
lbtab
.byte 0,64,128,192,0,64,128,192,0,64,128,192
.byte 0,64,128,192,0,64,128,192,0,64,128,192,0
hbtab
.byte 224,225,226,227,229,230,231,232,234,235,236,237
.byte 239,240,241,242,244,245,246,247,249,250,251,252,254

;table of video screen matrix hibyte offset per line 0-24
btab .byte 0,0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3

ptab2
.byte %10000000,%01000000,%00100000,%00010000,%00001000,%00000100,%00000010,%00000001
.byte %01000000,%00000000,%00010000,%00000000,%00000100,%00000000,%00000001,%00000000
.byte %10000000,%00000000,%00100000,%00000000,%00001000,%00000000,%00000010,%00000000
.byte %11000000,%00000000,%00110000,%00000000,%00001100,%00000000,%00000011,%00000000
ptab3
.byte %01111111,%10111111,%11011111,%11101111,%11110111,%11111011,%11111101,%11111110
.byte %00111111,%00000000,%11001111,%00000000,%11110011,%00000000,%11111100,%00000000
.byte %00111111,%00000000,%11001111,%00000000,%11110011,%00000000,%11111100,%00000000
.byte %00111111,%00000000,%11001111,%00000000,%11110011,%00000000,%11111100,%00000000

;baud rates not supported (too fast for IRQ) 3600,4800,7200,9600,19200
baudrates .word 50,75,110,134,150,300,600,1200,1800,2400

;MDBASIC parity parameter value range 0-4 to register M51CDR bits 7,6,5
;0 = XX0 (0,64,128, or 192) = No Parity Generated or Received
;1 = 001 (32)  = Odd Parity Transmitted and Received
;2 = 011 (96)  = Even Parity Transmitted and Received
;3 = 101 (160) = Mark Parity Transmitted and Received
;4 = 111 (224) = Space Parity Transmitted and Received
parity .byte %00000000,%00100000,%01100000,%10100000,%11100000

;PLAY command temp vars used during IRQ
temp1     .byte 0
temp2     .byte 0
playoct   .byte 4  ;default octave 4
playlen   .byte 30 ;notelength for each voice
playindex .byte 0  ;index of current char in play string for each voice

;** music notes table ***
;These numbers represent the middle octave for the SID freq control registers
;CLOCK is NTSC=1022727.143, PAL=985248.611
;FREQUENCY=REGVAL*(CLOCK/16777216)Hz
;REGVAL=FREQUENCY/(CLOCK/16777216)Hz
;https://pages.mtu.edu/~suits/notefreqs.html
;There are 8 octaves each with 12 notes 8*12 = 96 total notes
;The notes here are octave 4 on NTSC clock. The others are derived from these.
;
notes ;A    B    C    D    E    F    G
.word 3609,4051,4292,4817,5407,5729,6431
;flats(-) are derived from sharps(#). A- is in previous octave (G#)
;*for invalid notes C-/B# = B and F-/E# = E
;      A-
.word 3406
;      B-  *C-   D-   E-  *F-   G-
;      A#  *B#   C#   D#  *E#   F#   G#
.word 3824,4051,4547,5104,5407,6069,6813
;these are the same notes above but for PAL systems (30 byte offset)
.word 3746,4205,4455,5001,5613,5947,6675
.word 3536
.word 3969,4205,4720,5298,5613,6300,7072

;INF() memory locations
infbytes
.byte PNTR      ;csr logical column
.byte TBLX      ;csr physical line
.byte BLNSW     ;csr blink disabled, 0=no, 1=yes
.byte LNMX      ;csr max columns on current line (39 or 79)
.byte GDBLN     ;ASCII value of char under csr (when blinking)
.byte TMPASC    ;ASCII value of last character printed to screen
.byte CHANNL    ;current i/o file number (set by CMD)
.byte LDTND     ;num open files
.byte NDX       ;num chars in keyboard buffer
infwords
.word SHFLAG    ;shift flag
.word COLOR     ;current foreground color for text
.word GDCOL     ;color under cursor
.word PALNTSC   ;PAL or NTSC
.word $ff80     ;Kernal version/system id
.word playindex ;index of next note to play in play string
.word playoct   ;current play octave
.word lastplotx ;last plotted x coordinate
.word lastploty ;last plotted y coordinate

ascctrl
.byte 5,13,14,17,18,19,20,28
.byte 29,30,31,129,141,142,144,145
.byte 146,147,149,150,151,152,153,154
.byte 155,156,157,158,159
ascctlfn
.rta txtclr,txtcr,txtlc,txtdwn,txtrvson,txthome,txtbs,txtclr
.rta txtright,txtclr,txtclr,txtclr,txtcr,txtuc,txtclr,txtcsrup
.rta txtrvsoff,txtbitclr,txtclr,txtclr,txtclr,txtclr,txtclr,txtclr
.rta txtclr,txtclr,txtleft,txtclr,txtclr

;table for converting ascii to screen code
;except for ascii 64 which is screen code 0
;codes 192-223 same as 96-127
;codes 224-254 same as 160-190
;code  255 same as 126
asc2scr
;codes 160 to 255
.byte 32,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111
.byte 112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127
.byte 64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79
.byte 80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95
.byte 32,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111
.byte 112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,94

;scroll direction vectors up,down,left,right
scrolls .rta scroll0,scroll1,scroll2,scroll3

;10 tokens use line numbers that need to be renumbered when using renum
gotok
.byte TOKEN_GOTO,TOKEN_GOSUB,TOKEN_RETURN,TOKEN_THEN,TOKEN_ELSE
.byte TOKEN_RESUME,TOKEN_TRACE,TOKEN_DELETE,TOKEN_RUN,TOKEN_RESTORE

;strings
addchr .shift "+chr$("  ;used by keylist to display non-printable chars
nolin  .null "65535"    ;used by renum to find bad GO statements

;KEY (continued)
keyy
 ldx #0        ;even/odd key string offset
 lsr           ;key# 1-8 converted to index 0-4
 bcs oddkey    ;carry set for odd key nums
 sec
 sbc #1        ;convert 1-4 as index 0-3
 ldx #64
oddkey
 stx $61
 asl           ;key index (0 to 3) * 16
 asl
 asl
 asl
; clc carry is already clear from asl of a value 0 to 3
 adc $61
 tax
 lda $52
 beq endass   ;assign empty str
 cmp #16      ;check max str len
 bcc okass    ;length is between 1 and 15
 lda #15      ;enforce max length of 15
 sta $52
okass ldy #0
nextc lda ($50),y
 sta keybuf,x
 inx
 iny
 cpy $52      ;end of new string?
 bne nextc
endass
 lda #0 ;terminate string with zero-byte
setchr
 sta keybuf,x
 inx
 iny
 cpy #16      ;fill remaining bytes with 0
 bcc setchr
 jmp memnorm  ;switch LORAM back to LOROM
;
;**************************
;KEY LIST (continued)
keylistt
 jsr printcr
 lda #"1"
 sta $fd      ;current key#
 lda #0
 sta $fb      ;string offset in buffer
nextke
 jsr kprnt
 lda $fd
 cmp #"8"
 beq stpkeylst
 inc $fd       ;key#
 lda $fb      ;bit6=0 is odd key# else even key#
 eor #%01000000 ;bit7 is always 0
 sta $fb
 asl          ;sets minus flag if even key#
 bmi nextke
 lsr          ;restores value and clears carry flag
 adc #16      ;16 bytes per key
 bne nextke-2 ;will always branch
stpkeylst
 jsr CHRGET
 jmp memnorm  ;switch LORAM back to LOROM
;--------
kprnt
 lda #<keystr ;print the keyword KEY
 ldy #>keystr
 jsr printstr
 lda $fd      ;key#
 jsr CHROUT   ;print key number
 lda #","     ;followed by comma
 jsr CHROUT
 lda $fb      ;index offset
 sta $fc      ;current index
pkstr
 jsr printqt  ;print opening quote
nextlt
 ldy $fc
 lda keybuf,y
 beq stoppr
 sta $63
 cmp #13      ;carriage return?
 beq crqt
 cmp #34      ;quote?
 bne nocrqt
crqt
 jsr printqt
crqt2
 lda #<addchr
 ldy #>addchr
 jsr printstr ;+CHR$(
 lda #0       ;hibyte in a reg
 sta $62
 jsr rom4     ;switch ROM in and call routines to print int in FAC1
 lda #")"
 jsr CHROUT
 inc $fc      ;index offset
 ldy $fc
 lda keybuf,y
 beq chrprs
 cmp #13
 beq crqt2
 cmp #34
 beq crqt2
 lda #"+"
 jsr CHROUT
 jmp pkstr
nocrqt
 jsr CHROUT
noquot inc $fc
 jmp nextlt
stoppr
 jsr printqt
chrprs jmp printcr
;
;**************************
;initialize BASIC vectors
newvec
;set colors customizable by POKEing the lda value
 lda #14        ;light blue
 sta COLOR      ;current text color
 lda #0         ;black
 sta BGCOL0     ;background color black
 lda #0         ;black
 sta EXTCOL     ;border color black
 jsr initmdb    ;initialize MDBASIC
 lda #$80       ;all keys to repeat
 sta RPTFLAG
 lda #<immed
 sta IMAIN
 lda #>immed
 sta IMAIN+1
 lda #<toknew
 sta ICRNCH
 lda #>toknew
 sta ICRNCH+1
 lda #<newfun
 sta IEVAL
 lda #>newfun
 sta IEVAL+1
 lda #<list
 sta IQPLOP
 lda #>list
 sta IQPLOP+1
 lda #<brkirq
 sta CBINV
 lda #>brkirq
 sta CBINV+1
 lda #<keychk
 sta KEYLOG
 lda #>keychk
 sta KEYLOG+1
 lda #<newload
 sta ILOAD
 lda #>newload
 sta ILOAD+1
 lda #<newsave
 sta ISAVE
 lda #>newsave
 sta ISAVE+1
 lda #$ff   ;MDBASIC takes 8k of the BASIC RAM area
 sta $37    ;the highest address is now $7FFF (32767)
 lda #$7f   ;highest address used by BASIC, originally $9FFF (40959)
 sta $38
 rts
;
;**************************
linedraw
 lda $50   ;x2 lobyte
 sec
 sbc $fb
 sta $57
 lda $51   ;x2 hibyte
 sbc $fc
 sta $58
 lda $52   ;y2
 sec
 sbc $fd  ;destination y coord
 sta $6b  ;temp var for distance = y1-y2
 ldy #1
 ldx #0
 lda $fc  ;determine which x coord is larger
 cmp $51
 bcc storxy
 bne looper
 lda $50
 cmp $fb
 bcs storxy
looper ldy #$ff
 ldx #$ff
 lda $fb
 sec
 sbc $50
 sta $57
 lda $fc
 sbc $51
 sta $58
storxy sty $6f
 stx $70
 ldy #1
 lda $52
 cmp $fd   ;determine which y coord is larger
 bcs stya7
 tya       ;flip signed int value
 eor #$ff  ;by calc 2's compliment
 tay
 iny
 lda $fd
 sec
 sbc $52
 sta $6b ;y distance
stya7
 sty $a7 ;y coord step
 lda #0
 sta $59
 sta $69
 ldx $57
 ldy $58
 bne b5ne
 cpx $6b
 bcs b5ne
 ldx $6b
 jsr fac
 sta $69
 jmp drwlin
fac sty $6e
 tya
 lsr      ;will set carry bit if odd
 stx $6d
 txa
 ror      ;will pull in carry bit
 rts
b5ne jsr fac
 sta $59
drwlin lda #0
 sta $9e
 sta $9f
 sta $6a
 sta $5a
starts jsr pokadd
 lda $fc
 cmp $51
 bne aca3
 lda $fb
 cmp $50
 bne aca3
 lda $fd
 cmp $52
 bne aca3
 rts
aca3 lda $69
 clc
 adc $57
 sta $69
 lda $6a
 adc $58
 sta $6a
 cmp $6e
 beq a4is6e
 bcc a4cc
 bcs a4cs
a4is6e lda $69
 cmp $6d
 bcc a4cc
a4cs lda $69
 sbc $6d
 sta $69
 lda $6a
 sbc $6e
 sta $6a
 lda $fb
 clc
 adc $6f
 sta $fb
 lda $fc
 adc $70
 sta $fc
a4cc lda $59
 clc
 adc $6b
 sta $59
 lda $5a
 adc #$00
 sta $5a
 cmp $6e
 beq b7is6e
 bcc jmpout
 bne b7no6e
b7is6e lda $59
 cmp $6d
 bcc jmpout
b7no6e lda $59
 sbc $6d
 sta $59
 lda $5a
 sbc $6e
 sta $5a
 lda $fd
 clc
 adc $a7
 cmp #200       ;if y coord is out of bounds then we must be done
 bcs linedone
jmpout1 sta $fd ;y coordinate
jmpout jmp starts
linedone
 ldy XSAV       ;sprites can have a y coord to 255
 bne jmpout1    ;moving a sprite so continue
 rts
pokadd
 lda XSAV       ;flag 0=draw line, else move sprite
 beq setdot
;hack to move a sprite instead of plot line
 lda $fc        ;temp var hibyte of x coord
 beq nod010     ;x is less than 256
 lda $07        ;temp var of sprite's bit#
 ora MSIGX      ;MSB of sprites 0-7 x coordinate
 bne std010
nod010 lda $07
 eor #$ff
 and MSIGX
std010 sta MSIGX
 lda $fb
 ldy GARBFL     ;temp var of sprite reg index
 sta SP0X,y
 lda $fd
 sta SP0Y,y
;apply sprite move delay
 ldy $fe
 beq linedon
movewait
 lda LSTX     ;Matrix Coordinate of Last Key Pressed, 64=None Pressed
 cmp #$3f     ;STOP key?
 bne movwait  ;carry flag returned to caller to indicate STOP key pressed
 pla
 pla
 rts
movwait
 ldx #7
mowait dex
 bne mowait
 dey
 bne movewait
linedon rts
;
;**************************
setdot jsr ydiv8
 lda R6510
 pha
 and #%11111101 ;bit1 0=HIRAM
 sei            ;disable IRQ since kernal HIROM is switching to HIRAM
 sta R6510
 lda lastplott  ;plot type 00=off, 01=on, 10=flip, 11=none
 beq dotoff
 lsr
 beq doton
 bcc flipit
;no dot plot (locate pencil)
 pla
 sta R6510
 cli
 rts
flipit lda ptab3,x
 eor #$ff
 and ($c3),y
 ora ptab2,x
 eor ($c3),y
 jmp colorb
dotoff lda ptab2,x  ;read ptab2 and ptab3 in LORAM
 eor #$ff
 and ($c3),y        ;read bitmap in HIRAM
 jmp colorb
doton lda ($c3),y
 and ptab3,x
 ora ptab2,x
;write byte with bit pattern for the one bit in hires or 2 bits in mc mode
colorb sta ($c3),y
 pla
 sta R6510   ;restore Kernal HIROM ($e000-$ffff)
 cli
;apply color using video matrix and color RAM
 lda $fd     ;y coordinate
 lsr
 lsr
 lsr         ;video matrix line# (0-24) = y/8
 tay
 lda $fc     ;x coordinate hibyte
 lsr         ;into carry
 lda $fb     ;x coordinate lobyte
 ror         ;out of carry
 lsr         ;to calc x/8
 lsr
 clc
 adc $ecf0,y ;video matrix lowbyte at line y
 sta $c3
 lda btab,y  ;video maxtrix hibyte offset
 adc #$c8    ;video matrix starts at $c800 in bitmap mode
 sta $c4
 lda mapcolc1c2 ;hires dot color (hi nybble), bkgnd color of 8x8 sq (lo nybble)
 ldy #0
 sta ($c3),y
 lda $c4
 clc
 adc #$10     ;calculate beginning of color RAM
 sta $c4      ;which starts at $d800
 lda mapcolc3 ;background color mem used for mc mode
 sta ($c3),y
 rts
;---
ydiv8 lda $fd  ;y coordinate
 lsr
 lsr
 lsr
 tax
 lda $fb
 eor $fd
 and #%11111000
 eor $fd
 clc
 adc lbtab,x
 sta $c3
 lda hbtab,x
 adc $fc
 sta $c4
;
 lda $fb
 and #%00000111
 tax
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 beq ydiv8x
 txa
 and #%11111110
 ora mapcolbits ;apply color selection bit pattern
 tax
ydiv8x ldy #0
 rts
;
;**************************
;find index of ascii value in color code table
txtclr
 txa             ;ASCII value
 ldx #15         ;colors 0-15
clrtab
 cmp $e8da,x     ;PETASCII color code table
 beq xindex
 dex
 bpl clrtab
xindex
 txa             ;index of color
 asl             ;move to hi-nybble
 asl
 asl
 asl
 sta $61
 lda mapcolc1c2  ;hi nybble is dot color
 and #%00001111  ;erase hi nybble
 ora $61         ;temp storage of color
 sta mapcolc1c2  ;update global variable for colors
 rts
;
txtcr
 lda #0
 sta lastplotx
 sta lastplotx+1
txtdwn
 lda $58     ;text height scale
 asl         ;scale * 8
 asl
 asl
 clc
 adc lastploty
 cmp #200
 bcc oktxty
 rts
txtcsrup
 lda $58     ;text height scale
 asl         ;scale * 8
 asl
 asl
 sec
 sbc lastploty
 bpl oktxty
 rts
;
txtbitclr
 jsr bitmapclr
txthome
 lda #0
 sta lastplotx
 sta lastplotx+1
oktxty
 sta lastploty
 rts
;
txtright
 lda $57     ;char scale width (1-31)
 asl         ;scale * 8
 asl
 asl
 clc
 adc lastplotx
 tax
 lda lastplotx+1
 adc #0
 beq oktxtx
 cpx #64
 bcc oktxtx
 rts
txtleft
 lda $57     ;char scale width (1-31)
 asl         ;scale * 8
 asl
 asl
 sta $61     ;temp storage
 lda lastplotx
 sec
 sbc $61
 tax
 lda lastplotx+1
 sbc #0
 clc         ;flag indicating x not set
 bmi done2   ;not enough space so abort
oktxtx
 sta lastplotx+1
 stx lastplotx
 sec         ;flag indicating x set
done2 rts
txtbs
 jsr txtleft
 bcc done2
 lda #32     ;print space
 jmp txtprint
;
txtlc ;lower case current charset
;charset $d0,$d4 uppercase $d8,$dc lowercase
 lda $26
 ora #%00001000
 sta $26
 rts
txtuc ;upper case current charset
;charset $d0,$d4 uppercase $d8,$dc lowercase
 lda $26
 and #%11110111
 sta $26
 rts
txtrvsoff ;rvs off current charset
;charset $d0,$d8 norm, $d4,$dc rvs
 lda $26
 and #%11111011
 sta $26
 rts
txtrvson ;rvs on current charset
;charset $d0,$d8 norm, $d4,$dc rvs
 lda $26
 ora #%00000100
 sta $26
 rts
;
txtprint
 jsr times8     ;multiply A reg value by 8 result in $be,$bf
 lda $26        ;hibyte ptr to dot data of current charset
 clc
 adc $bf        ;add charset hibyte offset
 sta $bf
doloop3 lda $58
 sta $5a        ;temp var for y size multiplication by decrement loop
doloop2 lda #128
 sta $5c
doloop1 lda $57
 sta $5b        ;temp var for x size multiplication by decrement loop
doloop ldy $59
 lda R6510
 tax
 and #%11111011 ;bit2=0 switch in CHAREN ROM into bank $d000-$dfff
 sei
 sta R6510      ;use CHAREN (rom char images)
 lda ($be),y    ;read CHAREN byte data for character (8 bytes, y=byte#) 
 stx R6510
 cli
 and $5c
 beq nodot
 lda $ff        ;cmd plot type
nodot
 sta lastplott  ;dot plot type
 lda $fc        ;do not plot if x coordinate out of range
 beq getfd
 lda $fb
 cmp #$40
 bcs nextfc
getfd lda $fd   ;do not plot if y coordinate out of range
 cmp #$c8
 bcs nextfc
 jsr setdot     ;plot the dot
nextfc inc $fb
 bne dec5b
 inc $fc
dec5b dec $5b
 bne doloop
 lsr $5c
 bne doloop1
;next line of bits in char
 inc $fd
 lda lastplotx
 sta $fb
 lda lastplotx+1
 sta $fc
 dec $5a
 bne doloop2
 inc $59
 lda $59
 cmp #8
 bcc doloop3
 rts
;**************************
texter lda $58  ;y size
 beq texted
 lda $57        ;x size
 beq texted
 lda lastplott
 sta $ff        ;save plot type
nextchar lda #0
 sta $59        ;index variable for current byte of dot data in char
 ldy #0
 lda ($50),y    ;get character to display on bitmap
;convert ascii to screen code
 tay
 bpl cd0to127
 sec
 sbc #128+32
 bcc nonprt0
 tay
 lda asc2scr,y
 jmp dotptr
nonprt0
 tya            ;restore original ascii value
nonprt
 ldy #28        ;29 ascii values have funcs
fndctrl
 cmp ascctrl,y  ;ascii in ctrl code table?
 beq fndctrl2   ;yes, execute func indexed by y
 dey            ;no, check next code in table
 bpl fndctrl    ;if not in table then
 bmi nxtchar    ;do nothing
cd0to127
 cmp #32
 bcc nonprt
 cmp #64
 bcc dotptr
 sbc #64
;determine mem ptr of dot data
dotptr
 jsr txtprint
 jsr txtright
nxtchar
 inc $50
 bne noinc51
 inc $51
noinc51
 jsr getpoint   ;set current plot coordinates
 dec $52
 bne nextchar
 lda $ff        ;restore original plot type
 sta lastplott
texted jmp memnorm
;execute function index by y
fndctrl2
 tax            ;save ascii value
 tya
 asl
 tay
;push return function
 lda #>nxtchar-1
 pha
 lda #<nxtchar-1
 pha
;push call function
 lda ascctlfn+1,y
 pha
 lda ascctlfn,y
 pha
 rts
;
;**************************
painter
 ldx #1         ;bits per pixel, 1=hires, 2=multicolor
 ldy #$07
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 beq xy00
 inx
 dey
xy00 stx $5a
 sty $5b
 lda #0
 sta $57
beginp
 lda #0
 sta $58
 sta $59
startp lda $fc
 bne fcno0
 lda $fb
 beq nxtpnt
fcno0 lda $fb
 sec
 sbc $5a
 sta $fb
 lda $fc
 sbc #$00
 sta $fc
 jsr readb
 beq startp
 lda $fb
 clc
 adc $5a
 sta $fb
 lda $fc
 adc #$00
 sta $fc
nxtpnt inc $fd
 jsr readb
 bne law0
 lda $58
 bne fdm1
 jsr buffit
 lda #1
.byte $2c       ;alt entry point to defeat LDA #0 by making it BIT $00A9
law0 lda #0
 sta $58
fdm1 dec $fd
 dec $fd
 jsr readb
 bne law00
 lda $59
 bne fdp1
 jsr buffit
 lda #1
.byte $2c     ;alt entry point to defeat LDA #0 by making it BIT $00A9
law00 lda #0
 sta $59
fdp1 inc $fd
 jsr setdot   ;plot pixel
 lda $fb
 clc
 adc $5a
 sta $fb
 lda $fc
 adc #$00
 sta $fc
 beq peekit
 lda $fb
 cmp #$40
 bcs fillit
peekit jsr readb
 beq nxtpnt
fillit
 ldy $57
 beq epaint
 dey
 lda paintbuf1,y
 sta $fd
 lda paintbuf2,y
 sta $fc
 lda paintbuf3,y
 sta $fb
 sty $57
 lda $fd
 cmp #$c8
 bcs fillit
;check STOP key
 lda LSTX
 cmp #$3f
 beq epaint
 jmp beginp
epaint
 jmp memnorm  ;switch LORAM back to LOROM and HIRAM back to HIROM
;
readb jsr ydiv8
 lda R6510
 and #%11111101 ;bit1 0=HIRAM
 sei            ;disable IRQ since kernal HIROM is switching to HIRAM
 sta R6510
 lda ptab3,x
 eor #$ff
 and ($c3),y    ;bitmap in HIRAM
 tay
 lda R6510
 ora #%00000010 ;bit1 1=HIROM
 sta R6510
 cli
 txa
 and #%00000111
 tax
 tya
 cpx $5b
 bcs flgit
divid2 lsr
 inx
 cpx $5b
 bcc divid2
flgit cmp #$00
 rts
;
buffit ldy $57
 lda $fb
 sta paintbuf3,y
 lda $fc
 sta paintbuf2,y
 lda $fd
 sta paintbuf1,y
 inc $57
donee
 rts
;
;process radial line options
ciropts
;set flag for line draw
 lda #0    ;0=draw line, 1=move sprite
 sta XSAV
;init point1 and point2 for radial line
 ldx #2
pntcpy
 lda lastplotx,x
 sta $50,x
 sta $fb,x
 dex
 bpl pntcpy
;check quadrant bit flags for line segment
 asl $2a
 bcc copt2
 lda $fd
 clc
 adc $36
 bcs okopt1-2
 cmp #200
 bcc okopt1
 lda #199
okopt1 sta $fd
 jsr linedraw
copt2
 asl $2a
 bcc copt3
 lda $fb
 sec
 sbc $35
 sta $fb
 lda $fc
 sbc #0
 bpl okopt2
 lda #0
 sta $fb
okopt2 sta $fc
 jsr linedraw
copt3
 asl $2a
 bcc copt4
 lda $fd
 sec
 sbc $36
 bcs okopt3
 lda #0
okopt3 sta $fd
 jsr linedraw
copt4
 asl $2a
 bcc donee
 lda $fb
 clc
 adc $35
 sta $fb
 lda $fc
 adc #0
 beq okopt4
 ldx $fb
 cpx #64
 bcc okopt4
 ldx #63
 stx $fb
okopt4 sta $fc
 jmp linedraw
;
;**************************
circel
 lda $fb  ;center point x lobyte
 asl
 sta $61
 lda $fc  ;center point x hibyte
 rol
 sta $62
;
 lda $fd  ;center point y lobyte
 asl
 sta $63
 lda #0   ;center point y hibyte
 sta $fe
 rol
 sta $64
;
 lda $35  ;x radius size
 asl      ;diameter = 2*radius
 sta $11  ;x diameter lobyte
 sta $57
 sta $50
 lda #0
 rol
 sta $12  ;x diameter hibyte
 sta $58
 sta $51
;
 lda $36  ;y radius size
 asl      ;diameter = 2*radius
 sta $14  ;y diameter
 lda #0
 rol
 sta $15  ;y diameter hibyte
;
 lda #2
 ldx #3
 ldy #1
 jsr curve
 ldx #2
tb6tc4 lda $59,x
 sta $22,x
 dex
 bpl tb6tc4
 lda $15
 sta $58
 sta $51
 lda $14
 sta $57
 sta $50
 lda #2
 ldx #2
 ldy #1
 jsr curve
 lda $5a
 sta $0f
 lda $59
 sta $0e
 ldx #0
 stx $65
 lda $15    ;y diameter hibyte
 cmp $12    ;x diameter hibyte
 bne cbne
 lda $14    ;y diameter lobyte
 cmp $11    ;x diameter lobyte
 bne cbne
 ldy $15
 bne csca
 cmp #2     ;x & y diameter is 2?
 beq jloops ;special case, no looping needed
cbne bcs csca
 inc $65
csca
 stx $66
 lda $65
 beq ce00
 jsr tfb2b2
 inc $66
jloops jmp loops
ce00 jsr loops
 inc $66
tfb2b2 lda $11
 sta $fb
 sta $57
 lda $12
 sta $fc
 sta $58
 lda #0
 sta $fd
 sta $fe
 lda $0f
 sta $51
 lda $0e
 sta $50
 lda #2
 ldx #4
 ldy #1
 jsr curve
 ldx #3
tb6td8 lda $59,x
 sta $25,x
 dex
 bpl tb6td8
 lsr $28
 ror $27
 ror $26
 ror $25
 lda $fe
 beq be00
 jsr drwcir
 jmp fd512
be00 jsr tfb2d4
 lda $fb
 clc
 adc $61
 sta $fb
 lda $fc
 adc $62
 lsr
 sta $fc
 ror $fb
 lda $fd
 clc
 adc $63
 sta $fd
 lda $fe
 adc $64
 lsr
 sta $fe
 ror $fd
 lda $2a   ;circle options
 and #%00001001 ;show quad 1 or 4?
 beq no0
 jsr plotc
no0 lda $61
 sec
 sbc $6f
 sta $fb
 lda $62
 sbc $70
 lsr
 sta $fc
 ror $fb
 lda $2a   ;circle options
 and #%00000110 ;show quad 2 or 3?
 beq no180
 jsr plotc
no180 jsr t2d4fb
fd512 lda $fd
 clc
 adc #2
 sta $fd
 sta $57
 lda $fe
 adc #$00
 sta $fe
 sta $58
 ldx #2
tc4taf lda $22,x
 sta $50,x
 dex
 bpl tc4taf
 lda #3
 ldx #3
 ldy #1
 jsr curve
 ldx #0
 ldy #3
 clc
d8pb6 lda $25,x
 adc $59,x
 sta $25,x
 inx
 dey
 bne d8pb6
 bcc dpcsym
 inc $28
dpcsym
 lda $fc
 sta $58
 lda $fb
 sta $57
 lda $0f
 sta $51
 lda $0e
 sta $50
 lda #2
 ldx #3
 ldy #1
 jsr curve
 lda $28
 bne ne2db
 ldx #2
xpostv lda $25,x
 cmp $59,x
 bcc ccd8b6
 bne ne2db
 dex
 bpl xpostv
 bmi ccd8b6
ne2db ldx #0
 ldy #3
 sec
d8mb6 lda $25,x
 sbc $59,x
 sta $25,x
 inx
 dey
 bne d8mb6
 bcs csd8b6
 dec $28
csd8b6 lda $fb
 sec
 sbc #2
 sta $fb
 lda $fc
 sbc #0
 sta $fc
ccd8b6 jsr drwcir
 lda $66
 bne ne2cf
 ldx #2
t2b669 lda $59,x
 sta $69,x
 dex
 bpl t2b669
 lda $fe
 sta $58
 lda $fd
 sta $57
 ldx #2
tc4af lda $22,x
 sta $50,x
 dex
 bpl tc4af
 lda #3
 ldx #4
 ldy #1
 jsr curve
 lda $5c
 bne ne2b9
 ldx #2
xposte lda $59,x
 cmp $69,x
 bcs cs69
fd512a jmp fd512
cs69 bne ne2b9
 dex
 bpl xposte
ne2b9 lda $fb
 sta $67
 lda $fc
 sta $68
 lda $fd
 sta $6d
 lda $fe
 sta $6e
 rts
ne2cf lda $6d
 sec
 sbc $fd
 sta $69
 lda $6e
 sbc $fe
 sta $6a
 lda $6a
 bne fd512a
 lda $69
 cmp #3
 bcs fd512a
 rts
curve stx $5e
 sta $5d
 lda #0
 dex
f2b600 sta $59,x
 dex
 bpl f2b600
 sty $5f
 lda #$80
 sta $60
 ldx $5f
cc2bd and $57,x
 bne bdb2ne
 lsr $60
 lda $60
 bcc cc2bd
 ror $60
 lda $60
 dec $5f
 dex
 bpl cc2bd
 rts
lsr2bd lsr $60
 bcc ccd2
 ror $60
 dec $5f
 bpl ccd2
 rts
ccd2 ldx #0
 ldy $5e
 clc
roling rol $59,x
 inx
 dey
 bne roling
 ldx $5f
 lda $57,x
 and $60
 beq lsr2bd
bdb2ne ldx #0
 ldy $5d
 clc
lda2b6 lda $59,x
 adc $50,x
 sta $59,x
 inx
 dey
 bne lda2b6
 bcc lsr2bd
 dex
x2bb inx
 cpx $5e
 bcs lsr2bd
 inc $59,x
 beq x2bb
 bne lsr2bd
drwcir jsr tfb2d4
 lda $fb
 clc
 adc $61
 sta $fb
 lda $fc
 adc $62
 lsr
 sta $fc
 ror $fb
 lda $fd
 clc
 adc $63
 sta $fd
 lda $fe
 adc $64
 lsr
 sta $fe
 ror $fd
 lda $2a   ;circle options
 and #%00001000 ;show quad 4?
 beq plotq3
 lda $29   ;hires mode?
 beq plotq4
 inc $fb
 bne plotq4
 inc $fc
plotq4
 jsr plotc ;quad4
plotq3
 lda $61
 sec
 sbc $6f
 sta $fb
 lda $62
 sbc $70
 lsr
 sta $fc
 ror $fb
 lda $2a   ;circle options
 and #%00000100 ;show quad 3?
 beq plotq2
 jsr plotc ;quad3
plotq2
 lda $63
 sec
 sbc $71
 sta $fd
 lda $64
 sbc $72
 lsr
 sta $fe
 ror $fd
 lda $2a   ;circle options
 and #%00000010 ;show quad 2?
 beq plotq1a
 jsr plotc ;quad2
plotq1a
 lda $61
 clc
 adc $6f
 sta $fb
 lda $62
 adc $70
 lsr
 sta $fc
 ror $fb
 lda $2a   ;circle options
 and #%00000001 ;show quad 1?
 beq t2d4fb
 lda $29   ;hires mode?
 beq plotq1
 inc $fb
 bne plotq1
 inc $fc
plotq1
 jsr plotc ;quad1
t2d4fb lda $6f
 sta $fb
 lda $70
 sta $fc
 lda $71
 sta $fd
 lda $72
 sta $fe
 rts
tfb2d4 lda $fb
 sta $6f
 lda $fc
 sta $70
 lda $fd
 sta $71
 lda $fe
 sta $72
 rts
loops
 lda $14
 sta $fd
 sta $57
 lda $15
 sta $fe
 sta $58
 lda #0
 sta $fb
 sta $fc
 ldx #2
c4af lda $22,x
 sta $50,x
 dex
 bpl c4af
 lda #3
 ldx #4
 ldy #1
 jsr curve
 ldx #3
b6d8 lda $59,x
 sta $25,x
 dex
 bpl b6d8
 lsr $28
 ror $27
 ror $26
 ror $25
 jsr tfb2d4
 lda $61
 sta $fb
 lda $62
 lsr
 sta $fc
 ror $fb
 lda $fd
 clc
 adc $63
 sta $fd
 lda $fe
 adc $64
 lsr
 sta $fe
 ror $fd
 lda $2a   ;circle options
 and #%00001100 ;show quad 3 or 4?
 beq no270
 jsr plotc
no270 lda $63
 sec
 sbc $71
 sta $fd
 lda $64
 sbc $72
 lsr
 sta $fe
 ror $fd
 lda $2a   ;circle options
 and #%00000011 ;show quad 1 or 2?
 beq no90
 jsr plotc
no90 jsr t2d4fb
loops2
 lda $fb
 clc
 adc #2
 sta $fb
 sta $57
 lda $fc
 adc #0
 sta $fc
 sta $58
 lda $0f
 sta $51
 lda $0e
 sta $50
 lda #2
 ldx #3
 ldy #1
 jsr curve
 ldx #0
 ldy #3
 clc
d8b6 lda $25,x
 adc $59,x
 sta $25,x
 inx
 dey
 bne d8b6
 bcc ncs
 inc $28
ncs lda $fe
 sta $58
 lda $fd
 sta $57
 ldx #2
c4af2 lda $22,x
 sta $50,x
 dex
 bpl c4af2
 lda #3
 ldx #4
 ldy #1
 jsr curve
 ldx #3
cd8b6 lda $25,x
 cmp $59,x
 bcc ncs2b6
 bne ne2b6
 dex
 bpl cd8b6
 bmi ncs2b6
ne2b6 ldy #4
 ldx #0
 sec
xnot0 lda $25,x
 sbc $59,x
 sta $25,x
 inx
 dey
 bne xnot0
 lda $fd
 sec
 sbc #2
 sta $fd
 lda $fe
 sbc #0
 sta $fe
ncs2b6 jsr drwcir
 lda $66
 bne fbm2d0
 ldx #3
x59 lda $59,x
 sta $69,x
 dex
 bpl x59
 lda $fc
 sta $58
 lda $fb
 sta $57
 lda $0f
 sta $51
 lda $0e
 sta $50
 lda #2
 ldx #3
 ldy #1
 jsr curve
 lda $6c
 beq zero6c
loopy
 jmp loops2
zero6c ldx #2
loopy2
 lda $59,x
 cmp $69,x
 bcc loopy
 bne nencs
 dex
 bpl loopy2
nencs lda $fb
 sta $67
 lda $fc
 sta $68
 lda $fd
 sta $6d
 lda $fe
 sta $6e
 rts
fbm2d0 lda $67
 sec
 sbc $fb
 sta $69
 lda $68
 sbc $fc
 sta $6a
 bne loopy
 lda $69
 cmp #3
 bcs loopy
c69w3 rts
;validate calculated coordinate, plot if ok, skip if not
plotc ldx $fc  ;x coordinate hibyte
 beq chbyc
 dex           ;x coordinate range 0-319
 bne c69w3     ;hibyte must be 0 or 1
 lda $fb       ;when x hibyte is 1
 cmp #64       ;max lobyte is 64
 bcs c69w3     ;do not plot out of range
chbyc lda $fe  ;y coordinate hibyte
 bne c69w3
 lda $fd
 cmp #200      ;y coordinate range 0-199
 bcs c69w3
 jmp setdot
;
;*********************
;load command re-write
;secondary address 16=SCREEN, 17=CHAREN, 18=BITMAP
;*********************
loadd
 ldx #0
 jsr lodsav  ;open file and check status, quit if needed
 jsr $f5af   ;print SEARCHING...
 ldx LA      ;logical file number
 jsr CHKIN   ;set default input logical file number
 jsr CHRIN   ;get first byte from file
 sta XSAV    ;save for later
 jsr chkio   ;check i/o status, error if needed
 jsr $f5d2   ;print LOADING...
 lda $fe     ;secondary address
 cmp #16     ;16=screen
 bne sdnot2
lodsrn
 jsr param   ;prepare text and color mem pointers
 lda XSAV    ;recall first read byte (border color)
lodscrn
 sta EXTCOL  ;set border color
 jsr CHRIN   ;get background color
 sta BGCOL0  ;set background color
 jsr CHRIN   ;get VMCSB
 sta VMCSB   ;VIC-II base address for video matrix and text dot data
 jsr CHRIN   ;get SCROLX
 sta SCROLX  ;save bit 4 for multicolor text or bitmap flag
 jsr CHRIN   ;get SCROLY
 sta SCROLY  ;save bit 5 for bitmap mode flag
 jsr CHRIN
 sta CI2PRA  ;VIC-II memory bank
lode
 jsr CHRIN   ;continue loading the rest of the data
 sta ($50),y ;storing the bytes in the text
 jsr CHRIN   ;and
 sta ($fb),y ;color memory areas
 jsr status  ;check for stop key or EOF and do not return here if so
 inc $50
 inc $fb
 bne lode
 inc $fc
 inc $51
 bne lode
 jmp romin   ;a valid file should never reach this line
sdnot2
 cmp #17     ;17=CHAREN
 bne sdnot3
 jsr param2
 lda XSAV    ;recall first read byte
lodchr
 sta ($50),y
 jsr status  ;check for stop key or EOF and do not return here if so
 jsr CHRIN   ;get next byte
 inc $50
 bne lodchr
 inc $51
 bne lodchr
 jmp romin   ;a valid file should not reach this line
sdnot3
 jsr param3  ;18=BITMAP
 lda XSAV
lodbm
 sta ($50),y
 jsr status  ;check for stop key or EOF and do not return here if so
 jsr CHRIN
 inc $50
 bne lodbm
 inc $51
 bne lodbm
 lda #$c8
 jsr param4
 jsr CHRIN   ;get first byte of screen info
 jmp lodscrn ;finish by loading the remainder of the file
;
;*********************
;save command re-write
;secondary address 16=SCREEN, 17=CHAREN, 18=BITMAP
;*********************
savee
 ldx #1
 jsr lodsav
 jsr $f68f   ;print SAVING filename
 ldx LA      ;current logical file number
 jsr CHKOUT  ;set stdout to current logical file number
 lda $fe     ;MDBASIC secondary address (16,17,18)
 cmp #16     ;16=screen
 bne sdvn2
;save screen
savscr
 jsr param
savscr2
 lda $51
 clc
 adc #3
 sta $fe
 lda EXTCOL  ;border color
 jsr CHROUT
 lda BGCOL0  ;background color
 jsr CHROUT
 lda VMCSB   ;VIC-II base address for video matrix and text dot data
 jsr CHROUT
 lda SCROLX  ;flags for multicolor text/bitmap, num cols flag
 jsr CHROUT
 lda SCROLY  ;flags for bitmap mode, ext color text mode, num rows flag
 jsr CHROUT
 lda CI2PRA  ;VIC-II memory bank
 jsr CHROUT
 ldy #0
saves
 lda ($50),y ;char
 jsr CHROUT
 lda ($fb),y ;color
 jsr CHROUT
 jsr status  ;check for stop key or i/o error and do not return here if so
 inc $fb
 inc $50
 bne srnend
 inc $fc
 inc $51
srnend lda $51
 cmp $fe
 bne saves
 lda $50
 cmp #232
 bne saves
 jmp romin
;save redefined charset
sdvn2
 cmp #17        ;17=CHAREN
 bne savbm
 jsr param2     ;prepare pointer for dot data base addr
savchr
 jsr byteout
 jsr status     ;check for stop key or EOF and do not return here if so
 inc $50
 bne savchr
 inc $51
 bne savchr
 jmp romin
;save a bitmap image with colors
savbm           ;18=BITMAP
 jsr param3     ;prepare pointers for bitmap and color mem
savbtm
 jsr byteout
 jsr status     ;check for stop key or EOF and do not return here if so
 inc $50
 bne savbtm
 inc $51
 bne savbtm
 lda #$c8       ;video matrix is at $C800
 jsr param4     ;prepare pointers for text and color mem
 jmp savscr2    ;finish by saving the screen mem bytes
;
byteout
 lda R6510
 tax
 and #%11111101 ;switch out Kernal for RAM at $e000-$ffff
 sei            ;disable IRQ while Kernal is switched out
 sta R6510
 lda ($50),y
 stx R6510      ;switch out RAM for Kernal at $e000-$ffff
 cli            ;restore IRQ now that Kernal is swiched back in
 jmp CHROUT     ;output byte to current output channel
;
lodsav          ;real secondary device in x reg
 lda SA         ;secondary address
 sta $fe        ;save MDBASIC secondary device # (16,17,18)
 stx SA         ;replace secondary device with desired real value
 jsr OPEN       ;perform OPEN
 bcs stopls     ;error, abort load or save
loded rts
;
status jsr STOP
 beq stopls
 jsr READST
 beq loded
stopls
 pla          ;discard caller return addr
 pla
 jmp romin    ;quit
;
param         ;setup screen pointers
 lda HIBASE   ;hibyte of top of text screen
param4
 ldx #0
 stx $fb      ;ptr to color memory
 ldx #$d8     ;$D800 = color memory location
 stx $fc
.byte $2c
param2        ;setup CHAREN pointer
 lda #$f0
.byte $2c
param3        ;setup bitmap pointer
 lda #$e0     ;hibyte of top of bitmap screen
;apply ptr
 ldy #0
 sty $50
 sta $51
 rts
;
;*********************
varss
 lda VARTAB   ;pointer of beginning of non-array variable storage
 ldy VARTAB+1
chk2d
 cmp ARYTAB   ;pointer of beginning of array variable storage
 bne copy2d   ;if both vectors point at same mem loc then no vars defined
 cpy ARYTAB+1
 bne copy2d
evar
 jmp memnorm  ;switch LORAM back to LOROM
copy2d
 sta $fb
 sty $fc
;check shift and stop keys
 jsr chkpause
 beq evar   ;stop key pressed
;get 2 byte variable name
;if both bytes have bit 7 clear then float
;if bit 7 of only the first byte is set then fn
;if bit 7 of only the second byte is set then string
;if bit 7 of both bytes is set then int
;bit patterns: 00=float, 01=string, 10=fn, 11=int
 ldy #0
 sty VALTYP   ;set default type to float
 lda ($fb),y  ;first byte
 sta $45      ;save for later
 asl          ;shift bit 7 into carry
 rol VALTYP   ;then roll carry into VALTYPE
 iny
 lda ($fb),y  ;second byte of var name
 sta $46      ;second char of var name (space if not needed)
 asl          ;shift bit 7 into carry
 rol VALTYP   ;then roll carry into VALTYPE
;skip over 2 byte name
 lda #2
 jsr addfb
;
 lda VALTYP  ;type 0=float, 1=string, 2=fn, 3=int
 cmp #2      ;fn
 beq nxtvar+3 ;skip fn types
 lda $45     ;get first char of name
 jsr prtchr  ;output first char of name
 lda $46     ;get second char of name
 jsr prtchr  ;output second char of name
 ldy VALTYP  ;var type?
 beq float   ;type 0 is float
 dey         ;type 1 string
 beq string  ;otherwise int
 lda #"%"    ;display integer symbol
 jsr CHROUT
 jsr printeq
 ldy #0
 lda ($fb),y ;first byte is lobyte for int
 pha
 iny
 lda ($fb),y ;second byte is hibyte for int
 tay         ;lobyte in y reg
 pla         ;hibyte in a reg
 jsr rom1    ;switch ROM in and call routines to print int in a,y regs
 jmp nxtvar  ;process next variable
float
 jsr printeq
 lda $fb
 ldy $fc
 jsr rom2
 jmp nxtvar
string lda #"$"
 jsr CHROUT
 jsr printeq
 jsr printqt
; ldy #0
 lda ($fb),y
 beq endquote
 sta $52
 iny
 lda ($fb),y
 sta $50
 iny
 lda ($fb),y
 sta $51
 ldy #0
prtstr lda ($50),y
 jsr CHROUT
 iny
 dec $52
 bne prtstr
endquote
 jsr printqt
nxtvar
 jsr printcr
;calc position for next variable
 lda $fb
 clc
 adc #5
 tax
 lda $fc
 adc #0
 tay
 txa
 jmp chk2d
;
;***************
dumpscreen2
 ldy #$00        ;secondary parmeter 0 = assume upper case with symbols
 lda #%00001000  ;bits1-3 determine dot matrix for chars
 bit VMCSB       ;text mode upper case with symbols or upper/lower case?
 bne upcase      ;yes, use upper/lower case text
 ldy #$07        ;7=upper and lower case, 0 = upper case and symbols
upcase jsr opnprt
;send control codes for ESC/P printers
;Set n/180-inch line spacing using codes ESC 3 n
 lda #27         ;ESC
 jsr CHROUT
 lda #"3"        ;3
 jsr CHROUT
 lda #25         ;n
 jsr CHROUT
 ldy #0
 sty $fb
 sty $fe         ;column counter
 lda HIBASE      ;top page of screen mem hibyte
 sta $fc
pchr lda ($fb),y ;scan (screen) code
;convert scan code to a printable standard character
 pha
 bpl testit
 and #%01111111
testit cmp #32
 bcs big32       ;larger than 32
add64 clc
 adc #64
 jmp dumpit
big32 cmp #64
 bcc dumpit
 cmp #96
 bcs add64       ;larger than 96
; clc not needed here since already clear
 adc #32
dumpit sta $61
 pla
 bpl regchr
 lda #18        ;RVS mode on
 jsr CHROUT
 lda $61
 jsr CHROUT
 lda #146       ;RVS mode off
 jsr CHROUT
 jmp nxchar
regchr lda $61
 jsr CHROUT
nxchar inc $fe
 lda $fe
 cmp #40         ;40 columns?
 bne infbfc
 jsr printcr
 lda #0
 sta $fe
infbfc inc $fb
 bne stopyn
 inc $fc
stopyn lda $fc
 cmp #7          ;check if last address
 bne pchr
 lda $fb
 cmp #232
 bne pchr
 lda #$13        ;home
 jmp CHROUT
;
dumpbitmap2
 ldy #5          ;secondary param - binary graphic
 jsr opnprt
;send printer codes for ESC/P printers
;set line spacing using codes ESC A n
;lines per inch is n/60 or n/72 for 9-pin printers
 lda #27         ;ESC
 jsr CHROUT
 lda #65         ;A
 jsr CHROUT
 lda #8          ;n
 jsr CHROUT
;prepare mem pointer starting at $FE07
 lda #$07
 sta $fb
 lda #$fe
 sta $fc
 jsr screenon   ;turn screen off for exclusive use of data bus (faster)
 lda #$28
 sta $fe
 lda #15
 sta $61
;begin loop to print 300x200 bitmap image
nxtbit lda $61
 eor #$ff
 sta $61
 ldx #$00
;send printer codes for ESC/P printers
;select 60-dpi graphics using codes ESC K nL nH
 lda #27    ;ESC
 jsr CHROUT
 lda #75    ;K
 jsr CHROUT
 lda #144   ;nL
 jsr CHROUT
 lda #1     ;nH
 jsr CHROUT
;print data
 lda #25
 sta $fd
 lda #8
 sta $61  ;temp var
pekbyt
;select HIRAM and disable IRQ
 lda R6510
 pha            ;save current mem bank setting
 and #%11111101 ;switch i/o devices out for RAM
 sei            ;disable irqs while i/o devices are not available
 sta R6510      ;apply mem bank new selection
;read byte from bitmap image in HIRAM
 lda ($fb),y
 tax            ;save in x
 pla
 sta R6510      ;switch out RAM for i/o devices
 cli            ;enable irqs now that i/o devices are restored
 txa            ;get saved value
 and $61
 cmp #16        ;hi nybble to lo nybble for indexing 
 bcc ttatx      ;value is between 0 and 15, good index
 lsr            ;move hi nybble to lo nybble for indexing
 lsr
 lsr
 lsr
ttatx tax
;translate graphic info to printable value
 lda bmdt,x  ;read value from bitmap data table stored in LORAM
 jsr CHROUT
 jsr CHROUT  ;print image 2x original size
 jsr STOP    ;stop key pressed?
 beq aldone
 lda $fb
 bne s1fbfc
 dec $fc
s1fbfc dec $fb
 dec $61     ;temp var
 bne pekbyt
 lda $fb
 sec
 sbc #56
 sta $fb
 lda $fc
 sbc #1
 sta $fc
 dec $fd
 bne pekbyt-5
 lda $fb
 clc
 adc #64
 sta $fb
 lda $fc
 adc #$1f
 sta $fc
 jsr printcr ;carriage return
 lda #10     ;line feed
 jsr CHROUT
 lda $61
 bpl posnum
 jmp nxtbit
posnum lda $fb
 clc
 adc #8
 sta $fb
 bcc tcic
 inc $fc
tcic dec $fe
 beq aldone
 jmp nxtbit
aldone
 jsr screenoff    ;turn screen back on
;restore printer to 1/6 inch line using control codes for ESC/P printers
 lda #27    ;ESC
 jsr CHROUT
 lda #"2"
 jsr CHROUT
 rts
;
;***********************
;scroll up
scroll0
 jsr wrapit
nxtup lda $fb
 clc
 adc #40
 sta $57
 lda $fc
 adc #0
 sta $58
 lda $fd
 clc
 adc #40
 sta $59
 lda $fe
 adc #0
 sta $5a
;
 ldy $be       ;columns to scroll
 dec $bf       ;rows to scroll
 bpl cpyup
wrapup
 lda XSAV      ;wrap?
 bne gowrapup
 ldx COLOR     ;current foreground color
 lda #32       ;space char
 bne sftup     ;always branches
gowrapup
 lda paintbuf1,y
 ldx paintbuf2,y
sftup
 sta ($fb),y   ;char
 txa
 sta ($fd),y   ;color
 dey
 bpl wrapup
 jmp memnorm  ;switch LORAM back to LOROM
cpyup lda ($57),y
 sta ($fb),y
 lda ($59),y
 sta ($fd),y
 dey
 bpl cpyup
 lda $57
 sta $fb
 lda $58
 sta $fc
 lda $59
 sta $fd
 lda $5a
 sta $fe
 jmp nxtup
;scroll down
scroll1 jsr calcptr
 jsr wrapit
nxtdwn ldy $be ;columns to scroll
 lda $fb
 sec
 sbc #40
 sta $57
 lda $fc
 sbc #0
 sta $58
 lda $fd
 sec
 sbc #40
 sta $59
 lda $fe
 sbc #0
 sta $5a
 dec $bf
 bpl cpydwn
 bmi wrapup
cpydwn lda ($57),y
 sta ($fb),y
 lda ($59),y
 sta ($fd),y
 dey
 bpl cpydwn
 lda $57
 sta $fb
 lda $58
 sta $fc
 lda $59
 sta $fd
 lda $5a
 sta $fe
 jmp nxtdwn
;scroll left
scroll2 ldy #0
 lda ($fb),y
 sta $50
 lda ($fd),y
 sta $51
cpyleft iny
 lda ($fb),y
 dey
 sta ($fb),y
 iny
 lda ($fd),y
 dey
 sta ($fd),y
 iny
 cpy $be     ;columns to scroll
 bne cpyleft
 jsr scrollh
 dec $bf     ;rows to scroll
 bpl scroll2
hsdone jmp memnorm ;switch LORAM back to LOROM
;scroll right
scroll3 ldy $be ;columns to scroll
 lda ($fb),y
 sta $50
 lda ($fd),y
 sta $51
cpyright dey
 lda ($fb),y
 iny
 sta ($fb),y
 dey
 lda ($fd),y
 iny
 sta ($fd),y
 dey
 bne cpyright
 jsr scrollh
 dec $bf     ;rows to scroll
 bpl scroll3
 bmi hsdone
;--scroll subs--
;--------------
scrollh
 lda XSAV      ;wrap?
 bne gowrap
 ldx COLOR     ;current foreground color
 lda #32       ;screen code for space char
 bne shiftchar ;always branches
gowrap
 lda $50       ;saved char
 ldx $51       ;saved color
shiftchar
 sta ($fb),y   ;char
 txa
 sta ($fd),y   ;color
;add 40 to text ptr
 lda $fb
 clc
 adc #40
 sta $fb
 sta $fd
 lda $fc
 adc #0
 sta $fc
;add 40 to color ptr
 sec
 sbc HIBASE
 jmp wwww
;--------------
;calculate text and color RAM mem pointers
calcptr
 ldy $bf     ;line number
 lda $ecf0,y ;video matrix lowbyte at line y
 clc
 adc $fb     ;add offset to current ptr
 sta $fb     ;lobyte ptr to text RAM
 sta $fd     ;lobyte ptr to color RAM
 lda btab,y  ;video maxtrix hibyte offset
 tax
 adc HIBASE  ;video matrix hibyte
 sta $fc     ;hibyte ptr to text RAM
;determine hibyte of color RAM
 txa
wwww
 clc         ;add text ptr hibyte offset
 adc #$d8    ;offset from current text ptr hibyte
 sta $fe     ;apply hibyte of color RAM ptr
 rts
;--------------
;store column to be wrapped
wrapit
 lda XSAV        ;wrap flag
 beq wdone
 ldy $be         ;columns to scroll
cpybuf lda ($fb),y
 sta paintbuf1,y ;char mem buffer
 lda ($fd),y
 sta paintbuf2,y ;color mem buffer
 dey
 bpl cpybuf
wdone
 rts
;--------------
;play next note
nextn0          ;start over from beginning
 lda #0
 sta playindex
nextn2
 jsr notegot
 beq wdone
nextn
 cmp #"h"       ;notes A-G only
 bcs nonnote
 sec            ;ascii of notes must start at A (65)
 sbc #"a"       ;ascii - A(65) starts at 0 for A, 1 for B, etc.
 bmi plyrpt
 ldx PALNTSC    ;NTSC or PAL?
 beq wrdidx
 clc            ;adjust note index for PAL
 adc #15        ;to use regvals for 50Hz clock
wrdidx asl      ;convert node index to word index
 tax
 jsr noteget
 beq regnote

 cmp #"-"       ;flat?
 beq fltshr
 cmp #"#"       ;sharp?
 bne regnote
 inx            ;adjust index for sharps
 inx
fltshr
 jsr noteget    ;skip over symbol
 txa            ;adjust index
 clc
 adc #14
 tax

regnote
 ldy notes+1,x  ;hibyte of note
 lda notes,x    ;lobyte of note
 jsr octadj     ;adjust note for octave
 ldx playvoice  ;voice offset
 sta FRELO1,x   ;voice x freq lo byte
 tya
 sta FREHI1,x   ;voice x freq hi byte
 lda playwave   ;select current waveform
 ora #%00000001 ;and start attack/decay/sustain cycle
 sta VCREG1,x   ;make sound via waveform setting

 jsr notegot
 jsr getdigitval+3
 bmi notewait
 bne notewait2  ;use provided wait time
notewait
 lda playlen    ;use current note length wait time
notewait2
 sta playtime   ;start of timer
endply
 rts

plyrpt
 cmp #"@"-"a" ;255 repeat?
 beq nextn0

octchg
 cmp #"<"-"a" ;251 octave down
 bne octup2
prevoct
 dec playoct
 bpl nextn3
 bmi nextoct
octup2
 cmp #">"-"a" ;253 octave up
 bne nextn3
nextoct
 inc playoct
 lda playoct
 cmp #9
 bcs prevoct
nextn3
 inc playindex  ;skip over char
 jmp nextn2

nonnote
 cmp #"v"
 bne notepause
 jsr getdigitval
 beq skipnote   ;voice 0 invalid
 cmp #4         ;voice 1,2 or 3 only
 bcs skipnote
 jsr getvoff    ;get voice register offset
 cmp playvoice  ;already using this voice?
 beq skipnote
 tax            ;new voice register offset
 ldy playvoice  ;old voice register offset
 lda playwave   ;current waveform
 sta VCREG1,y   ;start decay cycle for current voice
 jsr inivoc     ;set new voice
 jmp nextn2

notepause
 cmp #"p"
 bne notelen
 lda playwave   ;current waveform
 ldx playvoice  ;SID reg offset
 sta VCREG1,x   ;bit0 = 0 start release
 jsr getdigitval
 bpl notewait2  ;use supplied wait length
 bmi notewait   ;use current note length as default wait length

notelen
 cmp #"l"
 bne noteoct
 jsr getdigitval
 sta playlen    ;apply new note length
 jmp nextn2

noteoct
 cmp #"o"
 bne notewave
 jsr getdigitval
 cmp #9
 bcs skipnote
 sta playoct
 bcc skipnote   ;always branches
 
notewave
 cmp #"w"
 bne nextn3
 jsr getdigitval
 cmp #9
 bcs skipnote
 asl            ;convert to waveform bit pattern
 asl
 asl
 asl
 sta playwave   ;set new waveform
skipnote jmp nextn2

getdigitval     ;get 2-digit value 0-99
 jsr noteget
 beq donedig    ;end of string, assume 0 value
 sec            ;convert ascii digit to binary value
 sbc #"0"       ;first char must be digit between 0 and 9
 bpl digit9
nondigit
 lda #$ff       ;flag for non numeric digit
donedig
 rts
digit9
 cmp #10
 bcs nondigit
 sta temp1      ;1's place value or 10's place value, not sure yet
 jsr noteget
 beq digitdone  ;end of string; use first digit as value
 sec            ;calc valid 10's place value
 sbc #"0"
 bmi digitdone  ;non digit; use first digit as value
 cmp #10
 bcs digitdone  ;non-digit; use first digit as value
 sta temp2      ;save as new 1's place value
 jsr noteget    ;skip over digit
;perform math (8*x)+(2*x)+y
 lda temp1      ;convert first digit to 10 times the value
 asl temp1      ;(2*x)
 asl            ;(8*x)
 asl
 asl
;clc not needed here since asl will move 0 to carry since value is 0 to 9
 adc temp1      ;+(2*x)
 adc temp2      ;+y
 sta temp1
 rts
digitdone
 lda temp1
 rts            ;result returned in accumulator

;returns char in A reg; zero-flag set indicate no more notes
noteget
 inc playindex
 beq nonote
notegot
 ldy playindex
 lda playbuf1,y
 beq nonote
 cmp #" "
 beq noteget
nonote rts

octadj
 ldx playoct
 cpx #4
 beq octdone
 sta temp1
 sty temp2
 bcc octdwn
 txa
 sbc #4
 tax
octup
 asl temp1
 rol temp2
 bcs octmax ;max regval reached
 dex
 bne octup
 beq octend
octdwn
 lda #4
 sec
 sbc playoct
 tax
octdwn2
 lsr temp2
 ror temp1
 dex
 bne octdwn2
octend
 lda temp1
 ldy temp2
octdone rts
octmax
 lda #$ff ;max regval reached starting at
 tay      ;octave 8 note B for PAL
 rts      ;octave 8 note C for NTSC
;--------------
;process DRAW string
godraww
 cmp #"u"
 bne chkdwn
udraw jsr drawup
 jsr dodraw
 bcc udraw
 bcs nxtmove
chkdwn cmp #"d"
 bne chkleft
ddraw jsr drawdwn
 jsr dodraw
 bcc ddraw
 bcs nxtmove
chkleft cmp #"l"
 bne chkright
ldraw jsr drawleft
 jsr dodraw
 bcc ldraw
 bcs nxtmove
chkright cmp #"r"
 bne chkupleft
rdraw jsr drawright
 jsr dodraw
 bcc rdraw
 bcs nxtmove
chkupleft cmp #"e"
 bne chkupright
edraw jsr drawup
 jsr drawleft
 jsr dodraw
 bcc edraw
 bcs nxtmove
chkupright cmp #"f"
 bne chkdwnleft
fdraw jsr drawup
 jsr drawright
 jsr dodraw
 bcc fdraw
 bcs nxtmove
chkdwnleft cmp #"g"
 bne chkdwnright
gdraw jsr drawdwn
 jsr drawleft
 jsr dodraw
 bcc gdraw
 bcs nxtmove
chkdwnright cmp #"h"
 bne baddraww
hdraw jsr drawdwn
 jsr drawright
 jsr dodraw
 bcc hdraw
 bcs nxtmove
baddraww
 clc           ;carry flag used as a flag to
nxtmove        ;indicate draw cmd not recognized
 rts           ;carry clear=bad, carry set=good
;
dodraw
 jsr setdot
 dec $14       ;decrement draw length
 bne dodraw2
 dec $15
 sec           ;flag for done
 bmi dodraw2+1 ;draw length complete
dodraw2 clc    ;flag for more
 rts
;
drawup
 ldx $fd
 dex
 cpx #200
 bcc setx
 ldx #199      ;wrap from top to bottom
setx stx $fd
 rts
drawdwn
 ldx $fd
 inx
 cpx #200
 bcc setx
 ldx #0
 beq setx ;always branches
drawleft
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 beq dlhires
 jsr dlhires
dlhires lda $fb
 bne nodece
 dec $fc
 bne dlwrap
nodece dec $fb
 rts
dlwrap
 lda #63
 sta $fb
 lda #1
 sta $fc
 rts
drawright
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 beq drhires
 jsr drhires
drhires
 inc $fb
 bne drhi2
 inc $fc
drhi2 lda $fc
 beq drdone
 lda $fb
 cmp #64
 bcc drdone
drwrap
 ldx #0
 stx $fc
 stx $fb
drdone rts
;--------------
;perform BITMAP FILL
bitfil
 ldy #$01    ;signed y inc value
;calc number of lines to fill
 lda $52     ;point2 y coordinate
 sec
 sbc $fd     ;calc abs(y1-y2)
 bcs filines ;y1 <= y2
 eor #$ff    ;y1 > y2 so use 2's compliment
 clc         ;to negate
 adc #1
 dey         ;change fill direction to up
 dey
filines
 sty $0a     ;fill up $ff or down $01
 sta COUNT   ;num lines to fill
;calc line width and fill left or right
 lda #0      ;0-=draw right,1=draw left
 sta $ff     ;horizontal fill direction
 lda $50     ;point2 x coordinate lobyte
 sec
 sbc $fb
 tax
 lda $51     ;point2 x coordinate hibyte
 sbc $fc
 tay
 bpl setwidth
;x1 > x2 so negate result & reverse draw direction
 inc $ff     ;draw left
 txa
 eor #$ff    ;16-bit 2's compliment
 clc
 adc #1
 tax
 tya
 eor #$ff
 adc #0
 tay
;
setwidth
 stx $bb
 stx $14
 sty $bc
 sty $15
;set height
 lda $fb
 sta $07
 lda $fc
 sta $08
linefil
 jsr setdot
 lda $ff
 beq fillright
fillleft
 jsr drawleft
 jsr dodraw
 bcc fillleft
 bcs filldone
fillright
 jsr drawright
 jsr dodraw
 bcc fillright
;restore x coordinate
filldone
 lda $08
 sta $fc
 lda $07
 sta $fb
;inc to next line
 lda $fd  ;current y
 clc      ;use signed value
 adc $0a  ;to inc or dec value
 sta $fd
;reset draw width
 lda $bb
 sta $14
 lda $bc
 sta $15
;
 dec COUNT
 bpl linefil
 jmp memnorm
;
;***OPEN RS-232 CHANNEL***
openrs232
 sta mdbio
 ldx #2          ;device 2 = RS-232
 ldy #0          ;secondary channel
 jsr SETLFS
 ldx LDTND       ;use current count as 0-based index for file in tables
 inc LDTND       ;inc total file handle count
 lda LA          ;current logical file number
 sta LAT,x       ;store file descriptors into master table
 lda SA          ;current secondary address
 ora #%01100000  ;flag bits 5&6 to indicate rs-232 channel
 sta SA          ;needs special handling on close or error
 sta SAT,x
 lda FA          ;current device number
 sta FAT,x
;***PREPARE RS-232 DEVICE WITH DEFAULTS***
 jsr $f483       ;init IRQ timers (y reg loaded with 0)
 sty RSSTAT      ;reset RS-232 Status
;bits 0-3 of M51CTR are used to set the baud rate as follows:
;0=User Defined (not supported here yet)
;1=50,2=75,3=110,4=134.5,5=150,6=300,7=600,8=1200,9=1800
;10=2400,11=3600,12=4800,13=7200,14=9600,15=19200
 lda #%00001000  ;1200 baud, 8 data bits, 1 stop bit
 sta M51CTR      ;bits3-0=baud, bit4:unused, bits6-5=data bits, bit7=stop bits
 lda #%00000000  ;no parity, full duplex, 3-line handshake
 sta M51CDR      ;bits7-5:parity, bit4=duplex, bits3-1:unused, bit0=handshake
;if implementing user-defined baud rate, the value placed here would be
;TIMING = (CLOCK/(BAUDRATE/2))-100 in binary little-endian format
;thus the max baud rate for NTSC machines is 20454 and 19705 on PAL machines.
;however near maximum is not suggested and prone to errors;
;the IRQ facilitating the send and receive buffers cannot keep up beyond 2400.
 lda #0         ;user defined baud rate not supported by C64 Kernal
 sta M51AJB     ;lobyte of timing for user defined baud rate
 sta M51AJB+1   ;hibyte of timing for user defined baud rate
;
;parse parameters (if any)
 jsr CHRGET
 beq opn232
 cmp #","
 beq getdb
 jsr rom3
;find baud rate index of 10 possible
 ldx #11
nxtbaud
 dex
 beq badserial  ;baud rate not found in table
 txa
 asl
 tay
 lda baudrates-2,y
 cmp $14
 bne nxtbaud
 lda baudrates-2+1,y
 cmp $15
 bne nxtbaud
 stx XSAV       ;index found
 lda M51CTR     ;current setting (from default)
 and #%11110000 ;remove current setting bits 0-3
 ora XSAV       ;apply selected baud rate from index
 sta M51CTR     ;set baud rate
 jsr CHRGOT
 bne getdb
opn232
 jmp open232
getdb
 jsr ckcom2     ;check for and skip over comma, misop err if missing
;get data bits
 cmp #","       ;another comma?
 beq getstpbits
 jsr rom3       ;data bits (word length) 5,6,7 or 8
 bne badserial  ;hibyte must be zero
 cmp #5
 bcc badserial
 cmp #9
 bcs badserial
;need bit pattern 00=8, 01=7, 10=6, 11=5
;use 2's compliment to negate then add 8
 eor #$ff
 adc #9         ;1 for 2's comp, 8 for offset
 and #%00000011 ;just want first 2 bits
 lsr            ;shift bit positions
 ror            ;from 0,1 to 6,5
 ror            ;using carry bit
 lsr            ;arriving at bits 6,5
 sta XSAV
 lda M51CTR
 and #%10011111 ;clear target bits 5 and 6
 ora XSAV
 sta M51CTR
 jsr CHRGOT
 beq opn232
 bne getstpbits
;
badserial
 ldx #14        ;illegal qty error
 sec            ;flag to calling subroutine that error occured
 rts
;
getstpbits
 jsr CHRGET
 cmp #","
 beq getduplex
 jsr rom3       ;get stop bits (0 or 1)
 bne badserial  ;hibyte must be 0
 cmp #2
 bcs badserial
 eor #%00000001 ;convert for register since 0=1 stop bit, 1=0stop bits
 lsr            ;shift bit position 0 to 7
 ror            ;by way of carry
 sta XSAV
 lda M51CTR
 and #%01111111 ;clear target bit 7
 ora XSAV
 sta M51CTR
 jsr CHRGOT
 beq open232
;
getduplex
 jsr CHRGET
 cmp #","
 beq getparity
 jsr rom3       ;duplex (0 or 1)
 bne badserial  ;hibyte must be zero
 cmp #2
 bcs badserial
 asl            ;shift bit position 0
 asl            ;to position 4
 asl
 asl
 sta XSAV
 lda M51CDR
 and #%11101111 ;clear target bit 4
 ora XSAV
 sta M51CDR
 jsr CHRGOT
 beq open232
;
getparity
 jsr CHRGET
 cmp #","
 beq gethndshk
 jsr rom3       ;parity (0 to 4)
 bne badserial  ;hibyte must be zero
 cmp #5
 bcs badserial
 tay
 lda parity,y   ;convert param value to bit pattern value
 sta XSAV
 lda M51CDR
 and #%00011111 ;clear target bits 5-7
 ora XSAV       ;parity param value
 sta M51CDR
 jsr CHRGOT
 beq open232
;
gethndshk
 jsr CHRGET
 jsr rom3       ;handshake 0 or 1
 bne badserial  ;hibyte must be zero
 cmp #2
 bcs badserial
 lda M51CDR
 and #%11111110 ;clear target bit 0
 ora $14        ;handshake param value
 sta M51CDR
;
;open the RS-232 channel
open232
 jsr $ef4a      ;get word length for current RS-232 character into x reg
 stx BITNUM     ;RS-232: number of bits left to be sent/received
 lda M51CTR     ;RS-232: Mock 6551 Control Register
 and #$0f       ;if baud (first 4 bits) = 0 then
 beq xf446      ;user defined baud rate
 asl            ;else calc word ptr offset of baud timing prescaler
 tax
 lda PALNTSC    ;PAL/NTSC Flag
 bne xf43a      ;0=NTSC
;NTSC timing
 ldy $fec1,x    ;prescaler table for NTSC
 lda $fec0,x
 jmp xf440
;PAL timing
xf43a
 ldy $e4eb,x    ;prescaler table for PAL
 lda $e4ea,x
;set prescaler timing registers
xf440           ;RS-232: Nonstandard Bit Timing (user defined baud rate)
 sty M51AJB+1
 sta M51AJB
;
xf446
 lda M51AJB
 asl
 jsr $ff2e      ;calculate time required to send a bit
 lda M51CDR     ;RS-232: Mock 6551 Command Register
 lsr            ;bit 7=handshake 0=3-line, 1=x-line
 bcc xf45c      ;clear carry means x-line
 lda CI2PRB     ;Data Port B
 asl            ;check bit 7 Data Set Ready (DSR) Pin L on User Port
 bcs xf45c      ;carry set then ready for transmission
;set error status and skip to end of buffer
 jsr $f00d      ;set error status: bit 6 DTR (Data Set Ready) Signal Missing
;advance index to send/receive buffers
xf45c lda RIDBE ;index to end of receive buffer
 sta RIDBS      ;index to start of receive buffer
 lda RODBE      ;RODBE RS-232 index to end of transmit buffer
 sta RODBS      ;index to start of transmit buffer
;set receive buffer pointer
 lda #$0
 ldy #$ce       ;input buffer at $CE00-$CEFF
 sta $f7        ;lobyte ptr to RS-232 input buffer
 sty $f8        ;hibyte ptr to RS-232 input buffer
;set send buffer pointer
 sta $f9        ;lobyte ptr to RS-232 output buffer
 iny            ;output buffer is next page of 256 bytes at $CF00-$CFFF
 sty $fa        ;hibyte ptr to RS-232 output buffer
 ldx #0         ;success code
 clc            ;flag to calling subroutine that open was successful
 rts
;--------------
;perform POKE in specified range
pokee
;make sure start is less than or equal to end
 lda $14
 sec
 sbc $fb
 lda $15
 sbc $fc
 bcs okpoke ;start is less than end
;swap start and end
 lda $14
 ldy $fb
 sta $fb
 sty $14
 lda $15
 ldy $fc
 sta $fc
 sty $15
okpoke
 ldy #0
poker
 lda $fe  ;poke value
 ldx $fd  ;poke type 0=set,1=and,2=or,3=eor
 beq poke0
 dex
 bne poke2
 and ($fb),y
 jmp poke0
poke2 dex
 bne poke3
 ora ($fb),y
 jmp poke0
poke3
 eor ($fb),y
poke0 sta ($fb),y
 ldx $fc
 cpx $15
 bne nxtpg
 ldx $fb
 cpx $14
 beq poked
nxtpg
 inc $fb
 bne poker
 inc $fc
 bne poker
poked jmp memnorm
;--------------
;perform HEX$(s$) for 32-bit number
hexx
 ldy #$50    ;use last 9 bytes in BUF
 lda $62
 jsr dechex
 lda $63
 jsr dechex
 lda $64
 jsr dechex
 lda $65
 jsr dechex
 lda #$00
 sta BUF,y   ;string len is 8 chars plus a zero-byte terminator
;find most significant digit index
 ldy #$50
nxzro
 lda BUF,y
 beq hexzro  ;reached end of str so value is 0
 cmp #"0"    ;ignore leading zeros
 bne msd
 iny
 bne nxzro
hexzro dey
msd tya      ;A(lo) Y(hi) is ptr to zero-term str
 ldy #>BUF
 rts
dechex pha
 lsr
 lsr
 lsr
 lsr
 cmp #$0a
 bcc add48
 clc
 adc #$07
add48 clc
 adc #48
 sta BUF,y
 iny
 pla
 and #$0f
 cmp #$0a
 bcc noadd7
 clc
 adc #$07
noadd7 clc
 adc #48
 sta BUF,y
 iny
doneit
 rts
;--------------
;perform FIND
findd
 ldy #0
 lda (TXTTAB),y
 sta $fb
 iny
 lda (TXTTAB),y
 sta $fc
 ora $fb
 beq doneit
 iny
 lda (TXTTAB),y
 sta CURLIN
 iny
 lda (TXTTAB),y
 sta CURLIN+1
 jsr CHRGOT
 beq doneit     ;nothing to find
 cmp #"""
 bne noquo
 jsr CHRGET
noquo lda TXTTAB
 clc
 adc #4
 sta $fd
 lda TXTTAB+1
 adc #0
 sta $fe
check ldy #$00
 lda (TXTPTR),y
 beq linend
 tax
 lda ($fd),y
 beq linend
 txa
 cmp ($fd),y
 beq search
 inc $fd
 bne check
 inc $fe
 jmp check
search iny
 lda (TXTPTR),y
 beq founit
 cmp ($fd),y
 beq search
 tya
 clc
 adc $fd
 sta $fd
 lda $fe
 adc #$00
 sta $fe
 jmp check
founit lda CURLIN
 sta $14
 lda CURLIN+1
 sta $15
 jsr lstrap
 jsr findlnr  ;search for line#
 lda #$91     ;crsr up
 jsr CHROUT
linend
 jsr STOP
 beq prgend
 lda $fb
 sta $fd
 lda $fc
 sta $fe
 ldy #$00
 lda ($fd),y
 sta $fb
 iny
 lda ($fd),y
 sta $fc
 iny
 lda ($fd),y
 sta CURLIN
 iny
 lda ($fd),y
 sta CURLIN+1
 iny
 tya
 clc
 adc $fd
 sta $fd
 lda $fe
 adc #$00
 sta $fe
 lda $fc
 ora $fb
 bne check
prgend
 rts
;--------------

;get 8-byte time string into string buffer
getimstr
 lda TO2HRS      ;reading pauses capture of time to these registers
;convert 12 hour clock to 24 hour clock
 php
 and #%00011111
 cmp #%00010010  ;12 in BCD format
 bne not12
 plp
 bmi getime      ;12pm use as is
 lda #0          ;12am is first hour
 beq getime      ;always branches
not12
 plp
 bpl getime      ;1am to 11am use as is
 clc             ;adjust 1pm to 11pm ==> 13 to 23 
 sed
 adc #%00010010  ;12 in BCD format
 cld
getime
 ldy #$50        ;use last 9 bytes in BUF
 jsr timedig
 lda TO2MIN
 jsr timedig
 lda TO2SEC
 jsr timedig
 lda TO2TEN      ;reading resumes capture of time to these registers
 rts
;get time digit
timedig
 tax
 lsr
 lsr
 lsr
 lsr
 clc
 adc #"0"
 sta BUF,y
 iny
 txa
 and #15
 clc
 adc #"0"
 sta BUF,y
 iny
 lda #":"
 cpy #$58
 bcc setterm
 lda #0
setterm
 sta BUF,y
 iny
 rts
;
;build expression to convert time$ to time in seconds since midnight
;expression to calc seconds since midnight
tim2sec
.text "00.0" ;placeholder for seconds and one tenth of second
.byte $aa    ;plus token
.text "60"
.byte $ac    ;times token
.text "00"   ;placeholder for minutes
.byte $aa    ;plus token
.text "3600"
.byte $ac    ;times token
.text "00"   ;placeholder for hours
.byte 0
;
dotime
;copy to buffer the expression string for calc time in seconds
 ldy #18
cpytimfun
 lda tim2sec,y
 sta BAD+10,y
 dey
 bpl cpytimfun
;get time string
 jsr getimstr  ;returns TO2TEN in A reg
;1/10 second
 and #15
 clc
 adc #"0"
 sta BAD+13    ;one tenth of a second
;seconds
 lda BUF+$56
 sta BAD+10
 lda BUF+$57
 sta BAD+11
;minutes
 lda BUF+$53
 sta BAD+18
 lda BUF+$54
 sta BAD+19
;hours
 lda BUF+$50
 sta BAD+26
 lda BUF+$51
 sta BAD+27
;return ptr to string result
 ldx #<BAD+10
 ldy #>BAD+10
 rts
;
;set time from string
settimee
 lda CI2CRB      ;bit7: select target 0=clock,1=alarm
 and #%01111111  ;writing to TOD registers set the clock
 sta CI2CRB
;
 ldy $52
 cpy #8
 bne badtim2+1
 dey             ;index 0 to 7
 jsr timenum     ;seconds
 sta $24
 dey             ;ignore colon 
 jsr timenum     ;minutes
 sta $23
 dey             ;ignore colon 
 jsr timenum     ;hours
 beq morn        ;midnight
 sed
 cmp #%00100100  ;BCD 24
 bcs badtim2
 cmp #%00010011  ;BCD 13
 bcc morn        ;noon will flip am/pm flag
 sbc #%00010010  ;BCD 12
 ora #%10000000  ;set am/pm flag to pm
morn
 sta TO2HRS
 lda $23
 sta TO2MIN
 lda $24
 sta TO2SEC
 lda #0
 sta TO2TEN  ;start clock capture
 cld
 clc
 rts
timenum
 lda ($50),y ;seconds one's places
 cmp #"0"
 bcc badtim
 cmp #":"
 bcs badtim
 sec
 sbc #"0"
 sta $25
 dey
 lda ($50),y  ;seconds ten's place
 cmp #"0"
 bcc badtim
 cmp #"6"
 bcs badtim
 dey
 asl
 asl
 asl
 asl
 ora $25
 rts
badtim
 pla
 pla
badtim2 cld
 sec
 rts
;
;Initialize the TOD clock on both CIA 6526 chips.
;MDBASIC uses TOD #2 for TIME and TIME$ functions.
;Determine what the TOD clock frequency should be (50 or 60 Hz)
;by measuring how fast the timer can count down during the TOD's
;one-tenth of a second cycle.
initclk
 lda CI2CRB      ;bit7: select target 0=clock,1=alarm
 and #%01111111  ;writing to TOD registers set the clock
 sta CI2CRB
;
 lda #$00
 ldy #$ff
 ldx #%00010001  ;start continous timer with forced latch
                 ;count signals on CNT line at pin 4 of user port
                 ;set TOD #2 to use 60Hz by default
;init countdown timer A to count down from $ffff
 sty TI2ALO
 sty TI2AHI
;wait for first change
 sta TO2TEN       ;start TOD capture to registers
t1 cmp TO2TEN     ;wait for first change
 beq t1           ;to begin the measurement
 stx CI2CRA       ;start the timer to measure TOD #2 clock speed
 lda TO2TEN       ;wait for the next change
t2 cmp TO2TEN     ;to take a timer measurement
 beq t2
;capture time for TOD to increment one tenth of a second
 ldx TI2AHI       ;only interested in hibyte value
;determine bit pattern for system
 lda #%01111111   ;assume NTSC bit pattern for AND opration
 cpx #100         ;should be over 99 for 60Hz else 50Hz
 bcs sethz        ;carry is clear when 50Hz else 60Hz
 eor #$ff         ;flip bit pattern to prepare for OR operation
sethz
 tay              ;save bit pattern
;use bit pattern with AND/OR operation for system
 bcs clk60        ;carry is clear when 50Hz else 60Hz
 ora CIACRA       ;set bit7 to 1 for 50Hz
 tax
 tya
 ora CI2CRA
 bcc setclk       ;always branches
clk60
 and CIACRA       ;set bit7 to 0 for 60Hz
 tax
 tya              ;recall bit pattern
 and CI2CRA
setclk
 sta CI2CRA       ;apply to TOD #2
 stx CIACRA       ;apply to TOD #1
 rts
;
;begin processing input stream
filess
 ldx #$fe       ;start file count at -2
 stx $50        ;to ignore header and footer
 inx
 stx $51
 ldx mdbin
 jsr CHKIN
 bcs err7e
 jsr CHRIN      ;skip 2-byte file header
 jsr CHRIN
 bcs err7e
blocks
 jsr prtlin
 bcs err7e
 beq nxtfile
chkeof
 and #%01000000 ;bit6=EOF/EOI, bit7=device not present, bits0-1 device timeout
 bne filecnt    ;bit6=1? EOF, print file count
 lda #5         ;DEVICE NOT PRESENT
 sec            ;flag for error
err7e rts
nxtfile
 inc $50        ;increment file count
 bne chkps
 inc $51
chkps
 jsr chkpause
 bmi blocks     ;zero means stop, carry set
;
 inc $50
 bne filecnt
 inc $51
filecnt
 ldx $50
 lda $51
 clc            ;flag for success
 rts
;
chkpause
 lda #$01
chkpauser
 bit SHFLAG     ;shift key?
 bne chkpauser  ;wait till released
 jmp STOP       ;stop key?
;
prtlin
 ldx mdbin
 jsr CHKIN
 bcs pdone
 jsr CHRIN      ;skip 2 byte line header
 jsr CHRIN
 bcs pdone
 jsr CHRIN      ;read the 2-byte disk block size
 sta $63        ;and store result in FAC1
 jsr CHRIN      ;to later convert to string
 sta $62        ;for writing to output channel
 jsr READST     ;get last input status, 0=ok
 sta $61        ;save input status
 jsr stdout     ;set current i/o to output channel if needed
 bcs pdone      ;quit if error
 lda $61        ;check current input status
 bne pdone      ;quit if done
 jsr rom4       ;print 2-byte binary number in FAC1
 lda #" "
 jsr CHROUT
;print zero-terminated string in file then print CR
 jsr bufio7e    ;on return, carry set means error in i/o
 bcs pdone      ;either input or output failed
 pha            ;save status result of i/o
 jsr printcr    ;write carriage return to current output channel
 pla            ;restore status result
pdone rts
stdout
 clc            ;clear carry used as error flag
 ldx CHANNL     ;current output channel
 beq pdone      ;zero indicates default so no need to CHKOUT
 jmp CHKOUT     ;set current output channel, carry set on error
;
;read with buffering from current input channel
;write to current output channel
bufio7e
 ldx mdbin
bufio
 sta XSAV       ;write flag
 jsr CHKIN
 bcs iodone1
 ldy #0
bufrd
 jsr READST     ;last read status
 bne buf0
 jsr CHRIN
 bcs iodone1    ;carry indicates error, accumulator holds error#
 beq buf1       ;found zero-byte terminator
 cmp #13        ;carriage return?
 beq buf0       ;yes, stop reading now
 sta paintbuf1,y
 iny
 bne bufrd     ;keep looking for zero byte or CR char
 beq buf2      ;255 byte string
buf0
 lda #0        ;set zero-byte terminator
buf1
 sta paintbuf1,y
buf2
 jsr READST    ;last read status
 sta $61       ;save for returning the read status
 jsr stdout    ;switch current i/o channel to output channel
 bcs iodone1   ;carry indicates error, accumulator holds error#
 lda XSAV      ;write flag
 beq iodone1
 ldy #0
bufwr
 lda paintbuf1,y
 beq iodone    ;stop if zero-byte termination found
 jsr CHROUT
 bcs iodone1   ;stop if output error
 iny
 bne bufwr
iodone
 lda $61
iodone1 rts
;
getdot
 jsr getpoint   ;put last plot coordinates in x = $fb,$fc and y = $fd
 jsr ydiv8
 lda R6510
 pha
 and #%11111101 ;bit1 0=HIRAM
 sei            ;disable IRQ since kernal HIROM is switching to HIRAM
 sta R6510
 lda ($c3),y
 tay
 pla
 sta R6510
 cli
 tya
 and ptab2,x    ;only desired bits, multicolor uses 2
 beq rtndot     ;pixel not set
 lda #1         ;pixel set
rtndot
 ldy #0
 rts
;
inff
;prepare FAC1 for 4-byte int return value
 ldx #0
 stx $70
 stx $63
 stx $62
 ldx #$a0
 stx $61
;process INF parameter in accumulator
 tax
 beq phycol
 cmp #10
 bcc useinfbytes
 beq csraddr
 cmp #20
 beq getdot
 bcs membank
 sec
 sbc #11        ;first infoword index
 asl            ;double byte ptr index
 tax
 ldy infwords+1,x
 lda infwords,x
 jmp goodinf1
csraddr         ;get text address of current line
 ldy $d2        ;hibyte
 lda $d1        ;lobyte
 rts
phycol
;physical line = (logicalCol > maxCols) ? logicalCol-maxCols : logicalCol
 lda PNTR       ;logical column
 sec
 sbc #40        ;max column number for a physical line
 bpl goinf      ;logicalCol was larger than maxCols
 lda #1         ;use logical col (index 1)
useinfbytes
 ldy #0         ;hibyte
 tax            ;register index
 lda infbytes-1,x ;register indexed holds lobyte value
goodinf1 sta $14
 sty $15
 ldy #0
 lda ($14),y
 rts
membank
 cmp #22
 bcs fstart
 lda CI2PRA
 and #%00000011
 eor #%00000011
 ldy #0
 rts
fstart
 cmp #23
 beq fend
 bcs infptrs
 lda STAL    ;last load start address
 ldy STAL+1
 rts
fend
 lda EAL     ;last load end address
 ldy EAL+1
 rts
infptrs
 sbc #24
 cmp #13
 bcs infcolor
 asl
 tax
 lda TXTTAB,x
 ldy TXTTAB+1,x
 rts
infcolor
 sbc #13
 cmp #15
 bcs spritexy
 tax
 lda EXTCOL,x
 and #%00001111  ;hi nybble is not used for all color registers
goinf
 ldy #0
 rts
spritexy
 sbc #15
 tax         ;register offset (0-15)
 lsr         ;odd or even index check
 bcc xcoord  ;even is x coordinate
 lda SP0X,x  ;odd is y coordinate
 bcs goinf   ;always branches
xcoord
 tay         ;sprite number (0-7)
 lda bitweights,y
 ldy #0      ;hibyte for result
 and MSIGX   ;is bit y is set
 beq nomsb   ;no, then x coord <= 255
 iny         ;yes, x coord > 255
nomsb
 lda SP0X,x  ;lobyte for x coord
 rts
;
clrflg lda TXTPTR
 sta $22
 lda TXTPTR+1
 sta $23
 lda VARTAB
 sta $24
 lda VARTAB+1
 sta $25
 ldy #$00
 sty COUNT
 sty XSAV
 rts
;*********
;prepare for FIND with 65535 as param
f65535
 lda #>BUF
 sta TXTPTR+1
 lda #<BUF
 sta TXTPTR
 ldy #$05
ffff lda nolin,y
 sta BUF,y
 dey
 bpl ffff
 rts
;
renumer
 jsr chrget
 jsr chrget
 bne serch
 jsr tofac
strnum jsr chrget
 jsr chrget
 beq f65535  ;end of renum process
 jsr chrget
 lda $63
 sta (TXTPTR),y
 jsr chrget
 lda $62
 sta (TXTPTR),y
 jsr addinc
 beq strnum
serch jsr chrget
 jsr chrget
nocrap jsr chrget
craper cmp #"""
 bne tokgo
;skip over expression in quotes
crap jsr chrget
 beq renumer  ;end of line
 cmp #"""
 bne crap
 beq nocrap
tokgo tax
 beq renumer
 bpl nocrap
;check if token is a statement that use line numbers
 ldx #10     ;there are 10 tokens that reference a line number
chktok
 cmp gotok-1,x
 beq sav7a
 dex
 bne chktok
 beq nocrap
sav7a lda TXTPTR
 sta OLDLIN
 lda TXTPTR+1
 sta OLDLIN+1
 jsr CHRGET
 bcs craper
 jsr evalnum
 jsr replac
 lda OLDLIN+1
 sta TXTPTR+1
 lda OLDLIN
 sta TXTPTR
;apply new line number
 ldy #$00
 ldx #$00
numchr lda BAD,x
 cmp #"0"
 bcc less0
 pha
 jsr CHRGET
 bcc skp2d
 jsr inc2d
skp2d pla
 ldy #$00
 sta (TXTPTR),y
 inx
 bne numchr
less0 jsr CHRGET
 bcs workdone
dec2d jsr clrflg
 dec XSAV
work ldy COUNT
 iny
 lda ($22),y
 ldy XSAV
 iny
 sta ($22),y
 jsr pntreq
 beq crush1
 inc $22
 bne work
 inc $23
 bne work
 jsr bufer
crush1
 lda VARTAB
 bne ne2d
 dec VARTAB+1
ne2d dec VARTAB
 jsr CHRGOT
 bcc dec2d
workdone
 cmp #","
 beq sav7a
 jmp craper
;
replac jsr tofac
goagan jsr chrget
 jsr chrget
 bne isline
 lda #$ff
 sta $62
 sta $63
 bmi pnl
isline jsr chrget
 cmp $14
 bne nexlin
 jsr chrget
 cmp $15
 bne nexlin+3
pnl jmp fltstr
;
nexlin jsr chrget
 jsr addinc
 beq goagan
inc2d jsr clrflg
 inc XSAV
 jsr bufer
 inc VARTAB
 bne gbwyc
 inc VARTAB+1
gbwyc rts
;
pne2 lda $24
 bne ne24
 dec $25
ne24 dec $24
bufer ldy COUNT
 lda ($24),y
 ldy XSAV
 sta ($24),y
 jsr pntreq
 bne pne2
 rts
;
addinc lda $63
 clc
 adc $33
 sta $63
 lda $62
 adc $34
 sta $62
necg jsr chrget
 bne necg
 rts
;
pntreq lda $22
 cmp $24
 bne gbhah
 lda $23
 cmp $25
gbhah rts
;
;copy input buffer to string variable
buf2strg
 ldy $fd
copyer
 lda paintbuf1-1,y
 dey
 sta ($35),y
 bne copyer
 jmp memnorm
;
.end
