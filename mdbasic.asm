; ***MDBASIC***
; by Mark D Bowren
; (c)1985-2023 Bowren Consulting, Inc. (www.bowren.com)
;
;zero-page registers
R6510  = $01 ;LORAM/HIRAM/CHAREN RAM/ROM selection and cassette control register
LDTND  = $98 ;Number of Open I/O Files/Index to the End of File Tables
NDX    = $c6 ;number of keys in keyboard buffer
BLNSW  = $cc ;Cursor Blink Enable: 0=Flash Cursor
GDBLN  = $ce ;ASCII value of char under csr (when blinking)
PNTR   = $d3 ;Logical Cursor Column on Current Line
LNMX   = $d5 ;Maximum Length of Physical Screen Line (39 or 79)
TBLX   = $d6 ;Current Cursor Physical Line Number
TMPASC = $d7 ;ASCII value of last character printed to screen
INSRT  = $d8 ;Editor Current Insert Character Count

;BASIC and Kernal work registers
BAD    = $0100 ;Tape Input Error Log and string work area
BUF    = $0200 ;BASIC Line Editor Input Buffer
COLOR  = $0286 ;Current Foreground Color for Text
GDCOL  = $0287 ;Color of Character under Cursor
HIBASE = $0288 ;Top Page of Screen Memory
RPTFLAG= $028a ;which keys repeat 0=only cursor, insert, delete and spacebar keys, 64=no keys, 128=all keys
SHFLAG = $028d ;SHIFT/CTRL/Logo Keypress flags Bit0 SHIFT, Bit1 Commodore Logo Key, Bit2 Ctrl Key
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
LPENY  = $d014 ;Light Pen Vertical Position (0-200) corresponds exactly to the current raster scan line
SPENA  = $d015 ;Sprite Enable Register - Bit 0  Enable Sprite 0 (1=sprite is on, 0=sprite is off)

SCROLX = $d016 ;Horizontal Fine Scrolling and Multicolor Control Register
;Bits 0-2 Fine scroll display horizontally by X dot positions (0-7)
;Bit 3 Select a 38-column or 40-column text display (1=40 columns, 0=38 columns)
;Bit 4 Enable multicolor text or multicolor bitmap mode (1=multicolor on, 0=multicolor off)
;Bit 5 Video chip reset (0=normal operations, 1=video completely off)
;Bits 6-7 Unused
;

YXPAND = $d017 ;Sprite Vertical Expansion Register
;Bit 0-7 is Sprite 0-7 flag to expand sprite n vertically (1=double height, 0=normal height)

VMCSB  = $d018 ;VIC-II Chip Memory Control Register
;Bit 0 Unused
;Bits 1-3 Text character dot-data base address within VIC-II 16K addressable memory.
;   The default is %100 (4) = 4 * 1K = $1000 (4096) the address of the Character Dot-Data area in ROM.
;   The uppercase characters are the first 2K.  The alternate character set which contains both
;   upper and lowercase characters are in the second 2K.  To shift to the alternate
;   character set, you must change the value of this nybble to %110 (6) = 6 * 1K = $1800 (6144).
;
;Bits 4-7 Video matrix base address within VIC-II 16K addressable memory
;   The default is %0001 (1) = 1 * 1K = $0400 (1024).
;   Select which 1024-byte area of memory will contain the screen codes for characters on screen.
;   The last eight bytes of this 1K area are used as pointers to select the 64-byte block of memory
;   for each sprite.
;
SPBGPR = $d01b ;Sprite to Foreground Display Priority Register
SPMC   = $d01c ;Sprite Multicolor Registers
XXPAND = $d01d ;Sprite Horizontal Expansion Register
EXTCOL = $d020 ;Border Color Register
BGCOL0 = $d021 ;Background Color 0 background color for text modes, sprites and mc bitmap. Default 6 (blue)
BGCOL1 = $d022 ;Background Color 1 multicolor mode bits 6 and 7 bit-pair 01 (screen codes  64-127) Default 1 (white)
BGCOL2 = $d023 ;Background Color 2 multicolor mode bits 6 and 7 bit-pair 10 (screen codes 128-191) Default 2 (red)
BGCOL3 = $d024 ;Background Color 3 multicolor mode bits 6 and 7 bit-pair 11 (screen codes 192-255) Default 3 (cyan)
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
POTX   = $d419 ;Read Position of Game Paddle 1 or 3, POTX+1 ($d41a) = Paddle 2 or 4

;Complex Interface Adapter (CIA) #1 Registers ($DC00-$DC0F)
CIAPRA = $dc00 ;Data Port Register A
CIAPRB = $dc01 ;Data Port Register B
CIDDRA = $dc02 ;Data Direction Register A
CIACRA = $dc0e ;Control Register A

;Complex Interface Adapter (CIA) #2 Registers ($DD00-$DD0F)
CI2PRA = $dd00 ;Data Port Register A
;Bits 0-1 Select VIC-II 16K addressable memory bank (0-3)
;  00 Bank 3 (49152-65535, $C000-$FFFF) 16K RAM / Memory mapped I/O, character ROM, 4K Kernal
;  01 Bank 2 (32768-49151, $8000-$BFFF) 16K RAM / BASIC text, 8K BASIC interpreter ROM
;  10 Bank 1 (16384-32767, $4000-$7FFF) 16K RAM / BASIC text
;  11 Bank 0 (    0-16383, $0   -$3FFF) 16K RAM / system variables, screen RAM, BASIC text
;*See zero-page memory location $01 bits 0 & 1 for RAM/ROM switching
CI2PRB = $dd01 ;Data Port Register B
C2DDRA = $dd02 ;Data Direction Register for port A (CI2PRA)

;CIA #2 Timer A
TI2ALO = $dd04 ;Timer A (lo byte)
TI2AHI = $dd05 ;Timer A (hi byte)
;
;TOD #2 Registers
TO2TEN = $dd08 ;TOD clock tenths of a seconds in BCD format (lower nibble)
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
IERROR = $0300 ;Vector to the Print BASIC Error Message Routine
IMAIN  = $0302 ;Vector to the Main BASIC Program Loop
IGONE  = $0308 ;Vector to the Routine That Executes the Next BASIC Program Token

;IRQ Control
MDBIRQ = $0313 ;MDBASIC IRQ Control Register bit0=play,bit1=playsprite,bit2=key
CINV   = $0314 ;Vector to IRQ Interrupt Routine

;CBM BASIC functions
GETSTK = $a3fb ;check for space on stack
REASON = $a408 ;check for space in memory
READY  = $a474 ;print READY
LINKPRG= $a533 ;relink lines of tokenized program text
INLIN  = $a560 ;input a line to buffer from keyboard (max 88 chars)
FINDLN = $a613 ;search for line number using ptr at $2b, $2c
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
STROUT = $ab1e ;print msg from str whose addr is in the Y (hi byte) and A (lo byte) registers
GET    = $ab7b ;perform GET
INPUTN = $aba5 ;perform INPUT#
INPUT  = $abbf ;perform INPUT
READ   = $ac06 ;perform READ
NEXT   = $ad1e ;perform NEXT
FRMNUM = $ad8a ;evaluate a numeric expression and/or check for data type mismatch, store result in FAC1
TESTNUM= $ad8d ;test if last expression was numeric, error if not
FRMEVL = $ad9e ;evaluate expression
PARCHK = $aef1 ;eval expr inside parentheses
CHKCLS = $aef7 ;check for and skip closing parentheses
CHKOPN = $aefa ;check for and skip opening parentheses 
CHKCOM = $aefd ;check for and skip comma
DIM    = $b081 ;perform DIM
PTRGET = $b08b ;search for a variable & setup if not found
AYINT  = $b1bf ;convert FAC1 to a signed integer in FAC1
GIVAYF = $b391 ;convert 16-bit signed integer to floating point (a=hibyte y=lobyte)
ERRDIR = $b3a6 ;check if prg is running in direct mode/cause error
DEF    = $b3b3 ;perform DEF
STRLIT = $b487 ;scan and setup pointers to a string in memory ptr A lobyte, Y hibyte
GETSPA = $b4f4 ;alloc space in mem for string
FRESTR = $b6a3 ;discard a temporary string
GETBYTC= $b79b ;Input a Parameter Whose Value Is Between 0 and 255
GETADR = $b7f7 ;convert FAC1 to unsigned 16-bit integer; result in $14 lobyte, $15 hibyte
POKE   = $b824 ;perform POKE
WAIT   = $b82d ;perform WAIT
FADDH  = $b849 ;add 0.5 to FAC1
NEGFAC = $b947 ;replace FAC1 with its 2's complement (make it a negative number)
DIV10  = $bafe ;divide FAC1 by 10
MUL10  = $bae2 ;multiply FAC1 by 10
FMULT  = $ba28 ;copy mem pointed by Y(hi),A(lo) to FAC2 then FAC1=FAC1*FAC2
FDIVT  = $bb12 ;divide FAC2 by FAC1 FAC1 = (FAC2/FAC1)
MOVFM  = $bba2 ;move a 5-byte floating point number from memory to FAC1, ptr = A=lobyte, Y=hibyte
MOV2F  = $bbca ;move a 5-byte floating point number from FAC1 to memory $57-$5B BASIC numeric work area
MOVEF  = $bc0f ;copy FAC1 to FAC2 without rounding
QINT   = $bc9b ;convert FAC1 into a 4-byte (32-bit) signed integer within FAC1
INT    = $bccc ;perform INT
FINLOG = $bd7e ;add signed integer to FAC1
INPRT  = $bdc2 ;print IN followed by a line number
LINPRT = $bdcd ;print 2-byte number stored in A (hibyte), X (lobyte)
FOUT   = $bddd ;convert contents of FAC1 to ASCII String

;CBM BASIC routines to raise a specific error
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
TOKEN_INPUTN  = $84
TOKEN_INPUT   = $85
TOKEN_DIM     = $86
TOKEN_READ    = $87
TOKEN_GOTO    = $89
TOKEN_RUN     = $8a
TOKEN_RESTORE = $8c
TOKEN_GOSUB   = $8d
TOKEN_RETURN  = $8e
TOKEN_STOP    = $90
TOKEN_ON      = $91
TOKEN_WAIT    = $92
TOKEN_OPEN    = $9f
TOKEN_CLOSE   = $a0
TOKEN_PRINT   = $99
TOKEN_LIST    = $9b
TOKEN_CLR     = $9c
TOKEN_SYS     = $9e
TOKEN_NEW     = $a2
TOKEN_TO      = $a4
TOKEN_THEN    = $a7

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
FIRST_FUN_TOK = $f4  ;first MDBASIC function
TOKEN_KEY     = $f6
TOKEN_ERROR   = $f7
TOKEN_PI      = $ff  ;PI symbol token

*=$8000 ;"MDBASIC RAM Memory Block"

;cartridge identifier
.word resvec,runstp        ;new reset and runstop vectors
.byte $c3,$c2,$cd,$38,$30  ;necessary for cartridge indicator
;
mesge .byte 147
.text "mdbasic 23.06.23"
.byte 13
.text "(c)1985-2023 mark bowren"
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
.shift "locate"
.shift "disk"
.shift "delete"
.shift "files"
.shift "color"
.shift "move"
.shift "sprite"
.shift "multi"
.shift "expand"
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
.shift "find"
;functions
.shift "time"
.shift "round"
;statement & function
keystr .shift "key"
.shift "error"
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
.rta CMD    ;$9d CMD
.rta sys    ;$9e SYS augmented
.rta $e1be  ;$9f OPEN
.rta close  ;$a0 CLOSE augmented
.rta GET    ;$a1
.rta new    ;$a2
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
.rta locate  ;$d4
.rta disk    ;$d5
.rta delete  ;$d6
.rta files   ;$d7
.rta color   ;$d8
.rta move    ;$d9
.rta sprite  ;$da
.rta multi   ;$db
.rta expand  ;$dc
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
.rta find    ;$f3
.rta time    ;$f4 cmd and func
.rta SNERR   ;$f5 placeholder for round (not a command, func only)
.rta key     ;$f6 cmd & func
.rta error   ;$f7 cmd & func
;
;MDBASIC Function Dispatch Table
funtab
.word fntime, round                ;$f4,$f5
.word keyfn, err                   ;$f6,$f7 are both a command and a function
.word ptr, inf, pen, joy, pot, hex ;$f8,$f9,$fa,$fb,$fc,$fd
.word instr, $ae9e                 ;$fe,$ff (PI Constant)
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
usrerr .shift "user defined"          ;36
;
erradd .word misop, ilvne, illspr, ilcoor, cantre, usrerr
;
;Program Tokenization process - text to tokens via vector ($0304)
;
toknew ldx $7a
 ldy #$04
 sty $0f
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
 bit $0f        ;variable used by Program Tokenization process
 bvs takchr
 cmp #"?"
 bne skip
 lda #TOKEN_PRINT ;PRINT token?
 bne takchr
skip cmp #"0"
 bcc skip1
 cmp #"<"
 bcc takchr
skip1 sty $71
 lda #FIRST_CMD_TOK ;first command token
 sta $0b        ;index into the text input buffer/number of array subscripts
 ldy #$ff
 stx $7a
 dex
cmplop iny
 inx
tstnxt lda BUF,x
 sec
 sbc newcmd,y
 beq cmplop
 cmp #$80      ;last letter?
 bne nxtcmd
 ora $0b
tachr1 ldy $71
takchr inx
 iny
 sta BUF-5,y
 lda BUF-5,y
 beq endln
 sec
 sbc #":"
 beq skip2
 cmp #$49       ;data-:
 bne skip3
skip2 sta $0f
skip3 sec
 sbc #$55       ;rem-:
 bne nxtchr
 sta $08
remlop lda BUF,x
 beq takchr
 cmp $08
 beq takchr
getchr iny
 sta BUF-5,y
 inx
 bne remlop
nxtcmd ldx $7a
 inc $0b        ;count
contin iny
 lda newcmd-1,y
 bpl contin
 lda newcmd,y
 bne tstnxt
 beq oldtok
notfou lda BUF,x
 bpl tachr1
endln sta BUF-3,y
 dec $7b
 lda #$ff
 sta $7a
 rts
oldtok ldy #0
 sty $0b
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
 ora $0b
 bne tachr1
nxtold ldx $7a
 inc $0b
cont1 iny
 lda RESLST-1,y
 bpl cont1
 lda RESLST,y
 bne oldtst
 beq notfou
;
;Evaluate tokens via vector (IGONE)
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
 jsr LP2       ;$E5B4 get char in keyboard buffer
 sta keyentry  ;use K=KEY(0) to get value
 inc keyflag   ;pause key trapping
 lda #3        ;actually 5 since jsr counts for 2
 jsr GETSTK    ;check for space on stack
 lda #>onkey1-1
 pha
 lda #<onkey1-1
 pha
 lda $7b       ;save basic text ptr
 pha           ;of the statement to execute 
 lda $7a       ;after returning from subroutine
 pha           ;and it's
 lda $3a       ;basic line# to RETURN
 pha
 lda $39
 pha
 lda #TOKEN_GOSUB
 pha
 lda keyptr
 sta $7a
 lda keyptr+1
 sta $7b
 lda keyline
 sta $39
 lda keyline+1
 sta $3a
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
let jmp LET     ;perform LET
badtok jmp SNERR
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
;Evalutate functions via vector IEVAL ($030A) originally pointing to EVAL $AE86
;
newfun lda #$00   ;0=number, 255=string - all funcs take a one numeric parameter
 sta $0d          ;Flag Type of Data (String or Numeric) to enforce data type
 jsr CHRGET
 bcc xbcf3        ;numeric digit 0 to 9
 jsr $b113        ;Check If .A Register Holds Alphabetic ASCII Character
 bcs isvar        ;is alpha
 cmp #"@"
 beq octal        ;octal value
 bcs funtok       ;probably token
 cmp #"$"         ;hex value literal?
 beq hexa
 cmp #"%"         ;binary value literal?
 beq binary
oldfun
 jmp $aead        ;execute CBM BASIC function
isvar jmp $af28   ;get value of variable
xbcf3 jmp $bcf3   ;convert ASCII numerals into float with result in FAC1
funtok
 cmp #FIRST_FUN_TOK ;CBM basic max token for functions?
 bcc oldfun       ;bad func token - will raise error
 sbc #FIRST_FUN_TOK
 asl              ;index * 2 for word pointer indexing
 tay              ;prepare for direct indexing
 lda funtab,y     ;lobyte value of address for function
 sta $55          ;lobyte for indirect addressing
 lda funtab+1,y   ;hibyte value of address for function
 sta $56          ;hibyte for indirect addressing
 jmp $0054        ;execute function
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
;clear $5d-$60 work area and $61-$66 FAC1
clrfac lda #0
 ldx #10
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
 cmp #$b2         ;equal sign token
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
clrtime
 lda #%10010010  ;BCD 12am (am/pm flag=1 because it flips on write when hour is 12)
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
 jsr chrget   ;advance txtptr 1 position and get the char
 dec R6510
 cmp #"$"
 bne time2
;get time as a string
 jsr getimstr
 inc R6510
 ldy #>BAD
 lda #<BAD
 jsr STRLIT
 jmp CHRGET
;get time in seconds since midnight
time2
 jsr dotime
 inc R6510
 lda $7a
 pha
 lda $7b
 pha
 stx $7a
 sty $7b
 jsr FRMNUM
 pla
 sta $7b
 pla
 sta $7a
 rts
;
;******************************************
;LIST command re-write to decode new tokens via vector ($0306)
;Supports freezing the listing while holding down the shift key.
;This routine is called repeatedly until the entire list range is complete.
;******************************************
list pha      ;save a reg from CHRGET
 tya          ;also 
 pha          ;save y reg from CHRGET
shift lda #$01 ;check mem ctrl reg
 bit SHFLAG   ;0=none, 1=shift key, 2=logo key, 4=ctrl key
 bne shift    ;bit pattern 001=shift, 010=commodore, 100=ctrl (any combo)
 pla          ;restore y reg
 tay
 pla          ;restore a reg
 bpl out      ;less than 128 is non token so just output char as-is
 bit $0f      ;quote mode enabled?
 bmi out      ;bit7 set means yes so just output char as-is
 cmp #TOKEN_PI  ;pi token?
 beq out        ;just output pi symbol as-is
 cmp #FIRST_CMD_TOK ;first MDBASIC command token?
 bcs newlst     ;greater or equal to first MDBASIC token so decode the command text
 jmp $a724      ;perform part of CLR cmd. done here.
out jmp $a6f3  ;output byte as it is on cmd line
newlst
 sbc #FIRST_CMD_TOK-1  ;calc index
 tax          ;index to x reg soon to subtract 1 so 0-based index
 sty $49      ;store y reg value from CHRGET
 ldy #$ff
nextt dex     ;next token index
 beq found    ;if we are on first token index then it must be a match
loop1 iny
 lda newcmd,y ;get command's next text char from table
 bpl loop1    ;the last character has bit 7 on as a flag of end-of-string
 bmi nextt    ;reached end of string with no match so try next command
found iny     ;found a command match for every chr in string
 lda newcmd,y ;get current char in command string
 bmi oldend   ;if on last chr of command then continue with old list function
 jsr CHROUT
 jmp found    ;next char
oldend jmp $a6ef ;list old
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
 bne istrue      ;non-zero means expression is true
 ldx #TOKEN_ELSE
 jsr DATAN+5     ;scan for end of line terminator (byte 0) or ELSE token
 tax             ;a reg holds byte found, either 0 or ELSE token
 beq nxtline     ;end of line so go to next line
 tya             ;y holds num bytes to advance txtptr forward
 clc             ;add offset to txtptr
 adc $7a
 sta $7a
 lda $7b
 adc #0
 sta $7b
 jsr CHRGET      ;skip over ELSE token to next char or token
 bcc goto        ;ascii numerials indicate line number for GOTO
endlin
 jmp tstcmd      ;process statements on the rest of current line
istrue
 jsr CHRGOT      ;check current char is numeric digit
 bcs endlin
goto jmp GOTO    ;preform goto
nxtline jmp REM  ;perform REM to advance txtptr to next line
;
vars dec R6510
 jmp varss
;
;*******************
;DISK S$                         - DOS command string
;"S0:myfile.bas"                 - delete a file
;"N0:label,id"                   - full format disk with a label and id
;"N0:label"                      - soft format (BAM only) with label
;"I0:"                           - initialize disk (clears last error and moves head to track 0, sector 0)
;"C0:sourceFile=destinationFile" - copy a file
;"R0:newfileName=oldfileName"    - rename a file
;"V0:"                           - validate (defragment) disk
donehere rts
disk
 jsr getstr0     ;get DOS string
 beq donehere
 jsr SETNAM
 jsr getdskdev   ;get disk device num in x reg
 lda #$7e        ;file handle 126
 ldy #$0f        ;secondary channel 15 = DOS channel
 jsr SETLFS
 jsr OPEN        ;performs OPEN 126,8,15, "string"
 bcs err126
 lda $9d         ;display message if not in prg mode
 bpl closeit     ;don't display disk status
 dec R6510
 jsr bufio
 inc R6510
 bcs err126
 jsr CLRCHN      ;restore default devices
closeit
 jmp clse7e
;
err7ee
 inc R6510
err126
 pha
 jsr clse7e
 pla
 tax
 jmp (IERROR)
;
;*******************
;FILES [volume$]
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
 sta $63        ;actual length allocated
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
 lda #$7e       ;file handle 126
 ldy #$00       ;secondary 0
 jsr SETLFS
 jsr OPEN       ;perform OPEN 126,8,0,S$
 bcs err126     ;handle error
 dec R6510
 jmp filess
;get and validate the disk device number 8-11, default 8
getdskdev
 ldx #8         ;default device 8
 jsr comchk
 bne illdev-1
 jsr getval     ;get single byte value in x reg
 cmp #8
 bcc illdev
 tax
 cmp #12
 bcs illdev
 rts
illdev
 ldx #9         ;illegal device number error
 jmp (IERROR)
;
;Open MDBASIC file handle for printer
opnprt0
 ldy #$00       ;secondary parameter $FF=not used, 5=binary graphic
opnprt          ;7=upper/lower case chars, 0=Upper case and symbol chars
 lda #$7f       ;file handle 127
 ldx #$04       ;device 4
 jsr SETLFS     ;set logical file parameters BASIC eq open 127,4,0
 lda #$00       ;zero byte file name length (no name)
 jsr SETNAM     ;set file name
 jsr OPEN       ;perform OPEN 127,4,0,""
 bcc prtopen    ;clear carry flag means success
err127
 pha
 jsr clse7f
 pla
 tax
 jmp (IERROR)
prtopen
 ldx #$7f     ;pass file handle param into CHKOUT via x reg
 jsr CHKOUT   ;make 127 the current I/O file number
 bcs err127
 rts
;
;*******************
;The DUMP command supports multiple options based on a second required token (or expression)
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
dumpexpr jsr opnprt0
 jsr $aa9d   ;perform print of expression
 jmp clse7f
dumpvars jsr opnprt0
 jsr vars
 jmp closer+2
dumplist jsr opnprt0
 lda #$01
 sta listflag
 jsr opget   ;calls CHRGET first thing!
 jsr $a6c9   ;perform list
 jsr printcr ;print carriage return
 jmp clse7f
dumpscreen dec R6510 ;switch to LORAM ($a000-$bfff)
 jsr dumpscreen2
closer inc R6510     ;switch to LOROM ($a000-$bfff)
 jsr clse7f
 jmp CHRGET
dumpbitmap dec R6510 ;switch LOROM to LORAM
 jsr dumpbitmap2
 jmp closer
dumpfiles jsr opnprt0
 ldx #$7f
 stx $13    ;redirect std output to device on file 127
 jsr CHRGET ;skip over FILES token
 jsr files
;close MDBASIC file handles and restore std io channels
 lda #0
 sta $13
clse7e
 lda #$7e
 jsr CLOSE
clse7f
 lda #$7f
 jsr CLOSE
 jmp CLRCHN     ;restore default devices as current I/O channels
;
;*******************
; FILL x1,y1 TO x2,y2, [scanCode], [color]
fill
 jsr getcoords
 jsr ckcom2     ;check for and skip over comma, misop err if missing
 cmp #","       ;another comma?
 beq srncol
 jsr skip73     ;get scan code
 sta $bb
;fill text
 ldx $bf        ;line count
pokep ldy $be
nextp lda $bb
 sta ($fb),y
 dey
 bpl nextp
 lda $fb
 clc
 adc #40
 sta $fb
 lda $fc
 adc #$00
 sta $fc
 dex
 bpl pokep
srncol
 jsr chkcomm    ;check and skip over comma, quit if not found
 jsr skip73     ;get color
 sta $02
;fill color
 ldx $bf
nxtc ldy $be
 lda $02
nxtcol sta ($fd),y
 dey
 bpl nxtcol
 lda #40
 clc
 adc $fd
 sta $fd
 lda $fe
 adc #$00
 sta $fe
 dex
 bpl nxtc
 rts
;
tokopn
 dec R6510
 jsr openrs232
 inc R6510
 bcc tokopn-1 ;clear carry indicates success
 jmp (IERROR) ;otherwise x reg has error number
;
tokclse
 lda #126     ;logical file number
 jsr $f314    ;find the index of an opened logical file number to X reg
 bne clsd232  ;zero flag indicates not found
 jsr $f31f    ;set current logical file, current device, and current seconday address
 txa
 jsr $f2f2    ;remove from table of open files
 jsr $f483    ;Initialize IRQ Timers and data direction registers
 lda #0       ;clear hibytes of I/O buffers for RS-232 to indicate not used
 sta $f8      ;hibyte ptr to RS-232 input buffer
 sta $fa      ;hibyte ptr to RS-232 output buffer
clsd232
 jmp CHRGET   ;finally, skip over token
;
tokprt
 ldx #126
 stx $13      ;set current I/O channel (logical file) number
 jsr $e118    ;BASIC wrapper for CHKOUT with error handling
 jsr CHRGET   ;position txtptr on first char of expression
 jsr PRINT    ;perform CBM BASIC PRINT
waitout
 jsr STOP
 beq end232   ;STOP key pressed, abort print
 lda ENABL    ;transmitting when bit0 is 1
 and #1
 bne waitout
end232
 lda $13       ;current I/O channel (logical file) number for UNLSN and UNTALK
 jsr CLRCHN    ;restore default i/o devices and send UNLSN and UNTALK to serial device
 ldx #$00      ;logical file number 0=none
 stx $13       ;set current I/O channel (logical file) number
 rts
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
 stx $bb       ;timeout lobyte
 stx $bc       ;timeout hibyte
 inx           ;1=timeout disabled
 stx $53       ;timeout disabled by default 0=enabled, 1=disabled
 cmp #TOKEN_WAIT
 bne tokread
 lda #8        ;bit 3 same as status bit of empty buffer
 sta $fe
 jsr CHRGET
 cmp #TOKEN_READ
 beq tokread   ;no timeout supplied
 jsr skp73_    ;get timeout 0-65535
 stx $bb
 sty $bc
 txa
 ora $bc
 beq tokread   ;zero timeout is a disabled timeout
 dec $53       ;enable timeout
tokread
 lda #TOKEN_READ
 jsr CHKCOM+2  ;skip over READ token otherwise SYNTAX ERROR
 ldx #126      ;file number 126
 stx $13       ;current I/O channel (cmd logical file) number
 jsr $e11e     ;BASIC wrapper for CHKIN with error error handling
;get or create pointer to string pointer provided as param
 jsr PTRGET    ;search for a var & setup if not found
 sta $49       ;variable address is returned in a (lo byte) and y (hi byte) registers
 sty $4a       ;every string variable is a pointer consisting of 3 bytes, 2 for ptr, 1 for length
;handle numeric read
 ldx $0d       ;0=numeric, 255=string
 inx
 stx $97       ;0=string,1=float
 beq rdstr
 ldx $0e       ;float or int?
 beq rdnum     ;float stores 5 bytes
 inc $97       ;2=int
rdnum
 sta $35       ;ptr of a numeric variable
 sty $36       ;is the ptr of the value
 lda #1        ;length of 1 byte
 sta $02       ;read one byte
 sta $fd       ;offset to store byte
 bne chksent   ;always branches 
;allocate space for new string 
rdstr
 lda #$ff      ;max string length
 jsr GETSPA    ;alloc new str return ptr in $35,$36 and length in A reg
 sta $02       ;actual length allocated
;change pointer to newly allocated string
 ldy #0
 sty $fd       ;offset to store bytes
 sta ($49),y   ;string length byte
 iny
 lda $35       ;lobyte str ptr
 sta ($49),y   ;var lobyte str ptr
 iny
 lda $36       ;hibyte str ptr
 sta ($49),y   ;var hibyte str ptr
;check if sentinel param supplied
chksent
 lda #0
 sta $fb       ;flag for sentinel check
 jsr CHRGOT
 cmp #TOKEN_TO
 bne savesb
 jsr getval    ;get sentinel byte param value
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
 lda $bb
 sta $14
 lda $bc
 sta $15
;read next byte with timeout (if enabled)
 jsr waitread
 bcs setstrlen
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
 lda $97       ;string type
 bne goread    ;numeric variable use only 1 byte
nxtbyte
 inc $fd       ;next index in string
 dec $02       ;reduce byte count for read
 beq setstrlen ;stop reading
 bne goread    ;keep reading if more room in string

;include byte in string length
strdone
 inc $fd       ;string length = index+1

;return result based on data type
setstrlen
 ldx $97       ;type 0=string, 1=float, 2=int
 beq setstr
 dex
 beq setflt
 lda #0        ;make hibyte zero
 tay
 sta ($35),y
 beq done232   ;always branches
setflt
 ldy $61       ;byte read is lobyte
 lda #0        ;zero hibyte
 jsr GIVAYF    ;convert binary int to float with result in FAC1
 ldx $35       ;copy the result in FAC1
 ldy $36       ;to the variable memory
 jsr $bbd7     ;copy FAC1 to memory
done232
 jmp end232
setstr
 lda $fd
 ldy #0
 sta ($49),y   ;string length byte
 beq done232   ;always branches
;
;read a byte with timeout (if enabled)
waitread
 jsr $f086     ;CHRIN for RS-232 device
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
 lda $53       ;timer flag 0=timeout enabled, else disabled
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
;*******************
;OLD takes no parameters
old lda #$08
 sta $0802
 jsr LINKPRG
 lda $22   ;apply calculated end-of-prg pointer
 clc
 adc #$02
 sta $2d   ;Pointer to the Start of the BASIC Variable Storage Area
 sta $2f   ;Pointer to the Start of the BASIC Array Storage Area
 sta $31   ;Pointer to End of the BASIC Array Storage Area (+1), and the Start of Free RAM
 lda $23
 adc #$00
 sta $2e
 sta $30
 sta $32
 rts
;
;*******************
; VOL n   where n=0 to 15
vol
 jsr getval15_0 ;only values from 0-15 allowed
 sta $02
 lda md418   ;read current value which includes upper nibble that holds the resonance
 and #%11110000  ;clear volume bits only
 ora $02     ;apply new volume bits only
setvol
 sta md418   ;maintain global var value for reading
 sta SIGVOL  ;SID register lower nibble is volume, upper nibble is filter type
 rts
;
;*******************
; SYS address [,a] [,x] [,y] [,p]
sys
 jsr skp73_   ;get 2-byte int into $14 lobyte, $15 hibyte
 lda #252     ;prepare for loop of 4 registers
 sta $02      ;current register index offset
regloop
 jsr CHRGOT   ;another param?
 beq sysend
 jsr $b7f1    ;evaluate expression to single byte int to x reg
 txa
 inc $02      ;next regsiter index
 ldy $02
 sta $030c-253,y
 bne regloop
sysend
 jmp SYS+6    ;perform remainder of SYS
;
;*******************
; WAIT location, mask1 [,mask2]
; WAIT jiffies
wait
 jsr skp73_   ;get 2-byte int into $14 lobyte, $15 hibyte
 jsr CHRGOT   ;another param?
 beq delay2   ;no, do jiffy wait
 jsr $b7f1    ;evaluate expression to single byte int to x reg
 jmp WAIT+3   ;continue with original WAIT cmd
delay2        ;entry point for internal use; set x and y reg accordingly
 clc          ;flag for STOP key
 txa
 bne decx
 tya
 beq stopnow
 dey
decx dex
 lda $a2      ;jiffy clock updated 60 times per sec.
dlay2 cmp $a2
 beq dlay2
 lda $c5      ;Matrix Coordinate of Last Key Pressed, 64=None Pressed
 cmp #$3f     ;STOP key?
 bne delay2   ;carry flag will be returned to caller to indicate STOP key pressed
stopnow rts
;
;*******************
; AUTO      :uses last used setting, default is 10
; AUTO n    :where n=1 to 1023
; AUTO OFF  :turn off auto numbering
auto
 beq applyauto
 cmp #TOKEN_OFF
 beq autooff
 cmp #TOKEN_ON
 bne autoset
 jsr applyauto
 jmp CHRGET
autooff
 lda $e449     ;$A483 is original main loop
 sta IMAIN
 lda $e44a
 sta IMAIN+1
 jmp CHRGET
autoset jsr skp73
 lda $15
 cmp #4       ;max auto-line number value is 1023
 bcc okauto
badauto jmp FCERR
okauto ora $14
 beq badauto  ;0 is also not allowed
 lda $15
 sta autonum+1
 lda $14
 sta autonum
applyauto lda #<aline
 sta IMAIN
 lda #>aline
 sta IMAIN+1
 rts
;**auto line numbering routine**
aline jsr INLIN
 stx $7a
 sty $7b
 jsr CHRGET
 tax
 beq aline
 ldx #$ff
 stx $3a
 bcc doauto
 jmp $a496
doauto jsr LINGET
 jsr CHRGOT
 beq eauto
 lda $14
 clc
 adc autonum
 sta $63
 lda $15
 adc autonum+1
 sta $62
 ldx #$90   ;FAC1 exponent
 sec
 jsr $bc49  ;convert FAC1 to float
 jsr FOUT+2 ;convert FAC1 to ASCII String at $0100
 ldy #1
getnum lda BAD-1,y ;Work Area for Floating Point to String Conversions
 beq endnum
 sta KEYD-1,y   ;put char in keyboard buffer
 iny
 bne getnum
endnum lda #32  ;space char
 sta KEYD-1,y
 sty NDX        ;num chars in keyboard buffer
eauto jmp $a49f ;main loop for direct mode
;
;*******************
; TRACE line#
;runs the program with trace mode enabled
trace pha ;save CHRGET value
 lda #$ff
 sta $f9
 sta $fa
 ldx #<exccmd
 ldy #>exccmd
 jsr settrace
 pla     ;restore CHARGET value
 jmp RUN
;**trace subroutine during prg execution
trace1 lda $9d ;prg mode?
 beq trace2
 lda #$00
 ldx #<execut
 ldy #>execut
settrace
 sta traceflag
 stx IGONE
 sty IGONE+1
 rts
trace2 lda $39
 cmp $f9
 bne copyln
 lda $3a
 cmp $fa
 beq etrace
copyln lda $39 ;copy line#
 sta $14
 sta $f9
 lda $3a
 sta $15
 sta $fa
 ldy #$4f
 lda HIBASE
 sta clrtop+2
 lda #" "
clrtop sta $0400,y
 dey
 bpl clrtop
 lda PNTR   ;remember current cursor position
 pha
 lda TBLX
 pha
 jsr weglst ;find and display line number in $14,$15
 pla        ;restore original cursor position
 tax
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
weglst
 jsr FINDLN ;find BASIC line number in $14,$15
 bcc endprg ;line not found
 jsr $a82c  ;test STOP key for break in program
 lda #19    ;chr 19 = cursor home
 jsr CHROUT
 ldy #$01
 sty listflag
 jmp $a6d7  ;perform LIST of current line
endprg
 jsr $a67a  ;empty the stack
 jmp HALT
;
;*******************
; RUN
; RUN linenum
; RUN filename$,[device],[secondary]
oldrun jmp RUN  ;CBM BASIC - perform RUN
newrun
 php
 jsr detrap     ;turn off error trapping in case it was enabled in previous run
 jsr clearerr   ;clear last error info
 lda #0
 sta keyflag    ;ensure key trapping is off
 plp
 beq oldrun     ;RUN without params
 bcc oldrun     ;RUN with line num param
 lda #$00       ;RUN with file params
 sta $0a        ;load or verify? 0=load, 1=verify
 jsr $e1d4      ;process file parameters
 jsr RUNC       ;reset ptr to current text char to the beginning of program text
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
 sta $0a
 jsr $e1d4   ;set params for load, verify and save
 lda $2d
 sec
 sbc #$02
 tax
 lda $2e
 sbc #$00
 tay
 lda #$00
 jsr LOAD    ;load from a device
 bcs ioerr   ;carry set indicates error
 jsr READST  ;read i/o status word
 and #%10111111 ;did an error occur other than EOF/EOI (bit6)?
 beq okmerg  ;no error
 jmp LODERR  ;raise LOAD ERROR
okmerg stx $2d
 sty $2e
 jsr $a659   ;reset txt ptr to beginning of prg then perform CLR
 jsr LINKPRG ;relink lines of tokenized prg text
 jmp READY   ;main basic loop
ioerr jmp $e0f9 ;handle i/o error
;
;*******************
;secondary address 2=SCREEN, 3=CHAREN, 4=BITMAP
newlod
 cpx #5
 bcs oldload
 dec R6510      ;switch from HIROM to HIRAM (a000-bfff)
 jmp loadd      ;continue under rom with the rest of the new load routine
romin
 lda R6510
 ora #%00000001 ;ensure HIROM active
 sta R6510
 jsr CLRCHN     ;restore default I/O devices
 lda $b8        ;close current open file
 jmp CLOSE
;
newload
 sta $93     ;flag for load routine 0=Load, 1=Verify
 ldx $b9     ;secondary address
 stx $02     ;save for use after load to determine if mem ptrs need to be restored
 cpx #2
 bcs newlod  ;indicates MDBASIC load
oldload
;CBM code from original vector location $f4a5 to perform load
 lda #0
 sta $90    ;kernal I/O status
 lda $ba    ;get current device number
 bne xf4b2  ;0=keyboard
xf4af jmp $f713 ;load from keyboard or screen
xf4b2 cmp #3
 beq xf4af  ;3=screen
 bcs xf4b8  ;4=printer,8-9=disk
 jmp $f533  ;1=dataset, 2=rs-232
xf4b8 ldy $b7 ;length of current filename
 bne xf4bf
 jmp $f710  ;handle error #8 - MISSING FILE NAME ERROR
xf4bf ldx $b9 ;current secondary address
 jsr $f5af  ;print SEARCHING
 lda #$60
 sta $b9    ;current secondary address
 jsr $f3d5  ;open file
 lda $ba    ;current device number
 jsr TALK   ;send talk to a device on the serial bus
 lda $b9    ;current secondary address
 jsr TKSA   ;send a secondary address to a device on the serial bus after talk
 jsr ACPTR  ;receive a byte of data from a device on the serial bus
 sta $ae    ;low byte of address for load which will increment to the end address
 sta $c1    ;remember start address
 lda $90    ;kernal I/O status
 lsr        ;bit 1 = serial read timeout
 lsr        ;shift right into carry to detect timeout
 bcc oklod
 jmp $f704  ;handle error #4 - FILE NOT FOUND
oklod
 jsr ACPTR  ;receive a byte of data from a device on the serial bus
 sta $c2    ;remember start address
 jsr $f4e3  ;continue with original LOAD subroutine
 bcc oklod2 ;carry set indicates error
 jmp $e0f9  ;handle i/o error
oklod2
 jsr READST ;read the I/O status
 and #%10111111 ;did an error occur other than EOF/EOI (bit6)?
 beq oklod3 ;no error
 jmp LODERR ;raise LOAD ERROR
oklod3
 lda $9d    ;display message if not in prg mode, #$C0=kernal & ctrl, #$80=ctrl only
 bpl lodone ;don't display load addresses
 jsr $ab3f ; print space
 ldx $2b    ;assume BASIC mem load
 lda $2c
 ldy $02    ;secondary device: 0=BASIC load, 1=binary
 beq prtmem
 ldx $c1    ;print mem ptr from file
 lda $c2
prtmem
 jsr LINPRT ;print 2-byte binary value
 lda #"-"
 jsr CHROUT
 ldx $ae    ;ptr to end addr of loaded file
 lda $af
 jsr LINPRT ;print 2-byte binary value
lodone
 lda $0a    ;load=0 or 1=verify
 bne lodbas
 lda $02    ;secondary address
 bne lodbin
lodbas
 ldx $ae    ;restore x,y ptr to end of prg from load subroutine
 ldy $af
 clc        ;no error
 rts
lodbin
 lda $2c    ;check if binary load was actually a BASIC prg
 cmp $c2    ;by comparing the start address of loaded binary
 bne isbin  ;with the start address of BASIC prg mem
 lda $c1    ;if it was loaded exactly in BASIC mem
 cmp $2b    ;then finish load as usual to init mem ptrs
 beq lodbas ;this will kill the current running BASIC prg
isbin       ;otherwise do not return to calling subroutine
 pla        ;to prevent adjusting BASIC memory pointers
 pla        ;this way no corruption of BASIC mem will occur
 rts        ;and the running BASIC program can continue
;
;*******************
;secondary address 2=SCREEN, 3=CHAREN, 4=BITMAP
newsave
 lda $b9      ;secondary address
 cmp #5
 bcs oldsav   ;file handles >=128 are MDBASIC file handles
 cmp #2
 bcs newsav
oldsav
 jmp $f5ed    ;perform normal save
newsav
 dec R6510
 jmp savee
;
;mdbasic bsave need 47 bytes
bsave
 beq osave
 jsr FRMEVL   ;eval expression
;set file defaults
 lda #$00     ;default file#
 ldx #$01     ;default device 1 (tape)
 ldy #$00     ;default secondary 0
 jsr SETLFS   ;set logical file params, A=file#,X=device,Y=secondary
;check last expression evaluated data type
 lda $0d      ;numeric or string?
 beq bsaver
;continue as if normal subroutine
 jsr $e25a    ;FRESTR and SETNAM
 jsr $e1e6    ;get remaining save params
 jmp $e159    ;save BASIC prg to device
osave
 jmp $e156
bsaver
 jsr skp73+3  ;start address
 stx $c1
 sty $c2
 jsr dbyval   ;end address
 stx $ae
 sty $af
 jsr CHRGET   ;advance to next param or end of statment
 jsr $e1d4    ;set parms for LOAD, VERIFY, SAVE
 ldx $ae      ;ptr to end address
 ldy $af      ;in x and y reg
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
 jmp $A8F8
findlnr
 inc R6510
 jsr FINDLN   ;search for line#
 jsr $a6c9    ;perform list (print line on screen)
 dec R6510
 rts
;
;*******************
; DELETE line  (delete one line)
; DELETE start-end  (delete all lines from start to end)
delete
 jsr opget2
 lda $5f
 sta $24
 lda $60
 sta $25
 jsr FINDLN
 dec R6510
 jsr deletee
 inc R6510
relink jsr LINKPRG
 lda $22
 ldx $23
 clc
 adc #$02
 sta $2d
 bcc savex
 inx
savex stx $2e
 jsr $a659  ;reset txt ptr to beginning of prg then perform CLR
 jmp endprg
;
;end of renum; list any go tokens with 65535 as line number (errors)
erenum jsr LINKPRG
 lda #>BUF
 sta $7b
 lda #<BUF
 sta $7a
 ldy #$05
ffff lda nolin,y
 sta BUF,y
 dey
 bpl ffff
 jsr find ;find all 65535
 jmp relink
;
;*******************
; RENUM            :use defaults, start at 10 inc by 10
; RENUM start      :start line specified, default inc 10
; RENUM start, inc :use both start and inc specified
renum
 bne renumm  ;param1 specified
 lda #10     ;no params, set default
 sta $35     ;start at line 10, inc by 10
 sta $33
 lda #0
 sta $36
 jmp hiinc
renumm jsr LINGET  ;convert ascii to binary
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
 bne okinc
 jmp FCERR    ;increment of 0 not allowed
okinc lda $15
hiinc sta $34
 jsr RUNC     ;reset ptr to beginning of basic prg
strt jsr chrget
 jsr chrget
 bne serch
 jsr tofac
strnum jsr chrget
 jsr chrget
 beq erenum
 jsr chrget
 lda $63
 sta ($7a),y
 jsr chrget
 lda $62
 sta ($7a),y
 jsr addinc
 beq strnum
serch jsr chrget
 jsr chrget
nocrap jsr chrget
craper cmp #"""
 bne tokgo
;skip over expression in quotes
crap jsr chrget
 beq strt
 cmp #"""
 bne crap
 beq nocrap
tokgo tax
 beq strt
 bpl nocrap
;check if token is a statment that use line numbers
 ldx #10     ;there are 10 tokens that reference a line number
chktok
 cmp gotok-1,x
 beq sav7a
 dex
 bne chktok
 beq nocrap
sav7a lda $7a
 sta $3b
 lda $7b
 sta $3c
 jsr CHRGET
 bcs craper
 jsr FRMNUM
 jsr GETADR
 jsr replac
 dec R6510
 jsr worker
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
pnl ldx #$90
 sec
 jsr $bc49    ;convert FAC1 to float
 jmp FOUT+2   ;convert FAC1 to string
nexlin jsr chrget
 jsr addinc
 beq goagan
inc2d jsr clrflg
 inc $97
 jsr bufer
 inc $2d
 bne gbwyc
 inc $2e
gbwyc rts
;
clrflg lda $7a
 sta $22
 lda $7b
 sta $23
 lda $2d
 sta $24
 lda $2e
 sta $25
 ldy #$00
 sty $0b
 sty $97
 rts
;
tofac lda $35
 sta $63
 lda $36
 sta $62
 jmp RUNC       ;reset ptr to current basic text to beginning
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
chrget ldy #$00
 inc $7a
 bne ne7a
 inc $7b
ne7a lda ($7a),y
 rts
;
bufer ldy $0b
 lda ($24),y
 ldy $97
 sta ($24),y
 jsr pntreq
 bne pne2
 rts
pne2 lda $24
 bne ne24
 dec $25
ne24 dec $24
 jmp bufer
;
pntreq lda $22
 cmp $24
 bne gbhah
 lda $23
 cmp $25
gbhah rts
;
;******************
; RESTORE       - set first data line as next DATA READ
; RESTORE line# - set line# as next DATA READ
restor
 beq oldrst
 jsr skp73
 jsr getlin        ;find the line specified
 stx $41           ;set DATA ptr to the start of line
 sty $42
 rts
oldrst jmp RESTORE ;original CBM RESTORE takes no params
;
;*******************
; NEW [SYS]
new beq oldnew
 cmp #TOKEN_SYS
 bne oldnew
 jmp ($fffc)
oldnew jmp NEW
;
;*******************
; POKE mem, value
; POKE mem1 TO mem2, value, [operation]
;operation is optional (default 0): 0=SET,1=AND,2=OR,3=EOR
poke
 jsr skp73_      ;get 2-byte int in $14,$15
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
 jsr skp73_      ;get 2-byte int in $14,$15
 jsr ckcom2      ;check for and skip over comma, misop err if missing
 jsr GETBYTC+3   ;get poke value
 stx $fe         ;set poke value
 lda #0
 sta $fd         ;set default poke type 0=SET,1=AND,2=OR,3=EOR
 jsr comchk      ;poke type param?
 bne gopoke
 jsr GETBYTC
 stx $fd
 cpx #4
 bcs baderr2
gopoke
 dec R6510
 jmp pokee
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
;
undef jmp UNDEFST   ;UNDEF'D STATEMENT
baderr2 jmp FCERR   ;illegal qty err
;
; ERROR CLR    :clear last error data
; ERROR OFF    :turn off error trapping
; ERROR errnum :raise error (1-35)
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
 jsr skip73z  ;valid error number is 1-127
 bmi baderr2  ;128 and over is invalid
 tax
 jmp (IERROR)
;
;*******************
; SWAP A, B    SWAP A%, B%    SWAP A$, B$
swap
 beq mop4
 jsr PTRGET    ;get param1
 sta $14
 sty $15
 lda $0d       ;data type string or numeric
 sta $fd       ;save param1 data type
 lda $0e       ;numeric type int or float
 sta $fe       ;save param1 numeric type
 jsr ckcom2    ;check for and skip over comma, misop err if missing
 jsr PTRGET    ;get param2
 lda $0e       ;param2 numeric type, int or float
 cmp $fe       ;does param2 have the same numeric type as param1?
 bne nomtch    ;mismatch
 ldx $fd       ;param1 numeric type, int or float
 cpx $0d       ;does param2 have the same num/string type as param1
 bne nomtch    ;mismatch
 lda #1
 inx           ;$FF=string so $FF+1 = 0
 beq isstr     ;string uses 3 bytes 0-2
 ldx $0e       ;int or float?
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
gfn jsr skip73    ;get file number
 jsr CLOSE        ;close file if open, ignore if not
 jsr chkcomm      ;check for comma, quit if none otherwise skip over it
 jmp gfn          ;process next file number
clsfiles
 jsr CLALL        ;close all open files
 jsr CLRCHN       ;restore default devices
 jmp CHRGET
;
;*******************
; ON ERROR GOTO line
; ON ERROR RESUME NEXT
; ON KEY GOSUB line
; ON i GOTO line1,line2,...linen
baderr jmp SNERR  ;syntax err
;
on
 beq mop4
 cmp #TOKEN_ERROR
 beq onerror
 cmp #TOKEN_KEY
 beq onkey
 jmp ONGOTO   ;perform ON
onkey jsr CHRGET
 cmp #TOKEN_GOSUB
 beq onkeygo
 cmp #TOKEN_OFF
 bne baderr
 jmp onkeyoff
onkeygo
 jsr getline
 stx keyptr   ;of the line# specified
 sty keyptr+1
 lda $14
 sta keyline
 lda $15
 sta keyline+1
 lda #1
 sta keyflag  ;turn on key trapping
 rts
onerror jsr CHRGET
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
 ldx #<resumenext  ;apply ON ERROR RESUME NEXT
 ldy #>resumenext  ;so that all errors will be ignored
 jsr seterrvec     ;and failed statement are skipped
 jmp CHRGET
errgoto jsr getline
 stx txtptr        ;of the line# specified
 sty txtptr+1
 lda $14
 sta errtrap
 lda $15
 sta errtrap+1
entrap         ;enable error trapping
 ldx #<trap
 ldy #>trap
 bne seterrvec ;hibyte of vector will always be non-zero
detrap         ;enable error trapping
 ldx #<errors
 ldy #>errors
seterrvec
 stx IERROR
 sty IERROR+1
 rts
;
;*******************************************
; error trap routine for ON ERROR RESUME NEXT
;*******************************************
resumenext     ;ON ERROR RESUME NEXT
 lda $9d       ;0=suppress msgs (program running mode)
 bne quitrun
 txa
 bmi olerr
 stx errnum    ;update last error number
 lda $3a       ;update last BASIC line# causing error
 sta errline+1
 lda $39
 sta errline
nxtstmt        ;prepare next stmt for execution
 ldy #0
 lda ($7a),y
 bne _a807
 ldy #2
 lda ($7a),y
 clc
 bne _a7ce
 jmp endprg    ;return control to main BASIC loop
_a7ce iny
 lda ($7a),y
 sta $39
 iny
 lda ($7a),y
 sta $3a
 tya
 adc $7a
 sta $7a
 bcc _a7e1
 inc $7b
_a7e1 jmp pullit
_a807 cmp #$3a
 beq _a7e1
 jmp REM
;
quitrun
 jsr detrap    ;disable error trapping
olerr jmp errors
;*******************************************
; general error trap routine
;*******************************************
trap lda $9d   ;MSGFLG Flag Kernal Message Control
 bne quitrun   ;$C0=kernal & ctrl, $80=ctrl only, $40=kernal only, $00=none
 txa           ;x holds the error num
 bmi olerr     ;bit 7 on means no error
 stx errnum    ;set current error number
 jsr detrap    ;disable error trapping
 lda #3        ;3 plus the 2 for this jsr is 5 bytes
 jsr GETSTK    ;ensure space on stack, out of mem err if not
 lda $3e       ;save the BASIC text ptr
 pha           ;of the beginning of the stmt
 lda $3d       ;that caused the error
 pha           ;and save the BASIC
 lda $3a       ;line# for resume
 sta errline+1
 pha
 lda $39
 sta errline
 pha
 lda #TOKEN_ERROR ;error token
 pha
 lda txtptr
 sta $7a
 lda txtptr+1
 sta $7b
 lda errtrap
 sta $39
 lda errtrap+1
 sta $3a
 jmp NEWSTT   ;setup next statement for execution and continue BASIC main loop
;
;*******************
; RESUME line#
; RESUME NEXT
resume
 pla          ;discard calling subroutine
 pla
 tsx
 lda BAD+1,x
 cmp #TOKEN_ERROR
 beq okresu
 ldx #35      ;can't resume error
 jmp (IERROR)
okresu
 jsr clearerr ;clear last error info
 jsr CHRGOT
 beq resum    ;no token or digit, then resume with statement that caused the error
 cmp #TOKEN_NEXT ;next token?
 bne resume0
;perform RESUME NEXT - next statement after the one that caused the error
 pla          ;discard ERROR token
 pla 
 sta $39      ;pull line number from stack and make current
 pla 
 sta $3a
 pla 
 sta $7a      ;pull text ptr from stack and make current
 pla 
 sta $7b
 ldy #0       ;the first stmt on line will begin
 lda ($7a),y  ;at the end marker of previous line.
 bne skpstmt  ;zero here indicates previous line
 lda $7a      ;preceded by the 4-byte line header
 clc          ;which will be skipped over so that
 adc #4       ;txtptr is on the byte that began
 sta $7a      ;the stmt that caused the error.
 lda $7b
 adc #0
 sta $7b
skpstmt
 jsr CHRGET   ;get current char at txtptr
 jsr DATA     ;scan for start of next BASIC stmt
 jmp nxtstmt  ;setup next stmt for execution
;perform RESUME line#
resume0
 pla          ;discard ERROR token
 pla          ;discard line number that caused the error
 pla
 pla          ;discard txt ptr of error
 pla
 jsr CHRGOT
 jsr GOTO     ;perform goto (adjust txt ptr to given line num)
 jmp nxtstmt
;perform RESUME - with statement that caused the error
resum pla     ;discard ERROR token
 pla          ;pull line number from stack and make current
 sta $39
 pla 
 sta $3a
 pla          ;pull text ptr from stack and make current
 sta $7a
 pla 
 sta $7b
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
 lda #0
 sta $10      ;SUBFLG Subscript Reference to an Array or User-Defined Function Call (FN)
 jsr entrap   ;enable error trapping
 jmp (IGONE)  ;read and execute the next statement
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
 ldx #12      ;RETURN WITHOUT GOSUB
 jmp (IERROR)
oldrtn jmp RETURN+2
;
;*******************
; MOVE sprite#, x1, y1 [TO x2, y2, speed]
; MOVE sprite# TO x2, y2, [speed]
move
 jsr sprnum    ;get sprite# and 2^sprite# ($bf)
 tya           ;sprite number 0-7
 asl           ;convert to 2-byte index for registers
 sta $0f       ;sprite# * 2
 jsr CHRGOT
 cmp #TOKEN_TO
 bne getfrom
;get current x and y coordiates for move starting point
 ldy $0f       ;sprite# * 2
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
 jsr skp73
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
 ldy $0f       ;sprite# * 2
 lda $14
 sta SP0X,y    ;sprite x coord
 sta $fb
gety
 jsr chkcomm
 jsr skp73     ;y2 coord
 bne badxy     ;hibyte must be 0
 txa
 ldy $0f       ;sprite# * 2
 sta SP0Y,y    ;sprite y coord
 sta $fd
 jsr CHRGOT
 cmp #TOKEN_TO
 beq moveto    ;TO token not present so we are done
 rts
moveto
 jsr backuppoint ;save last plot used by graphics commands
 lda $bf       ;temp var holding 2^sprite# value
 sta $07       ;temp var for moving sprite on a line
 jsr dbyval    ;get x2 coordinate
 cpy #2        ;must be between 0 and 511
 bcs badxy     ;illegal coordinate error
 stx lastplotx
 sty lastplotx+1
 jsr ckcom2    ;check for and skip over comma, misop err if missing
 jsr skp73     ;get y2 coordinate
 bne badxy     ;hibyte must be 0
 stx lastploty
;getspeed
 lda #20
 sta $fe       ;default speed is 20
 jsr comchk
 bne nosped    ;no move speed specified?
 jsr getval    ;get the speed param 0-255
 sta $fe       ;temp storage for move speed
nosped lda #$01
 sta moveflag  ;flag to tell LINE cmd to move a sprite instead of plot line
 jsr strtln    ;calculate line and move sprite along that line at given speed
 dec moveflag  ;reset flag back to 0 for LINE cmd
 jmp restorepoint
badxy jmp hellno
;
spriteon
 dey            ;0 dec to 255 for all sprites
spriteoff
 sty SPENA
 plp
 jmp CHRGET
;
;*******************
;SPRITE ON | OFF  - turns on or off all 8 sprites
;SPRITE [sprite# 0-7], [0=on,1=off], [color 0-16], [0=normal,1=multicolor],
;       [data pointer 0-255], [foreground priority 0=over,1=under]
sprite
 php
 ldy #0
 cmp #TOKEN_OFF
 beq spriteoff
 cmp #TOKEN_ON
 beq spriteon
 plp
 jsr sprnum     ;sprite# returned in $be and 2^sprite# in $bf
 jsr ckcom2     ;check for and skip over comma, misop err if missing
 cmp #","       ;get next char and compare to comma
 beq scr        ;another comma so skip param
 jsr getbool2   ;sprite visible 0=off, 1=on
 lda $14        ;visible param
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
 jsr getval15   ;color 0-15
 ldy $be        ;sprite# 0-7
 sta SP0COL,y   ;sprite y's color
smcr jsr chkcomm ;stop now if no more params
 beq spntr
 jsr getbool2   ;get multicolor flag 0 or 1
 bne setm
 lda $bf        ;2^sprite#
 eor #$ff       ;sprite# bit off
 and SPMC       ;sprite multicolor flags
 jmp skipmc
setm lda SPMC
 ora $bf        ;2^sprite#
skipmc sta SPMC
 jsr chkcomm
spntr cmp #","
 beq prorty
 jsr skip73     ;get sprite data ptr 0-255 (ptr*64)=start address

;determine VIC-II base addr
 jsr ptrhi      ;get hibyte of sprite ptr start address
 sta $62        ;sprite pointers are in the last 8 bytes of 1K screen RAM
 lda #$f8       ;lobyte of offset to first byte of sprite data ptrs
 sta $61        ;ptr to first sprite data ptr, ie bank 0 with 1K offset is $07F8

;apply ptr param
 lda $14        ;sprite ptr from cmd param
 ldy $be        ;sprite# 0-7
 sta ($61),y    ;sprite y's data ptr

prorty
 jsr chkcomm    ;check for comma, if end of statement then do not return here
 jsr getbool2   ;get sprite to foreground graphics/text priority: 0=over, 1=under
 bne okpri
 lda $bf        ;2^sprite#
 eor #$ff       ;prepare to turn off bit for sprite
 and SPBGPR     ;turn off bit for sprite
 sta SPBGPR     ;apply new value
 rts
okpri lda $bf   ;2^sprite#
 ora SPBGPR     ;turn on bit for sprite
 sta SPBGPR     ;apply new value
 rts
;
xpon dey        ;0 dec to 255 for all sprites
xpoff
 sty XXPAND
 sty YXPAND
 plp
 jmp CHRGET
;
;*******************
; EXPAND ON | OFF          ;expand all sprites either ON or OFF
; EXPAND sprite#           :expand both x and y axis of sprite#
; EXPAND sprite#, [x], [y] :where x and y are 0=expand off, 1=expand on
expand
 php
 ldy #0
 cmp #TOKEN_OFF
 beq xpoff
 cmp #TOKEN_ON
 beq xpon
 plp
 jsr sprnum     ;get sprite# and store in $be and 2^sprite# in $bf
 jsr CHRGOT
 bne getexpxy
 lda XXPAND
 ora $bf        ;2^sprite#
 sta XXPAND
 lda YXPAND     ;y expand
 ora $bf        ;2^sprite#
 sta YXPAND
 rts
getexpxy
 jsr CHRGET
 cmp #","
 beq magy
 jsr getbool2   ;expand x param
 beq expx
 lda XXPAND
 ora $bf        ;2^sprite#
 bne setmagx    ;always branches
expx lda $bf    ;2^sprite#
 eor #$ff
 and XXPAND     ;x expand off
setmagx
 sta XXPAND     ;x expand on
magy
 jsr chkcomm
 jsr getbool2   ;expand y param
 beq clry
 lda YXPAND     ;y expand
 ora $bf        ;2^sprite#
 bne setmagy    ;always branches
clry lda $bf    ;2^sprite#
 eor #$ff       ;prepare to turn off target bit
 and YXPAND
setmagy sta YXPAND
 rts
;
;*******************
;MULTI [TEXT] [cc1], [cc2]         - multicolor text mode
;MULTI COLOR  [eb1], [eb2], [eb3]  - extended background color mode
;MULTI SPRITE [sc1], [sc2]         - multicolor sprite mode color bit patterns 01,11
multi
 cmp #TOKEN_SPRITE
 beq mcspri
 cmp #TOKEN_COLOR
 bne chrmap
;MULTI COLOR [eb1], [eb2], [eb3]
 lda SCROLY        ;horiz fine scrolling and control reg
 ora #%01000000    ;turn on bit 6 - enable extended background color mode for text
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
;*******************
; LOCATE [col], [row], [blink] - param values can be omitted to use current value
locate
 beq mop6
 pha
 sec         ;flag for read
 jsr PLOT    ;read current position
 stx $bb     ;x (col)
 sty $bc     ;y (row)
 pla
 cmp #","
 beq row
 jsr skp73   ;get value as int: x=lobyte, y=hibyte
 bne badloc  ;hibyte must be zero
 cpx #40     ;>=40?
 bcs badloc
 stx $bc
 jsr CHRGOT
 beq column  ;end of statement
row jsr CHRGET
 cmp #","
 beq column
 jsr skp73   ;get value as int: ;x=lobyte, y=hibyte
 bne badloc  ;hibyte must be zero
 cpx #25     ;25 is max line number
 bcs badloc
 stx $bb
column
 ldx $bb
 ldy $bc     ;y holds the line (from temp storage area)
 clc         ;clear carry is flag to write new value
 jsr PLOT    ;read/set cursor position on screen
 jsr chkcomm ;if current char is a comma then continue otherwise quit now
 jsr getbool2
 eor #1      ;flip value so that 1=on 0=off
 sta 204     ;Flash Cursor 0=Flash Cursor, non-zero No Cursor
 rts
;
;*****************
mop6 jmp missop    ;missing operand error
badloc jmp hellno  ;illegal coordinate error
;
;*******************
; COLOR [foregndColor (0-31)], [backgndColor (0-15)], [borderColor (0-15)]
color
 beq mop6
 cmp #","
 beq nochar
 jsr skip73
 sta COLOR      ;current cursor foreground color
nochar jsr chkcomm ;if no more params then stop now, otherwise get next char
 cmp #","       ;another comma?
 beq noback
 jsr getval15
 sta BGCOL0     ;background color
noback jsr chkcomm
 jsr getval15
 sta EXTCOL     ;border color
 rts
;
;*******************
designon
 lda CI2PRA     ;bits 0-1 mem bank, 00=bank3, 01=bank2, 10=bank1, 11=bank0
 and #%11111100 ;select VIC-II 16K mem bank 3 ($C000-$FFFF)
 sta CI2PRA     ;base address is now $C000
 lda #%00101100 ;video matrix offset %0010 (2*1K) = $0800; char dot data offset at %110 (6*1K) = $1800
 sta VMCSB      ;bit 0 unused; bits 1-3 char dot data base addr; bits 4-7 video matrix base addr
 lda #$c8       ;video matrix is at $c800
 sta HIBASE     ;let Kernal know video matrix is at $c800 so printed chars will be visible
 lda SCROLX
 and #%11101111 ;bit 4 off disable multicolor text/bitmap mode
 sta SCROLX
 lda SCROLY
 and #%11011111 ;turn off bitmap mode
 sta SCROLY
 jmp CHRGET
;*****************
designoff jsr norm
 jmp CHRGET
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
 lda #$f0    ;target location $F000-$FFFF
 sta $bc
 lda #$00
 sta $bb
 sta $be
 lda #$d0    ;source location 4K CHAREN at $D000-$DFFF
 sta $bf
 lda R6510
 pha
 and #%11111011 ;bit2=0 switch out I/O and bring in CHAREN ROM into bank $d000-$dfff
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
dodesign
 jsr skip73    ;get screen code
 jsr times8    ;multiply A reg value times 8
 jsr ckcom2    ;check for and skip over comma, misop err if missing
 jsr getbool2  ;get charset
 beq charset0
 lda #$f8      ;charset 1 at $f800
.byte $2c      ;defeat lda #$f0 by making it bit $f0a9
charset0
 lda #$f0      ;charset 0 at $f000
 clc
 adc $bf
 sta $bf
 ldy #$00      ;loop for all 8 bytes of data
gtdata
 sty $02
 jsr ckcom2    ;check for and skip over comma, misop err if missing
 jsr skip73
 ldy $02
 sta ($be),y
 iny
 cpy #8
 bne gtdata
 rts
;
;*******************
;
bitmapclr
 lda #$e0  ;bitmap located at $e000
 sta $63
 lda #$00
 sta $62
 ldy #0
clrbyt sta ($62),y
 iny
 bne clrbyt
 inc $63
 bne clrbyt
 rts
;
;BITMAP CLR (does not switch to bitmap mode)
;BITMAP FILL x1,y1 TO x2,y2, [plotType], [color]
;BITMAP OFF
;BITMAP [colorMode], [bkgndColor]
;colorMode 0=hires, 1=multicolor (mc)
;bkgndColor is applied based on colorMode:
;hires mode sets color RAM in video matrix with initialization of all 1000 bytes
;mc mode sets the single bkgrnd color register BGCOL0
;MAPCOL c1,c2,c3 (c3 mc mode only) to change colors:
;hires c1 (0-15) dot color, c2 (0-15) 8x8 square bkgnd color, c3 not used but can be set
;multicolor uses dual plotted bit pattern to select the color:
; BGCOL0 (0-15) color for bit pattern 00
;     c1 (0-15) color for bit pattern 01
;     c2 (0-15) color for bit pattern 10
;     c3 (0-15) color for bit pattern 11
;NOTE when in mc mode graphics cmds use color index 1-3 (not color value 0-15)
;as a color parameter to select the color from mc pallete
;
bitmap
 beq mop7
 cmp #TOKEN_CLR
 beq bmclr
 cmp #TOKEN_OFF
 beq bmoff
 cmp #TOKEN_ON
 beq bmon
 cmp #TOKEN_FILL
 bne bitscr
; BITMAP FILL x1,y1 TO x2, y2, plotType, color
 jsr CHRGET
 jsr getpnt
 jsr point2
 jsr types
 dec R6510
 jmp bitfil         ;perform FILL on rect; put code under ROM
;
mop7 jmp missop
;
bmon jsr bitmapon
 bne bmclr+3        ;always branches
bmoff jsr norm
 bne bmclr+3        ;always branches
bmclr jsr bitmapclr
 jmp CHRGET
bitscr jsr getbool2 ;colorMode 0 or 1
 beq hiresmode
 lda SCROLX         ;horiz fine scrolling and control reg
 ora #%00010000     ;turn on bit 4 - enable multi color text or bitmap mode
 sta SCROLX
 jsr comchk
 bne bitmapon       ;no more params so turn bitmap mode on
 jsr getval15_
 sta BGCOL0
bitmapon
 lda C2DDRA         ;data direction for port A (CI2PRA)
 ora #%00000011     ;bits 0-1 are set to 1=output (default)
 sta C2DDRA
 
 lda #%11000100     ;bits 0-1 VIC-II 16K memory bank 00=bank3 (49152-65535, $C000-$FFFF)
 sta CI2PRA         ;send bits out data port register

 lda SCROLY         ;turn on bitmap graphics mode
 ora #%00100000     ;bit#5 1=on, 0=off
 sta SCROLY         ;apply setting to control register
 lda #%00101100     ;bit 0 unused
                    ;bits 1-3 text dot-data base offset=  110=6 ->6K offset from base $c000+6K=$d800
                    ;bits 4-7 video matrix base offset = 0010=2 ->2K offset from base $c000+2K=$c800
 sta VMCSB          ;apply setting to control register
 rts
hiresmode
 lda SCROLX         ;turn off mulicolor mode
 and #%11101111     ;set bit#4 0=off, 1=on
 sta SCROLX         ;apply setting to control register
 jsr comchk
 bne bitmapon
 jsr CHRGET
 jsr getc2          ;background color for all 8x8 squares
 ldy #0
pokcol sta $c800,y  ;fill color mem for entire screen
 sta $c900,y
 sta $ca00,y        ;1000 bytes, not 1024 bytes
 sta $cb00-24,y     ;this overlaps to prevent clearing sprite ptrs
 iny
 bne pokcol
 beq bitmapon       ;will always branch
;
;*******************
;
;MAPCOL changes the default colors to be used when plotting dots on a bitmap
;In hires mode (c1,c2):
; c1 plot color (0-15) of dot in same 8x8 square (upper 4-bits scan code in Video Matrix)
; c2 background color (0-15) of dot in 8x8 square (lower 4-bits in Color RAM)
;In multi color mode (c1,c2,c3):
;    00 Background Color Register 0 BGCOL0 ($D021)
; c1 01 Upper nibble of Video Matrix (scan code)
; c2 10 Lower nibble of Video Matrix (scan code)
; c3 11 Lower nibble of Color RAM for Video Matrix ($D800-$DBE8)
;
;hires mapcol 0,1   dot color black, background (of 8x8 square of dot) white
;multi mapcol 0,1,2 bit patterns 01=black, 10=white, 11=red (00 is the background color in BGCOL0)
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
 and #%00001111  ;erase hi nibble
 sta $02         ;tmp storage
 jsr getval15    ;c1 (0-15) changes the color of the plotting dots
 asl             ;move low nibble to high nibble
 asl
 asl
 asl
 ora $02         ;apply new value to high nibble while keeping original low nibble value
 sta mapcolc1c2  ;replace global variable storage for c1 (plot color)
 rts
getc2
 lda mapcolc1c2  ;again, get global variable storage but for low nibble this time
 and #%11110000  ;erase lo nibble
 sta $02         ;temp var for final byte value calculation
 jsr getval15    ;c2 (0-15) changes the background of the 8 x 8 square
 ora $02
 sta mapcolc1c2  ;update global variable for colors
 rts
;
;*******************
; PULSE voc#(1-3), width%(0-100)
pulse jsr ppw
 pha          ;save voice SID register offset (voice#-1)*7
 jsr ckcom2   ;check for and skip over comma, misop err if missing
 jsr FRMNUM   ;get width% (0.00 to 100.00)
 lda #<m4095  ;REGVAL=ROUND(40.95*WIDTH%) result in range 0-4095
 ldy #>m4095  ;y=hi byte, a=lo byte pointer to 5-byte FAC value
 jsr FMULT    ;multiply FAC1 by a value in memory ptr A=lo byte, Y=hi byte
 jsr doround  ;round FAC1 to nearest whole number
 jsr GETADR   ;convert FAC1 to 2-byte integer in $14,$15
 pla
 tay          ;register offset for voice
 lda $15
 cmp #$10     ;0-4095 only (12-bit value)
 bcs badwav
 sta PWHI1,y  ;Pulse Waveform Width (hi nibble)
 lda $14
 sta PWLO1,y  ;Pulse Waveform Width (lo byte)
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
badwav jmp FCERR
wave jsr ppw   ;get voice SID register offset (voice#-1)*7
 sta $bb       ;save SID register offset
 jsr ckcom2    ;check for and skip over comma, misop err if missing
 jsr skip73    ;get waveform single byte operand value into $14
 sta $02       ;get waveform value selection
 cmp #9        ;first bad value
 bcs badwav    ;greater or equal to first bad value
 asl           ;multiply waveform value * 16 to target bits 4-7
 asl
 asl
 asl
 sta $02       ;waveform
 jsr comchk
 bne waveit
 jsr getbool
 ora $02       ;position is bit 0
 sta $02
 jsr comchk
 bne waveit
 jsr getbool
 asl           ;position is bit 1
 ora $02
 sta $02
 jsr comchk
 bne waveit
 jsr getbool
 asl           ;position is bit 2
 asl
 ora $02
 sta $02
 jsr comchk
 bne waveit
 jsr getbool
 asl           ;position is bit 3
 asl
 asl
 ora $02
 sta $02
waveit
 ldx $bb
 lda $02
 sta VCREG1,x ;select waveform and start release cycle if attack/decay/sustain is already started
 rts
;
;subroutine to clear SID
sidclr
 ldy #$18        ;clear all 24 SID registers
 lda #0
clrsid sta FRELO1,y
 dey
 bpl clrsid
 sta md417       ;mock register for $d417 Filter Resonance Control Register
 sta md418       ;mock register for $d418 Volume and Filter Select Register
 rts
;*******************
;NTSC and PAL hold the value of 1Hz (based on clock speed)
;REG_VAL=FREQUENCY/NTSC
;VOICE CLR
;VOICE voice#, frequency(0 to 3995 for NTSC machines)
voice
 php
 cmp #TOKEN_CLR  ;clr token?
 bne getfreq
 plp
 jsr sidclr
 jmp CHRGET
getfreq
 plp
 jsr ppw         ;returns SID register offset (voice#-1)*7 in accumulator
 pha
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
 pla
 tay
 lda $15
 sta FREHI1,y
 lda $14
 sta FRELO1,y    ;store result in data control reg for voice
 rts
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
 beq draw-1
;save current txt ptr
 lda $7a
 pha
 lda $7b
 pha
;set txt ptr to string start
 lda $50
 sta $7a
 lda $51
 sta $7b
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
 jsr getval
 cmp #4         ;0-3 only
 bcs baddraw
 sta lastplott
 jmp nxtmov
godraw
 pha
 jsr dbyval
 pla
;
 dec R6510
 jsr godraww
 inc R6510
 bcs nxtmov    ;draw cmd was valid
baddraw jmp FCERR  ;syntax error in draw string
nxtmov
 jsr comchk    ;another draw cmd?
 beq drawloop2
 pla
 sta $7b
 pla
 sta $7a
 jmp savepoint ;save final x and y coordinates plotted
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
;*******************
; LINE INPUT ["prompt",] A$
; LINE INPUT# filenum, A$
; LINE x1,y1 TO x2, y2, plotType, color
line
 beq mop3
 cmp #TOKEN_INPUTN ;input# token (line input#)
 bcc liner         ;line x,y to a,z
 jsr ERRDIR        ;throw error if direct mode; only x reg is affected
 cmp #TOKEN_INPUT  ;input token (line input)
 beq getprompt     ;perform line input prompt$, var$
 bcc lineinput     ;perfrom lineinput# num%, var$
badinp jmp SNERR
lineinput
 jsr getval   ;get single byte param in a reg, misop err if missing
 tax
 jsr CHKIN    ;redirect std input to file handle stored in x reg 
 jsr readline
 jmp $abb5    ;final part of input# to restore i/o channel
getprompt
 jsr peekop
 cmp #"""
 bne readline
 jsr getstr   ;string is returned in registers y=hi byte, x=lo byte, a=length
 txa          ;x reg has low byte of str ptr but next func needs it in a reg 
 jsr STROUT   ;print str whose addr is in y reg (hi byte) and a reg (lo byte)
 jsr comchk
 bne badinp
readline
 jsr INLIN    ;input a line to buffer from keyboard (80 chars max from keyboard)
 ldy #0       ;count number of characters input (not sure if routine returns it)
fndend lda BUF,y
 beq inpend
 iny
 bne fndend
inpend tya    ;y reg = string length
 jsr GETSPA   ;alloc space in mem for string returning address ptr in $35,$36
 pha          ;save number of bytes allocated on stack
 tay
 jsr CHRGET
 dey
copyer lda BUF,y ;copy string to variable storage
 sta ($35),y
 dey
 bpl copyer
 jsr PTRGET   ;search for a var & setup if not found
 sta $49      ;variable address is returned in a (lo byte) and y (hi byte) registers
 sty $4a      ;every string variable is a pointer consisting of has 3 bytes, 2 for ptr, 1 for length
 pla          ;recall from stack the number of bytes that were allocated in mem
 ldy #0
 sta ($49),y  ;save it to the variable's string length byte
 iny
 lda $35      ;get lo byte of str ptr
 sta ($49),y  ;save it to variable's str ptr info
 iny
 lda $36      ;get hi byte of str ptr
 sta ($49),y  ;save it to variable's str ptr info
 jsr comchk
 beq readline
 rts
;
mop3 jmp missop
;
; LINE x1,y1 TO x2, y2, plotType, color
liner jsr getpnt
 jsr point2
 jsr types
;entry point for MOVE sprite command
strtln
 dec R6510
 jsr linedraw
 inc R6510
;fall through savepoint subroutine
savepoint
 ldx #2
 lda $fb,x
 sta lastplotx,x
 dex
 bpl savepoint+2
 rts
getpoint
 ldx #2
 lda lastplotx,x
 sta $fb,x
 dex
 bpl getpoint+2
 rts
swappoint
 ldx #2
 lda $fb,x
 pha
 lda lastplotx,x
 sta $fb,x
 pla
 sta lastplotx,x
 dex
 bpl swappoint+2
 rts
backuppoint    ;save plot coordinate
 ldy #2
 lda lastplotx,y
 sta lastplotx2,y
 dey
 bpl backuppoint+2
 rts
restorepoint  ;restore last plot coordinate
 ldy #2
 lda lastplotx2,y
 sta lastplotx,y
 dey
 bpl restorepoint+2
 rts
;
point2
 jsr savepoint ;copy point 1 to last plot point acting as point 2
 lda #TOKEN_TO
 jsr CHKCOM+2  ;skip over TO token, syntax error if not there
 jsr pntweg    ;get the coordinates
 jmp swappoint ;swap point1 and point2
;get x,y coordinates
pntweg beq mop3
 jsr skp73     ;get x coordinate, returns lobyte in x, hibyte in y
 jsr xytim2
 tya
 beq okvalu
 cpy #2     ;valid value is 0 or 1 for x coord hibyte
 bcs hellno
 cpx #$40   ;valid values 0 to 63
 bcs hellno
okvalu stx $fb
 sty $fc
 jsr ckcom2  ;check for and skip over comma, misop err if missing
 jsr skp73   ;x=lobyte, y=hibyte
 bne hellno  ;y reg holds hibyte for y coordinate and must be zero
 cpx #200    ;x reg holds lobyte for y coordinate and must be between 0 and 199
 bcs hellno
 stx $fd     ;y coordinate
 rts
xytim2 
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 beq theend ;hires mode
 txa  ;adjust offset to read ptab2/ptab3 for multicolor plot bits
 asl  ;multiply x coordinate by 2
 tax
 tya
 rol
 tay
theend rts
hellno ldx #34 ;illegal coordinate error
 jmp (IERROR)
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
; options are represented in 8 bits grouped by nibbles:
; bits0-3: quadrant visible 0=no,1=yes, bits4-7: radius line visible 0=no,1=yes
circle
 jsr getpnt     ;center point x,y
 jsr ckcom2     ;check for and skip over comma, misop err if missing
 jsr skip73     ;x radius size
 bmi illqty8    ;max x radius 127
 sta $35
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 sta $29        ;0=hires mode, otherwise multicolor mode
 beq hirescir
 asl $35        ;mc mode needs 2x size
 bmi illqty8    ;mc mode limit is 63
hirescir
 jsr ckcom2     ;check for and skip over comma, misop err if missing
 jsr skip73     ;y radius size
 bmi illqty8    ;max y radius 127
 sta $36
 lda #%00001111 ;default options
 sta $2a        ;variable to hold value
 jsr CHRGOT
 beq docircle
 jsr CHRGET     ;position for options param
 cmp #","
 beq circlept   ;skip if comma found
 jsr skip73     ;get options value
 sta $2a        ;circle options
circlept
 jsr types      ;get optional plot type and color; use last used values if not supplied
docircle
 lda $35
 ora $36
 beq endcir     ;x and y radius size are both zero
 cmp #2         ;smallest valid x,y radius size is 2,1 or 1,2
 bcs okcirc     ;plot the circle
 jmp plotit     ;just plot a dot
okcirc
 dec R6510        ;switch LOROM to LORAM
 jsr circel
 jsr ciropts
 inc R6510
endcir rts
;
;*******************
norm
 lda C2DDRA      ;data direction for port A (CI2PRA)
 ora #%00000011  ;bits 0-1 are set to 1=output (default)
 sta C2DDRA
 lda CI2PRA
 ora #%00000011  ;select VIC-II 16K mem bank 0 ($0000-$4000)
 sta CI2PRA
 lda #$04        ;text page for kernal prints
 sta HIBASE      ;top page of screen mem
 lda #%00010101  ;bit0 is always 1; bits1-3 text chr dot data base address in 1K chunks
 sta VMCSB       ;bits 4-7 video matrix base address in 1K chunks
 lda #%11001000  ;display on, multicolor text/bitmap mode off
 sta SCROLX      ;40 columns, 0 horizontal scroll scan lines
 lda #%00011011  ;extended color text mode off, bitmap mode off
 sta SCROLY      ;display on, 25 rows, 3 vertical scroll scan lines
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
 jsr getval   ;charset 0-3
 cmp #4       ;0,1 uppercase 2,3 lower case
 bcs illqty8
 asl          ;calc hibyte offset
 asl          ;0=0, 1=4, 2=8, 3=12
;clc not needed here since zero was shifted into carry by asl
 adc $26
 sta $26      ;charset 0=$d000,1=$d400,2=$d800,3=$dc00
;sizes
 jsr comchk
 bne ne 
 jsr getval
 cmp #32      ;max size is 31
 bcs illqty8
 sta $57      ;user specified x size
 jsr comchk
 bne ne
 jsr getval
 cmp #32      ;max y size is 31
 bcs illqty8
 sta $58      ;user specified y size
 jsr comchk
 bne ne
 jsr types
ne dec R6510
 jmp texter
;
screenoff
 lda SCROLY
 and #%11101111  ;bit4 = 1 screen off
 jmp setscrly
screenon
 lda SCROLY
 ora #%00010000  ;bit4 = 0 screen on
setscrly
 sta SCROLY
 rts
;
;*******************
; SCREEN CLR
; SCREEN ON|OFF
; SCREEN cols, rows where cols=38|40, rows=24|25
screen
 beq mop
 cmp #TOKEN_ON
 bcc setscr
 bne scnoff
 jsr screenon
 jmp CHRGET
scnoff cmp #TOKEN_OFF
 bne clrscrn
 jsr screenoff
 jmp CHRGET
clrscrn cmp #TOKEN_CLR
 bne setscr
 lda #147        ;clear screen
 jsr CHROUT
 jmp CHRGET
setscr cmp #","
 beq cklast
 jsr skip73      ;get num cols
 cmp #40         ;38 or 40 only
 bne chk38
 lda SCROLX
 ora #%00001000  ;bit 3 on 40 columns
 sta SCROLX
 jmp cklast
chk38 cmp #38
 bne illqty3
 lda SCROLX
 and #%11110111  ;bit 3 off 38 columns
 sta SCROLX
cklast jsr chkcomm
 jsr skip73      ;get num rows
 cmp #25         ;24 or 25 only
 bne chk24
 lda SCROLY
 ora #%00001000  ;bit 3 on 25 rows
 sta SCROLY
 rts
chk24 cmp #24
 bne illqty3
 lda SCROLY
 and #%11110111  ;bit 3 off 24 rows
 sta SCROLY
 rts
;
mop jmp missop
;
;*******************
; KEY LIST      -list current function key assignments
; KEY OFF       -turn off key trapping (enabled by ON KEY)
; KEY CLR       -clear the keyboard buffer
; KEY WAIT [A$] -wait for any keypress, optionally store in str var
; KEY n, "string"  where n = (1-8)
key
 beq mop
 cmp #TOKEN_CLR
 beq keyclr
 cmp #TOKEN_WAIT
 beq keywait
 cmp #TOKEN_OFF
 beq onkeyoff
 cmp #TOKEN_LIST
 beq keylist
 jsr FRMEVL   ;eval expression
 lda $0d      ;number or string?
 beq keynum   ;number is key assign
 jsr getstr2  ;get source string as first param
 beq doget-1
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
 inc keystrflg  ;flag for manual key string
 jmp keyirqon
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
okkey sta $02     ;save key#
 jsr ckcom2       ;check for and skip over comma, misop err if missing
 jsr getstr0
 dec R6510
 jmp keyy
onkeyoff lda #0
 sta keyflag
 sta keyentry
 jmp CHRGET
keywait
 lda NDX
 beq keywait
 jsr CHRGET
 bne doget
 rts
doget
 jmp GET          ;peform GET
keyclr
 lda #0
 sta NDX
 jmp CHRGET
keylist dec R6510
 jmp keylistt
;
;*******************
; SCROLL x1, y1 TO x2, y2, [direction 0-3], [wrap 0-1]
scroll
 jsr getcoords
 lda #0
 sta $ff         ;default direction 0=up
 sta $02         ;default wrap 0=no wrap
 jsr comchk
 bne okscroll
 jsr getval      ;direction 0-3
 cmp #4
 bcs illqty3
 sta $ff
 jsr comchk
 bne okscroll
 jsr getbool     ;wrap: 0=no, 1=yes
 sta $02         ;wrap param temp var
okscroll
 lda $ff         ;direction temp var
 asl             ;convert to 2 byte offset
 tay
 dec R6510         ;enable LORAM
 lda scrolls+1,y ;scroll direction vector hibyte
 pha
 lda scrolls,y   ;scroll direction vector lobyte
 pha
 rts
;-----------
badscroll jmp hellno ;illegal coordinate error
getcoords
 jsr skp73_      ;x1
 bne badscroll   ;hibyte should be 0
 cpx #40         ;max columns
 bcs badscroll
 stx $fb
 stx $be
 jsr ckcom2      ;check for and skip over comma, misop err if missing
 jsr skp73       ;y1
 bne badscroll   ;hibyte must be 0
 cpx #25         ;max rows
 bcs badscroll
 stx $bf
 dec R6510
 jsr calcptr
 inc R6510
 lda #TOKEN_TO   ;token to skip over
 jsr CHKCOM+2    ;check for and skip over TO token, syntax error if not found
 jsr skp73_      ;x2
 bne badscroll   ;hibyte must be 0
 cpx #40
 bcs badscroll
 txa
 sec
 sbc $be
 sta $be
 jsr ckcom2      ;check for and skip over comma, misop err if missing
 jsr skp73       ;y2
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
adsr jsr ppw
 sta $bb       ;SID register offset for voice
 jsr ckcom2    ;check for and skip over comma, misop err if missing
 jsr getval15  ;attack
 asl
 asl
 asl
 asl
 sta $bc       ;attack duration
 jsr getval15_ ;decay duration
 ora $bc
 ldx $bb
 sta ATDCY1,x  ;apply attack and decay
;sustain and release params
 jsr chkcomm   ;if end of statement quit now
 jsr getval15  ;sustain volume
 asl
 asl
 asl
 asl
 sta $bc
 jsr getval15_ ;release duration
 ora $bc
 ldx $bb
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
 jmp iverr      ;illegal voice number
filter
 beq mop2
 cmp #TOKEN_VOICE
 bne getfreq1
;FILTER VOICE voice#, boolean
 jsr getval     ;get voice 1-4
 beq novoice
 cmp #5         ;4=external input via pin 5 of audio/video port
 bcs novoice
 tay            ;voice 1 to 4
 dey            ;index 0 to 3
 lda bitweights,y ;bit pattern for RESON register
 sta $02        ;remember for later
 jsr comchk     ;check if they supplied a boolean?
 bne filteron   ;missing boolean assumes on, syntax FILTER VOICE voice#
 jsr getbool    ;get on/off expression, 0=off, 1=on
 bne filteron
 eor $02        ;flip all bits to turn off voice# bit
 and md417      ;all other bits will remain as they were
 jmp setfilter
filteron
 lda $02
 ora md417      ;affect only bit for voice
setfilter
 sta md417      ;remember this new setting for this register
 sta RESON      ;make setting active in SID register
 rts
; FILTER cutoff, [resonance], [type]
getfreq1
 cmp #","
 beq reson
 jsr FRMNUM     ;convert current expression to a number and store in FAC1
 jsr MOVEF      ;copy FAC1 to FAC2 (numerator) frequency value 
 lda #<five8
 ldy #>five8
 jsr MOVFM      ;copy mem to FAC1 pointed by a & y
 jsr FDIVT      ;fac1 = (FAC2/FAC1)
 jsr doround    ;round FAC1 to nearest whole number
 jsr GETADR     ;convert FAC1 to unsigned 16-bit int
;now subtract 30/5.8 = 5.172...or just 5 (close enough)
 lda $14
 sec
 sbc #5
 sta $14
 lda $15
 sbc #0
 bmi illqty5    ;if bit 7 in hibyte set then we rolled over from 0
 sta $15
;range check register value max from 0-2047 allowed
 cmp #8           ;2048 or larger is above SID's range
 bcc okfilt
illqty5 jmp FCERR ;illegal qty err
mop2 jmp missop
okfilt
;11-bit value stored in a wacky way in filter registers
;   $15       $14     Source binary number
; *****111  11111111  Possible bits used, *=not used since out of range
;  $d416     $d415    Target Filter frequency registers
; 11111111  XXXXX111  Target bits to map from bits above, X=not used
;put bits 0-2 from $14 into $D415
;shift hibyte left 5 times to promote lower 3 bits to highest 3 bits, fill with 0
;store lobyte in SID lower byte register (bits 3-7 will be ignored by SID)
;shift lobyte right 3 times to demote upper 5 bits
;merge upper and lower bits as hibyte and store in SID upper byte register
 asl
 asl
 asl
 asl
 asl
 sta $02
 lda $14
 sta CUTLO  ;since upper 5 bits are not used by SID, no need to set them 0
 lsr
 lsr
 lsr
 ora $02    ;merge upper and lower bits to make a full byte for SID upper byte
 sta CUTHI
;
;now get resonance parameter if present
;the filter resonance control register $d417 
;Bits 4-7  Set filter resonance 0-15 0=none, 15=max
;
reson
 jsr chkcomm  ;if no more params then quit otherwise continue
 jsr getval15  ;get resonance param
 asl          ;resonance is stored in the upper nibble
 asl          ;so this value must be shifted left
 asl
 asl
 ora md417    ;include voice number in lower nibble
 sta md417    ;store for future read (cannot read SID registers, only write)
 sta RESON    ;SID's Output Filter Resonance Control Register
;get the type 0-4 if present, stop otherwise
;Store in SID Register $d418 bits 4-6
;Bits 0-3 Select output volume (0-15)
;Bit4 Select low-pass filter, 1=low-pass on, 0=off
;Bit5 Select band-pass filter, 1=band-pass on, 0=off
;Bit6 Select high-pass filter, 1=high-pass on, 0=off
;Bit7 Disconnect output of voice 3, 1=disconnect, 0=connect
;since SID registers can only be written to, an alternate var will hold on to it
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
 jsr skip73   ;filter type 0-4
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
 sta $02      ;temp storage
 lda md418    ;current value using SID mock register
 and #%10001111 ;clear bits 4-6, keep the rest
 ora $02      ;apply new value
 jmp setvol
;
;*******************
; PLAY S$
; PLAY STOP
; PLAY SPRITE sprite#, startPtr, endPtr, speed
; PLAY SPRITE [sprite#] OFF
play
 beq mop2
 cmp #TOKEN_STOP
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
 jsr getval     ;startPtr
 ldx $be        ;sprite index 0-7
 sta ptrbegin,x
 tay
 jsr ptrhi
 sta ptr3ref+2
 sta ptr2ref+2
 sta ptr1ref+2
 tya
ptr3ref sta $07f8,x
 jsr getval     ;endPtr
 ldx $be
 sta ptrend,x
 jsr getval     ;jiffies between images
 ldx $be
 sta aniwait,x
 sta anidly,x
;turn sprite animation on for sprite specified
 lda $bf        ;sprite bit weight
 ora aniopt
 sta aniopt
 jmp anion      ;turn on sprite animation and ensure MDB IRQ is on
;
playyy
 jsr getstr0
 beq play-1     ;quit now if string is empty
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
 jmp playirqon  ;play in background
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
 jsr playirqoff
 ldx playvoice  ;SID reg offset
 lda playwave   ;select current waveform; start release cycle
 sta VCREG1,x   ;start decay cycle
 sec
 rts

initvoice
 ldx #30        ;default note length to 30 jiffies (approx 1/60 sec.)
 stx playlen    ;note duration 30/60=0.50 sec.

 ldx #4         ;default octave 4
 stx playoct

;init SID registers for selected voice
 tax
inivoc
 stx playvoice  ;voice register offset

 lda #%01000000 ;select default waveform to pulse; start decay cycle
 sta playwave
 sta VCREG1,x

 lda #0
 sta FRELO1,x
 sta FREHI1,x
 sta PWLO1,x    ;in case user select pulse waveform
 lda #$08       ;set the pulse duty cycle to 50% (12-bit reg val 0-4095, 25%=2048)
 sta PWHI1,x    ;lower nibble only, upper nibble is unused (12-bit value)

 lda #$10       ;set attack duraction to 8ms (hi nibble)
 sta ATDCY1,x   ;and decay duration to 6ms (lo nibble)
 lda #$F8       ;set sustain volume to 15 (hi nibble)
 sta SUREL1,x   ;and release duration to 300ms (lo nibble)

 lda md418      ;all voices volume register mirror
 bne initvoice-1 ;if volume is 0 (off) turn it up!
 ora #%00001111 ;set volume to max
 jmp setvol
;
;******************************************
;* mdbasic functions instr(), ptr(), csr(), pen(), joy(), pot(), hex$() *
;******************************************
; I = INSTR(offset,src$,find$)
;if first expression is int type then offset provided otherwise offset=1 (default)
instr
 jsr CHRGET   ;process next cmd text
 jsr CHKOPN   ;Check for and Skip Opening Parentheses
 jsr FRMEVL   ;eval expression
 lda $0d      ;check if numeric (offset param) or string (source$ param)
 beq getoffsetparam
 lda #0       ;default zero index
 sta $02
 jsr getstr2  ;get source string as first param
 jmp getsrcparam
badidx
 jmp BSERR    ;BAD SUBSCRIPT ERROR
getoffsetparam
 jsr GETBYTC+6 ;convert FAC1 to an unsigned byte value 0-255 into X reg
 txa
 beq badidx   ;BASIC string indexes are based at 1 (not 0)
 sta $02      ;start index
 jsr ckcom2   ;check for and skip over comma, misop err if missing
 jsr getstr0  ;get source string as second param
getsrcparam
 stx $fb
 sty $fc
 sta $fd      ;len of src str
 jsr ckcom2   ;check for and skip over comma, misop err if missing
 jsr getstr0  ;find string a = len($52), x=lobyte($50) ptr, y=hibyte($51) ptr
 lda $fd      ;src len
 beq notfound ;zero length strings cannot be searched
 cmp $02      ;start index
 bcc notfound ;start index beyond src len
 cmp $52      ;find len > src len
 bmi notfound ;find str cannot be found in a shorter src string
 dec $02      ;convert start index to a zero-based index
tryagain
 ldy $02      ;index of current char in source str
 ldx #0       ;index of first char in find str
 inc $02      ;prepare index for return value or next position to compare
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
 ldy $02      ;index of beginning of str found in source str
.byte $2c     ;defeat ldy #0 as BIT $00A0
notfound ldy #0
 lda #0       ;hibyte 0 since strings cannot be longer than 255
instrend
 jsr GIVAYF   ;convert 16-bit signed int in A,Y regs to 5-byte float in FAC1
 jmp CHKCLS   ;check for and skip closing parentheses
;
;*******************
; P=PTR(x) or P=PTR(x%) or P=PTR(x$) where x is the variable name
ptr
 jsr CHRGET
 jsr PARCHK
 lda $48
 ldy $47
 jmp GIVAYF
;
;*******************
; V = ROUND(n)   where n=num to round to nearest whole number
; V = ROUND(n,d) where n=num to round, d(-9 to +9) = num of places left (-) or right (+) of decimal point
round
 jsr CHRGET
 jsr CHKOPN     ;Check for and Skip Opening Parentheses
 jsr FRMNUM     ;get numeric param1 - number to round
;save param1
 jsr MOV2F      ;move a 5-byte floating point number from FAC1 to memory $57-$5B BASIC numeric work area
;get param2
 lda #0
 sta $14        ;default 0 decimal places (round to whole number)
 sta $15        ;default first direction right
 jsr comchk
 bne round1
;get param2
 jsr CHRGET
 jsr FRMNUM
 jsr AYINT      ;convert FAC1 to a signed integer in FAC1
 lda $66        ;sign, $00=Positive, $FF=Negative
 sta $15
 beq round2
 jsr NEGFAC     ;replace FAC1 with its 2's complement
round2
 lda $64        ;hi byte
 bne illqty7
 lda $65        ;lo byte
 cmp #10
 bcs illqty7    ;range is -9 to +9
 sta $14
round1
 jsr CHKCLS     ;check for and skip closing parentheses
;restore param1 to FAC1
 lda #$57
 ldy #$00
 jsr MOVFM      ;move a 5-byte floating point number from memory to FAC1 A=lo, Y=hi
 ldx $14
 beq addhalf
;move decimal point to the right or left based on sign of num places
 jsr movedec
 lda $15
 eor #$ff       ;toggle move direction for 2nd call
 sta $15
addhalf
 jsr doround    ;round FAC1 to nearest whole number
;move decimal point to the left
 ldx $14
 beq nomul
;move decimal point
movedec
 txa
 beq nomul
 pha
 lda $15          ;direction: 0=right else left
 beq xmul10
 jsr DIV10        ;divide FAC1 by 10
 jmp xmul10+3
xmul10 jsr MUL10  ;multiply FAC1 by 10
 pla
 tax
 dex
 bne movedec
nomul rts
;
;*******************
; J = JOY(n) where n=joystick number 1 or 2
joy
 jsr getfnparam
 beq illqty7
 cmp #3
 bcs illqty7
 tay
 dey
 tya
 eor #%00000001
 tay
 lda CIAPRA,y
 tay             ;remember full value
 and #%00001111  ;lower nibble holds position value
 sta $14
 tya             ;restore full value
 and #%00010000  ;bit 4 is fire button
 sta $15
 lda #$0f
 sec
 sbc $14
 ldx $15
 cpx #$10
 beq nofire
 ora #%10000000 ;fire flag
nofire tay      ;lowbyte
 jmp nobutt
illqty7 jmp FCERR ;display illegal qty error
;
;*******************
; K = KEY   - used with ON KEY GOSUB to indicate key causing event
keyfn
 jsr CHRGET
 ldy keyentry
 jmp nobutt
;
;*******************
; E = ERROR(n) where n=0 Error Number, n=1 Error Line Number
err
 jsr getfnparam
 tya
 beq errorno
 dey
 bne illqty7
 ldy errline
 lda errline+1
 jmp GIVAYF  ;convert binary int to FAC then return
errorno
 ldy errnum  ;y=lobyte
 jmp nobutt
;
;*******************
; P = POT(n) where n=potentiometer number (1-4)
pot
 jsr getfnparam
 beq illqty7
 cmp #5         ;valid values 1 to 4
 bcs illqty7
 pha            ;save pot num
 ldx #%11000000 ;bits 0=input, 1=output, set bits 6 and 7 to output
 sei
 stx CIDDRA     ;data direction reg A
 cmp #$03       ;pot 3 and 4 are on port 2
 bcs port2
 ldy CIAPRB     ;data port reg B
 ldx #$40
 bne setprt     ;always branches
port2 lsr
 ldx #$80       ;bit 7 set to read paddles on Port A or B
 ldy CIAPRA
setprt stx CIAPRA
 sty $02
 ldy #$80       ;wait for latch to fully engage
waitl dey
 bpl waitl
 lsr
 tay
 lda POTX,y
 ldx #$ff       ;set data direction to output
 stx CIDDRA
 cli
 tay            ;lobyte value
 pla            ;restore pot num 1-4
 lsr            ;carry set if odd pot num
 lda #%00000100 ;odd port nums should check bit 2 for fire button
 bcs and02      ;odd port num
 asl            ;even port nums should check bit 3 for fire button
and02 bit $02   ;fire button pressed?
 bne nobutt
butt lda #$01   ;hibyte 1 to indicate fire button pressed
.byte $2c       ;defeats LDA #0 by making it BIT $00A9
nobutt lda #0   ;hibyte 0 to indicate fire button not pressed
 jmp GIVAYF     ;convert binary int to FAC then return
;
;*******************
; P = PEN(n) where (n=0:x, n=1:y) to read $D013 (X) and $D014 (Y) Light Pen Registers.
;For PENY there are 200 visible scan lines possible so value is exact.
;For PENX there are only eight bits available for 320 possible horizontal
;screen positions, the value here is accurate only to every second dot position.
;The number here will range from 0 to 160 and must be multiplied by 2 in order
;to approximate the actual horizontal dot position of the light pen.
;
pen
 jsr getfnparam
 beq penx
 cmp #2
 bcs illqty7
 ldy LPENY   ;y=lobyte
 jmp nobutt
penx lda LPENX
 asl         ;multiply by 2, bit 7 into carry
 tay         ;lo byte
 bcc nobutt  ;result was less than 256 so hi byte = 0
 bcs butt    ;result was more than 256 so hi byte = 1
;
;H$ = HEX$(n) where n is a signed 32-bit signed integer
hex
 jsr CHRGET  ;process next cmd text
 jsr PARCHK  ;get term inside parentheses
 jsr TESTNUM ;ensure expression was numeric, error if not
 jsr QINT    ;convert FAC1 into a signed 32-bit int in FAC1
 dec R6510
 jsr hexx
 inc R6510
 jmp STRLIT
;*******************
;
; V = INF(n) where n = 0 to 15 to select info
inf
 jsr getfnparam
 dec R6510
 jsr inff
 inc R6510
 jmp $b8d7   ;convert unsigned 4-byte int in FAC1 to a 5-byte float in FAC1
;
;********************
;* new reset vector *
;********************
resvec
 ldx #$ff    ;highest stack ptr offset (empty stack)
 sei         ;disable interrupts
 txs         ;clear the stack
 cld         ;ensure decimal mode is turned off
 stx SCROLX  ;reset video chip (bit5) visibly by using 38 cols (bit3) & mc mode (bit4)
 jsr IOINIT  ;init CIA i/o devices
 jsr RAMTAS  ;init RAM, tape buffer & screen
 jsr RESTOR  ;restore default I/O vectors
 jsr CINT    ;init screen editor and VIC-II chip
 cli
 jsr $e453   ;copy BASIC vectors to RAM
 jsr INIT    ;initialize BASIC
 lda R6510
 and #%11111110
 sta R6510   ;switch LOROM to LORAM
 jsr newvec  ;set new vectors to MDBASIC routines
 jsr initclk ;init TOD clock #2
 inc R6510   ;switch LORAM to LOROM
 lda #<mesge
 ldy #>mesge
 jsr STROUT  ;display MDBASIC banner
 lda $2b     ;ptr to start of BASIC program text
 ldy $2c
 jsr REASON  ;check free mem
 jsr $e430   ;prints the BYTES FREE message
 jmp $e39d   ;to basic main loop
;***********************
;* new RUN-STOP vector *
;***********************
runstp lda #$7f
 sta CI2ICR
 ldy CI2ICR
 bmi nothin
 jsr $f6bc       ;scan keyboard for STOP key with result in $91
 jsr STOP        ;determine if STOP key was pressed
 bne nothin      ;if not, continue with NMI handler
;********************
;* new BREAK vector *
;********************
brkirq jsr IOINIT
 lda #$04        ;initialize to 1024 ($0400)
 sta HIBASE      ;text page hi byte pointer
 jsr $e518       ;initialize screen and keyboard
 lda R6510
 and #%11111110
 sta R6510       ;switch LOROM to LORAM
 jsr newvec      ;init vectors
 inc R6510       ;switch LORAM to LOROM
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
;*********************
;* new error handler *
;*********************
errors lda listflag  ;is LIST currently executing?
 beq error2   ;list flag is off
 lda #0
 sta listflag ;list flag off
 rts
error2 txa
 bpl doerr    ;bit 7 off means error condition
 jsr detrap   ;ensure error trapping off
 jmp READY    ;print READY. then continue with BASIC main loop
doerr
 sta $02
 jsr romin    ;ensure HIROM enabled and restore default devices
 lda #0       ;default I/O channel keyboard/screen
 sta $13      ;set current I/O channel
 jsr $a67a    ;empty system and temp string stacks
 jsr norm     ;set text mode normal display
 lda $9d      ;0=prg mode
 beq prgmode
 lda #13      ;cr
.byte $2c     ;defeat lda #147 by making it BIT $93A9
prgmode lda #147 ;clr screen
 jsr CHROUT
 lda #$80     ;only control messages - SEARCHING, SAVING, FOUND, etc.
 jsr SETMSG
 jsr $ab45    ;print question mark
 lda $02      ;current error num
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
cbmerr lda $a326,x  ;$A328-$A364 Error Message Vector Table
 ldy $a327,x
hibyer jsr printstr
 lda #$69    ;address of the zero-terminated string ($a369) = "  ERROR"
 ldy #$a3
 jsr STROUT  ;print str whose addr is in y reg (hi byte) and a reg (lo byte)
 ldy $3a     ;hibyte of txtptr of beginning of line where error occured
 iny         ;a value of $ff indicates immediate mode
 bne inline  ;otherwise program mode
 jmp READY   ;print READY. then continue with BASIC main loop
inline
 jsr INPRT   ;display text IN {line#}
 lda $39     ;put the current BASIC line number
 sta $14     ;where FINDLN expect to see it
 lda $3a
 sta $15
 jsr FINDLN   ;set position to line num causing error
 jsr printcr
 lda #$01     ;set LIST flag on
 sta listflag
 jsr $a6c9    ;perform LIST line#
 ldx #$02     ;place cursor on first digit of line number
 ldy #$00     ;which would be line 2 column 0
 clc
 jsr PLOT
 jmp (IMAIN)  ;main BASIC loop
;
; IRQ for key decode overriden to support function keys
keychk
 lda $9d        ;control messages enabled?
 beq nokey      ;no, use original routine
 lda $d4        ;editor in quote mode?
 bne nokey      ;yes, use original routine
 lda #%00000100 ;keypump irq flag
 bit MDBIRQ     ;is keypump irq already enabled?
 bne nokey      ;yes, use normal decode routine
 lda #$81       ;standard keyboard matrix decode table $eb81
 ldy SHFLAG     ;0=none, 1=shift key, 2=logo key, 4=ctrl key
 beq stdkey
 dey            ;zero flag set if only shift key pressed
 bne nokey      ;unsupported key combo for function keys
 lda #$c2       ;shifted keyboard matrix decode table $ebc2
stdkey sta $f5
 lda #$eb
 sta $f6
 ldy $cb        ;matrix coordinate of current key pressed
 lda ($f5),y
 tax
 cpy $c5        ;matrix coordinate of last key pressed, 64=None Pressed
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
 lda #%00000100
 ora MDBIRQ
 sta MDBIRQ
 jsr mdbirqon
 ldx #$ff
 jmp $eae9      ;continue regular key decode func
;
; IRQ driven key pump into keyboard buffer
keypump
 ldx NDX        ;if keyboard buffer is empty
 bne irqdone2   ;then forward to original IRQ vector
 lda R6510
 and #%11111110
 sta R6510      ;switch LOROM to LORAM
 ldx keyidx     ;offset to next char to process
 lda keybuf,x   ;get next char
 ldy keystrflg  ;manual key string flag?
 beq nostrflg
 lda paintbuf3,x
nostrflg
 inc R6510      ;switch LORAM to LOROM
 tax
 beq alldone    ;zero-terminated string
 sta KEYD       ;place char in keyboard buffer
 inc NDX        ;indicate 1 char waiting in buffer
 inc keyidx     ;advance index to next char
 bne irqdone2   ;continue original IRQ vector
alldone
 lda #0
 sta keystrflg
 jmp keyirqoff
irqdone2
 rts
;
;------------------------
;MDBASIC IRQ Handler
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
 beq irqnorm
 jsr keypump
irqnorm
 jmp (irqtmp)   ;orgininal irq vector
mdbirqoff2
 jsr mdbirqoff
 jmp (irqtmp)   ;orgininal irq vector
;
;Sprite animation IRQ routine
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
ptr1ref lda $07f8,x
 cmp ptrend,x
 bcc setptr
 lda ptrbegin,x ;reset ani img to first
.byte $2c       ;prevent adc #1
setptr adc #1
ptr2ref sta $07f8,x
 tya            ;restore bit pattern
nxtani
 dex
 bpl chkani
anidone rts
;
;MDBASIC IRQ control routines
playirqoff
 lda #%11111110
.byte $2c
anioff
 lda #%11111101
.byte $2c
keyirqoff
 lda #%11111011
 and MDBIRQ
 sta MDBIRQ
 rts
;
playirqon
 lda #%00000001
.byte $2c
anion
 lda #%00000010
.byte $2c
keyirqon
 lda #%00000100
 ora MDBIRQ
 sta MDBIRQ
 sei
 jsr mdbirqon
 cli
 rts
;ensure MDBASIC IRQ handler is enabled
;this subroutine assumes IRQs are already disabled
;caller should be an IRQ subroutine or use sei/cli
mdbirqon
 ldy CINV+1
 cpy #>mdbirqhdl
 beq mdbirqon-1
 ldx CINV
 stx irqtmp
 sty irqtmp+1
 ldx #<mdbirqhdl
 ldy #>mdbirqhdl
setirq
 stx CINV
 sty CINV+1
 rts
mdbirqoff
 ldx irqtmp   ;restore IRQ vector
 ldy irqtmp+1 ;back to original $EA31 or user-defined address
 jmp setirq
;
;*******************************
;* general purpose subroutines *
;*******************************
;get voice param and convert to SID register offset in accumulator
ppw
 jsr getvoc  ;1,2,3
ppw2 tax
 dex         ;0,1,2
 lda sidoff,x
 rts
;
;*******************
;multiply the value in accumulator by 8 returning word in $be,$bf
times8
 pha
 lda #0
 sta $be    ;result lobyte
 sta $bf    ;result hibyte
 pla        ;num to multiply by 8
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
;*******************
opget jsr CHRGET
opget2 bcc okopge  ;text found (not a token)
 beq okopge        ;end of line found
 cmp #$ab          ;subtract token? (- is a token)
 beq okopge
 cmp #"-"
 beq okopge
sytxer jmp SNERR   ;print syntax error
okopge jsr LINGET  ;convert asci decimal number to a 2-byte binary line number
 jsr FINDLN        ;search for line number
 jsr CHRGOT        ;get current char on line pointed ($7a-$7b) CHRGOT
 beq linnul
 cmp #$ab          ;subtract token? (- is a token)
 beq okopg2
 cmp #"-"
 bne sytxer
okopg2 jsr CHRGET
 jsr LINGET        ;convert asci decimal number to a 2-byte binary line number
 bne sytxer
linnul lda $14
 ora $15
 bne opgot
 lda #$ff
 sta $14
 sta $15
opgot rts
;******************
peekop ldy #1
peekoper lda ($7a),y
 beq peekdone
 iny
 cmp #" "
 beq peekoper
peekdone rts
;******************
comchk
 ldy #0            ;quickly check
 lda ($7a),y       ;if current txtptr
 cmp #","          ;is on a comma
 rts
;*******************
chkcomm
 ldy #0            ;quickly check
 lda ($7a),y       ;if current txtptr
 cmp #","          ;is on a comma
 beq comma         ;return normally if comma exists
 pla               ;don't return to caller if no comma
 pla
 rts
;*******************
;check for and skip over comma, misop err if missing
ckcom2
 ldy #0
 lda ($7a),y
 cmp #","
 bne missop
comma jmp CHRGET
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
;******************
getbool jsr CHRGET
 beq missop
getbool2 jsr skip73
 cmp #2
 bcs illqty4
 tax               ;sets zero flag if a reg is zero
 rts               ;result is in both a and x reg
getval15_ jsr CHRGET
getval15_0 beq missop
getval15 jsr skip73
 cmp #16           ;enforce 0-15 range
 bcs illqty4       ;branch not taken saves 1 cycle
 rts               ;faster for non error condition
illqty4 jmp FCERR  ;illegal quanity error
;*******************
;get a single byte int (0-255) throw error if bad data type or range
getval jsr CHRGET
skip73z beq missop
skip73 jsr FRMNUM  ;eval numeric expr & type, store result in FAC1
fac2int jsr GETADR ;convert FAC1 to unsigned 2 byte int in $14,$15
 lda $15           ;if hi byte is not zero then
 bne illqty4       ;throw ill qty err
 lda $14           ;return the result in the a register
 rts
;*******************
;get a single-byte numeric parameter inside parentheses
getfnparam
 jsr CHRGET        ;process next cmd text
 jsr PARCHK        ;get term inside parentheses
 jsr TESTNUM       ;ensure expression was numeric, error if not
 jmp fac2int
;
;*******************
dbyval jsr CHRGET  ;get a 2 byte int (0-65535)
skp73_ beq missop
skp73 jsr FRMNUM   ;eval numeric expr & type
 jsr GETADR        ;convert fac1 to unsigned 2 byte int
 ldx $14
 ldy $15
 rts
;*******************
types
 jsr chkcomm     ;if current char is comman skip over it
 cmp #","
 beq noparam     ;another comma found so skip plot type param
 jsr skip73      ;get plot type value
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
 jsr skip73      ;get color selection, multicolor selection index 1-3
 beq illqty4
 cmp #4          ;mc mode color index selection is 1,2 or 3
 bcs illqty4
;convert index to ptab offset 1=8, 2=16, 3=24
 asl
 asl
 asl
 sta mapcolbits  ;offset = index * 8 where index in (1,2,3)
 rts
;*****************
sprnum
 jsr skip73z
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
 lda VMCSB      ;calc screen RAM page offset (n*1K) 1=$0400, 2=$0800, 3=$0C00, etc.
 and #%11110000 ;upper nibble holds the number of 1K chunks
 lsr            ;convert to offset for hibyte, 1K=(4*256), 1=4, 2=8, 3=12, etc.
 lsr            ;zero shifted into carry by lsr so it is clear
 adc $62        ;base+offset
 adc #3         ;the end of screen RAM is 1K more
 rts
;*******************
getvoc
 jsr skip73z
 beq iverr
 cmp #4
 bcc getvoc-1
iverr ldx #32    ;illegal voice number
 jmp (IERROR)
;*******************
;this function entry point is called by commands PLOT,LINE,CIRCLE,PAINT
getpnt
 jsr pntweg      ;get x,y, plot type
 jmp savepoint
;*******************
;these routines are used by command under ROM
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
 jsr skp73
 php             ;save zero flag indicating hibyte non-zero
 txa             ;lobyte also in A reg for convenience
 dec R6510
 plp
 rts
rom4
 inc R6510
 jsr LINPRT+4   ;print 2-byte binary number in FAC1
 dec R6510
 rts
;
;round FAC1 to the nearest whole number by adding .5 then truncate
doround
 jsr FADDH       ;add .5 to value in FAC1
 jmp INT         ;perform INT
;
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

nolin .null "65535"
filestr .null " files."

;table for calculating 2^n where n=0-7
bitweights .byte 1,2,4,8,16,32,64,128

;SID voice register offsets
sidoff .byte 0,7,14
;
;10 tokens use line numbers that need to be renumbered when using renum
gotok
.byte TOKEN_GOTO,TOKEN_GOSUB,TOKEN_RETURN,TOKEN_THEN,TOKEN_ELSE
.byte TOKEN_RESUME,TOKEN_TRACE,TOKEN_DELETE,TOKEN_RUN,TOKEN_RESTORE
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

traceflag  .byte 0 ;trace flag 0=off, 1-on
listflag   .byte 0 ;list flag 0=off, 1=on
keyflag    .byte 0 ;key capture mode 0=disabled, 1=enabled, 2=paused
keyentry   .byte 0 ;scan code of the last key captured with ON KEY statement enabled
errnum     .byte 0 ;last error number that occured, default 0 (no error)
errline    .word $FFFF ;last line number causing error, default -1 (not applicable)
errtrap    .word 0 ;error handler line number
txtptr     .word 0 ;basic txt ptr of statement causing error
stackptr   .byte 0 ;stack ptr before cmd execution for use by ON ERROR
keyptr     .word 0 ;basic txt ptr of statement for ON KEY GOSUB line#
keyline    .word $FFFF ;line number for ON KEY subroutine
keyidx     .byte 0 ;current index of char to put in keyboard buffer via IRQ
keystrflg  .byte 0 ;flag to indicate key string from KEY cmd

;SID mock registers
md417      .byte 0   ;holds current filter control and resonance
md418      .byte 0   ;holds current volume (lo nibble) and filter type (hi nibble)

irqtmp     .word 0   ;temp IRQ vector storage

;sprite animation
aniopt     .byte 0     ;enables animation for 8 sprites with 8 bit flags
anidly     .repeat 8,0 ;IRQ delay per sprite
aniwait    .repeat 8,0 ;param
ptrbegin   .repeat 8,0 ;param
ptrend     .repeat 8,0 ;param

;GENERAL GRAPHICS COMMANDS (Shared):
lastplotx  .word 0   ;last plotted dot's x coord
lastploty  .byte 0   ;last plotted dot's y coord
lastplott  .byte 1   ;last plot type used
mapcolc1c2 .byte $15 ;current plot color; hi nibble = c1 (white), lo nibble = c2 (green)
mapcolc3   .byte 7   ;c3 (1-3) multicolor mode color for bit pattern 11 (default yellow)
mapcolbits .byte 8   ;(values 8,16,24) used for plotting a dot on a multi-color bitmap

moveflag   .byte 0   ;flag for LINE function for which cmd is executing; 0=LINE, otherwise MOVE

autonum    .word 10  ;hold 2-byte value of auto line numbering setting

;PLAY command use only - used during IRQ
playtime   .byte 0   ;current jiffies till next note
playvoice  .byte 0   ;SID register offset for play voice
playwave   .byte 0   ;waveform for play notes

;********************************************************************
;* Temp variables during subroutine execution only (local vars)     *
;********************************************************************

;this temp space is used to save last plot x,y so sprite move can use it
lastplotx2
.word 0  ;temp copy of last used x coord
.byte 0  ;temp copy of last used y coord
;
;********************************************************************
;* ROM barrier at A000-BFFF switch to RAM using $01 before entry    *
;********************************************************************
;
*=$a000 ;"MDBASIC RAM under ROM Memory Block"

;temp storage for PAINT and SCROLL command
paintbuf1 .repeat 256,0
paintbuf2 .repeat 256,0
paintbuf3 .repeat 256,0

playbuf1 .repeat 256,0

;table for converting ascii to screen code
;zero value indicates ctrl or non-displayable char
;except for ascii 64 which is screen code 0
;codes 192-223 same as 96-127
;codes 224-254 same as 160-190
;code  255 same as 126
asc2scr
.repeat 32,0
.byte 32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63
.byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,64,65,66
.byte 67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95
.repeat 32,0
.byte 32,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117
.byte 118,119,120,121,122,123,124,125,126,127,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78
.byte 79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,32,97,98,99,100,101,102,103,104,105
.byte 106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,94

ascctrl
.byte 5,13,14,17,18,19,20,28
.byte 29,30,31,129,141,142,144,145
.byte 146,147,149,150,151,152,153,154
.byte 155,156,157,158,159
ascctlfn
.word txtclr,txtcr,txtlc,txtdwn,txtrvson,txthome,txtbs,txtclr
.word txtright,txtclr,txtclr,txtclr,txtcr,txtuc,txtclr,txtcsrup
.word txtrvsoff,txtbitclr,txtclr,txtclr,txtclr,txtclr,txtclr,txtclr
.word txtclr,txtclr,txtleft,txtclr,txtclr

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
;
.byte 0 ;place holder for this spot seems to be messed up on start
;
;PLAY command temp vars used during IRQ
temp1     .byte 0
temp2     .byte 0
playoct   .byte 4  ;default octave 4
playlen   .byte 30 ;notelength for each voice
playindex .byte 0  ;index of current char in play string for each voice
;** play table ***
;These numbers represent the middle octave and are only for the SID freq control registers
;CLOCK is NTSC=1022727.143, PAL=985248.611
;FREQUENCY=REGVAL*(CLOCK/16777216)Hz
;REGVAL=FREQUENCY/(CLOCK/16777216)Hz
;https://pages.mtu.edu/~suits/notefreqs.html
;There are 8 octaves each with 12 notes 8*12 = 96 total notes
;The notes here are octave 4 on NTSC clock. The others are derived from these.
;
;             A    B    C    D    E    F    G
notes  .word 3609,4051,4292,4817,5407,5729,6431
;flats(-) are derived from sharps(#). A- is in previous octave (G#)
;for invalid notes C-/B# = B and F-/E# = E
;             A-
fnotes .word 3406
;             B-  *C-   D-   E-  *F-   G-
;             A#  *B#   C#   D#  *E#   F#   G#
snotes .word 3824,4051,4547,5104,5407,6069,6813
;these are the same notes above but for PAL systems (30 byte offset)
.word        3746,4205,4455,5001,5613,5947,6675
.word        3536
.word        3969,4205,4720,5298,5613,6300,7072

;table of video screen matrix hibyte offset per line 0-24
btab .byte 0,0,0,0,0,0,0,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3

baudrates .word 50,75,110,134,150,300,600,1200,1800,2400
;baud rates not supported (too fast for IRQ) 3600,4800,7200,9600,19200
;MDBASIC parity parameter value range 0-4 to register M51CDR bits 7,6,5
;0 = XX0 (0,64,128, or 192) = No Parity Generated or Received
;1 = 001 (32)  = Odd Parity Transmitted and Received
;2 = 011 (96)  = Even Parity Transmitted and Received
;3 = 101 (160) = Mark Parity Transmitted and Received
;4 = 111 (224) = Space Parity Transmitted and Received
parity .byte %00000000,%00100000,%01100000,%10100000,%11100000
;
;tables for plotting dots on a bitmap per line 0-24
lbtab .byte 0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0,64,128,192,0
hbtab .byte 224,225,226,227,229,230,231,232,234,235,236,237,239,240,241,242,244,245,246,247,249,250,251,252,254

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
;
;scroll direction vectors up,down,left,right
scrolls .rta scroll0,scroll1,scroll2,scroll3

;strings for keylist
addchr .shift "+chr$("

;INF() memory locations
infbytes
.byte PNTR   ;csr logical column
.byte TBLX   ;csr physical line
.byte BLNSW  ;csr blink enabled
.byte LNMX   ;csr max columns on current line (39 or 79)
.byte GDBLN  ;ASCII value of char under csr (when blinking)
.byte TMPASC ;ASCII value of last character printed to screen
.byte INSRT  ;insert char count
.byte LDTND  ;num open files
.byte NDX    ;num chars in keyboard buffer
infwords
.word SHFLAG
.word COLOR  ;current foreground color for text
.word GDCOL  ;color under cursor
.word $02A6  ;PAL or NTSC
.word $FF80  ;Kernal version/system id
.word playindex ;index of next note to play in play string
;
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
;
;KEY (continued)
keyy
 ldx #0        ;even/odd key string offset
 lda $02       ;key# 1 to 8
 lsr           ;index 0 to 4
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
 clc
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
endass lda #0 ;terminate string with zero-byte
 sta keybuf,x
 inx
 iny
 cpy #16      ;fill remaining bytes with 0
 bcc endass+2
 jmp memnorm  ;switch LORAM back to LOROM
;
;KEY LIST (continued)
;
keylistt
 jsr printcr
 lda #"1"
 sta $02      ;current key#
 lda #0
 sta $fb      ;string offset in buffer
nextke
 jsr kprnt
 lda $02
 cmp #"8"
 beq stpkeylst
 inc $02
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
 lda #<keystr
 ldy #>keystr
 jsr printstr
 lda $02      ;key#
 jsr CHROUT
 lda #","
 jsr CHROUT
 lda $fb      ;index offset
 sta $fc      ;current index
pkstr
 jsr printqt
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
linedraw
 lda lastplotx   ;destination x coord lobyte
 sec
 sbc $fb
 sta $57
 lda lastplotx+1 ;destination x coord hibyte
 sbc $fc
 sta $58
 lda lastploty
 sec
 sbc $fd  ;destination y coord
 sta $6b  ;temp var for distance = y1-y2
 ldy #1
 ldx #0
 lda $fc  ;determine which x coord is larger
 cmp lastplotx+1
 bcc storxy
 bne looper
 lda lastplotx
 cmp $fb
 bcs storxy
looper ldy #$ff
 ldx #$ff
 lda $fb
 sec
 sbc lastplotx
 sta $57
 lda $fc
 sbc lastplotx+1
 sta $58
storxy sty $6f
 stx $70
 ldy #1
 lda lastploty
 cmp $fd   ;determine which y coord is larger
 bcs stya7
 tya       ;flip signed int value
 eor #$ff  ;by calc 2's compliment
 tay
 iny
 lda $fd
 sec
 sbc lastploty
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
b5ne jsr fac
 sta $59
 jmp drwlin
fac sty $6e
 tya
 lsr      ;will set carry bit if odd
 stx $6d
 txa
 ror      ;will pull in carry bit
 rts
drwlin lda #0
 sta $9e
 sta $9f
 sta $6a
 sta $5a
starts jsr pokadd
 lda $fc
 cmp lastplotx+1
 bne aca3
 lda $fb
 cmp lastplotx
 bne aca3
 lda $fd
 cmp lastploty
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
 ldy moveflag   ;sprites can have a y coord to 255
 bne jmpout1    ;moving a sprite so continue
 rts
pokadd
 lda moveflag   ;flag 0=LINE cmd, >0=MOVE cmd
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
 ldy $0f        ;temp var of sprite reg index
 sta SP0X,y
 lda $fd
 sta SP0Y,y
;apply sprite move delay
 ldy $fe
 beq linedon
movewait
 lda $c5      ;Matrix Coordinate of Last Key Pressed, 64=None Pressed
 cmp #$3f     ;STOP key?
 bne movwait  ;carry flag will be returned to caller to indicate STOP key pressed
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
colorb sta ($c3),y ;write byte with bit pattern targeting the one bit in hires or 2 bits in mc mode
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
 lda mapcolc1c2 ;hires dot color (hi nibble) and background color of 8x8 square (lo nibble)
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
 asl             ;move to hi-nibble
 asl
 asl
 asl
 sta $61
 lda mapcolc1c2  ;hi nibble is dot color
 and #%00001111  ;erase hi nibble
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
 bmi txtbs-1 ;not enough space so abort
oktxtx
 sta lastplotx+1
 stx lastplotx
 sec         ;flag indicating x set
 rts
txtbs
 jsr txtleft
 bcc txtbs-1
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
readstr ldy #0
 lda ($50),y    ;get character to display on bitmap
;convert ascii to screen code
 tay
 lda asc2scr,y
 bne dotptr
;scan for match in table
 cpy #"@"       ;@ is screen code 0
 beq dotptr
 tya            ;find func to support nonprintable ascii
nonprt
 ldy #28        ;29 ascii values have funcs
fndctrl
 cmp ascctrl,y  ;ascii in ctrl code table?
 beq fndctrl2   ;yes, execute func indexed by y
 dey            ;no, check next code in table
 bpl fndctrl    ;if not in table then
 bmi nxtchar    ;do nothing
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
 lda ascctlfn,y
 sta $55
 lda ascctlfn+1,y
 sta $56
 lda #>nxtchar-1
 pha
 lda #<nxtchar-1
 pha
 jmp $0054
;
;**************************
painter
 ldx #$01
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
 lda $c5      ;Matrix Coordinate of Last Key Pressed, 64=None Pressed
 cmp #$3f     ;STOP key?
 bne paintit  ;stop painting
 jmp norm     ;restore text mode (stop key pressed)
epaint
 jmp memnorm  ;switch LORAM back to LOROM and HIRAM back to HIROM
paintit 
 lda $fc
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
 jmp beginp
readb jsr ydiv8
 stx $aa
 lda R6510
 and #%11111101 ;bit1 0=HIRAM
 sei            ;disable IRQ since kernal HIROM is switching to HIRAM
 sta R6510
 lda ptab3,x
 eor #$ff
 and ($c3),y    ;bitmap in HIRAM
 pha
 lda R6510
 ora #%00000010 ;bit1 1=HIROM
 sta R6510
 cli
 lda $aa
 and #%00000111
 tax
 pla
 cpx $5b
 bcs flgit
divid2 lsr
 inx
 cpx $5b
 bcc divid2
flgit cmp #$00
 rts
buffit ldy $57
 lda $fb
 sta paintbuf3,y
 lda $fc
 sta paintbuf2,y
 lda $fd
 sta paintbuf1,y
 inc $57
 rts
;
;process radial line options
ciropts
 asl $2a
 bcc copt2
 jsr getpoint ;get last plot coordinates
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
 jsr getpoint ;get last plot coordinates
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
 jsr getpoint ;get last plot coordinates
 lda $fd
 sec
 sbc $36
 bcs okopt3
 lda #0
okopt3 sta $fd
 jsr linedraw
copt4
 asl $2a
 bcc noopt4
 jsr getpoint ;get last plot coordinates
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
 jsr linedraw
noopt4
 rts
;
;**************************
circel
 lda $fb
 asl
 sta $61  ;center x
 lda $fc
 rol
 sta $62
 lda $fd
 asl
 sta $63  ;center y
 lda #0
 sta $fe
 rol
 sta $64
;
 lda $35  ;x radius size
 asl
 sta $11
 sta $57
 sta $50
 lda #0
 rol
 sta $12
 sta $58
 sta $51
;
 lda $36  ;y radius size
 asl      ;diameter = 2*radius
 sta $14  ;y diameter
 lda #$00
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
 lda #0
 sta $65
 lda $15
 cmp $12
 bne cbne
 lda $14
 cmp $11
cbne bcs csca
 inc $65
csca lda #0
 sta $66
 lda $65
 beq ce00
 jsr tfb2b2
 inc $66
 jmp loops
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
 lda $2a
 and #%00001001 ;quad 1 and 4
 beq no0
 jsr plotc ;0 degrees
no0 lda $61
 sec
 sbc $6f
 sta $fb
 lda $62
 sbc $70
 lsr
 sta $fc
 ror $fb
 lda $2a
 and #%00000110 ;quad 2 and 3
 beq no180
 jsr plotc ;180 degrees
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
 jmp fd512
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
 beq zro6a
 jmp fd512
zro6a lda $69
 cmp #3
 bcc cc69
 jmp fd512
cc69 rts
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
 lda $2a
 and #%00001000
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
 lda $2a
 and #%00000100
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
 lda $2a
 and #%00000010
 beq plotq1_
 jsr plotc ;quad2
plotq1_
 lda $61
 clc
 adc $6f
 sta $fb
 lda $62
 adc $70
 lsr
 sta $fc
 ror $fb
 lda $2a
 and #%00000001
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
 lda $2a
 and #%00001100
 beq no270
 jsr plotc ;270 degrees
no270 lda $63
 sec
 sbc $71
 sta $fd
 lda $64
 sbc $72
 lsr
 sta $fe
 ror $fd
 lda $2a
 and #%00000011
 beq no90
 jsr plotc ;90 degrees
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
loopy jmp loops2
zero6c ldx #2
 lda $59,x
 cmp $69,x
 bcc loopy
 bne nencs
 dex
 bpl zero6c+2
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
;secondary address 2=SCREEN, 3=CHAREN, 4=BITMAP
;*********************
loadd
 ldx #0
 jsr lodsav  ;open file and check status, quit if needed (no return here, ROM in)
 jsr $f5af   ;print SEARCHING...
 jsr $f5d2   ;print LOADING...
 ldx $b8     ;logical file number
 jsr CHKIN   ;define output channel
 lda $02     ;original secondary address
 cmp #2      ;2=screen
 bne sdnot2
lodsrn jsr param ;prepare text and color mem pointers
 jsr CHRIN   ;get border color
 sta EXTCOL  ;set border color
 jsr CHRIN   ;get background color
 sta BGCOL0  ;set background color
 jsr CHRIN   ;get VMCSB
 sta VMCSB   ;set VMCSB
 jsr CHRIN   ;get SCROLX
 sta SCROLX  ;save bit 4 for multicolor text or bitmap flag
 jsr CHRIN   ;get SCROLY
 sta SCROLY     ;save bit 5 for bitmap mode flag
 jsr CHRIN
 sta CI2PRA
lode jsr CHRIN  ;continue loading the rest of the data
 sta ($c3),y    ;storing the bytes in the text
 jsr CHRIN      ;and
 sta ($fb),y    ;color memory areas
 jsr status     ;check for stop key or EOF and do not return here if so
 inc $fb
 inc $c3
 bne lode
 inc $fc
 inc $c4
 bne lode
sdnot2 cmp #3   ;3=CHAREN
 bne sdnot3
 jsr param2
lodchr jsr CHRIN
 sta ($c3),y
 jsr status     ;check for stop key or EOF and do not return here if so
 inc $c3
 bne lodchr
 inc $c4
 bne lodchr
sdnot3 jsr param3 ;4=BITMAP
lodbm jsr CHRIN
 sta ($c3),y
 jsr status    ;check for stop key or EOF and do not return here if so
 inc $c3
 bne lodbm
 inc $c4
 bne lodbm
 jsr param     ;prepare pointers for text and color mem
 lda #$c8      ;override hibyte to point at bitmap mem $C800
 sta $c4
 jmp lodsrn+3  ;finish by loading the remainder of the file
;
;*********************
;save command re-write
;secondary address 2=SCREEN, 3=CHAREN, 4=BITMAP
;*********************
savee
 ldx #1
 jsr lodsav
 jsr $f68f   ;print SAVING
 ldx $b8     ;current logical file number
 jsr CHKOUT  ;set stdout to current logical file number
 lda $02     ;fake secondary address (2,3,4)
 cmp #2      ;2=screen
 bne sdvn2
;save screen
savscr jsr param
 lda $c4
 clc
 adc #3
 sta $02
 lda EXTCOL  ;border color
 jsr CHROUT
 lda BGCOL0  ;background color
 jsr CHROUT
 lda VMCSB
 jsr CHROUT
 lda SCROLX  ;save bit 4 for multicolor text or bitmap flag 
 jsr CHROUT
 lda SCROLY  ;save bit 5 for bitmap mode flag
 jsr CHROUT
 lda CI2PRA
 jsr CHROUT
 ldy #0
saves lda ($c3),y
 jsr CHROUT
 lda ($fb),y
 jsr CHROUT
 jsr status  ;check for stop key or EOF and do not return here if so
 inc $fb
 inc $c3
 bne srnend
 inc $fc
 inc $c4
srnend lda $c4
 cmp $02
 bne saves
 lda $c3
 cmp #232
 bne saves
 jmp romin
;save redefined charset
sdvn2 cmp #3    ;3=CHAREN
 bne savbm
 jsr param2     ;prepare pointer for dot data base addr
savchr sei
 dec R6510
 lda ($c3),y
 inc R6510
 cli
 jsr CHROUT
 jsr status     ;check for stop key or EOF and do not return here if so
 inc $c3
 bne savchr
 inc $c4
 bne savchr
 jmp romin
;save a bitmap image with colors
savbm jsr param3 ;4=BITMAP, prepare pointers for bitmap and color mem
savbtm sei
 dec R6510        ;read byte from bitmap under ROM
 lda ($c3),y
 inc R6510
 cli
 jsr CHROUT
 jsr status     ;check for stop key or EOF and do not return here if so
 inc $c3
 bne savbtm
 inc $c4
 bne savbtm
 jsr param      ;prepare pointers for text and color mem
 lda #$c8       ;override hibyte to correct for bitmap mem at $C800
 sta $c4
 jmp savscr+3   ;finish by saving the screen mem bytes
;
lodsav          ;real secondary device in x reg
 lda $b9        ;secondary address
 sta $02        ;save fake secondary device # (2,3,4)
 stx $b9        ;replace secondary device with desired real value
 jsr OPEN       ;perform OPEN
 bcc status-1   ;return if ok, close and fail if error
 jmp err127
;
 rts
status jsr STOP
 beq stopls
 lda $90
 beq status-1
stopls pla
 pla
 jmp romin
;
param ldy #0  ;($c3) = ptr to text memory
 sty $c3
 sty $fb      ;($fb) = ptr to color memory
 lda HIBASE   ;top page of screen memory
 sta $c4
 lda #$d8     ;$D800 = color memory location
 sta $fc
 rts
param2 ldy #0 ;ptr to CHAREN
 sty $c3
 lda #$f0
 sta $c4
 rts
param3 ldy #0 ;ptr to BITMAP
 sty $c3
 lda #$e0
 sta $c4
 rts
;
;*********************
varss
 lda $2d      ;vector ($2d,$2e) beginning of non-array variable storage
 ldy $2e
chk2d
 cmp $2f      ;vector ($2f,$30) beginning of array variable storage
 bne copy2d   ;if both vectors point at same mem loc then no vars defined
 cpy $30
 bne copy2d
evar
 jmp memnorm  ;switch LORAM back to LOROM
copy2d
 sta $fb
 sty $fc
 jsr STOP     ;was STOP key pressed
 beq evar     ;yes, abort processing
 ldy #0
 sty $0d      ;set default type to 0=float
 lda ($fb),y  ;first 2 bytes is variable name
 sta $45      ;first char of var name
 and #$80     ;last char has bit 7 set
 beq bitoff
 lda #2
 sta $0d      ;set to type 2 fn
bitoff iny
 lda ($fb),y
 sta $46      ;second char of var name (or space if not needed)
 and #$80     ;check bit 7 flag for int type
 beq type
 inc $0d      ;set to type 3 int
type lda $fb  ;skip over 2 byte name
 clc
 adc #2
 sta $fb
 lda $fc
 adc #0
 sta $fc
 lda $0d     ;type 0=float, 1=string, 2=fn, 3=int
 cmp #2      ;fn
 beq nxtvar+5 ;skip fn types
 lda $45     ;get first char of name
 jsr prtchr  ;output first char of name
 lda $46     ;get second char of name
 jsr prtchr  ;output second char of name
 lda $0d     ;var type?
 beq float   ;type 0 is float
 cmp #1      ;type 1 is string
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
 ldy #0
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
sft lda SHFLAG
 cmp #1
 beq sft
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
 lda #27
 jsr CHROUT
 lda #"3"
 jsr CHROUT
 lda #25
 jsr CHROUT
 lda #0
 sta $fb
 sta $fe         ;column counter
 lda HIBASE      ;top page of screen mem hibyte
 sta $fc
 ldy #$00
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
dumpit sta $02
 pla
 bpl regchr
 lda #18
 jsr CHROUT
 lda $02
 jsr CHROUT
 lda #146
 jsr CHROUT
 jmp nxchar
regchr lda $02
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
 lda #$13
 jmp CHROUT
;
dumpbitmap2
 ldy #5          ;secondary param - binary graphic
 jsr opnprt
;send printer control codes
 lda #27
 jsr CHROUT
 lda #$41
 jsr CHROUT
 lda #8
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
 sta $02
nxtbit lda $02
 eor #$ff
 sta $02
 ldx #$00
;send printer codes
 lda #27
 jsr CHROUT
 lda #75
 jsr CHROUT
 lda #144
 jsr CHROUT
 lda #1
 jsr CHROUT
 lda #25
 sta $fd
 lda #8
 sta $61  ;temp var
pekbyt
;select HIRAM and disable IRQ
 lda R6510      ;bit0 0=LORAM 1=LOROM ($a000-$bfff), bit1 0=HIRAM 1=HIROM ($e000-$ffff)
 pha            ;save current mem bank setting
 and #%11111101 ;turn bit 1 off for HIRAM
 sei            ;****disable irqs while HIROM is not available
 sta R6510      ;apply mem bank new selection
;read byte from bitmap image in HIRAM
 lda ($fb),y
 tax            ;save in x
 pla
 sta R6510      ;restore mem bank original selection
 cli            ;****enable irqs
 txa            ;get saved value
 and $02
 cmp #16        ;hi nibble to lo nibble for indexing 
 bcc ttatx      ;value is between 0 and 15, good index
 lsr            ;move hi nibble to lo nibble for indexing 
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
 jsr printcr
 lda #10
 jsr CHROUT
 lda $02
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
 lda #27
 jsr CHROUT
 lda #50
 jsr CHROUT
 rts
;
;***********************
;scroll up
scroll0
 jsr wrapit
 lda $fb
 clc
 adc #40
 sta $ac
 lda $fc
 adc #0
 sta $ad
 lda $fd
 clc
 adc #40
 sta $ae
 lda $fe
 adc #0
 sta $af
;
 ldy $be       ;num bytes to move
 dec $bf
 bpl cpyup
wrapup
 lda $02       ;wrap?
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
cpyup lda ($ac),y
 sta ($fb),y
 lda ($ae),y
 sta ($fd),y
 dey 
 bpl cpyup
 lda $ac
 sta $fb
 lda $ad
 sta $fc
 lda $ae
 sta $fd
 lda $af
 sta $fe
 jmp scroll0+3
;scroll down
scroll1 jsr calcptr
 jsr wrapit
nxtdwn ldy $be
 lda $fb
 sec
 sbc #40
 sta $ac
 lda $fc
 sbc #0
 sta $ad
 lda $fd
 sec 
 sbc #40
 sta $ae
 lda $fe
 sbc #0
 sta $af
 dec $bf
 bpl cpydwn
 bmi wrapup
cpydwn lda ($ac),y
 sta ($fb),y
 lda ($ae),y
 sta ($fd),y
 dey 
 bpl cpydwn
 lda $ac
 sta $fb
 lda $ad
 sta $fc
 lda $ae
 sta $fd
 lda $af
 sta $fe
 jmp nxtdwn
;scroll left
scroll2 ldy #0
 lda ($fb),y
 sta paintbuf1
 lda ($fd),y
 sta paintbuf2
cpyleft iny 
 lda ($fb),y
 dey 
 sta ($fb),y
 iny 
 lda ($fd),y
 dey 
 sta ($fd),y
 iny 
 cpy $be
 bne cpyleft
 jsr scrollh
 dec $bf
 bpl scroll2
 jmp memnorm ;switch LORAM back to LOROM
;scroll right
scroll3 ldy $be
 lda ($fb),y
 sta paintbuf1
 lda ($fd),y
 sta paintbuf2
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
 dec $bf
 bpl scroll3
 bmi scroll3-3
;--scroll subs--
;--------------
scrollh
 lda $02       ;wrap?
 bne gowrap
 ldx COLOR     ;current foreground color
 lda #32       ;space char
 bne shiftchar ;always branches
gowrap
 lda paintbuf1 ;saved char
 ldx paintbuf2 ;saved color
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
 clc
 adc #$d8
 sta $fe
 rts 
;--------------
wrapit
 lda $02
 beq calcptr-1
 ldy $be
cpybuf lda ($fb),y
 sta paintbuf1,y ;char mem buffer
 lda ($fd),y
 sta paintbuf2,y ;color mem buffer
 dey
 bpl cpybuf
 rts
;calculate text and color RAM mem pointers
calcptr
 ldy $bf     ;line number
 lda $ecf0,y ;video matrix lowbyte at line y
 clc
 adc $fb     ;add offset to current ptr
 sta $fb     ;lobyte ptr to text RAM
 sta $fd     ;lobyte ptr to color RAM
 lda btab,y  ;video maxtrix hibyte offset
 adc HIBASE  ;video matrix hibyte
 sta $fc     ;hibyte ptr to text RAM
;determine hibyte of color RAM
 lda #$d8    ;hibyte of color RAM first byte
 sec         ;remove text ptr hibyte offset
 sbc HIBASE  ;since already included in text ptr
 clc         ;add text ptr hibyte offset
 adc $fc     ;offset from current text ptr hibyte
 sta $fe     ;apply hibyte of color RAM ptr
 rts
;
nextn0       ;start over from beginning
 lda #0
 sta playindex
nextn2
 jsr notegot
 beq nextn2-1
nextn
 cmp #"h"       ;notes A-G only
 bcc goodnote
 jmp nonnote
goodnote
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

 cmp #"#"       ;sharp?
 bne noshar
 jsr noteget    ;skip over txt
 lda snotes+1,x
 tay
 lda snotes,x
 jmp playnote   ;always branches

noshar
 cmp #"-"       ;flat?
 bne regnote
 jsr noteget    ;skip over txt
 lda fnotes+1,x
 tay
 lda fnotes,x
 jmp playnote   ;always branches

regnote
 lda notes+1,x
 tay
 lda notes,x

playnote
 jsr octadj
 ldx playvoice
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

plyrpt cmp #"@"-"a"
 beq nextn0

octchg
 cmp #"<"-"a" ;251 ;<
 bne octup2
prevoct
 dec playoct
 bpl nextn3
 bmi nextoct
octup2
 cmp #">"-"a" ;253  ;>
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
 jsr ppw2       ;get voice register offset
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
 beq digit9-1   ;end of string, assume 0 value
 sec            ;convert ascii digit to binary value
 sbc #"0"       ;first char must be digit between 0 and 9
 bpl digit9
nondigit
 lda #$ff       ;flag for non numeric digit
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
;
godraww
 stx $bb    ;draw length
 sty $bc
;
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
 dec $bb       ;decrement draw length
 bne dodraw2
 dec $bc
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
;
bitfil
 ldy #$01    ;signed y inc value
;calc number of lines to fill
 lda lastploty
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
 sta $02     ;num lines to fill
;calc line width and fill left or right
 lda #0      ;0-=draw right,1=draw left
 sta $ff     ;horizontal fill direction
 lda lastplotx
 sec
 sbc $fb
 tax
 lda lastplotx+1
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
;
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
 lda $14
 sta $bb
 lda $15
 sta $bc
;
 dec $02
 lda $02
 cmp #0
 bpl linefil
 jmp memnorm
;
;***OPEN RS-232 CHANNEL***
openrs232
 lda #126        ;file handle 126
 ldx #2          ;device 2 = RS-232
 ldy #0          ;secondary channel
 jsr SETLFS
 jsr $f30f       ;find the file in the logical file table
 bne xf359       ;zero flag=1 means file not currently open
 jsr $f6fe       ;handle FILE OPEN error
 tax             ;error code 2
 sec
 rts
xf359 ldx LDTND  ;number of open i/o files/index to the end of file tables
 cpx #10         ;limit 10
 bcc xf362
 jsr $f6fb       ;handle TOO MANY FILES error
 tax             ;error code 1
 sec
 rts
xf362 inc LDTND  ;inc total file handle count
 lda $b8         ;current logical file number
 sta LAT,x       ;store file descriptors into master table
 lda $b9         ;current secondary address
 ora #%01100000  ;flag bits 5&6 to indicate rs-232 channel
 sta $b9         ;needs special handling on close or error
 sta SAT,x
 lda $ba         ;current device number
 sta FAT,x
;***PREPARE RS-232 DEVICE WITH DEFAULTS***
 jsr $f483       ;init IRQ timers (y reg loaded with 0)
 sty RSSTAT      ;reset RS-232 Status
;bits 0-3 of M51CTR are used to set the baud rate as follows:
;0=User Defined (not supported here yet)
;1=50,2=75,3=110,4=134.5,5=150,6=300,7=600,8=1200,9=1800,10=2400,11=3600,12=4800,13=7200,14=9600,15=19200
 lda #%00001000  ;1200 baud, 8 data bits, 1 stop bit
 sta M51CTR      ;bits3-0=baud, bit4:unused, bits6-5=data bits, bit7=stop bits
 lda #%00000000  ;no parity, full duplex, 3-line handshake
 sta M51CDR      ;bits7-5:parity, bit4=duplex, bits3-1:unused, bit0=handshake
;if implementing user-defined baud rate, the value placed here would be
;TIMING = (CLOCK/(BAUDRATE/2))-100 in binary little endian format
;therefore the max baud rate for NTSC machines is 20454 and 19705 on PAL machines.
;however near maximum is not suggested and prone to errors;
;in fact, the IRQ facilitating the send and receive buffers cannot keep up beyond 2400.
 lda #0          ;user defined baud rate not supported by C64 Kernal
 sta M51AJB      ;lobyte of timing for user defined baud rate
 sta M51AJB+1    ;hibyte of timing for user defined baud rate
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
 beq badserial ;baud rate not found in table
 txa
 asl
 tay
 lda baudrates-2,y
 cmp $14
 bne nxtbaud
 lda baudrates-2+1,y
 cmp $15
 bne nxtbaud
 stx $02       ;index found
 lda M51CTR    ;current setting (from default)
 and #%11110000 ;remove current setting bits 0-3
 ora $02       ;apply selected baud rate from index
 sta M51CTR    ;set baud rate
 jsr CHRGOT
 bne getdb
opn232 jmp open232
getdb
 jsr ckcom2    ;check for and skip over comma, misop err if missing
;get data bits
 cmp #","      ;another comma?
 beq getstpbits
 jsr rom3      ;data bits (word length) 5,6,7 or 8
 bne badserial ;hibyte must be zero
 cmp #5
 bcc badserial
 cmp #9
 bcs badserial
;need bit pattern 00=8, 01=7, 10=6, 11=5
;use 2's compliment to negate then add 8
 eor #$ff
 adc #9        ;1 for 2's comp, 8 for offset
 and #%00000011 ;just want first 2 bits
 lsr           ;shift bit positions
 ror           ;from 0,1 to 6,5
 ror           ;using carry bit
 lsr           ;arriving at bits 6,5
 sta $02
 lda M51CTR
 and #%10011111 ;clear target bits 5 and 6
 ora $02
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
 sta $02
 lda M51CTR
 and #%01111111 ;clear target bit 7
 ora $02
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
 sta $02
 lda M51CDR
 and #%11101111 ;clear target bit 4
 ora $02
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
 sta $02
 lda M51CDR
 and #%00011111 ;clear target bits 5-7
 ora $02
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
 sta $02
 lda M51CDR
 and #%11111110 ;clear target bit 0
 ora $02
 sta M51CDR
;
;open the RS-232 channel
open232
 jsr $ef4a      ;get the word length for the current RS-232 character into x reg
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
;
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
;
hexx
 ldy #$00
 lda $62
 jsr dechex
 lda $63
 jsr dechex
 lda $64
 jsr dechex
 lda $65
 jsr dechex
 lda #$00
 sta BAD,y
;find most significant digit index
 ldy #0
nxzro
 lda BAD,y
 beq hexzro ;reached end of str so value is 0
 cmp #"0"   ;ignore leading zeros
 bne msd
 iny
 bne nxzro
hexzro dey
msd tya   ;A(lo) Y(hi) is ptr to zero-term str
 ldy #$01
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
 sta BAD,y
 iny
 pla
 and #$0f
 cmp #$0a
 bcc noadd7
 clc
 adc #$07
noadd7 clc
 adc #48
 sta BAD,y
 iny
 rts
;
findd
 ldy #0
 lda ($2b),y ;should be $0801
 sta $fb
 iny
 lda ($2b),y ;should be $0802
 sta $fc
 ora $fb
 beq findd-1
 iny
 lda ($2b),y ;should be $0803
 sta $39
 iny
 lda ($2b),y ;should be $0804
 sta $3a
 jsr CHRGOT
 beq findd-1 ;nothing to find
 cmp #"""
 bne noquo
 jsr CHRGET
noquo lda $2b
 clc
 adc #4
 sta $fd
 lda $2c
 adc #0
 sta $fe
check ldy #$00
 lda ($7a),y
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
 lda ($7a),y
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
founit lda $39
 sta $14
 lda $3a
 sta $15
 lda #$01
 sta listflag
 jsr findlnr  ;search for line#
 lda #$91     ;crsr up
 jsr CHROUT
 jsr STOP
 beq prgend
linend lda $fb
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
 sta $39
 iny
 lda ($fd),y
 sta $3a
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
;
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
 ldy #0
 jsr timedig
 lda TO2MIN
 jsr timedig
 lda TO2SEC
 jsr timedig
 lda TO2TEN ;reading resumes capture of time to these registers
 rts
;get time digit
timedig
 pha
 lsr
 lsr
 lsr
 lsr
 clc
 adc #"0"
 sta BAD,y
 iny
 pla
 and #15
 clc
 adc #"0"
 sta BAD,y
 iny
 lda #":"
 cpy #8
 bcc setterm
 lda #0
setterm
 sta BAD,y
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
 lda BAD+6
 sta BAD+10
 lda BAD+7
 sta BAD+11
;minutes
 lda BAD+3
 sta BAD+18
 lda BAD+4
 sta BAD+19
;hours
 lda BAD
 sta BAD+26
 lda BAD+1
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
;init TOD clocks
;determines what the TOD clock frequency should be (50 or 60 Hz)
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
 stx CI2CRA       ;start the timer to measure TOD clock #2
 lda TO2TEN       ;wait for the next change
t2 cmp TO2TEN     ;to take a timer measurement
 beq t2
;capture time for TOD to increment one tenth of a second
 ldx TI2AHI       ;only interested in hibyte value
;set both 6526 CIA chips TOD frequency 
 cpx #100         ;should be 112 for 60Hz, 50 for 50Hz
 bcc clk50
 lda #%01111111   ;clear bit 7 for 60Hz
 and CIACRA       ;apply to TOD #1 (TOD #2 already set)
 sta CIACRA
 rts
clk50
 lda #%10000000   ;set bit 7 for 50Hz
 ora CIACRA
 sta CIACRA       ;apply to TOD #1
 lda #%10000000
 ora CI2CRA
 sta CI2CRA       ;apply to TOD #2
 rts
;
;initialize BASIC vectors
newvec
;set colors customizable by POKEing the lda value
 lda #14        ;light blue
 sta COLOR      ;current text color
 lda #0         ;black
 sta BGCOL0     ;background color black
 lda #0         ;black
 sta EXTCOL     ;border color black
;
 lda #0
 sta traceflag  ;trace flag off
 sta keyflag    ;key trapping off
 jsr sidclr
 lda #$80       ;all keys to repeat
 sta RPTFLAG
 lda #<toknew
 sta $0304
 lda #>toknew
 sta $0305
 lda #<newfun
 sta $030a
 lda #>newfun
 sta $030b
 lda #<list
 sta $0306
 lda #>list
 sta $0307
 lda #<execut
 sta IGONE
 lda #>execut
 sta IGONE+1
 jsr detrap
 lda #<brkirq
 sta $0316
 lda #>brkirq
 sta $0317
 lda #<keychk
 sta $028f
 lda #>keychk
 sta $0290
 lda #<newload
 sta $0330
 lda #>newload
 sta $0331
 lda #<newsave
 sta $0332
 lda #>newsave
 sta $0333
 lda #$FF   ;MDBASIC takes 8k of the BASIC RAM area
 sta $37    ;the highest address is now $7FFF (32767)
 lda #$7F   ;$37-$38 holds value of highest address used by BASIC, originally $9FFF (40959)
 sta $38
 rts
;
;begin processing input stream
filess
 lda #$ff       ;start file count at -1 to not count footer
 sta $50
 sta $51
 jsr chkin7e
 bcs err7e
 jsr CHRIN      ;skip 2-byte file header
 bcs err7e
 jsr CHRIN
 jsr prtlin2    ;get and print directory header (label & id)
 bne chkeof
 lda #$92       ;RVS off
 jsr CHROUT
blocks
 jsr prtlin
 beq chkshft
chkeof
 and #%01000000 ;bit6=EOF/EOI, bit7=device not present, bits0-1 indicate device timeout
 bne filecnt    ;bit6=1? EOF, print file count
 lda #5         ;DEVICE NOT PRESENT
err7e jmp err7ee
chkshft lda #$01
shift2 
 bit SHFLAG     ;shift key?
 bne shift2     ;wait till released
 inc $50        ;increment file count
 bne nxtfile
 inc $51
nxtfile
 jsr STOP       ;stop key?
 bmi blocks
 bpl endfiles
filecnt
 ldx $50
 lda $51
 sta $62
 stx $63
 jsr rom4       ;print 2-byte binary number in FAC1
 lda #<filestr
 ldy #>filestr
 jsr printstr   ;print str whose addr is in y reg (hi byte) and a reg (lo byte)
endfiles jsr clse7e
 jmp memnorm
;
prtlin
 jsr chkin7e
 bcs err7e
prtlin2
 jsr CHRIN      ;skip 2 byte line header
 jsr CHRIN
 jsr CHRIN      ;read the 2-byte disk block size
 sta $63        ;and store result in FAC1
 jsr CHRIN      ;to later convert to string
 sta $62        ;for writing to output channel
 jsr READST     ;get last input status, 0=ok
 bne stdout0    ;stop now and switch to output channel if needed
 jsr stdout0    ;set current i/o to output channel
 bcs err7e
 jsr rom4       ;print 2-byte binary number in FAC1
 lda #" "
 jsr CHROUT
;print zero-terminated string in file then print CR
 jsr bufio      ;on return, carry set means error in i/o
 bcs err7e      ;either input or output failed
 pha            ;save status result of i/o
 jsr printcr    ;write carriage return to current output channel
 pla            ;restore status result
 rts
;
stdout0
 pha
 jsr stdout
 pla
 rts
stdout
 clc            ;clear carry used as error flag
 ldx $13        ;current redirected output channel
 beq stdout-1   ;0 indicates default so no need to CHKOUT
 jmp CHKOUT     ;restore original I/O channel, carry set on error
;
chkin7e
 ldx #$7e       ;current file number
 jmp CHKIN      ;designate a Logical file as the current input channel
;read with buffering from channel 126
;write to current output channel
bufio
 jsr chkin7e
 bcs rddone
 ldy #0
bufrd
 jsr READST    ;last read status
 bne bufwrite+3
 jsr CHRIN
 bcs rddone    ;carry indicates error, accumulator holds error#
 sta paintbuf1,y
 beq bufwrite
 iny
 bne bufrd
 jsr bufwrite  ;if more than 255 bytes then print it
 bcc bufio     ;read next 255 bytes
rddone rts     ;return with carry set indicating error
;write buffer up to 255 bytes
bufwrite
 iny
 bne zzz
 lda #0
 sta paintbuf1,y
zzz
 jsr READST    ;last read status
 sta $61       ;save for returning the read status
 jsr stdout    ;switch current i/o channel to output channel
 bcs rddone    ;carry indicates error, accumulator holds error#
 ldy #0
bufwr
 lda paintbuf1,y
 beq iodone    ;stop if zero-byte termination found
 jsr CHROUT
 bcs iodone1   ;stop if output error
 iny
 bne bufwr
 rts
 jsr stdout    ;switch current i/o channel to output channel
 bcs iodone1   ;carry indicates error, accumulator holds error#
iodone
 lda $61
iodone1 rts
;
inff
 tax
 beq phycol
 cmp #10
 bcc useinfbytes
 beq csraddr
 cmp #17
 beq membank
 bcs basline
 sec
 sbc #11
 asl
 tax
 lda infwords+1,x
 tay
 lda infwords,x
 jmp goodinf1
csraddr         ;get text address of current line
 ldy $d2        ;hibyte
 lda $d1        ;lobyte
 jmp int4
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
goinf
 ldy #0
int4
 sta $65   ;lobyte
 sty $64   ;hibyte
 lda #0
 sta $70
 sta $63
 sta $62
 lda #$a0
 sta $61
 rts
membank
 lda CI2PRA
 and #%00000011
 eor #%00000011
 jmp goinf
basline
 cmp #18
 bne dtaline
 ldy $3a
 lda $39
 jmp int4
dtaline
 ldy $40
 lda $3f
 jmp int4
;
deletee
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
 sta $7a
 stx $7b
 lda $24
 sec
 sbc $7a
 tax
 lda $25
 sbc $7b
 tay
 bcs deldone
 txa
 clc
 adc $2d
 sta $2d
 tya
 adc $2e
 sta $2e
 ldy #$00
copy lda ($7a),y
 sta ($24),y
 iny
 bne copy
 inc $7b
 inc $25
 lda $2e
 cmp $25
 bcs copy
deldone rts
;
worker
 lda $3c
 sta $7b
 lda $3b
 sta $7a
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
 sta ($7a),y
 inx
 bne numchr
less0 jsr CHRGET
 bcs workdone
dec2d jsr clrflg
 dec $97
work ldy $0b
 iny  
 lda ($22),y
 ldy $97
 iny
 sta ($22),y
 jsr pntreq
 beq zzzz
 inc $22
 bne work
 inc $23
 bne work
 jsr bufer
zzzz
 lda $2d
 bne ne2d
 dec $2e
ne2d dec $2d
 jsr CHRGOT
 bcc dec2d
workdone
 jmp memnorm
;
.end
