; ***MDBASIC***
; by Mark D Bowren
; (c)1985-2021 Bowren Consulting, Inc. (www.bowren.com)
;
;TODO add the following commands/functions:
;ON SPRITE GOSUB line#      (sprite-sprite collision)
;ON SPRITE TEXT GOSUB line# (sprite-text collision)
;P = PLOT(x,y)              (get point on BITMAP screen; result P: 0=clear, 1=set)
;SORT(array_var_name)       (sort a single dimensioned array in ascending order)
;IDEA: consider making the PLAY command take a string array of length 1, 2 or 3 then play together on 1, 2 or 3 voices
;
COLOR  = $0286 ;Current Foreground Color for Text
HIBASE = $0288 ;(648) Top Page of Screen Memory
SHFLAG = $028d ;SHIFT/CTRL/Logo Keypress flags Bit0 SHIFT, Bit1 Commodore Logo Key, Bit2 Ctrl Key

;Memory-Mapped I/O Registers
SP0X   = $d000 ;Sprite 0 Horizontal Position
SP0Y   = $d001 ;Sprite 0 Vertical Position
MSIGX  = $d010 ;Most Significant Bits of Sprites 0-7 Horizontal Position
SCROLY = $d011 ;Vertical Fine Scrolling and Control Register
;
;Bits 0-2 Fine scroll display vertically by X scan lines (0-7)
;Bit 3 Select a 24-row or 25-row text display (1=25 rows, 0=24 rows)
;Bit 4 Blank the entire screen to the same color as the background (0=blank)
;Bit 5 Enable bitmap graphics mode (1=enable)
;Bit 6 Enable extended color text mode (1=enable)
;Bit 7 High bit (Bit 8) of raster compare register at 53266 ($D012)
;
LPENX  = $d013 ;Light Pen Horizontal Position (0-160) must by multiplied by 2
LPENY  = $d014 ;Light Pen Vertical Position (0-200) corresponds exactly to the current raster scan line.
SPENA  = $d015 ;Sprite Enable Register - Bit 0  Enable Sprite 0 (1=sprite is on, 0=sprite is off)
SCROLX = $d016 ;Horizontal Fine Scrolling and Multicolor Control Register
;
; Bits 0-2 Fine scroll display horizontally by X dot positions (0-7)
; Bit 3 Select a 38-column or 40-column text display (1=40 columns, 0=38 columns)
; Bit 4 Enable multicolor text or multicolor bitmap mode (1=multicolor on, 0=multicolor off)
; Bit 5 Video chip reset (0=normal operations, 1=video completely off)
; Bits 6-7 Unused
;

YXPAND = $d017 ;Sprite Vertical Expansion Register - Bit 0-7 is Sprite 0-7 flag to expand sprite n vertically (1=double height, 0=normal height)

VMCSB  = $d018 ;VIC-II Chip Memory Control Register
;
; Bit 0 Unused,
; Bits 1-3 Text character dot-data base address within VIC-II 16K addressable memory.
;    The default is %100 (4) = 4 * 1K = $1000 (4096) the address of the Character Dot-Data area in ROM.
;    The uppercase characters are the first 2K.  The alternate character set which contains both
;    upper and lowercase characters are in the second 2K.  To shift to the alternate
;    character set, you must change the value of this nybble to %110 (6) = 6 * 1K = $1800 (6144).
;
; Bits 4-7 Video matrix base address within VIC-II 16K addressable memory
;    The default is %0001 (1) = 1 * 1K = $0400 (1024).
;    Select which 1024-byte area of memory will contain the screen codes for characters on screen.
;    The last eight bytes of this 1K area are used as pointers to select the 64-byte block of memory
;    for each sprite.
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
POTX   = $d419 ;Read Game Paddle 1 or 3 Position
;POTY   = $d41a ;54298 Read Game Paddle 2 or 4 Position

;Complex Interface Adapter (CIA) #1 Registers ($DC00-$DC0F)
CIAPRA = $dc00 ;Data Port Register A
CIAPRB = $dc01 ;Data Port Register B
CIDDRA = $dc02 ;Data Direction Register A

;Complex Interface Adapter (CIA) #2 Registers ($DD00-$DD0F)
CI2PRA = $dd00 ;Data Port Register A
;
;Bits 0-1 Select VIC-II 16K addressable memory bank (0-3)
;  00 Bank 3 (49152-65535, $C000-$FFFF) 16K RAM / Memory mapped I/O, character ROM, 4K Kernel
;  01 Bank 2 (32768-49151, $8000-$BFFF) 16K RAM / BASIC text, 8K BASIC interpreter ROM
;  10 Bank 1 (16384-32767, $4000-$7FFF) 16K RAM / BASIC text
;  11 Bank 0 (    0-16383, $0   -$3FFF) 16K RAM / system variables, screen RAM, BASIC text
;*See zero-page memory location $01 bits 0 & 1 for RAM/ROM switching
;
C2DDRA = $dd02 ;Data Direction Register for port A (CI2PRA)
CI2ICR = $dd0d ;Interrupt Control Register

;CBM command line functions
CHRGET = $0073 ;Get Next BASIC Text Character
CHRGOT = $0079 ;Get Current BASIC Text Character
KEYD   = $0277 ;Keyboard Buffer (Queue)
IMAIN  = $0302 ;Vector to the Main BASIC Program Loop

;CBM BASIC functions
GETSTK = $a3fb ;check for space on stack
READY  = $a474 ;print READY
REASON = $a408 ;check for space in memory
LINKPRG= $a533 ;relink lines of tokenized program text
INLIN  = $a560 ;input a line to buffer from keyboard (max 88 chars)
FINDLN = $a613 ;search for line number using ptr at $2b, $2c
CLEAR  = $a65e ;perform CLR
RUNC   = $a68e ;reset ptr of current text char to the beginning of prg text
NEWSTT = $a7ae ;setup next statement for execution
RUN    = $a871 ;peform RUN
GOTO   = $a8a0 ;perform GOTO
DATAN  = $a906 ;search BASIC text for the end of the current statement
ONGOTO = $a94b ;perform ON
LINGET = $a96b ;convert an ASCII decimal number to a 2-byte binary line number
STROUT = $ab1e ;print msg from str whose addr is in the Y (hi byte) and A (lo byte) registers
FRMNUM = $ad8a ;evaluate a numeric expression and/or check for data type mismatch, store result in FAC1
FRMNUM2= $ad8d ;validate numeric data type in FAC1
FRMEVL = $ad9e ;evaluate expression
PARCHK = $aef1 ;eval expr inside parentheses
CHKCLS = $aef7 ;check for and skip closing parentheses
CHKOPN = $aefa ;check for and skip opening parentheses 
CHKCOM = $aeff ;check for and skip comma
PTRGET = $b08b ;search for a variable & setup if not found
AYINT  = $b1bf ;convert FAC1 to a signed integer in FAC1
GIVAYF = $b391 ;convert 16-bit signed integer to floating point (a=hibyte y=lobyte)
ERRDIR = $b3a6 ;check if prg is running in direct mode/cause error
STRLIT = $b487 ;scan and setup pointers to a string in memory
GETSPA = $b4f4 ;alloc space in mem for string
FRESTR = $b6a3 ;discard a temporary string
GETBYTC= $b79b ;input a parameter whose value is between 0 and 255
GETADR = $b7f7 ;convert FAC1 to unsigned 16-bit integer; result in $14 lobyte, $15 hibyte
OVERR  = $b97e ;print overflow error message
FMULT  = $ba28 ;multiply FAC1 by value in memory pointed to by A (lobyte) and Y (hibyte) registers
FDIVT  = $bb12 ;divide FAC2 by FAC1 FAC1 = (FAC2/FAC1)
MOVFM  = $bba2 ;move a 5-byte floating point number from memory to FAC1, ptr = A=lobyte, Y=hibyte
MOV2F  = $bbca ;move a 5-byte floating point number from FAC1 to memory $57-$5B BASIC numeric work area
MOVEF  = $bc0f ;copy FAC1 to FAC2 without rounding
ROUND  = $bc1b ;round FAC1 to whole number
INT    = $bccc ;perform INT
FINLOG = $bd7e ;add signed integer to FAC1
LINPRT = $bdcd ;print 2-byte number stored in A (hibyte), X (lobyte)
FOUT   = $bddd ;convert contents of FAC1 to ASCII String

;CBM BASIC error message display functions
SNERR  = $af08 ;syntax error
BSERR  = $b245 ;bad subscript error
FCERR  = $b248 ;illegal quanity

;Commodore 64 Kernal functions
LP2    = $e5b4 ;get a character from the keyboard buffer
RESET  = $fce2 ;power-on reset
SETMSG = $ff90 ;set kernal msg ctrl flag
TKSA   = $ff96 ;send secondary address after TALK
ACPTR  = $ffa5 ;input byte from serial bus
TALK   = $ffb4 ;command serial bus device to TALK
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

; 
;Commodore 64 BASIC commands
;Token #   Statement  Routine Address   
;128 $80   END        43057 $A831
;129 $81   FOR        42818 $A742
;130 $82   NEXT       44318 $AD1E
;131 $83   DATA       43256 $A8F8
;132 $84   INPUT#     43941 $ABA5
;133 $85   INPUT      43967 $ABBF
;134 $86   DIM        45185 $B081
;135 $87   READ       44038 $AC06
;136 $88   LET        43429 $A9A5
;137 $89   GOTO       43168 $A8A0
;138 $8A   RUN        43121 $A871 *overridden by MDBasic
;139 $8B   IF         43304 $A928 *overridden by MDBasic
;140 $8C   RESTORE    43037 $A81D *overridden by MDBasic
;141 $8D   GOSUB      43139 $A883
;142 $8E   RETURN     43218 $A8D2
;143 $8F   REM        43323 $A93B
;144 $90   STOP       43055 $A82F
;145 $91   ON         43339 $A94B *overridden by MDBasic
;146 $92   WAIT       47149 $B82D
;147 $93   LOAD       57704 $E168
;148 $94   SAVE       57686 $E156
;149 $95   VERIFY     57701 $E165
;150 $96   DEF        46003 $B3B3
;151 $97   POKE       47140 $B824
;152 $98   PRINT#     43648 $AA80
;153 $99   PRINT      43680 $AAA0
;154 $9A   CONT       43095 $A857
;155 $9B   LIST       42652 $A69C
;156 $9C   CLR        42590 $A65E
;157 $9D   CMD        43654 $AA86
;158 $9E   SYS        57642 $E12A
;159 $9F   OPEN       57790 $E1BE
;160 $A0   CLOSE      57799 $E1C7
;161 $A1   GET        43899 $AB7B
;162 $A2   NEW        42562 $A642
;
;Commodore 64 BASIC keywords
;Token #    Keyword
;163 $A3    TAB(
;164 $A4    TO
;165 $A5    FN
;166 $A6    SPC(
;167 $A7    THEN
;168 $A8    NOT
;169 $A9    STEP
;
;Commodore 64 BASIC Operators
;Token #   Operator           Routine Address
;170 $AA   + (ADD)            47210 $B86A
;171 $AB   - (SUBTRACT)       47187 $B853
;172 $AC   * (MULTIPLY)       47659 $BA2B
;173 $AD   / (DIVIDE)         47890 $BB12
;174 $AE   ^ (EXPONENTIATE)   49019 $BF7B
;175 $AF   AND (LOGICAL AND)  45033 $AFE9
;176 $B0   OR (LOGICAL OR)    45030 $AFE6
;177 $B1   > (GREATER THAN)   49076 $BFB4
;178 $B2   = (EQUAL TO)       44756 $AED4
;179 $B3   < (LESS THAN)      45078 $B016
;
;Commodore 64 BASIC Functions
;Token #   Function  Routine Address
;180 $B4   SGN       48185 $BC39
;181 $B5   INT       48332 $BCCC
;182 $B6   ABS       48216 $BC58
;183 $B7   USR         784 $0310
;184 $B8   FRE       45949 $B37D
;185 $B9   POS       45982 $B39E
;186 $BA   SQR       49009 $BF71
;187 $BB   RND       57495 $E097
;188 $BC   LOG       45794 $B9EA
;189 $BD   EXP       49133 $BFED
;180 $BE   COS       57956 $E264
;191 $BF   SIN       57963 $E26B
;192 $C0   TAN       58036 $E2B4
;193 $C1   ATN       58126 $E30E
;194 $C2   PEEK      47117 $B80D
;195 $C3   LEN       46972 $B77C
;196 $C4   STR$      46181 $B465
;197 $C5   VAL       47021 $B7AD
;198 $C6   ASC       46987 $B78B
;199 $C7   CHR$      46828 $B6EC
;200 $C8   LEFT$     46848 $B700
;201 $C9   RIGHT$    46892 $B72C
;202 $CA   MID$      46903 $B737
;203 $CB   GO        *This is not a statement or function. It allows GO TO instead of GOTO
;
;MDBASIC Keywords
;203 $CB   OFF       *Stole this token from CBM's GO token (obsolete)
;204 $CC   ELSE
;MDBASIC Command Tokens
;204 $CD   MERGE
;.
;.
;250 $F8   ERROR
;
;MDBASIC Function Tokens
;249 $F9   CSR
;250 $FA   PEN
;251 $FB   JOY
;252 $FC   POT
;253 $FD   HEX$
;254 $FE   INSTR
;CBM PI Token
;255 $FF   PI
;

TOKEN_NEXT    = $82
TOKEN_INPUT_  = $84
TOKEN_INPUT   = $85
TOKEN_DIM     = $86
TOKEN_GOTO    = $89
TOKEN_RUN     = $8a
TOKEN_RESTORE = $8c
TOKEN_GOSUB   = $8d
TOKEN_STOP    = $90
TOKEN_ON      = $91
TOKEN_PRINT   = $99
TOKEN_LIST    = $9b
TOKEN_CLR     = $9c
TOKEN_NEW     = $a2
TOKEN_TO      = $a4
TOKEN_THEN    = $a7

FIRST_CMD_TOK = $cb  ;first MDBASIC token
TOKEN_OFF     = $cb  ;OFF keyword token
TOKEN_ELSE    = $cc
TOKEN_VARS    = $cf
TOKEN_COLOR   = $d8
TOKEN_SPRITE  = $da
TOKEN_BITMAP  = $df
TOKEN_TEXT    = $e6
TOKEN_SCREEN  = $e7
TOKEN_RESUME  = $e8
TOKEN_SOUND   = $eb
FIRST_FUN_TOK = $f5  ;first MDBASIC function
TOKEN_KEY     = $f6
TOKEN_ERROR   = $f7
TOKEN_PI      = $ff  ;PI symbol token

*=$8000 ;"MDBASIC RAM Memory Block"

;cartridge identifier
.word resvec,runstp        ;new reset and runstop vectors
.byte $c3,$c2,$cd,$38,$30  ;necessary for cartridge indicator
;
mesge .byte 147
.text "mdbasic 21.12.05"
.byte 13
.text "(c)1985-2021 mark bowren"
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
.shift "delay"
.shift "files"
.shift "color"
.shift "move"
.shift "sprite"
.shift "multi"
.shift "expand"
.shift "reset"
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
.shift "sound"
.shift "pulse"
.shift "vol"
.shift "filter"
.shift "play"
.shift "auto"
.shift "old"
.shift "trace"
.shift "find"
.shift "delete"
;functions
.shift "round"
;statement and a function
.shift "key"
.shift "error"
;functions only CSR PEN JOY POT HEX$ INSTR
.shift "ptr"
.shift "csr"
.shift "pen"
.shift "joy"
.shift "pot"
.shift "hex$"
.shift "instr"
.byte 0 ;needed terminator
;
;mdbasic command tokens starting at FIRST_CMD_TOK
cmdtab
;keywords
.rta SNERR ;$cb token - placeholder for OFF token (has no address to call)
.rta else  ;$cc token - using this cmd by itself behaves like the REM statement
;commands
.rta merge,  dump,   vars,    circle, fill           ;$d1 token
.rta scroll, swap,   locate,  disk,   delay,   files ;$d7
.rta color,  move,   sprite,  multi,  expand,  RESET ;$dd
.rta design, bitmap, mapcol,  plot,   line,    paint ;$e3
.rta draw,   renum,  text,    screen, resume,  adsr  ;$e9
.rta wave,   sound,  pulse,   vol,    filter,  play  ;$ef
.rta auto,   old,    trace,   find,   delete         ;$f4
;funcs & cmds
.rta SNERR                        ;$f5 placeholder for round (not a command, func only)
.rta key,    error                ;$f6,$f7 are cmds and funcs
funtab
.word round                       ;$f5 (this entry not used by executor)
.word _key, err                   ;$f6,$f7 are both a command and a function
.word ptr,csr, pen, joy, pot, hex ;$f8,$f9,$fa,$fb,$fc,$fd
;note: instr ($fe this entry not used by executor)
;token $ff is reserved for PI
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
illfun .shift "illegal function call" ;34
cantre .shift "can't resume"          ;35
;
erradd .word misop, ilvne, illspr, illfun, cantre
;
newvec jsr nwvec2
 lda #<mesge
 ldy #>mesge
 jmp STROUT 
nwvec2 lda #0
 sta EXTCOL     ;border color black
 sta BGCOL0     ;background color black
 sta traceflag  ;trace flag off
 sta keyflag    ;key trapping off
 ldy #15
noblink sta blinkcol,y   ;all colors turn off flash flag
 dey
 bpl noblink
 jsr sidclr
 lda #$80       ;value to enable all keys to repeat
 sta $028A      ;which keys will repeat 0=only cursor, insert, delete and spacebar keys, 64 = no keys, 128=all keys
 lda #14        ;light blue
 sta 646        ;current foreground color for text
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
 lda #<exccmd
 sta $0308
 lda #>exccmd
 sta $0309
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
 jmp CLEAR  ;perform CLR
;
;Program Tokenization process - text to tokens via vector ($0304)
;
toknew ldx $7a
 ldy #$04
 sty $0f
nxtchr lda $0200,x
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
tstnxt lda $0200,x
 sec
 sbc newcmd,y
 beq cmplop
 cmp #$80      ;last letter?
 bne nxtcmd
 ora $0b
tachr1 ldy $71
takchr inx
 iny
 sta $01fb,y
 lda $01fb,y
 beq end
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
remlop lda $0200,x
 beq takchr
 cmp $08
 beq takchr
getchr iny
 sta $01fb,y
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
notfou lda $0200,x
 bpl tachr1
end sta $01fd,y
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
oldtst lda $0200,x
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
 lda $a09d,y
 bpl cont1
 lda RESLST,y
 bne oldtst
 beq notfou
;
nextstmt
 ldy #0
 lda ($7a),y
 bne _a807
 ldy #2
 lda ($7a),y
 clc
 bne _a7ce
 jmp $a84b  ;return control to main BASIC loop
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
_a7e1 rts
_a807 cmp #$3a
 beq _a7e1
 jmp SNERR
;
;after ON KEY RETURN re-enable key trapping
onkey1
 lda keyflag   ;if key trapping turned off manually during subroutine
 beq nocmd     ;then no need to switch pause to on
 dec keyflag   ;otherwise switch from paused (2) to on (1)
nocmd
 jmp NEWSTT    ;find beginning of next statement and execute
;
;Evaluate tokens via vector ($0308)
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
 lda $c6       ;num chars in keyboard buffer
 beq nocmd
 jsr LP2       ;$E5B4 get char in keyboard buffer
 sta keyentry  ;use K=KEY(0) to get value
 inc keyflag   ;pause key trapping
 lda #3
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
 jmp NEWSTT     ;find beginning of next statement and execute
;
tstcmd
 cmp #FIRST_CMD_TOK
 bcs oknew      ;if token is greater than or equal to FIRST_CMD_TOK then MDBASIC
 cmp #TOKEN_RESTORE
 bne notrestore
 jmp restor
notrestore
 bcs oldcmd
 cmp #TOKEN_RUN
 beq do_run
 bcc oldcmd2
 jmp if
do_run
 jsr detrap     ;turn off error trapping incase it was enabled in previous run
 lda #0
 sta keyflag    ;ensure key trapping is off
 jsr CHRGOT
 sec
 jmp newrun
oldcmd
 cmp #TOKEN_ON
 bne oldcmd2
 jmp on
oldcmd2
 jsr CHRGOT
 sec
 jmp $a7ed      ;execute CBM statement
oknew
 sbc #FIRST_CMD_TOK ;first mdbasic cmd token for index calc start at 0
 asl            ;index * 2 for word pointer indexing
 tax
 lda cmdtab+1,x ;hibyte
 pha
 lda cmdtab,x   ;lobyte
 pha
 jmp CHRGET
;
;evaluate inline octal value denoted by @
octal jsr clrfac
nexto jsr CHRGET
 sec
 sbc #"0"
 bmi end1
 cmp #8
 bcs end1
 pha
 lda $61    ;exponent
 beq zero3
 adc #3     ;increase by 2^3 = 8
 sta $61
 beq over
zero3 pla
 beq nexto
 jsr FINLOG
 jmp nexto
;
;Evalutate functions via vector IEVAL ($030A) originaly pointing to EVAL $AE86
;
pi jmp $ae9e        ;move value of PI into FAC1
newfun lda #$00   ;0=number, 255=string - all funcs take a one numeric parameter
 sta $0d          ;Flag Type of Data (String or Numeric) to enforce data type
 jsr CHRGET
 cmp #"@"
 beq octal        ;octal value
 bcs funtok
 cmp #"%"         ;binary value literal?
 beq binary
 cmp #"$"         ;hex value literal?
 beq hexa
oldfun jsr CHRGOT ;process func as usual
 jmp $ae8d        ;execute original CBM basic func
funtok
 cmp #FIRST_FUN_TOK  ;CBM basic max token for functions?
 bcc oldfun       ;original basic func token
 beq rounder      ;perform ROUND
 cmp #$FE         ;$FE=INSTR, $FF=PI token
 beq instr1
 bcs pi
 sec        ;prepare for subtraction operation (set carry flag)
 sbc #FIRST_FUN_TOK  ;calc index for first mdbasic func starting at 0
 asl              ;index * 2 for word pointer indexing
 pha              ;save index on stack
 jsr CHRGET       ;process next cmd text
 jsr PARCHK       ;get term inside parentheses
 pla              ;retreive func index from stack
 tay              ;prepare for direct indexing
 lda funtab,y     ;lobyte value of address for function
 sta $55          ;lobyte for indirect addressing
 lda funtab+1,y   ;hibyte value of address for function
 sta $56          ;hibyte for indirect addressing
 jsr $0054        ;execute function
end1 jmp FRMNUM2  ;ensure numeric expression in FAC1, error if not
instr1 jmp instr  ;perform INSTR
rounder jmp round ;perform ROUND
over jmp OVERR
;clear $5d-$60 work area and $61-$66 FAC1 
clrfac lda #0
 ldx #10
loop sta $5d,x
 dex
 bpl loop
 rts
;----------------
;evaluate inline binary value denoted by %
binary jsr clrfac
nextb jsr CHRGET
 sec
 sbc #"0"
 bmi end1
 cmp #2
 bcs end1
 pha
 lda $61    ;exponent
 beq zero2
 inc $61    ;increase by 2^1 = 2
 beq over
zero2 pla
 beq nextb
 jsr FINLOG
 jmp nextb
;----------------
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
 pha
 lda $61    ;exponent
 beq zero
 clc
 adc #4     ;increase by 2^4 = 16
 bcs over
 sta $61
zero pla
 beq nexth
 jsr FINLOG ;add signed int to FAC1
 jmp nexth
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
 bit SHFLAG   ;shift flag 1=shift key, 2=commodore key, 4=ctrl key
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
; sec
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
 jsr $ab47    ;output char
 jmp found    ;next char
oldend jmp $a6ef ;list old
;
;**************************************
; if statement re-write to support else
;**************************************
if jsr CHRGET
 jsr FRMEVL
 jsr CHRGOT
 cmp #TOKEN_GOTO ;GOTO token? syntax IF X=1 GOTO 10
 beq condit
 lda #TOKEN_THEN ;THEN token
 jsr CHKCOM      ;check for and skip over THEN, error if not there
condit
 lda $61         ;expression result 0=false, otherwise true
 bne istrue      ;non-zero means expression is true
 jsr DATAN+3     ;find end of current BASIC statement
 beq isfalse     ;now check for ELSE
istrue
 jsr CHRGOT      ;check current char is numeric digit
 bcs endlin
goto jmp GOTO       ;preform goto
isfalse jsr CHRGOT  ;check next stmt
garbag cmp #$00   ;end of statement (no else)
 beq endlin       ;we are done
 cmp #TOKEN_ELSE  ;else token
 beq elser        ;go to the ELSE expression
 jsr CHRGET       ;otherwise position for next char in line
 jmp garbag       ;process next stmt
elser jsr CHRGET  ;process ELSE stmt
 bcc goto
endlin jmp tstcmd
else jmp $a93b    ;perform REM (ignore rest of line, skip to next stmt)
;
vars dec $01
 jmp varss
;
;FILES {string expression optional)
files
 bne strng
all0
 ldx #<direc    ;address of string
 ldy #>direc    ;for "$"
 lda #1         ;string len = 1 char
 bne setnaminf  ;always branches
strng jsr getstr0
 beq all0       ;zero length string is same as no string
 lda $50        ;insert $ symbol at first char in string
 bne skpd51     ;backup 1 char
 dec $51        ;of string by using ptr-1
skpd51 dec $50  ;TODO this is bad. writing byte outside string alloc!!
 ldy #0
 lda #"$"
 sta ($50),y    ;ensure first char of string is $
 inc $52        ;force string length to be 1 more byte TODO bad!!!
 lda $52        ;str len
 ldx $50        ;str ptr lobyte
 ldy $51        ;str ptr hibyte
setnaminf jsr SETNAM
 lda #$7f       ;file handle 127
 ldx #$08       ;device 8
 ldy #$00       ;secondary 0
 jsr SETLFS
 jsr OPEN       ;perform OPEN 127,8,0,S$
 bcc noerr
 jmp clos7f
noerr
 lda #$FF       ;start file count at -1 to not count footer
 sta $50
 sta $51
 ldx $b8        ;current file number
 jsr CHKIN      ;designate a Logical file as the current input channel
 jsr CHRIN      ;skip 2-byte file header
 jsr CHRIN
 jsr prtlin     ;get and print directory header (label & id)
 bne chkeof
 lda #$92       ;RVS off
 jsr CHROUT
blocks
 jsr prtlin
 beq chkshft
chkeof and #64  ;end of file?
 bne filecnt
 jsr clse7f
 ldx #4         ;file not found
 jmp ($0300)    ;raise error
chkshft  lda #$01
shift2 bit SHFLAG ;shift key?
 bne shift2
 inc $50
 bne nxtfile
 inc $51
nxtfile lda $91   ;stop key?
 bmi blocks
 bpl clse7f
filecnt
 ldx $50
 lda $51
 jsr LINPRT       ;print 2-byte binary number in FAC1
 lda #<filestr
 ldy #>filestr
 jsr STROUT       ;print str whose addr is in y reg (hi byte) and a reg (lo byte)
clse7f lda $b8
 jsr CLOSE        ;close file # in A register
 ldx #0           ;designate default output channel (0 = screen)
 jsr CHKOUT       ;$b8 = current std out file number, 0=default (screen)
 clc
 rts
prtlin
 jsr CHRIN        ;skip 2 byte line header
 jsr CHRIN
 lda $90          ;Kernal I/O Status Word (ST)
 bne prtdone
 jsr CHRIN        ;get 2-byte block size
 sta $63          ;and store in FAC1
 jsr CHRIN        ;to later convert to string
 sta $62          ;and print to screen
 jsr LINPRT+4     ;$bdd1 output 2-byte binary number in FAC1 to screen
 lda #" "
fprint jsr CHROUT
 jsr CHRIN        ;print zero-terminated string in file
 bne fprint
 lda #$0d         ;CR
 jsr CHROUT
 lda $90
prtdone rts
;
;*******************
;
;Open MDBASIC file handle for printer
openprint00 ldy #$00   ;secondary parameter $FF=not used, 5=binary graphic, 7=upper/lower case chars, 0=Upper case and symbol chars
openprint lda #$7f     ;file handle 127
 ldx #$04     ;device 4
 jsr SETLFS   ;set logical file parameters BASIC eq open 127,4,0
 lda #$00     ;zero byte file name length (no name)
 jsr SETNAM   ;set file name
 jsr OPEN     ;perform OPEN 127,4,0,""
 bcc prtopen  ;clear carry flag means success
 tax          ;save a reg
 pla          ;do not return to caller
 pla
 txa          ;remember a reg
 jmp clos7f   ;close and report error
prtopen ldx #$7f ;pass file handle param into CHKOUT via x reg
 jmp CHKOUT ;redirect std output to current open file, goes to $f250
;
;The DUMP command supports multiple options based on a second required token (or expression)
;DUMP LIST
;DUMP SCREEN
;DUMP BITMAP
;DUMP VARS
;DUMP {expression}
dump
 cmp #TOKEN_LIST   ;list token?
 beq dumplist
 cmp #TOKEN_SCREEN ;screen token?
 beq dumpscreen
 cmp #TOKEN_BITMAP ;bitmap token?
 beq dumpbitmap
 cmp #TOKEN_VARS   ;vars token
 bne dumpexpr
dumpvars jsr openprint00
 jsr vars
 jmp clse7f
dumpexpr jsr openprint00
 jsr $aa9d   ;perform print of expression
 jmp clse7f  ;close file handle 127
dumplist jsr openprint00
 lda #$01
 sta listflag
 jsr opget   ;calls CHRGET first thing!
 jsr $a6c9   ;perform list
 lda #$0d    ;carriage return?
 jsr CHROUT  ;print it
; jsr SETMSG   ;SETMSG set kernal message control flag
; jsr $aad7   ;perform print
 jmp clse7f
dumpscreen dec $01 ;switch to LORAM ($a000-$bfff)
 jsr dumpscreen2
closer jsr r6510   ;switch to LOROM ($a000-$bfff)
 jsr clse7f
 jmp CHRGET
dumpbitmap dec $01 ;switch LOROM to LORAM
 jsr dumpbitmap2
 jmp closer
;*******************
; FILL x1,y TO x2,y2, scanCode, [color]
fill
 lda HIBASE   ;top page of screen mem
 sta $fc
 jsr skip73   ;x1 (0-39)
 cmp #40
 bcc okx1
illqty jmp FCERR  ;display illegal qty error
okx1 sta $fb
 sta $be
 jsr getval   ;y1 (0-24)
 cmp #25      ;max is 25 so if greater than error
 bcs illqty
 sta $bf
 tay
 beq colbas
 lda $fb
 ldx $fc
 jsr times40
 sta $fb
 stx $fc
colbas jsr addoffset
 jsr CHRGOT
 cmp #TOKEN_TO
 beq getx2
 jmp SNERR
getx2 jsr getval ;x2
 cmp #40
 bcs illqty
 sec
 sbc $be    ;x2-x1
 sta $be
 jsr getval ;y2
 cmp #25
 bcs illqty
 sec
 sbc $bf   ;y2-y1
 sta $bf
 jsr comck2
 beq srncol
 jsr pokchr
 jsr chkcomm
srncol jsr getval
 sta $02
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
pokchr jsr skip73
 sta $bb
 ldx $bf
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
 rts
;
;*******************
old lda #$08
 sta $0802
 jsr LINKPRG
 clc
 lda $22   ;apply calculated end-of-prg pointer
 adc #$02
 sta $2d
 lda $23
 adc #$00
 sta $2e
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
 sta md418   ;maintain global var value for reading
 sta SIGVOL  ;SID register lower nibble is volume, upper nibble is filter type
 rts
;
;*******************
; DELAY n   where n=0-65535 jiffies
delay
 jsr skp73_  ;get 2-byte int in x (lobyte) and y (hibyte)
delay2       ;entry point for internal use; set x and y reg accordingly
 clc         ;flag for STOP key
 txa
 bne dec14
dec15 tya
 beq stopnow
 dey
dec14 dex
 lda $a2      ;jiffy clock updated 60 times per sec.
dlay2 cmp $a2
 beq dlay2
 lda $c5      ;Matrix Coordinate of Last Key Pressed, 64=None Pressed
 cmp #$3f     ;STOP key?
 bne delay2   ;carry flag will be returned to caller to indicate STOP key pressed
stopnow rts
;
;*******************
; AUTO   (no params) uses last used setting, default is 10
; AUTO OFF, AUTO n  where n=1 to 1023 
auto
 beq applyauto
 cmp #TOKEN_OFF
 beq autooff
 cmp #TOKEN_ON
 bne autoset
 jsr CHRGET
 jmp applyauto
autooff lda #$83
 sta IMAIN
 lda #$a4
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
getnum lda $00ff,y ;Work Area for Floating Point to String Conversions
 beq endnum
 sta KEYD-1,y   ;put char in keyboard buffer
 iny
 bne getnum
endnum lda #32 ;space char
 sta KEYD-1,y
 sty $c6
eauto jmp $a49f
;
;*******************
trace pha ;save CHRGET value
 lda #$00
 sta $a5
 lda #$ff
 sta $f9
 sta $fa
 sta traceflag
 lda #$00
 sta $9d
 pla     ;restore CHARGET value
 jmp RUN
trace1 lda $9d ;prg mode?
 beq trace2
 lda #$00
 sta traceflag
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
 lda #$20
clrtop sta $0400,y
 dey
 bpl clrtop
 lda $d3
 pha
 lda $d6
 pha
 jsr weglst
 pla
 tax
 pla
 tay
 clc
 jsr PLOT
shftky jsr STOP
 beq etrace
 lda #$01
 bit SHFLAG
 beq shftky
etrace rts
weglst lda #$01
 sta listflag
 jsr FINDLN
 ldy #$01
 lda ($5f),y
 beq endtrc
 jsr $a82c  ;Test STOP Key for Break in Program
 lda #$13
 jsr $aad9
 jmp $a6d7
endtrc jmp $a714
;
;*******************
newrun beq oldrun
 jsr CHRGET
 cmp #"""       ;literal string?
 beq lodrun
 cmp #"a"       ;variable?
 bcs lodrun
 cmp #0         ;prepare for original RUN with line num
oldrun jmp RUN  ;CBM BASIC - perform RUN
lodrun lda #$00 ;prepare for load
 sta $0a        ;load or verify? 0=load, 1=verify
 jsr $e1d4      ;set parms for LOAD, VERIFY, and SAVE
 jsr RUNC       ;reset ptr to current text char to the beginning of program text
 jsr $e16f      ;perform load
 jsr old
 lda #>NEWSTT-1 ;vector to $a7ae on stack for rts
 pha            ;setup next statement for execution
 lda #<NEWSTT-1
 pha
runit jmp $a659
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
 bcs brkerr
 jsr $ffb7   ;read i/o status word
 and #$bf
 beq okmerg
 ldx #29     ;load error
 jmp ($0300) ;error
okmerg stx $2d
 sty $2e
 jsr $a659   ;clear all variables
 jsr LINKPRG ;relink lines of tokenized prg text
 jmp READY   ;main basic loop
brkerr jmp $e0f9 ;kernal i/o routines
;
;*******************
;secondary address 2=SCREEN, 3=CHAREN, 4=BITMAP
newlod
 cpx #5
 bcs oldload
 dec $01        ;rom to ram (a000-bfff)
 jmp loadd      ;continue under rom with the rest of the new load routine
romin inc $01
 jmp clse7f
;
newload
 sta $93     ;flag for load routine (see $93) 0=Load, 1=Verify
 ldx $b9     ;secondary address
 cpx #2
 bcs newlod  ;indicates MDBASIC load
oldload
;CBM code from original vector location $f4a5 to perform load
 lda #0
 sta $90    ;kernal i/o status word (st)
 lda $ba    ;get current device number
 bne xf4b2  ;0=keyboard
xf4af jmp $f713 ;load from keyboard or screen
xf4b2 cmp #3
 beq xf4af  ;3=screen
 bcc jf533  ;1=dataset, 2=rs-232
 ldy $b7    ;length of current filename
 bne xf4bf
 jmp $f710  ;load first file on device i guess???
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
 sta $63    ;store in FAC1
 sta $ae    ;low byte of address for load
 lda $90    ;kernal i/o status word (st) 0=Ok
 lsr        ;128=Device not present, 64=EOF
 lsr        ;shifted left 2 times will set the carry flag
 bcs xf530  ;stop now
 jsr ACPTR  ;receive a byte of data from a device on the serial bus
 sta $62    ;store in FAC1
 jsr $f4e3  ;continue with original LOAD subroutine
;
 php        ;save result of processor flags and registers
 pha
 txa
 pha
 tya
 pha
;
 lda $9d    ;display message if not in prg mode, #$C0=kernel & ctrl, #$80=ctrl only
 bpl lodone ;don't display load addresses
 lda #" "
 jsr CHROUT
 jsr LINPRT+4 ;$bdd1 print 2-byte binary value in FAC1
 lda #"-"
 jsr CHROUT
 ldx $ae    ;ptr to end addr of loaded file
 lda $af
 jsr LINPRT
lodone
 pla
 tay
 pla
 tax
 pla
 plp
 rts
;
jf533 jmp $f533 ;load from dataset or rs232 device
xf530 jmp $F704 ;handle EOF or DEVICE NOT PRESENT as usual
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
 dec $01
 jmp savee
;
;*******************
; SWAP A, B    SWAP A%, B%    SWAP A$, B$
swap
 jsr PTRGET ;1st variable
 sta $14
 sty $15
 lda $0d ;$ or #
 sta $fd
 lda $0e ;data type int or float
 sta $fe ;var 1 int or float
 jsr ckcom2
 jsr CHRGET
 jsr PTRGET  ;search for a variable & setup if not found
 lda $0e     ;var 2 numeric data type, int or float
 cmp $fe     ;v2 has same numeric type as var 1?
 beq match   ;same type, good to go
nomtch ldx #22 ;TYPE MISMATCH ERROR
 jmp ($0300)  ;raise error
match lda $fd ;param1 data type $ or #
 cmp $0d      ;param2 data type $ or #
 bne nomtch
 ldy #$04   ;numeric use 5 bytes 0-4
 cmp #$ff   ;string?
 bne bytes  ;swap numeric data using FAC1 as buffer
 ldy #$02   ;strings only use 3 bytes 0-2
bytes sty $02
cpyvar lda ($14),y ;var1 to fac2
 sta $0069,y
 dey
 bpl cpyvar
 ldy $02
cpyvr2 lda ($47),y ;swap 2 to 1
 sta ($14),y
 dey
 bpl cpyvr2
 ldy $02
faccpy lda $0069,y ;swap 1 to 2
 sta ($47),y
 dey
 bpl faccpy
 rts
;
;*******************
;Perform FIND in BASIC program text
find
 ldy #0
 lda ($2b),y ;should be $0801
 sta $fb
 iny
 lda ($2b),y ;should be $0802
 sta $fc
 ora $fb
 bne prgyes
 jmp prgend
prgyes
 iny
 lda ($2b),y ;should be $0803
 sta $39
 iny
 lda ($2b),y ;should be $0804
 sta $3a
 jsr CHRGOT
 bne dofind
 jmp prgend
dofind cmp #"""
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
 jsr FINDLN   ;search for line#
 lda #$01
 sta listflag
 jsr $a6c9    ;perform list (print line on screen)
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
prgend lda #$0d
 jsr CHROUT
 pla
 pla
 jmp $e386
;
;*******************
delete jsr opget2
linadd lda $5f
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
noline sta $7a
 stx $7b
 lda $24
 sec
 sbc $7a
 tax
 lda $25
 sbc $7b
 tay
 bcs relink
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
relink jsr LINKPRG
 lda $22
 ldx $23
 clc
 adc #$02
 sta $2d
 bcc savex
 inx
savex stx $2e
 jsr $a659
 pla
 pla
 jmp $e386
erenum jsr LINKPRG
 lda #$02
 sta $7b
 lda #$00
 sta $7a
 ldy #$05
ffff lda nolin,y
 sta $0201,y
 dey
 bpl ffff
 jsr find ;65535
 jmp relink
;
;*******************
renum ;jsr CHRGET
 bne renumm  ;get parameters
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
 jsr $a68e    ;reset ptr to beginning of basic prg
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
crap jsr chrget
 beq strt
 cmp #"""
 bne crap
 beq nocrap
tokgo tax
 beq strt
 bpl nocrap  ;x reg is zero-based index of token
 ldx #9      ;there are 9 tokens that reference a line number
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
 lda $3c
 sta $7b
 lda $3b
 sta $7a
 ldy #$00
 ldx #$00
numchr lda $0100,x
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
 bcs ckcmma
carycl jsr dec2d
 jsr CHRGOT
 bcc carycl
ckcmma cmp #","
 beq sav7a
 bne craper
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
 jsr $bc49
 jmp $bddf
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
dec2d jsr clrflg
 dec $97
 jsr work
 lda $2d
 bne ne2d
 dec $2e
ne2d dec $2d
 rts
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
tofac lda $35
 sta $63
 lda $36
 sta $62
 jmp $a68e      ;reset ptr to current basic text to beginning
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
chrget ldy #$00
 inc $7a
 bne ne7a
 inc $7b
ne7a lda ($7a),y
 rts
work ldy $0b
 iny  
 lda ($22),y
 ldy $97
 iny
 sta ($22),y
 jsr pntreq
 bne pne
 rts
pne inc $22
 bne work
 inc $23
 bne work
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
pntreq lda $22
 cmp $24
 bne gbhah
 lda $23
 cmp $25
gbhah rts
;
;******************
oldrst jmp $a81d   ;original CBM RESTORE takes no params
restor jsr CHRGET  ;no param means use original restore cmd
 beq oldrst
 jsr skp73
 jsr FINDLN        ;find line number to restore and place in $5F,$60
 bcc undef         ;can't find line number specified
 lda $5f           ;adjust data ptr to new line number
 bne dec5f         ;by backing up 1 byte
 dec $60
dec5f dec $5f
 lda $5f
 sta $41           ;set DATA ptr to the line
 lda $60
 sta $42
 rts
;
;*******************
;get BASIC line number ($14,$15) and text ptr-1 in X,Y
getline
 jsr CHRGET
 jsr LINGET   ;convert an ascii # to 2 byte int
 jsr FINDLN   ;search for line#
 bcc undef 
 ldy $60
 ldx $5f 
 bne dec5f2
 dey
dec5f2 dex
 rts
;
clearerr
 ldy #0
 sty errnum
 dey           ;y is now #$FF
 sty errline   ;make last error line -1
 sty errline+1
 rts
undef ldx #17 ;UNDEF'D STATEMENT
 jmp ($0300)
;
; ERROR e  where e = error number (1-33)
error
 cmp #TOKEN_CLR
 beq errclr
 cmp #TOKEN_OFF
 bne raiseerr
 jsr detrap
errclr jsr clearerr
 jmp CHRGET
raiseerr
 jsr skip73_  ;valid error number is 1-127
 beq baderr2  ;0 is invalid
 bmi baderr2  ;128 and over is invalid
 tax
 jmp ($0300)
baderr2 jmp FCERR ;illegal qty err
baderr jmp SNERR  ;syntax err
; ON hack to support ON ERROR, ON KEY
on jsr CHRGET
 cmp #TOKEN_ERROR
 beq onerror
 cmp #TOKEN_KEY
 beq onkey
 jmp ONGOTO   ;perform ON
onkey jsr CHRGET
 cmp #TOKEN_GOSUB
 bne baderr
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
 bne baderr
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
 stx $0300
 sty $0301
 rts
;
;*******************************************
; error trap routine - x reg hold error #
;*******************************************
resumenext     ;ON ERROR RESUME NEXT
 lda $9d       ;0=suppress msgs (program running mode) 
 bne olerr
 txa
 bmi quitrun
 stx errnum    ;update last error number
 lda $3a       ;update last BASIC line# causing error
 sta errline+1
 lda $39
 sta errline
 jmp nnnn
quitrun
 jsr detrap    ;disable error trapping
 jmp $a480     ;MAIN BASIC program loop
olerr jmp errors
trap lda $9d   ;MSGFLG Flag Kernal Message Control, #$C0=kernel & ctrl, #$80=ctrl only, #$40=kernel only, #$00=none
 bne olerr
 txa
 bmi quitrun
 stx errnum    ;set current error number
 jsr detrap    ;disable error trapping
 lda #3        ;3 plus the 2 for this next call = 5 bytes
 jsr GETSTK    ;check for space on stack
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
 jmp NEWSTT   ;setup nxt statement for execution and continue BASIC main loop
;
;*******************
resume
 pla          ;discard calling subroutine
 pla
 tsx
 lda $0101,x 
 cmp #TOKEN_ERROR ;ERROR token?
 beq okresu
 ldx #35      ;can't resume error
 jmp ($0300)
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
 jsr CHRGET   ;step into stmt
 jsr $a8f8    ;find end of current BASIC stmt
pullit
 pla
 cmp #<xcmd+2
 bne pullit
 pla
 cmp #>xcmd+2
 bne pullit
 lda #$19     ;25=empty temp string index value
 sta $16      ;reset temp string stack 
 lda #0
 sta $10      ;SUBFLG Subscript Reference to an Array or User-Defined Function Call (FN)
 jsr entrap   ;enable error trapping then return to calling subroutine
 jmp ($0308)  ;read and execute the next statement
;perform RESUME linenum
resume0
 pla          ;discard ERROR token
 pla          ;discard line number that caused the error
 pla
 pla          ;discard txt ptr of error
 pla
 jsr CHRGOT
 jsr GOTO     ;perform goto (adjust txt ptr to given line num)
nnnn jsr nextstmt ;prepare next stmt for execution
 jmp pullit
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
 jmp pullit
;
;*******************
; COLOR [foregndColor (0-31)], [backgndColor (0-15)], [borderColor (0-15)]
; when foregndColor is > 15 flashing mode for color nibble is toggled
color
 cmp #","
 beq nochar
 jsr skip73
 sta COLOR      ;current cursor foreground color
 cmp #16
 bcc nochar-3   ;if char color > 15 then setup irq to blink that color
 jsr blinker    ;toggle blink flag for this color
 jsr chkcomm
nochar jsr comck2
 beq noback
 jsr getval15   ;skip73
 sta BGCOL0     ;background color
 jsr chkcomm
noback jsr getval15_
 sta EXTCOL     ;border color
 rts
blinker
 and #%00001111 ;only lo nibble is the char color
blinkcol2 tay
 lda blinkcol,y
 eor #1
 sta blinkcol,y ;apply new blink flag
 bne setfsh     ;not currently flashing so enable it
;check if there are any other colors needing the blink irq
 ldy #15
chkblink lda blinkcol,y
 bne ecolor     ;at least one flag still set so leave irq set
 dey
 bpl chkblink
irqea31
 ldx #$31       ;re-apply original irq vector of $EA31
 ldy #$ea
 bne setirqvec  ;always branches
setfsh
 lda #20        ;blink delay - irq executes 20 times before blink code executes
 sta blinkdly   ;global variable for char blink delay
 ldx #<flash    ;change irq vector to char flashing subroutine
 ldy #>flash
setirqvec       ;change IRQ vector
 sei            ;disable irq
 stx $0314
 sty $0315
 cli            ;enable irq
ecolor rts
;
;*******************
; MOVE sprite#, x1, y1 [TO x2, y2, speed]
; MOVE sprite# TO x2, y2, [speed]
move
 jsr sprnum ;get sprite# and 2^sprite# ($bf)
 tya        ;sprite number 0-7
 asl        ;convert to 2-byte index for registers
 sta $0f    ;sprite# * 2
 jsr backuppoint  ;save last plot used by graphics commands in case of from/to move
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
 jsr dbyval
 tya
 sta $fc         ;hibyte of x coordinate
 beq bitof       ;msb off
 cmp #2          ;x coordinate hibyte can only be 0 or 1
 bcc biton
 jmp FCERR       ;illegal qty - a valid x coordinate is between 0 and 511
biton lda MSIGX  ;Most Significant Bits of Sprites 0-7 Horizontal Position
 ora $bf         ;2^sprite# ie sprite0=1, sprite1=2, sprite3=4, etc.
 jmp msb
bitof lda $bf    ;sprite register offset 2^sprite#
 eor #$ff
 and MSIGX
msb sta MSIGX ;x coord hibyte
 ldy $0f      ;sprite# * 2
 lda $14
 sta SP0X,y   ;sprite x coord
 sta $fb
 jsr chkcomm
 jsr getval
 ldy $0f      ;sprite# * 2
 lda $14
 sta SP0Y,y   ;sprite y coord
 sta $fd
 jsr CHRGOT
 cmp #TOKEN_TO
 bne move-1   ;TO token not present so we are done
moveto
 lda $bf      ;temp var holding 2^sprite# value
 sta $07      ;temp var for moving sprite on a line
 jsr dbyval   ;get destination x coordinate
 cpy #2       ;must be between 0 and 511
 bcc okpnt2
 jmp FCERR    ;illeqal qty error
okpnt2 stx lastplotx
 sty lastplotx+1
 jsr ckcom2   ;throw misop if current char is not comma
 jsr getval   ;get destination y coordinate
 sta lastploty
;getspeed
 lda #20
 sta $fe      ;default speed is 0
 jsr comchk
 bne nosped   ;no move speed specified?
 jsr getval   ;get the speed param 0-255
 sta $fe      ;temp storage for move speed
nosped lda #$ff
 sta moveflag ;flag to tell LINE cmd to move a sprite instead of plot line
 jsr strtln   ;calculate line and move sprite along that line at given speed
 lda #0
 sta moveflag ;restore default flag 0 for LINE cmd
 jmp restorepoint
;
;*******************
;SPRITE [sprite#], [on/off], [color], [multi], [data pointer], [priority]
sprite
 jsr sprnum     ;sprite# returned in $be and 2^sprite# in $bf
 jsr ckcom2     ;throw misop if current char is not comma
 jsr comck2     ;get next char and compare to comma
 beq scr        ;another comma so skip param
 jsr skip73     ;not a comma so get the value
 cmp #2         ;sprite visible 0=off, 1=on
 bcc onezro
 jmp FCERR      ;illegal qty for boolean value
onezro lda $14  ;visible param
 bne spron
 lda $bf        ;turn sprite off
 eor #$ff
 and SPENA
 jmp onoff
spron lda SPENA ;turn sprite on
 ora $bf
onoff sta SPENA
 jsr chkcomm
scr jsr comck2
 beq smcr
 jsr skip73
 lda $be        ;sprite number 0-7
 tay
 lda $14
 sta SP0COL,y   ;sprite y's color
 jsr chkcomm
smcr jsr comck2
 beq spntr
 jsr skip73     ;get multicolor flag 0 or 1
 bne setm
 lda SPMC       ;mc off
 ora $bf        ;2^sprite
 sec
 sbc $bf
 jmp skipmc
setm cmp #1
 beq setmul
 jmp FCERR      ;illegal quantity error
setmul lda SPMC ;mc on
 ora $bf
skipmc sta SPMC
 jsr chkcomm
spntr jsr comck2
 beq prorty
 jsr skip73     ;get sprite data ptr 0-255 (ptr*64)=start address
;TODO make a subroutine out of this to return base addr
 lda CI2PRA     ;which VIC2 16K memory bank?
 and #%00000011 ;00=bank3, 01=bank2, 10=bank1, 11=bank0
 eor #%00000011 ;11=bank3, 10=bank2, 01=bank1, 00=bank0
 clc
 ror            ;move bits 0-1 to position 6-7 via carry
 ror
 ror
 sta $62        ;VIC-II Base Address hibyte 0=$00, 1=$40, 2=$80 ,3=$C0
 lda VMCSB      ;calc screen RAM page offset (n*1K) 1=1024($0400), 2=2048($0800), 3=3072($0C00), etc.
 and #%11110000 ;upper nibble holds the number of 1K chunks
 lsr            ;convert to offset for hibyte, 1K=(4*256), 1=4, 2=8, 3=12, etc.
 lsr
 clc
 adc $62        ;base+offset
 adc #3         ;the end of screen RAM is 1K more
 sta $62        ;sprite pointers are in the last 8 bytes of 1K screen RAM
 lda #$f8       ;lobyte of offset to first byte of sprite data ptrs
 sta $61        ;pointer to first byte of sprite data ptr, ie bank 0 with 1K offset is $04F8
 lda $14
 ldy $be
 sta ($61),y    ;sprite y's data ptr
;
prorty jsr chkcomm ;check for comma, if end of statement then do not return here
 jsr getval     ;get priority
 bne ontop
 lda $bf
 ora SPBGPR
 sta SPBGPR
 rts
ontop cmp #1
 beq okpri
 jmp FCERR
okpri lda $bf
 eor #$ff
 and SPBGPR
 sta SPBGPR
 rts
;
;*******************
;MULTI COLOR [eb1], [eb2], [eb3]  - extended background color mode
;MULTI TEXT [cc1], [cc2]          - multicolor text mode
;MULTI SPRITE [sc1], [sc2]        - multicolor sprite mode color bit patterns 01,11
;
multi
 cmp #TOKEN_TEXT   ;text token
 beq chrmap
 cmp #TOKEN_SPRITE ;sprite token
 beq mcspri
 cmp #TOKEN_COLOR  ;color token
 beq mulclr 
 jmp SNERR         ;syntax error
;MULTI COLOR [eb1], [eb2], [eb3]
mulclr lda SCROLY ;horiz fine scrolling and control reg
 ora #%01000000    ;turn on bit 6 - enable extended background color mode for text
 sta SCROLY        ;Vertical Fine Scrolling and Control Register
 jsr getval
 sta BGCOL1        ;ext bkgnd color reg#1
 jsr chkcomm       ;check for comma and don't return here if missing
 jsr getval
 sta BGCOL2        ;ext bkgnd color reg#2
 jsr chkcomm
 jsr getval
 sta BGCOL3        ;ext bkgnd color reg#3
 rts
;MULTI SPRITE [sc1], [sc2]
mcspri jsr getval
 sta SPMC0         ;mcspr reg#0
 jsr chkcomm
 jsr getval
 sta SPMC1         ;mcspr reg#1
 rts
;MULTI TEXT [cc1], [cc2]
chrmap lda SCROLX  ;horiz fine scrolling and control reg
 ora #%00010000    ;turn on bit 4 - enable multi color text or bitmap mode
 sta SCROLX
 jsr getval
 sta BGCOL1
 jsr chkcomm
 jsr getval
 sta BGCOL2
 rts
;
;*******************
expand
 jsr sprnum  ;get sprite# and store in $be and 2^sprite# in $bf
 jsr ckcom2  ;throw misop if current char is not comma
 jsr comck2  ;get next char and compare with comma
 beq magx2
 jsr skip73
 bne onchk
 lda XXPAND  ;x expand off
 ora $bf
 sec
 sbc $bf
 jmp magx
onchk cmp #$01
 beq doit
illqty2 jmp FCERR ;illegal quan.
doit lda XXPAND
 ora $bf
magx sta XXPAND   ;x expand on
 jsr chkcomm
magx2 jsr getval
 bne magchk
 lda YXPAND
 ora $bf
 sec
 sbc $bf
 jmp magy
magchk cmp #$01
 bne illqty2
sety lda YXPAND ;y expand
 ora $bf
magy sta YXPAND
 rts
cls lda #$93   ;clear screen
 jmp CHROUT    ;output char then rts
;
;*******************
; LOCATE col, row, [blink]
locate
 beq misngop
 pha
 sec         ;flag for read
 jsr PLOT    ;read current position
 stx $bb     ;x (col)
 sty $bc     ;y (row)
 pla
 cmp #","
 beq row
 jsr skip73  ;get value as int
 cmp #40     ;>=40?
 bcc row_
badloc
 jmp FCERR   ;illegal qty error
row_ sta $bc
 jsr CHRGOT
 beq column  ;end of statement
row jsr CHRGET
 cmp #","
 beq column
gavfy jsr skip73 ;get int value
 cmp #25     ;25 is max line number
 bcs badloc  ;illegal qty
 sta $bb
column
 ldx $bb
 ldy $bc     ;y holds the line (from temp storage area)
 clc         ;clear carry is flag to write new value
 jsr PLOT    ;read/set cursor position on screen
 jsr chkcomm ;if current char is a comma then continue otherwise quit now
 jsr getval  ;get int value for blink enable/disable
 cmp #2      ;0 or 1 is valid
 bcs badloc  ;illegal qty
 eor #1      ;flip value so that 1=on 0=off
 sta 204     ;Flash Cursor 0=Flash Cursor, non-zero No Cursor
 rts
;
;*****************
misngop jmp missop
;*******************
designon
 lda CI2PRA     ;bits 0-1 mem bank, 00=bank3, 01=bank2, 10=bank1, 11=bank0
 and #%11111100 ;select VIC-II 16K mem bank 3 ($C000-$FFFF)
 sta CI2PRA     ;base address is now $C000
 lda #%00101100 ;video matrix offset %0010 (2*1K) = $0800; char dot data offset at %110 (6*1K) = $1800
 sta VMCSB      ;bit 0 unused; bits 1-3 char dot data base addr; bits 4-7 video matrix base addr
 lda #$c8       ;video matrix is at $c800
 sta HIBASE     ;let Kernel know video matrix is at $c800 so printed chars will be visible
 lda SCROLX
 and #%11101111 ;bit 4 off disable multicolor text/bitmap mode
 sta SCROLX
; lda #%00011011 ;mc text mode off, bitmap mode off, screen on, screen 40x25, 3 virtical scan lines
; sta SCROLY
 jmp CHRGET
;*****************
designoff jsr norm
 jmp CHRGET
;*****************
design
 beq misngop
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
 lda $01
 pha
 and #%11111011 ;bit2=0 switch out I/O and bring in CHAREN ROM into bank $d000-$dfff
 sei
 sta $01
nex256 ldy #0
nexbyt lda ($be),y
 sta ($bb),y
 iny
 bne nexbyt
 inc $bf
 inc $bc
 bne nex256
 pla
 sta $01       ;back to normal
 cli
 jmp CHRGET
dodesign
 lda #$f0      ;perfrom design pokecode,d1,d2,...,d8
 sta $bf       ;$f000 = location of char bit data
 lda #$00
 sta $be
 jsr skip73    ;get pokecode
 beq skipcopy
 tay

 lda $be
 ldx $bf
 jsr times8    ;calc ptr (8 bytes per char)
 sta $be
 stx $bf

skipcopy
 jsr ckcom2    ;throw misop if current text is not a comma
 ldy #0        ;loop for all 8 bytes of data
gtdata sty $02
 jsr getval
 ldy $02
 sta ($be),y
 jsr ckcom2
 iny 
 cpy #7
 bne gtdata
 jsr getval
 ldy $02
 iny 
 sta ($be),y
 rts
;
;*******************
;
;BITMAP CLR (does not switch to bitmap mode)
;BITMAP [colorMode], [bkgndColor]
;colorMode 0=hires, 1=mc (multicolor); bkgndColor (0-15) background color
;bkgndColor is applied based on colorMode:
;hires mode sets color RAM in video matrix with initialization of all 1024 bytes
;mc mode sets the single bkgrnd color register BGCOL0     
;MAPCOL c1,c2,c3 (c3 mc mode only) to change colors:
;hires c1 (0-15) dot color, c2 (0-15) 8x8 square bkgnd color, c3 not used but can be set
;multicolor c1 (0-15) color for bit pattern 01, c2 (0-15) color for bit pattern 10, c3 (0-15) color for bit pattern 11
;NOTE when in mc mode graphics cmds use color index 1-3 (not color value 0-15) as a color parameter to select the color
;
bitmap
 cmp #TOKEN_CLR
 bne bitscr
bitclr lda #$e0     ;bitmap located at $e000
 sta $bf
 lda #$00
 sta $be
nxpart ldy #0
 lda #0             ;clear bitmap page
clrbyt sta ($be),y
 iny
 bne clrbyt
 inc $bf
 bne nxpart
 jmp CHRGET
illqty6 jmp FCERR   ;illegal qty
bitscr
 jsr skip73         ;colorMode 0 or 1
 beq hiresmode
 cmp #1
 bne illqty6
mcmode
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

; lda #$c8          ;hibyte of ptr to screen ram $c800
; sta HIBASE        ;top page of screen memory for Kernel prints
 rts
hiresmode 
 lda SCROLX         ;turn off mulicolor mode
 and #%11101111     ;set bit#4 0=off, 1=on
 sta SCROLX         ;apply setting to control register
 jsr comchk
 bne bitmapon
 jsr getc2          ;background color for all 8x8 squares
 ldy #0
pokcol sta $c800,y ;fill color mem for entire screen
 sta $c900,y
 sta $ca00,y
 sta $cb00,y
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
;    00 Background Color Register 0 (53281, $D021)
; c1 01 Upper four bits of Video Matrix (scan code)
; c2 10 Lower four bits of Video Matrix (scan code)
; c3 11 Color RAM nybble (area starts at 55296 ($D800))
;
;hires mapcol 0,1   dot color black, background (of 8x8 square of dot) white
;multi mapcol 0,1,2 bit patterns 01=black, 10=white, 11=red (00 is the background color in BGCOL0)
mapcol
 jsr getc1
 jsr chkcomm     ;if no more params then quit otherwise continue
 jsr getc2
getc3
 jsr chkcomm
 jsr getval15_   ;c3 (0-15) is used in multicolor mode only bit pattern 11
 sta mapcolc3    ;global variable - This color can be in the same 8x8 square that the previous colors c1 & c2 are in, with the sacrifice of horizontal resolution
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
 jsr getval15_   ;c2 (0-15) changes the background of the 8 x 8 square
 ora $02
 sta mapcolc1c2  ;update global variable for colors
 rts 
;
;*******************
;syntax PULSE voc#, width%  ie PULSE 1,50 sets voice 1 to 50%
pulse jsr ppw
 pha          ;save voice SID register offset (voice#-1)*7
 jsr ckcom2   ;throw misop if current char is not comma
 jsr CHRGET
 jsr FRMNUM   ;get width% (0.00 to 100.00)
 lda #<m4095  ;REGVAL=ROUND(40.95*WIDTH%) result in range 0-4095
 ldy #>m4095  ;y=hi byte, a=lo byte pointer to 5-byte FAC value
 jsr FMULT    ;multiply FAC1 by a value in memory ptr A=lo byte, Y=hi byte
 jsr ROUND    ;round FAC1 to whole number
 jsr GETADR   ;convert FAC1 to 2-byte integer in $14,$15
 pla
 tay          ;register offset for voice
 lda $15      ;$64
; cmp #$10    ;0-4095 only
; bcs badwav
 sta PWLO1,y  ;Pulse Waveform Width (lobyte)
 lda $14
 sta PWHI1,y  ;Pulse Waveform Width (hibyte)
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
;WAVE [voice#], [waveform], [gate], [sync], [ring], [disable]
;
wave jsr ppw  ;get voice SID register offset (voice#-1)*7
 sta $bb       ;save SID register offset
 jsr ckcom2    ;throw misop if current char is not a comma
 jsr getval    ;get waveform single byte operand value into $14
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
 jsr getval    ;get gate value 0 or 1
 cmp #2
 bcs badwav
 ora $02       ;position is bit 0
 sta $02
 jsr comchk
 bne waveit
 jsr getval    ;get sync value 0 or 1
 cmp #2
 bcs badwav
 asl           ;position is bit 1
 ora $02
 sta $02
 jsr comchk
 bne waveit
 jsr getval    ;get sync value 0 or 1
 cmp #2
 bcs badwav
 asl           ;position is bit 2
 asl
 ora $02
 sta $02
 jsr comchk
 bne waveit
 jsr getval    ;get disable value 0 or 1
 cmp #2
 bcs badwav
 asl           ;position is bit 3
 asl
 asl
 ora $02
 sta $02
waveit
 ldx $bb
 lda $02
 sta VCREG1,x ;select waveform and start release cycle if attac/decay/sustain is already started
 rts
badwav jmp FCERR
;
;subroutine to clear SID
sidclr
 ldy #$18        ;clear all 24 SID registers
 lda #0
clrsid sta FRELO1,y
 dey
 bpl clrsid
 sta md417       ;clear mirror mem for register $d417 Filter Cutoff Frequency (high byte)
 sta md418       ;clear mirror mem for register $d418 Filter Resonance Control Register
 rts
;*******************
;NTSC and PAL hold the value of 1Hz (based on clock speed)
;REG_VAL=FREQUENCY/NTSC
;Max Frequency is 4095.3 for machines using the NTSC clock
sound
 cmp #TOKEN_CLR  ;clr token?
 bne getfreq
 jsr sidclr
 jmp CHRGET
getfreq
 jsr ppw         ;returns SID register offset (voice#-1)*7 in accumulator
 pha
 jsr CHRGET
 jsr FRMNUM      ;convert current expression to a number and store in FAC1
 jsr MOVEF       ;copy FAC1 to FAC2 (numerator) frequency value
 lda $02a6       ;clock type 0=NTSC, 1=PAL 
 beq is_ntsc
 lda #<pal
 ldy #>pal
 jmp memfac
is_ntsc lda #<ntsc
 ldy #>ntsc
memfac jsr MOVFM ;copy mem to FAC1 pointed by a & y
 jsr FDIVT       ;fac1 = (FAC2/FAC1)
 jsr ROUND       ;round FAC1 to whole number
 jsr GETADR      ;convert FAC1 to unsigned 16-bit int
 pla
 tay
 lda $14
 sta FRELO1,y    ;store result in data control reg for voice
 lda $15
 sta FREHI1,y
 rts
;
;*******************
; DRAW S$
;P	Change plot type (0-3) 0=erase,1=plot,2=flip,3=none
;C	Change plot color (0-15 in hires, 1-3 in mc mode)
;U	UP
;D	DOWN
;L	LEFT
;R	RIGHT
;E	UP & LEFT
;F	UP & RIGHT
;G	DOWN & LEFT
;H	DOWN & RIGHT 
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
 jmp nxtmov2
chkplottype cmp #"p"
 bne godraw
 jsr getval15_  ;actually should be 0-3
 sta $fe ;lastplott
 jmp nxtmov2
godraw
 pha
 jsr dbyval
 stx $bb
 sty $bc
 pla
 cmp #"u"
 bne chkdwn
udraw jsr drawup
 jsr dodraw
 jmp udraw
chkdwn cmp #"d"
 bne chkleft
ddraw jsr drawdwn
 jsr dodraw
 jmp ddraw
chkleft cmp #"l"
 bne chkright
ldraw jsr drawleft
 jsr dodraw
 jmp ldraw
chkright cmp #"r"
 bne chkupleft
rdraw jsr drawright
 jsr dodraw
 jmp rdraw
chkupleft cmp #"e"
 bne chkupright
edraw jsr drawup
 jsr drawleft
 jsr dodraw
 jmp edraw
chkupright cmp #"f"
 bne chkdwnleft
fdraw jsr drawup
 jsr drawright
 jsr dodraw
 jmp fdraw
chkdwnleft cmp #"g"
 bne chkdwnright
gdraw jsr drawdwn
 jsr drawleft
 jsr dodraw
 jmp gdraw
chkdwnright cmp #"h"
 bne baddraw
hdraw jsr drawdwn
 jsr drawright
 jsr dodraw
 jmp hdraw
baddraw
 jmp SNERR
dodraw
 lda $fe ;lastplott
 cmp #3
 bcs noplot
 jsr plotit
noplot
 dec $bb       ;decrement draw length
 bne dodraw2
 dec $bc
 bmi nxtmov    ;draw length complete
dodraw2 rts
nxtmov
 pla           ;cancel jsr
 pla 
nxtmov2
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

;*******************
; PLOT x,y, type, color
plot jsr getpnt
 jsr types
plotit dec $01
 jsr setdot
 inc $01
 rts
;
;*******************
; LINE INPUT ["prompt",] A$
; LINE INPUT# filenum, A$
line
 cmp #TOKEN_INPUT_ ;input# token (line input#)
 bcc liner     ;line x,y to a,z
 jsr ERRDIR    ;throw error if direct mode; only x reg is affected
 cmp #TOKEN_INPUT  ;input token (line input)
 beq getprompt ;perform line input prompt$, var$
 bcc lineinput ;perfrom lineinput# num%, var$
badinp jmp SNERR     ;syntax error
lineinput
 jsr GETBYTC  ;eval expr into byte int value (returned in x reg)
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
 jsr CHRGOT
 cmp #","
 bne badinp
readline
 jsr INLIN    ;input a line to buffer from keyboard (80 chars max from keyboard)
 ldy #0       ;count number of characters input (not sure if routine returns it)
fndend lda $0200,y
 beq inpend
 iny
 bne fndend
inpend tya    ;y reg = string length
 jsr GETSPA   ;alloc space in mem for string returning address ptr in $35,$36
 pha          ;save number of bytes allocated on stack
 tay
 jsr CHRGET
 dey
copyer lda $0200,y ;copy string to variable storage
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
 jsr CHRGOT
 cmp #","
 beq readline
 rts
;
; LINE x1,y1 TO x2, y2, plotType, color
liner jsr getpnt
 jsr point2
 jsr types
;entry point for MOVE sprite command
strtln
 dec $01
 jsr linedraw
 inc $01
;fall through savepoint subroutine
savepoint
 ldx #3
 lda $fb,x
 sta lastplotx,x
 dex
 bpl savepoint+2
 rts
getpoint
 ldx #3
 lda lastplotx,x
 sta $fb,x
 dex
 bpl getpoint+2
 rts
swappoint
 ldx #3
 lda $fb,x
 pha
 lda lastplotx,x
 sta $fb,x
 pla
 sta lastplotx,x
 dex
 bpl swappoint+2
 rts
backuppoint   ;make temp copy of last x,y coords, plot type used by graphics cmds
 ldy #3
 lda lastplotx,y
 sta lastplotx2,y
 dey
 bpl backuppoint+2
 rts
restorepoint  ;temp copy last plot coords and plot type used by graphics cmds
 ldy #3
 lda lastplotx2,y
 sta lastplotx,y
 dey
 bpl restorepoint+2
 rts
;
point2 lda #TOKEN_TO
 jsr CHKCOM    ;skip over TO token, syntax error if not there
 jsr savepoint ;move point 1 to last plot point acting as point 2
 jsr pntweg
 jmp swappoint
;get x,y coordinates
pntweg jsr skp73 ;get x coordinate, returns lobyte in x, hibyte in y
 jsr xytim2
 tya
 beq okvalu
 cpy #2     ;valid value is 0 or 1 for x coord hibyte
 bcs hellno
 cpx #$40   ;valid values 0 to 63
 bcs hellno
okvalu stx $fb
 sty $fc
 jsr ckcom2  ;must have a comma before y coord value of 0-199 
 jsr dbyval  ;x=lobyte, y=hibyte
 bne hellno  ;y reg last loaded was not zero
 cpx #200    ;x reg has lobyte for y coordinate and must be between 0 and 199
 bcs hellno
 stx $fd     ;y coordinate
 rts
xytim2 
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 beq theend ;hires mode
 txa  ;adjust offset to read ptab2/ptab3 for multicolor plot bits
 asl  ;multiply x and y registers * 2
 tax
 tya
 rol
 tay
theend rts
hellno jmp FCERR ;throw illegal qty error
;
;*******************
paint jsr getpnt
 jsr types
 dec $01
 jmp painter
;
;*******************
; CIRCLE xcenter, ycenter, xsize, ysize, [plottype], [color]
circle jsr getpnt ;center point x,y
 jsr ckcom2  ;throw misop if current char is not comma
 jsr getval  ;x radius size
 sta $35
 jsr ckcom2  ;throw misop if current char is not comma
 jsr getval  ;y radius size
 sta $36
 jsr types   ;get optional plot type and color; use last used values if not supplied
 lda $35
 ora $36
 beq endcir  ;x and y radius size are both zero
 dec $01     ;switch LOROM to LORAM
 cmp #3      ;smallest x radius size
 bcs okcirc  ;plot the circle
 jsr setdot  ;plot just a dot
 jmp r6510
okcirc
 jsr circel
 jmp r6510   ;restore LOROM and HIROM
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
 lda #$04        ;text page for kernel prints
 sta HIBASE      ;top page of screen mem
 lda #%00010101  ;bit0 is always 1; bits1-3 text chr dot data base address in 1K chunks; bits 4-7 video matrix base address in 1K chunks
 sta VMCSB       ;VIC-II chip memory control register
 lda #%11001000  ;display on, multicolor text/bitmap mode off, 40 columns, 0 horizontal scroll scan lines
 sta SCROLX
 lda #%00011011  ;extended color text mode off, bitmap mode off, display on, 25 rows, 3 vertical scroll scan lines
 sta SCROLY
r6510 lda $01
 ora #%00000111  ;switch mem banks to normal mem mapped I/O RAM ($d000-$dfff), HIROM ($e000-$ffff), LOROM ($a000-$bfff)
 sta $01
 rts
;
; TEXT x,y "string", [charset], [sizeX], [sizeY], [plotType], [color]
text
 beq norm
 jsr getpnt  ;get plot x,y
 jsr ckcom2  ;raise misop err if current char is not comma
 jsr getstr  ;get text string returning ptr in vector ($50)
 lda #1      ;default size value
 sta $57     ;temp var of x size
 sta $58     ;temp var of y size
 lda #$d0    ;assume charset 0 at $d000
 sta $26     ;charset temp var
 jsr comchk
 bne ne
 jsr getval   ;charset 0 or 1
 beq sizes
 lda #$d8     ;charset 1 at $d800
 sta $26
sizes
 jsr comchk
 bne ne 
 jsr getval
 cmp #32      ;max size is 31
 bcc notspc
 jmp FCERR     ;illegal quantity error
notspc sta $57 ;user specified x size
 jsr comchk
 bne ne
 jsr getval
 cmp #32      ;max y size is 31
 bcs notspc-3
 sta $58      ;user specified y size
 jsr comchk
 bne ne
 jsr types
ne dec $01
 jsr texter
 inc $01
 rts
;
;*******************
; SCREEN ON|OFF
; SCREEN cols, rows where cols=38|40, rows=24|25
screen
 cmp #TOKEN_ON
 bne scnoff
 lda SCROLY
 ora #%00010000  ;bit4 = 0 screen on
 sta SCROLY
 jmp CHRGET
scnoff cmp #TOKEN_OFF ;off token?
 bne setscr
 lda SCROLY
 and #%11101111  ;bit4 = 1 screen off
 sta SCROLY
 jmp CHRGET
setscr jsr comchk
 beq cklast+3
 jsr skip73
 cmp #40
 bne chk38
 lda SCROLX
 ora #%00001000  ;bit 3 on 40 columns
 sta SCROLX
 jmp cklast
chk38 cmp #38
 beq set38
illqty3 jmp FCERR
set38 lda SCROLX
 and #%11110111  ;bit 3 off 38 columns
 sta SCROLX
cklast jsr chkcomm
 jsr getval
 cmp #25
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
;*******************
; SCROLL x1, y1 TO x2, y2, direction, color
scroll
 jsr getscroll
 jsr comchk
 beq okscroll1
 lda #0
 sta $ff
 beq okscroll2
okscroll1
 jsr getval      ;direction 0-3
 cmp #4
 bcc okscroll3
noscroll
 jmp FCERR       ;illegal quantity error
okscroll3 pha 
 jsr comchk
 bne okscroll4
 jsr getval      ;wrap? 0=no, 1=yes
 cmp #2
 bcs noscroll
 sta $ff
okscroll4 pla    ;what was the direction?
okscroll2 asl    ;convert to 2 byte offset
 tay
 lda scrolls,y   ;scroll direction vector lobyte
 sta $55
 lda scrolls+1,y ;scroll direction vector hibyte
 sta $56
 dec $01
 jmp $0054
;-----------
getscroll
 jsr skip73    ;x1
 cmp #40       ;max columns
 bcc goodscroll
badscroll jmp FCERR ;illegal quantity error
goodscroll sta $fb
 sta $be
 lda HIBASE   ;top page of screen memory
 sta $fc
 jsr ckcom2   ;throw misop if current char is not comma
 jsr getval   ;y1
 cmp #25      ;max rows
 bcs badscroll
 sta $bf
 tay 
 beq notime40
 lda $fb
 ldx $fc
 jsr times40
 sta $fb
 stx $fc
notime40 jsr addoffset
 jsr CHRGOT
 cmp #TOKEN_TO
 beq foundto
 jmp SNERR       ;syntax error
 jsr ckcom2      ;throw misop if current char is not comma
foundto jsr getval ;x2
 cmp #40
 bcs badscroll
 sec 
 sbc $be
 sta $be
 jsr ckcom2
 jsr getval ;y2
 cmp #25
 bcs badscroll
 sec 
 sbc $bf
 sta $bf
 rts 
;
;*******************
; ENVELOPE voice#, attack, decay
; ENVELOPE voice#, attack, decay, sustain release
adsr jsr ppw
 sta $bb       ;SID register offset for voice
 jsr ckcom2    ;raise error if current char is not a comma
 jsr getval15_ ;attack
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
 jsr getval15_ ;sustain volume
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
;FILTER SOUND voice#, [boolean] 
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
;3. round up the value to whole number using FAC ROUND subroutine
;4. convert FAC value to a 2-byte binary value using FAC subroutine
;5. subtract 5 from binary value
;6. range check to ensure not larger than 2047, error if so
;7. store the result in SID registers
;
filter
 cmp #TOKEN_SOUND ;SOUND token?
 bne getfreq1
;FILTER SOUND voice#, boolean
 jsr CHRGET
 jsr getvoc     ;get voice # 1-3
 cmp #3         ;convert voice# 1=1, 2=2, 3=4  bit values 001 010 100
 bne notvoc3
 tay            ;3 is bit pattern 011 and needs to be 100 for register
 iny
 tya
notvoc3
 sta $02        ;remember for later
 jsr comchk     ;check if they supplied a boolean?
 bne filteron   ;missing boolean assumes on, syntax FILTER SOUND voice#
 jsr getval     ;get on/off expression, 0=off, 1=on
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
 jsr FRMNUM     ;convert current expression to a number and store in FAC1
 jsr MOVEF      ;copy FAC1 to FAC2 (numerator) frequency value 
 lda #<five8
 ldy #>five8
 jsr MOVFM      ;copy mem to FAC1 pointed by a & y
 jsr FDIVT      ;fac1 = (FAC2/FAC1)
 jsr ROUND      ;round FAC1 to whole number
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
illqty5 jmp FCERR        ;illegal qty err
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
 jsr chkcomm  ;check if param present, if not do not return to me here and quit
 jsr getval15_ ;get resonance param
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
 jsr chkcomm  ;check if param present, if not do not return to me here and quit
 jsr getval   ;filter type 0-4
 cmp #5
 bcs illqty5
 cmp #3       ;3 and 4 need to add 1 to achive desired bit pattern
 bcc settype
 tay
 iny
 tya
settype
 asl        ;shift bits 0,1,2 to positions 4,5,6
 asl        ;ie 00000111 becomes 01110000
 asl
 asl
 sta $02    ;temp storage
 lda md418  ;current value of register
 and #%10001111  ;clear bits 4-6, keep the rest
 ora $02    ;apply new value
 sta md418  ;apply value to SID register
 sta SIGVOL
 rts
;*******************
; PLAY S$
; PLAY STOP
play
 cmp #TOKEN_STOP
 bne playy
 jsr playstop
 jmp CHRGET 
playy
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

 lda #1
 sta playtime   ;initial wait

 ldx #<playit
 ldy #>playit
 jmp setirqvec

playit
 dec playtime
 bne jea31
 lda $01
 pha
 and #%11111110
 sta $01
 jsr notegot
 beq stoplay
 jsr nextn
playgo
 pla
 sta $01
jea31 jmp $ea31
stoplay
 jsr playstop
 bne playgo  ;always branches

playstop
 ldx playvoice  ;SID reg offset
 lda playwave   ;select current waveform; start release cycle
 sta VCREG1,x   ;start decay cycle
 jmp irqea31

initvoice
 sta playvoice  ;voice register offset
 tax

 lda #%00010000 ;select default waveform to triangle; start decay cycle
 sta playwave
 sta VCREG1,x

 lda #30        ;default note length to 30 jiffies (approx 1/60 sec.)
 sta playlen    ;note duration 30/60=0.50 sec.

 lda #4         ;default octave 4
 sta playoct

 lda #0         ;init voice's registers
; sta FRELO1,x
; sta FREHI1,x
 sta PWLO1,x    ;in case user select pulse waveform
 lda #$04       ;set the pulse duty cycle to 25% (12-bit reg val 0-4095, 25%=1024)
 sta PWHI1,x    ;lower nibble only, upper nibble is unused (12-bit value)

 lda #$10       ;set attack duraction to 8ms (hi nibble)
 sta ATDCY1,x   ;and decay duration to 6ms (lo nibble)
 lda #$F8       ;set sustain volume to 15    (hi nibble)
 sta SUREL1,x   ;and release duration to 300ms (lo nibble)

 lda md418      ;all voices volume register mirror
 bne vocrdy     ;if volume is 0 (off) turn it up!
 ora #%00001111 ;set volume to max
 sta md418
 sta SIGVOL     ;SID main volume register
vocrdy rts

;
;*******************
; KEY n, "string"  where n = (1-8)
key
 cmp #TOKEN_LIST  ;LIST token?
 beq keylist
 cmp #TOKEN_OFF
 beq onkeyoff
keyass jsr skip73 ;get key#
 beq badkey
 cmp #9           ;only func keys 1-8
 bcc okkey
badkey jmp FCERR  ;illegal quantity error
okkey sta $02     ;save key#
 jsr ckcom2       ;throw misop if current char is not comma
 jsr getstr
 beq key-1        ;quit
 dec $01
 jmp keyy
onkeyoff lda #0
 sta keyflag
 sta keyentry
 jmp CHRGET
keylist dec $01
 jmp keylistt
;*******************
;DISK "DOS command string"
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
 lda #$7f        ;file handle 127
 ldx #$08        ;device 8
 ldy #$0f        ;secondary channel 15 = DOS channel
 jsr SETLFS
 jsr OPEN        ;performs OPEN 127,8,15, "string"
 bcc noeror
clos7f pha       ;retain result of open file which is index of error msg
 jsr noeror      ;close 127
 pla             ;retreive error msg index
 tax             ;x register holds index of error message
 jmp ($0300)     ;display error message
noeror lda $b8 ;#$7f  ;file handle 127
 jmp CLOSE       ;close file handle in accumulator
;
;******************************************
;* mdbasic functions instr(), ptr(), csr(), pen(), joy(), pot(), hex$() *
;******************************************
badsubscript
 ldx #18      ;BAD SUBSCRIPT ERROR
 jmp ($0300)  ;report error
; I% = INSTR(offset,src$,find$)
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
 beq notfound ;empty source string always returns 0
 bne getsrcparam
getoffsetparam
 jsr fac2int  ;get binary byte value 0-255 into A reg
 beq badsubscript ;BASIC strings index are based at 1 (not 0)
 sta $02      ;offset
 dec $02      ;convert to zero-based index
 jsr ckcom2   ;throw misop if current text is not a comma
 jsr getstr   ;get source string as second param
getsrcparam
 stx $fb
 sty $fc
 sta $fd      ;len of src str
 jsr ckcom2   ;throw misop if current text is not a comma
 jsr getstr   ;find string a = len($52), x=lobyte($50) ptr, y=hibyte($51) ptr
 stx hack+1
 sty hack+2
;
 lda $fd      ;src len
 beq notfound
 cmp $52      ;find len
 bmi notfound ;find len > src len
tryagain
 ldy $02      ;index of current char in source str
 ldx #0       ;index of first char in find str
nextchr
 cpy $fd      ;did we reach source str length?
 beq notfound ;yes then stop trying
 lda ($fb),y
hack cmp $ffff,x ;hack writing to this mem loc to set address
 bne nextchr2
 iny
 inx
 cpx $52      ;did we reach find str length?
 bne nextchr  ;no, keep going
 lda #0       ;hibyte 0 since strings cannot be longer than 255
 ldy $02      ;index of beginning of str found in source str
 iny          ;BASIC string index begins at 1 (not 0)
instrend
 jsr GIVAYF   ;convert 16-bit signed int value to FAC then return 
 jmp CHKCLS   ;Check for and Skip Closing Parentheses
nextchr2
 inc $02      ;advance forward 1 char in source
 bne tryagain ;it should always be not equal to zero here!
notfound
 ldy #0
 tya
 beq instrend
;
ptr
 lda $48
 ldy $47
 jmp GIVAYF

; V = ROUND(N,D)  where N=num to round, D=num of decimal places
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
 jsr FRMNUM     ;$AD8A
 jsr AYINT      ;convert FAC1 to a signed integer in FAC1
 lda $66        ;sign, $00=Positive, $FF=Negative
 sta $15
 beq round2
 jsr $B947      ;NEGFAC Replace FAC1 with Its 2's Complement
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
;add 0.5 to FAC1
 lda #<$BF11    ;the constat 0.5 in 5-byte FAC format
 ldy #>$BF11
 jsr $B867      ;add fac1 with a number in memory: fac1 = mem+fac1
;truncate fraction
 jsr INT        ;perform INT
;move decimal point to the left
 ldx $14
 beq nomul
 jmp movedec
;move decimal point
movedec
 txa
 beq nomul
 pha
 lda $15          ;direction: 0=right else left
 beq xmul10
 jsr $BAFE        ;divide FAC1 by 10
 jmp xmul10+3
xmul10 jsr $BAE2  ;multiply FAC1 by 10
 pla
 tax
 dex
 bne movedec
nomul rts
illqty7 jmp FCERR ;display illegal qty error
;
; J% = JOY(n) where n=joystick number 1 or 2
joy jsr GETADR
 lda $15
 bne illqty7
 lda $14
 beq illqty7
 cmp #3
 bcs illqty7
 tay
 dey
 tya
 eor #%00000001
 tay
 lda CIAPRA,y
 and #$0f
 sta $14
 lda CIAPRA,y
 and #$10   ;fire button
 sta $15
 sec
 lda #$0f
 sbc $14
 ldx $15
 cpx #$10
 beq nofire
 ora #%10000000 ;fire flag
nofire tay  ;lowbyte
 jmp nobutt
;
; K% = KEY(n) where n=0 ascii, n=1 flags
keybytes .word keyentry,SHFLAG,$00c5,$00cb,$00c6
;0 keyentry - used with ON KEY GOSUB to indicate key causing event
;1 $028d SHFLAG Shift/Ctrl/Logo key flags
;2 $00c5 LSTX Matrix Coordinate of Last Key Pressed
;3 $00cb SFDX Matrix Coordinate of Current Key Pressed
;4 $00c6 Num chars in key buf
_key
 jsr fac2int
 cmp #5
 bcs badpot
 asl
 tay
 lda keybytes,y
 sta $14
 lda keybytes+1,y
 sta $15
 ldy #0
 lda ($14),y
 tay
 jmp nobutt
;
;E% = ERROR(n) where n=0 Error Number, n=1 Error Line Number
err
 jsr fac2int
 cmp #1
 beq geterrline
 bcs badpot
 ldy errnum  ;y=lobyte
 jmp nobutt
geterrline
 ldy errline
 lda errline+1
 jmp GIVAYF  ;convert binary int to FAC then return
;
;p% = POT(n) where n=potentiometer number (1-4)
pot jsr GETADR
 lda $15
 bne badpot
 lda $14
 beq badpot
 cmp #$05
 bcc okpot
badpot jmp FCERR
okpot pha
 ldx #%11000000 ;bits 0=input, 1=output, set bits 6 and 7 to output
 sei
 stx CIDDRA     ;data direction reg A
 cmp #$03       ;pot 3 and 4 are on port 2
 bcs port2
 ldx #$40
 ldy CIAPRB     ;data port reg B
 jmp setprt
port2 lsr
 ldx #$80
 ldy CIAPRA
setprt stx CIAPRA
 sty $02
 ldy #$80
wait dey
 bpl wait
 lsr
 tay
 lda POTX,y
 ldx #$ff
 stx CIDDRA
 cli
 tay
 pla
 lsr
 bcc b2or4
 lda #$04
 jmp and02
b2or4 lda #$08
and02 and $02
 bne nobutt
butt lda #$01 ;hibyte
 bne endpot
nobutt lda #$00 ;hibyte
endpot jmp GIVAYF  ;convert binary int to FAC then return
;
;*******************
;C% = CSR(n) where n = 0 to 6 to select cursor info:
; 0=logical column, 1=physical line, 2=blink on/off, 3=max columns, 4=char under cursor
; 5=physical column, 6=color under cursor peek($0287), 7=address of csr line
csr
 jsr fac2int
 ldy #0
 tax
 cmp #5
 bcc usecsrbytes
 cmp #6
 bcs csrcolor
;physical line = (logicalCol > maxCols) ? logicalCol-maxCols  logicalCol 
 lda $d3      ;logical column
 sec
 sbc #40      ;max column number for a physical line
 bpl gocsr    ;logicalCol was larger than maxCols
 ldx #0       ;use logical col (type 0)
 beq usecsrbytes
csrcolor
 cmp #7
 bcs lastcsr
 ldy #$02     ;load address of register having color under cursor
 lda #$87     ;target address is $0287
 bne goodcsr1 ;always branches
lastcsr
 beq csraddr 
badcsr jmp FCERR
csraddr       ;get text address of current line
 lda $d2      ;hibyte
 ldy $d1      ;lobyte
 jmp GIVAYF
usecsrbytes lda csrbytes,x
goodcsr1 sta $14
 sty $15
 ldy #0
 lda ($14),y
gocsr tay     ;y=lobyte
 jmp nobutt
;*******************
;P% = PEN(n) where (n=0:x, n=1:y) to read $D013 (X) and $D014 (Y) Light Pen Registers.
;For PENY there are 200 visible scan lines possible so value is exact.
;For PENX there are only eight bits available for 320 possible horizontal
;screen positions, the value here is accurate only to every second dot position.
;The number here will range from 0 to 160 and must be multiplied by 2 in order
;to approximate the actual horizontal dot position of the light pen.
;
pen
 jsr fac2int ;FAC1 to signle byte value returned in A reg
 beq penx
 cmp #2
 bcs badcsr
 ldy LPENY   ;y=lobyte
 jmp nobutt
penx lda LPENX
 clc
 asl         ;multiply by 2, bit 7 into carry
 tay         ;lo byte
 bcc nobutt  ;result was less than 256 so hi byte = 0
 bcs butt    ;result was more than 256 so hi byte = 1
;
;H$ = HEX$(n) where n is a unsigned 2-byte integer (0-65535)
hex jsr GETADR
 ldy #$00
 lda $15
 jsr dechex
 lda $14
 jsr dechex
 lda #$00
 sta $00ff,y
 pla
 pla
 ldy #$ff
nxzro iny
 lda $00ff,y
 cmp #"0"
 beq nxzro
 cpy #$04
 bne addy
 dey
addy tya
 clc
 adc #$ff
 pha
 lda #$00
 adc #$00
 tay
 pla
 jmp STRLIT
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
 sta $00ff,y
 iny
 pla
 and #$0f
 cmp #$0a
 bcc noadd7
 clc
 adc #$07
noadd7 clc
 adc #48
 sta $00ff,y
 iny
 rts
;
;********************
;* new reset vector *
;********************
resvec ldx #$ff
 stx SCROLX
 jsr $fda3
 jsr $fd50 ;restore
 jsr $fd15 ;routines
 jsr $ff5b
 jsr $e453
 jsr $e3bf
 jsr newvec ;set vectors
 lda $2b    ;Pointer to the Start of BASIC Program Text
 ldy $2c
 jsr REASON ;Check for Space in Memory
 jsr $e430  ;prints the BYTES FREE message
 jmp $e39d  ;to basic main loop
;***********************
;* new RUN-STOP vector *
;***********************
runstp lda #$7f
 sta CI2ICR
 ldy CI2ICR
 bmi nothin
 jsr $f6bc
 jsr STOP
 bne nothin
;********************
;* new BREAK vector *
;********************
brkirq jsr $fda3
 lda #$04        ;initialize to 1024 ($0400)
 sta HIBASE      ;text page hi byte pointer
 jsr $e518       ;initialize screen and keyboard
 jsr nwvec2
 jsr CLALL
 lda #0
 sta CI2ICR      ;disable IRQ
 jsr irqea31     ;restore IRQ vector to $EA31
 lda #$7f
 sta CI2ICR      ;enable IRQ
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
 jmp READY    ;print READY. then continue with BASIC main loop
doerr asl     ;calc index
 tax
 sec
 sbc #62
 bcc romerr
 tax
 lda erradd,x
 sta $22
 lda erradd+1,x
 jmp hibyer
romerr lda $a326,x  ;$A328-$A364 Error Message Vector Table
 sta $22
 lda $a327,x
hibyer sta $23
 jsr norm
 lda #$80    ;only control messages - SEARCHING, SAVING, FOUND, etc.
 jsr SETMSG
 jsr CLRCHN  ;restore default devices
 lda #$00
 sta $13
 jsr $aad7   ;perform print 
 ldy $3a
 iny
 beq nipm
 jsr cls     ;clear screen
nipm jsr $ab45
 ldy #$00    ;loop print all chars in err msg
prterr lda ($22),y
 pha
 and #$7f    ;bit 7 is flag for last char in string, remove it to print actual char
 jsr $ab47
 iny
 pla
 bpl prterr  ;bit 7 not set on last char so keep going
 jsr $a67a   ;empty the stack
 lda #$69    ;address of the zero-terminated string ($a369) = "  ERROR"
 ldy #$a3
 jsr STROUT  ;print str whose addr is in y reg (hi byte) and a reg (lo byte)
 ldy $3a
 iny
 bne inline
 jmp READY   ;print READY. then continue with BASIC main loop
inline jsr $bdc2 ;display text IN {line#}
 lda $39
 sta $14
 lda $3a
 sta $15
 jsr FINDLN
 lda #$0d
 jsr CHROUT
 lda #$01     ;set LIST flag on
 sta listflag
 jsr $a6c9
 ldx #$02
 ldy #$00
 clc
 jsr PLOT
 jmp $a483  ;main BASIC loop
;
keychk lda $9d ;prg mode?
 beq nokey
 lda $d4        ;quote mode?
 bne nokey
 lda SHFLAG
 bne lgoshf
 lda #$81
 jmp bsky
lgoshf cmp #$02
 bcs nokey
 lda #$c2
bsky sta $f5
 lda #$eb
 sta $f6
 ldy $cb
 lda ($f5),y
 tax
 cpy $c5        ;matrix coordinate of last key pressed, 64=None Pressed
 bne norep
 jmp $eaf0      ;resume CBM func to decode keystroke 
norep cmp #$85  ;first func key? F1=$85, F3=$86, F5=$87, F7=$88, F2=$89, F4=$8A, F6=$9B, F8=$9C
 bcc nokey
 cmp #$8d       ;last function key (F8 = $8d)?
 bcc fkey
nokey jmp $eb48 ;setup proper keyboard decode table
fkey sec
 sbc #$85     ;convert to func key index 0-7
 asl
 asl          ;key index*16  (16 chars per function key)
 asl
 asl
 tay          ;key string index 
 ldx #$00     ;clear keyboard buffer
 stx $c6      ;count of keys in keyboard buffer
 lda $01
 pha
 dec $01
getlet lda keybuf,y ;get letter
 beq prnkey
 sta KEYD,x    ;keyboard buffer
 iny
 inx
 bne getlet
prnkey stx $c6 ;set num chars in keyboard buffer
 pla
 sta $01
 ldx #$ff
 jmp $eae9     ;continue regular key decode func
;
;*******************
;* color flash irq *
;*******************
flash
 dec blinkdly     ;blink delay
 bne irqdone      ;did we count down to zero? if so execute flashing code
 lda #20          ;reset flash delay
 sta blinkdly     ;global variable for flash delay
 lda blinktyp     ;get current flash type
 eor #%10000000   ;alternate bit 7 on every flash 
 sta blinktyp     ;new value to OR into screen RAM
;init mem pointers
 lda #$d8
 sta mem1+2
 lda HIBASE
 sta mem2+2
 sta mem3+2
;process 4 blocks of 256 bytes of screen text & color fast as possible
 ldy #0
mem1 lda $d800,y  ;get color of text
 and #%00001111   ;text color is in lo nibble, hi nibble may be random value so remove it
 tax
 lda blinkcol,x
 beq nxtblk       ;no so skip it
mem2 lda $0400,y  ;get text char to flash
 and #%01111111   ;ensure bit 7 is off
 ora blinktyp     ;apply alternating value for bit 7
mem3 sta $0400,y  ;apply new text char in screen RAM
nxtblk iny
 bne mem1         ;last text mem to stop is when ($fb) = $07E7
 lda mem1+2
 cmp #$db         ;last color mem hibyte
 beq irqdone
 inc mem1+2
 inc mem2+2
 inc mem3+2
 bne mem1         ;always branches
irqdone jmp $ea31 ;orgininal irq vector
;
;*******************************
;* general purpose subroutines *
;*******************************
;get voice index and SID register offset (voice#-1)*7
;returns voice index (zero-based) in Y reg and the SID register offset in A reg
ppw
 jsr getvoc  ;1,2,3
ppw2
 tay
 dey         ;0,1,2 returned in Y reg
 tya
;calc SID register offset voice1=0, voice2=7, voice3=14
times7       ;multiply A reg * 7 by adding 7, A times
 tax
 beq t7done
 lda #0
nxt7 clc
 adc #7
 dex
 bne nxt7    ;stop adding once x is 0
t7done rts   ;result in accumulator
;*******************
;add 8 to the double byte binary value in A (lobyte) and X (hibyte), Y times
;assumes caller will not multiply by 0 or exceed result of 65535
times8
 clc
 adc #8
 pha
 txa
 adc #0
 tax
 pla
 dey
 bne times8
 rts
;*******************
;add 40 to the double byte binary value in A(lobyte) and X(hibyte), Y times
;assumes caller will not multiply by 0 or exceed result of 65535
times40
 clc
 adc #40
 pha
 txa
 adc #0
 tax
 pla
 dey
 bne times40
 rts
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
comchk jsr CHRGOT  ;get current basic text chr
 cmp #","
 rts
;*******************
comck2 jsr CHRGET  ;get next basic text chr
 cmp #","
 rts
;*******************
chkcomm jsr CHRGOT ;check current basic text for comma
 cmp #","
 beq comma         ;return normally if comma exists
 pla               ;don't return to caller if no comma
 pla
comma rts
;*******************
;check for comma and throw missing op error if not there
ckcom2 jsr CHRGOT
 cmp #","
 beq comma
missop ldx #31     ;missing operand error
 jmp ($0300)       ;vector to print basic error message
;*******************
getstr jsr CHRGET  ;get next basic text chr
getstr0 beq missop
getstr_ jsr FRMEVL ;evaluate expression
getstr2 jsr FRESTR ;discard temp string
 stx $50           ;lowbyte
 sty $51           ;hibyte
 sta $52           ;length
return rts
;******************
getval15_ jsr CHRGET
getval15_0 beq missop
getval15 jsr skip73
 cmp #16           ;enforce 0-15 range
 bcc return
illqty4 jmp FCERR  ;illegal quan.
;*******************
;get a single byte int (0-255) throw error if bad data type or range
getval jsr CHRGET
skip73_ beq missop
skip73 jsr FRMNUM  ;eval numeric expr & type, store result in FAC1
fac2int jsr GETADR ;convert FAC1 to unsigned 2 byte int stored in $14,$15
 lda $15           ;if hi byte is not zero then
 bne illqty4       ;throw ill qty err
 lda $14           ;return the result in the a register
 rts
;*******************
dbyval jsr CHRGET  ;get a 2 byte int (0-65535)
skp73_ beq missop
skp73 jsr FRMNUM   ;eval numeric expr & type
 jsr GETADR        ;convert fac1 to unsigned 2 byte int
 ldx $14
 ldy $15
 rts
;*******************
sprnum
 jsr skip73_
 cmp #8            ;valid sprite numbers 0-7
 bcc less8
 ldx #33           ;illegal sprite number
 jmp ($0300)
less8 sta $be
 tay
 lda bitweights,y
 sta $bf           ;2^sprite#
 rts
;*******************
getvoc
 jsr skip73_
 beq iverr
 cmp #4
 bcc getvoc-1
iverr ldx #32
 jmp ($0300)      ;illegal voice number
;*******************
addoffset lda $fb
 sta $fd
 lda #$d8         ;hibyte of new page ptr
 sec
 sbc HIBASE       ;current page
 sta $02
 lda $fc
 clc
 adc $02
 sta $fe
 rts
;********************
;this function entry point is called by commands PLOT,LINE,CIRCLE,PAINT
getpnt
 lda lastplott
 sta $fe        ;default plot type
 jsr pntweg     ;get x,y, plot type
 jmp savepoint
;*******************
types jsr comchk ;current char a comma?
 beq gettypes
 lda lastplott  ;get last plot type
 sta $fe        ;current plot type
 rts
gettypes jsr CHRGET ;position to next char
 beq etypes     ;end of statement reached
 cmp #","
 beq noparam    ;another comma found so skip plot type param
 jsr skip73     ;get plot type value
 cmp #3         ;plot type 0=erase, 1=plot, 2=toggle
 bcc goodtype
badtype jmp FCERR ;illegal quantity error
goodtype sta lastplott
noparam jsr chkcomm  ;if current char is not a comma do not return here
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 bne mcplot
 jsr CHRGET
 jmp getc1      ;get hires plot color 0-15
mcplot jsr getval ;get color selection, multicolor selection index 1-3
 beq badtype
 cmp #4         ;mc mode color index selection is 1,2 or 3
 bcs badtype
 asl            ;convert index to ptab offset 1=8, 2=16, 3=24
 asl
 asl
 sta mapcolbits ;offset = index * 8 where index in (1,2,3)
etypes rts
;
;*****************
;these 2 routines are used by the VARS cmd since it runs under ROM but needs ROM functions
rom1 inc $01     ;switch to rom (a000-bfff)
 jsr GIVAYF      ;convert 16-bit signed int to float (a=hibyte y=lobyte)
 jmp prtnum
rom2 inc $01     ;switch to LOROM (a000-bfff)
 jsr MOVFM       ;move a float from memory to fac1
prtnum jsr FOUT  ;convert fac1 to ascii with str ptr in a,y registers
 jsr STROUT      ;print string ptr (a=lobyte y=hibyte)
 dec $01         ;switch to LORAM (a000-bfff)
 rts

;********************************************************************
;* Global Constant Storage
;********************************************************************

;CSR mem locs $d3=logical column, $d6=physical line, $cc=blink enabled, $d5=max columns for current line (39 or 79), $ce=char under csr (when blinking)
csrbytes .byte $d3,$d6,$cc,$d5,$ce

;scroll direction vectors up,down,left,right
scrolls .word scroll0,scroll1,scroll2,scroll3

direc .text "$"
nolin .null "65535"
filestr .null " files."

;table for calculating 2^n where n=0-7
bitweights .byte 1,2,4,8,16,32,64,128
;
;token list that use line numbers that need to be renumbered when using renum
; goto,gosub,then,else,resume,trace,delete,run,restore
gotok .byte TOKEN_GOTO,$8d,TOKEN_THEN,TOKEN_ELSE,TOKEN_RESUME,$f2,$f4,TOKEN_RUN,TOKEN_RESTORE
;
;SOUND command use SOUND voc#, frequency
;REG_VAL=FREQUENCY/(CLOCK/16777216)
;FREQUENCY=(REG_VALUE*CLOCK/16777216)Hz
;where CLOCK NTSC=1022730, PAL=985250
;NTSC 1Hz Freq Value = 1022730/16777216 = 0.0609594583
;PAL  1Hz Freq Value =  985250/16777216 = 0.0587254763
;below are the FAC values for 1 unit in Hz for both CLOCK speeds
;
ntsc .byte $7c,$79,$b0,$9f,$fc
pal  .byte $7c,$70,$8a,$20,$03
;
;PULSE command use PULSE voc#, width%
;used for converting register value to frequency in Hz
;Formula REG_VALUE = 40.95 * width%
m4095 .byte $86,$23,$cc,$cc,$cc ;FAC binary representation of 40.95
;FILTER command use FILTER frequency, resonance, type
five8 .byte $83,$39,$99,$99,$99  ;FAC binary representation of 5.8
;

;********************************************************************
;* Global Variable Storage
;********************************************************************

traceflag  .byte 0 ;trace flag 0=off, 1-on
listflag   .byte 0 ;list flag 0=off, 1=on
keyflag    .byte 0 ;key capture mode 0=disabled, 1=enabled, 2=paused
keyentry   .byte 0 ;byte0 the scan code of the last key captured with ON KEY statement enabled
errnum     .byte 0 ;last error number that occured, default 0 (no error)
errline    .word $FFFF ;last line number causing error, default -1 (not applicable)
errtrap    .word 0 ;error handler line number
txtptr     .word 0 ;basic txt ptr of statement causing error
stackptr   .byte 0 ;stack ptr before cmd execution for use by ON ERROR
keyptr     .word 0 ;basic txt ptr of statement for ON KEY GOSUB line#
keyline    .word $FFFF ;line number for ON KEY subroutine

;SOUND COMMAND:
md417      .byte 0   ;holds current filter control and resonance
md418      .byte 0   ;holds current volume (lo nibble) and filter type (hi nibble)

;blink flag for each color (when bit 7 is 1 then blink is off)
blinkcol   .repeat 16,0 ;16 bytes indicates which of the 16 colors (0-15) flash 0=flash, non-zero=no flash
blinkdly   .byte 20  ;color blink delay countdown temp var
blinktyp   .byte 0   ;bits to OR into screen ram to blink text; bit 7 flips on/off every flash
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
playtime   .byte 0  ;,0,0  ;current jiffies till next note for each voice
playvoice  .byte 0  ;,2,4  ;SID register offset for each voice
playwave   .byte 0  ;,0,0  ;waveform for each voice

;********************************************************************
;* Temp variables during subroutine execution only (local vars)     *
;********************************************************************

;this temp space is used to save last plot x,y so sprite move can use it
lastplotx2
.word 0  ;move cmd temp copy of last used x coord
.byte 0  ;move cmd temp copy of last used y coord
.byte 0  ;plot type
;
;********************************************************************
;* ROM barrier at A000-BFFF switch to RAM using $01 before entry    *
;********************************************************************
;
*=$a000 ;"MDBASIC RAM under ROM Memory Block"
.byte 0 ;place holder for this spot seems to be messed up on start
;*** function key assignments ***
keybuf .text "list"
.byte 13,0,0,0,0,0,0,0,0,0,0,0   ;F1
.text "load"
.byte 34,0,0,0,0,0,0,0,0,0,0,0   ;F3
.text "files"
.byte 13,0,0,0,0,0,0,0,0,0,0     ;F5
.text "keylist"
.byte 13,0,0,0,0,0,0,0,0         ;F7
.text "run"
.byte 13,0,0,0,0,0,0,0,0,0,0,0,0 ;F2
.text "save"
.byte 34,0,0,0,0,0,0,0,0,0,0,0   ;F4
.text "text"
.byte 13,0,0,0,0,0,0,0,0,0,0,0   ;F6
.text "cls"
.byte 13,0,0,0,0,0,0,0,0,0,0,0,0 ;F8
;
;strings for keylist
addcr .null "+chr$(13)"
;
;PLAY command temp vars used during IRQ
temp1     .byte 0
temp2     .byte 0
playoct   .byte 4  ;default octave 4
playlen   .byte 30 ;notelength for each voice
playfreq  .byte 0  ;note freq value offset for each voice
playindex .byte 0  ;index of current char in play string for each voice
;** play table ***
;These numbers represent the middle octave and are only for the SID freq control registers
;CLOCK is NTSC=1022730, PAL=985250
;FREQUENCY=REG_VALUE*(CLOCK/16777216)Hz
;REG_VALUE=FREQUENCY/(CLOCK/16777216)Hz
;https:;pages.mtu.edu/~suits/notefreqs.html
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
;
;align tables to the nearest page boundary (saves a cycle on read)
* = (* & $ff00)+$0100

;align to the nearest page boundary
playbuf1 .repeat 256,0
;playbuf2 .repeat 256,0
;playbuf3 .repeat 256,0

;temp storage for PAINT and SCROLL command
paintbuf1 .repeat 256,0
paintbuf2 .repeat 256,0
paintbuf3 .repeat 256,0

;tables for plotting dots on a bitmap
bmdt
.byte $00,$03,$0c,$0f,$30,$33,$3c,$3f,$c0,$c3,$cc,$cf,$f0,$f3,$fc,$ff
.byte $f2,$1a,$02,$12,$97,$20,$20,$20,$f2,$1a,$03,$20,$20,$20,$f2,$1b
.byte $04,$20,$f2,$09,$05,$a1,$20,$f2,$17,$05,$20,$20,$20,$20,$20,$20
.byte $20,$20,$20,$f2,$08,$06,$92,$a2,$a2,$12,$bc,$92,$a2,$bb,$f2,$1b
.byte $06,$12,$20,$f2,$0a,$07,$92,$a1,$f2,$1b,$07,$12,$20,$f2,$09,$08
.byte $a1,$bb,$f2,$1a,$08,$20,$20,$20,$f2,$09,$09,$be,$a1,$92,$bb,$f2
.byte $1a,$09,$12,$20,$92,$20,$12,$20,$f2,$1a,$0a,$20,$92,$20,$12,$20
.byte $f2,$19,$0b,$20,$20,$92,$20,$12,$20,$20,$f2,$05,$10,$92,$98,$00

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
;
;KEY LIST continued while LOROM is switched to LORAM
keyy stx $fb
 sty $fc
 lda $02
 lsr
 bcc even
 ldx #<keybuf
 stx $fd
 jmp strbuf
even ldx #<keybuf+$40
 stx $fd
 sec
 sbc #1
strbuf asl
 asl
 asl           ;key(#1-4)*16
 asl
 clc
 adc $fd
 sta $fd
 lda #>keybuf
 adc #0
 sta $fe
 ldy #0
nextc lda ($fb),y
 sta ($fd),y
 iny
 cpy #15     ;only the first 15 chars of string (indexed 0-14)
 beq endass
 cpy $52     ;end of new string?
 bne nextc
endass lda #0 ;terminate string with zero-byte
 sta ($fd),y
 iny
 cpy #16     ;fill remaining bytes with 0
 bcc endass+2
 jmp r6510   ;switch LORAM back to LOROM
;***************
;KEY LIST (code in LORAM)
;
keylistt lda #13 ;cr
 jsr CHROUT
 lda #$04
 sta $02
 lda #"1"
 sta $bb ;key#
 lda #<keybuf
 sta $fb ;odd keys F1, F3, F5, F7
 sta $be
 lda #>keybuf
 sta $fc  ;hi byte for odd keys
 sta $bf
 sta $fe  ;hi byte for even keys is same hi byte since all keys live inside 64 bytes
 lda #<keybuf+$40
 sta $fd ;even keys offset F2, F4, F6, F8
nextke jsr kprnt
 lda $fb
 clc
 adc #$10
 sta $fb
 lda $fd
 sta $be
 lda $fe
 sta $bf
 inc $bb ;key#+1
 jsr kprnt
 lda $fd
 clc
 adc #$10 ;16 bytes per string, per key
 sta $fd
 dec $02
 bne exchng
 jsr CHRGET
 jmp r6510 ;switch LORAM back to LOROM
exchng lda $fb
 sta $be
 lda $fc
 sta $bf
 inc $bb
 jmp nextke
;--------
kprnt ldy #$ff
 lda #"k"
 jsr CHROUT
 lda #"e"
 jsr CHROUT
 lda #"y"
 jsr CHROUT
 lda $bb
 jsr CHROUT
 lda #","
 jsr CHROUT
 lda #"""
 jsr CHROUT
nextlt iny
 lda ($be),y
 beq stoppr
 cmp #13 ;cr?
 bne nocr
 lda #"""
 jsr CHROUT
 ldx #0
nextch lda addcr,x ;print "+chr$(13)"
 beq addedcr
 jsr CHROUT
 inx
 bne nextch
addedcr iny
 lda ($be),y
 bne chrprs
 dey
 jmp prntit
chrprs lda #"+"
 jsr CHROUT
 lda #"""
 jsr CHROUT
 dey
 jmp nextlt
prntit lda ($be),y
 jsr CHROUT
 rts
nocr lda ($be),y
 jsr CHROUT
 jmp nextlt
stoppr lda #"""
 jsr CHROUT
 lda #$0d
 jmp CHROUT
;
;**************************
linedraw
 lda lastplotx  ;destination x coord lobyte
 sec
 sbc $fb
 sta $57
 lda lastplotx+1  ;destination x coord hibyte
 sbc $fc
 sta $58
 lda lastploty
 sec
 sbc $fd  ;destination y coord
 sta $6b  ;temp var for distance = y1-y2
 ldy #1
 ldx #0
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 beq noincy  ;hires mode
 iny
noincy lda $fc  ;determine which x coord is larger
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
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 beq dniy
 iny
dniy
 lda lastploty
 cmp $fd
 bcs stya7
 tya
 eor #$ff
 tay
 iny
 lda $fd
 sec
 sbc lastploty
 sta $6b
stya7 sty $a7
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
 cmp #200     ;if y coord is out of bounds then we must be done
 bcs linedone ;done
jmpout1 sta $fd      ;y coordinate
jmpout jmp starts
linedone ldy moveflag ;sprites can have a y coord to 255
 bne jmpout1   ;moving a sprite so continue
 rts
pokadd lda moveflag  ;flag 0=LINE cmd, >0=MOVE cmd
 beq reglin ;setdot
;hack to move a sprite instead of plot line
 lda $fc    ;temp var hibyte of x coord
 beq nod010 ;x is less than 256
 lda $07    ;temp var of sprite's bit#
 ora MSIGX  ;MSB of sprites 0-7 x coordinate 
 bne std010
nod010 lda $07
 eor #$ff
 and MSIGX
std010 sta MSIGX
 lda $fb
 ldy $0f    ;temp var of sprite reg index
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
;this section used by LINE plot only------------
reglin
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 beq setdot
 lda $fd
 cmp lastploty
 beq setdot
 cmp #199
 beq setdot
 inc $fd
 jsr setdot
 dec $fd
;
setdot jsr ydiv8
 lda $01
 pha
 and #%11111101 ;bit1 0=HIRAM
 sei            ;disable IRQ since kernel HIROM is switching to HIRAM
 sta $01
 lda lastplott  ;plot type 0=off, 1=on, 2=flip (use to be $fe)
 beq dotoff
 cmp #1
 beq doton
 bne flipit
dotoff lda ptab2,x  ;read ptab2 and ptab3 in LORAM
 eor #$ff
 and ($c3),y         ;read bitmap in HIRAM
 jmp colorb
doton lda ($c3),y
 and ptab3,x
 ora ptab2,x
 jmp colorb
flipit lda ptab3,x
 eor #$ff
 and ($c3),y
 ora ptab2,x
 eor ($c3),y
colorb sta ($c3),y ;write byte with bit pattern targeting the one bit in hires or 2 bits in mc mode
 pla
 sta $01    ;restore Kernel HIROM ($e000-$ffff)
 cli
;apply color
 lda $fb
 lsr
 lsr
 lsr
 sta $c3
 lda $fc
 asl
 asl
 asl
 asl
 asl
 clc
 adc $c3
 sta $c3
 lda #$c8  ;bitmap screen RAM at $C800 in bitmap mode
 adc #$00
 sta $c4
 lda $fd   ;y coordinate
 lsr
 lsr
 lsr
 beq noytim
 tay
 lda $c3
 ldx $c4
 jsr times40
 sta $c3
 stx $c4
noytim lda mapcolc1c2  ;hires dot color (hi nibble) and background color of 8x8 square (lo nibble)
 ldy #0
 sta ($c3),y
 lda $c4
 clc
 adc #$10    ;calculate beginning of color RAM
 sta $c4
 lda mapcolc3   ;background color mem used for mc mode
 sta ($c3),y ;$d800
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
 ora mapcolbits  ;apply color selection bit pattern
 tax
ydiv8x ldy #0
 rts
;
;**************************
texter lda $58  ;y size
 beq texter-1
 lda $57        ;x size
 beq texter-1
nextchar lda #0
 sta $59        ;index variable for current byte of dot data in char
 sta $be
 lda $26        ;charset 0 at $d000, charset 1 at $d800
 sta $bf        ;charset ptr hibyte
readstr ldy #0
 lda ($50),y    ;get character to display on bitmap
 cmp #"a"
 bcc lessthana
 cmp #96
 bcc goodalpha
 cmp #128
 bcs bit7set
minus128 sec 
 sbc #128
 jmp lessthana
bit7set cmp #$c0
 bcs minus128
goodalpha sec 
 sbc #$40
lessthana tay 
 beq doloop3
 lda $be
 ldx $bf
 jsr times8
 sta $be
 stx $bf
doloop3 lda $58
 sta $5a    ;temp var for y size multiplication by decrement loop
doloop2 lda #128
 sta $5c
doloop1 lda $57
 sta $5b    ;temp var for x size multiplication by decrement loop
doloop ldy $59
 lda $01
 pha
 and #%11111011 ;bit2=0 switch in CHAREN ROM into bank $d000-$dfff
 sei 
 sta $01     ;use CHAREN (rom char images)
 lda ($be),y ;read CHAREN byte data for character (8 bytes, y=byte#) 
 tax
 pla
 sta $01     ;back to normal
 cli
 txa 
 and $5c
 beq nextfc
 lda $fc
 beq getfd
 lda $fb
 cmp #$40
 bcs nextfc
getfd lda $fd
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
 lda $57
 asl
 asl
 asl
 clc 
 adc lastplotx
 sta lastplotx
 lda lastplotx+1
 adc #0
 sta lastplotx+1
 inc $50
 bne noinc51
 inc $51
noinc51
 jsr getpoint ;recall last plotted dot x,y and type
 dec $52
 beq textdone
 jmp nextchar
textdone rts
;
;**************************
painter jsr x1y7
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
.byte $2c       ;alt entry point to make next 2 lines this -> LDA #1, BIT $00A9
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
.byte $2c      ;alt entry point to make next 2 lines this -> LDA #1, BIT $00A9
law00 lda #$00
 sta $59
fdp1 inc $fd
 jsr setdot    ;set dot if needed
 lda $fb
 clc
 adc $5a
 sta $fb
 lda $fc
 adc #$00
 sta $fc
; jsr STOP  ;check if STOP key was pressed - uses kernel ROM so don't use this
 lda $c5   ;Matrix Coordinate of Last Key Pressed, 64=None Pressed
 cmp #$3f  ;STOP key?
 beq epant ;stop painting
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
epant jsr norm   ;restore text mode (stop key pressed)
epaint jmp r6510 ;switch LORAM back to LOROM and HIRAM back to HIROM
readb jsr ydiv8
 stx $aa
 lda $01
 and #%11111101 ;bit1 0=HIRAM
 sei            ;disable IRQ since kernel HIROM is switching to HIRAM
 sta $01
 lda ptab3,x
 eor #$ff
 and ($c3),y  ;bitmap in HIRAM
 pha
 lda $01
 ora #%00000010 ;bit1 1=HIROM
 sta $01
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
x1y7 ldx #$01
 ldy #$07
 lda SCROLX
 and #%00010000 ;check if multicolor mode on or off
 beq xy00
 inx
 dey
xy00 stx $5a
 sty $5b
 rts
;**************************
circel lda $fb
 sta $61
 lda $fc
 sta $62
 asl $61
 rol $62
 lda $fd
 sta $63
 lda #0
 sta $fe
 sta $64
 asl $63
 rol $64
 lda $35
 clc
 adc $35
 sta $11
 lda #$00
 adc #$00
 sta $12
 lda $36
 clc
 adc $36
 sta $14
 lda #$00
 adc #$00
 sta $15
 ldx #1
tcatb2 lda $11,x
 sta $57,x
 dex
 bpl tcatb2
 ldx #1
tcataf lda $11,x
 sta $50,x
 dex
 bpl tcataf
 lda #2
 ldx #3
 ldy #1
 jsr curve
 ldx #2
tb6tc4 lda $59,x
 sta $22,x
 dex
 bpl tb6tc4
 ldx #1
tcctb2 lda $14,x
 sta $57,x
 dex
 bpl tcctb2
 ldx #1
tcctaf lda $14,x
 sta $50,x
 dex
 bpl tcctaf
 lda #2
 ldx #2
 ldy #1
 jsr curve
 ldx #1
tb6tc7 lda $59,x
 sta $0E,x
 dex
 bpl tb6tc7
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
 lda $12
 sta $fc
 lda #0
 sta $fd
 sta $fe
 ldx #1
tfbtb2 lda $fb,x
 sta $57,x
 dex
 bpl tfbtb2
 ldx #1
tc7taf lda $0E,x
 sta $50,x
 dex
 bpl tc7taf
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
 lda lastplott
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
 sta $fc
 lsr $fc
 ror $fb
 lda $fd
 clc
 adc $63
 sta $fd
 lda $fe
 adc $64
 sta $fe
 lsr $fe
 ror $fd
 jsr plotc
 lda $61
 sec
 sbc $6f
 sta $fb
 lda $62
 sbc $70
 sta $fc
 lsr $fc
 ror $fb
 jsr plotc
 jsr t2d4fb
fd512 lda $fd
 clc
 adc #2
 sta $fd
 lda $fe
 adc #$00
 sta $fe
 ldx #1
tfd2b2 lda $fd,x
 sta $57,x
 dex
 bpl tfd2b2
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
dpcsym ldx #1
 lda $fb,x
 sta $57,x
 dex
 bpl dpcsym+2
 ldx #1
pc7af lda $0E,x
 sta $50,x
 dex
 bpl pc7af
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
 ldx #1
tfdtb2 lda $fd,x
 sta $57,x
 dex
 bpl tfdtb2
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
 sta $fc
 lsr $fc
 ror $fb
 lda $fd
 clc
 adc $63
 sta $fd
 lda $fe
 adc $64
 sta $fe
 lsr $fe
 ror $fd
 jsr plotc
 lda $61
 sec
 sbc $6f
 sta $fb
 lda $62
 sbc $70
 sta $fc
 lsr $fc
 ror $fb
 jsr plotc
 lda $63
 sec
 sbc $71
 sta $fd
 lda $64
 sbc $72
 sta $fe
 lsr $fe
 ror $fd
 jsr plotc
 lda $61
 clc
 adc $6f
 sta $fb
 lda $62
 adc $70
 sta $fc
 lsr $fc
 ror $fb
 jsr plotc
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
loops lda $14
 sta $fd
 lda $15
 sta $fe
 lda #0
 sta $fb
 sta $fc
 ldx #1
respls lda $fd,x
 sta $57,x
 dex
 bpl respls
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
 sta $fc
 lsr $fc
 ror $fb
 lda $fd
 clc
 adc $63
 sta $fd
 lda $fe
 adc $64
 sta $fe
 lsr $fe
 ror $fd
 jsr plotc
 lda $63
 sec
 sbc $71
 sta $fd
 lda $64
 sbc $72
 sta $fe
 lsr $fe
 ror $fd
 jsr plotc
 jsr t2d4fb
loops2 lda $fb
 clc
 adc #2
 sta $fb
 lda $fc
 adc #0
 sta $fc
 ldx #1
fbb2 lda $fb,x
 sta $57,x
 dex
 bpl fbb2
 ldx #1
c7af lda $0e,x
 sta $50,x
 dex
 bpl c7af
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
ncs ldx #1
 lda $fd,x
 sta $57,x
 dex
 bpl ncs+2
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
 ldx #1
plx lda $fb,x
 sta $57,x
 dex
 bpl plx
 ldx #1
plxx lda $0e,x
 sta $50,x
 dex
 bpl plxx
 lda #2
 ldx #3
 ldy #1
 jsr curve
 lda $6c
 beq zero6c
loops2_ jmp loops2
zero6c ldx #2
 lda $59,x
 cmp $69,x
 bcc loops2_
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
 lda $6a
 bne loops2_
 lda $69
 cmp #3
 bcs loops2_
c69w3 rts
plotc lda $fc
 beq chbyc
 cmp #2
 bcs c69w3
 lda $fb
 cmp #64
 bcs c69w3
chbyc lda $fe
 bne c69w3
 lda $fd
 cmp #200
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
; and #%00000010 ;bits 1,2,3 hold base addr for text dot data
; ora VMCSB      ;include other bits 4-7 that holds video matrix base addr
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
 jsr $f68f  ;print SAVING
 ldx $b8    ;current logical file number
 jsr CHKOUT ;set stdout to current logical file number
 lda $02    ;fake secondary address (2,3,4)
 cmp #2     ;2=screen
 bne sdvn2
savscr jsr param
 lda $c4
 clc
 adc #3
 sta $02
 lda EXTCOL ;border color
 jsr CHROUT
 lda BGCOL0 ;background color
 jsr CHROUT
 lda VMCSB
 jsr CHROUT
 lda SCROLX       ;save bit 4 for multicolor text or bitmap flag 
 jsr CHROUT
 lda SCROLY       ;save bit 5 for bitmap mode flag
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
sdvn2 cmp #3    ;3=CHAREN
 bne savbm
 jsr param2     ;prepare pointer for dot data base addr
savchr sei
 dec $01
 lda ($c3),y
 inc $01
 cli
 jsr CHROUT
 jsr status     ;check for stop key or EOF and do not return here if so
 inc $c3
 bne savchr
 inc $c4
 bne savchr
 jmp romin
;save a bitmap image with colors
savbm jsr param3  ;4=BITMAP, prepare pointers for bitmap and color mem
savbtm sei
 dec $01          ;read byte from bitmap under ROM
 lda ($c3),y
 inc $01
 cli
 jsr CHROUT
 jsr status  ;check for stop key or EOF and do not return here if so
 inc $c3
 bne savbtm
 inc $c4
 bne savbtm
 jsr param   ;prepare pointers for text and color mem
 lda #$c8    ;override hibyte to correct for bitmap mem at $C800
 sta $c4
 jmp savscr+3   ;finish by saving the screen mem bytes
;
lodsav          ;real secondary device in x reg
 lda $b9        ;secondary address
 sta $02        ;save fake secondary device # (2,3,4)
 stx $b9        ;replace secondary device with desired real value
 jsr OPEN       ;perform OPEN
 bcc status-1   ;return if ok, close and fail if error
 jmp clos7f
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
param2 ldy #0
 sty $c3
 lda #$f0
 sta $c4
 rts
param3 ldy #0
 sty $c3
 lda #$e0
 sta $c4
 rts
;
;*********************
varss lda $2d  ;vector ($2d,$2e) beginning of non-array variable storage
 cmp $2f        ;vector ($2f,$30) beginning of array variable storage
 bne copy2d     ;if both vectors point at same mem loc then no vars defined
 lda $2e
 cmp $30
 bne copy2d
 jmp evar
copy2d lda $2d
 sta $fb
 lda $2e
 sta $fc
xvar ldy #0
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
type lda $fb ;skip over 2 byte name
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
 and #$7f    ;remove bit 7
 jsr CHROUT  ;output first char of name
 lda $46     ;get second char of name
 and #$7f    ;remove bit 7
 jsr CHROUT  ;output second char of name
 lda $0d     ;var type?
 beq float   ;type 0 is float
 cmp #1      ;type 1 is string
 beq string  ;otherwise int
 lda #"%"    ;display integer symbol
 jsr CHROUT
 lda #"="    ;display equals symbol
 jsr CHROUT
 ldy #0
 lda ($fb),y ;first byte is lobyte for int
 pha
 iny
 lda ($fb),y ;second byte is hibyte for int
 tay         ;lobyte in y reg
 pla         ;hibyte in a reg
 jsr rom1    ;switch ROM in and call routines to print int in a,y regs
 jmp nxtvar  ;process next variable
float lda #"="
 jsr CHROUT
 lda $fb
 ldy $fc
 jsr rom2
 jmp nxtvar
string lda #"$"
 jsr CHROUT
 lda #"="
 jsr CHROUT
 lda #"""
 jsr CHROUT
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
endquote lda #"""
 jsr CHROUT
nxtvar lda #13
 jsr CHROUT
 jsr STOP
 beq evar
sft lda SHFLAG
 cmp #1
 beq sft
 lda $fb
 clc
 adc #5
 sta $fb
 lda $fc
 adc #0
 sta $fc
 lda $fb
 cmp $2f
 bne jp
 lda $fc
 cmp $30
 bne jp
evar
 jmp r6510 ;switch LORAM back to LOROM
jp jmp xvar
;
;****************

;***************
dumpscreen2
 ldy #$00        ;secondary parmeter 0 = assume upper case with symbols
 lda VMCSB       ;text mode upper case with symbols or upper/lower case?
 cmp #%00010111  ;23=upper/lower case, 44=upper case/symbols
 bne upcase      ;yes, use upper/lower case text
 ldy #$07        ;7=upper and lower case, 0 = upper case and symbols
upcase jsr openprint
 lda #27
 jsr CHROUT      ;Kernal Routine CHROUT output a character
 lda #"3"
 jsr CHROUT
 lda #25
 jsr CHROUT
 lda #0
 sta $fb
 sta $fe     ;column counter
 lda HIBASE  ;top page of screen mem hibyte
 sta $fc
 ldy #$00
pchr lda ($fb),y
 pha
 bpl testit
 sec
 sbc #128
testit cmp #32
 bcs big32    ;larger than 32
add64 clc
 adc #64
 jmp dumpit
big32 cmp #64
 bcc dumpit ;bcs big64  ;less than 64
; jmp dumpit
big64 cmp #96
 bcs add64   ;larger than 96
 clc
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
 lda #13
 jsr CHROUT
 lda #0
 sta $fe
infbfc inc $fb
 bne stopyn
 inc $fc
stopyn lda $fc
 cmp #7    ;check if last address 
 bne pchr
 lda $fb
 cmp #232
 bne pchr
 lda #$0d
 jsr CHROUT
 rts
dumpbitmap2 
 ldy #5   ;secondary param - binary graphic 
 jsr openprint
;send printer control codes
okbm lda #27
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
 lda SCROLY
 and #%11101111 ;turn off bitmap mode
 sta SCROLY
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
;select HIRAM and disabled IRQ
 lda $01        ;bit0 0=LORAM 1=LOROM ($a000-$bfff), bit1 0=HIRAM 1=HIROM ($e000-$ffff)
 pha            ;save current mem bank setting
 and #%11111101 ;turn bit 1 off for HIRAM
 sei            ;****disable irqs while HIROM is not available
 sta $01        ;apply mem bank new selection
;read byte from bitmap image in HIRAM
 lda ($fb),y
 tax            ;save in x
;restore mem bank original selection
 pla
 sta $01
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
 dec $61 ;temp var
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
 lda #13
 jsr CHROUT
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
aldone lda SCROLY
 ora #%00010000
 sta SCROLY
 lda #27
 jsr CHROUT
 lda #50
 jsr CHROUT
 rts
;
;***********************
;scroll up
scroll0 jsr wrapit
 ldy $be
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
 dec $bf
 bpl cpyup
 lda $ff      ;wrap?
 beq nowrapup
wrapup lda paintbuf1,y
 sta ($fb),y
 lda paintbuf2,y
 sta ($fd),y
 dey 
 bpl wrapup
 jmp r6510    ;switch LORAM back to LOROM
nowrapup lda #32
 sta ($fb),y
 lda COLOR    ;current cursor foreground color
 sta ($fd),y
 dey 
 bpl nowrapup
 bmi nowrapup-3
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
scroll1 ldy $bf
 beq notimes40
 lda $fb
 ldx $fc
 jsr times40
 sta $fb
 stx $fc
notimes40
 jsr addoffset
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
 lda $ff
 beq nowrapup
 bne wrapup
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
 jmp r6510 ;switch LORAM back to LOROM
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
scrollh lda paintbuf1
 ldx $ff     ;wrap?
 bne shiftchar
 lda #32     ;no wrap, use space
shiftchar sta ($fb),y
 lda paintbuf2
 ldx $ff
 bne shiftcolor
 lda COLOR   ;current foreground color for text
shiftcolor sta ($fd),y
;add 40 to text ptr
 lda $fb
 clc
 adc #40
 sta $fb
 lda $fc
 adc #0
 sta $fc
;add 40 to color ptr
 lda $fd
 clc 
 adc #40
 sta $fd
 lda $fe
 adc #0
 sta $fe
 rts 
;--------------
wrapit ldy $be
cpybuf lda ($fb),y
 sta paintbuf1,y ;char mem buffer
 lda ($fd),y
 sta paintbuf2,y ;color mem buffer
 dey 
 bpl cpybuf
 rts
;
nextn2
 jsr notegot
 beq nextn2-1
nextn
 cmp #"h"       ;notes A-G only
 bcs nonnote
goodnote
 sec            ;ascii of notes must start at A (65)
 sbc #"a"       ;ascii - A(65) starts at 0 for A, 1 for B, etc.
 bmi octchg
 asl            ;note# * 2 --> note numbers A=0, B=1, etc.
 sta playfreq   ;save current note freq offset (word ptr)
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
 ora #%00000001 ;and start attack/decay cycle
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

octchg
 cmp #251 ;<
 bne octup2
prevoct
 dec playoct
 bpl nextn3
 bmi nextoct
octup2
 cmp #253  ;>
 bne nextn2 
nextoct
 inc playoct
 lda playoct
 cmp #8
 bcs prevoct
nextn3
 inc playindex ;skip over char
 jmp nextn2

nonnote 
 cmp #"v"
 bne notepause
 jsr getdigitval
 beq badvoc     ;voice 0 invalid
 cmp #4         ;voice 1,2 or 3 only
 bcc goodvoc
badvoc lda #1  ;use default voice 1
goodvoc
 jsr ppw2       ;get SID register offset for voice 
 jsr initvoice
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
; bmi badnote  ;value 0-99 only
 sta playlen    ;apply new note length
 jmp nextn2

noteoct
 cmp #"o"
 bne notewave
 jsr getdigitval
 cmp #8
 bcs skipnote
 sta playoct
 bcc skipnote  ;always branches
 
notewave
 cmp #"w"
 bne skipnote
 jsr getdigitval
 cmp #9
 bcs skipnote
 asl          ;convert to waveform bit pattern
 asl
 asl
 asl
 sta playwave  ;set new waveform
 ldx playvoice
 sta VCREG1,x   ;bit0 = start release
skipnote jmp nextn2

getdigitval    ;get 2-digit value 0-99
 jsr noteget
 beq digitdone2 ;end of string, assume 0 value
 sec            ;convert ascii digit to binary value
 sbc #"0"       ;first char must be digit between 0 and 9
 bpl digit9
nondigit
 lda #$ff       ;flag for non numeric digit
 bne digitdone2
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
 clc
 adc temp1      ;+(2*x)
 adc temp2      ;+y
 sta temp1
digitdone
 lda temp1
digitdone2
 rts            ;result is in A reg and $fb

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
;end
