; OLDSTYLE CRACKTRO. CODED BY ALX/BW/XPJ

; This Source Code Form is subject to the terms of the MIT
; License. If a copy of the source was not distributed with
; this file, You can obtain one at:
; https://github.com/alexanderbazhenoff/brainwave-cracktro-v1

        ORG #6000


BORDER  EQU 4
D_LOGO  EQU 57452            ;DECRUNCHED LOGO
D_DLOP  EQU #8200+#21C+72    ;DECRUNCHED LOGO
                             ;OUTPUT PP
D_TOP   EQU #940B            ;DECRUNCHED TXT OTPUT PP
D_TAI   EQU #B9AE            ;DECRUNCHED TOP'S ADRESS INSTALL PP
D_SINL  EQU #DE00         ;DECRUNCHED SINUS TABL 4 LOGO'S ACTION
D_SINP  EQU #DC00         ;DECRUNCHED SINUS TABL 4 CHANGE PAGES
D_FONT  EQU #4000         ;REMOVED FONT
;DFONT  EQU #5000
D_FONTA EQU D_FONT-#108
D_PRNTX EQU #DB00         ;DECRUNCHED X TABL 4 PRINT CHR
D_PRNTY EQU #DA00         ;DECRUNCHED Y TABL 4 PRINT CHR
LOGOPIX EQU #4000+160+23           ;COORD (ADDR)
                                   ;OF LOGO PIXELS
LOGOATR EQU #5800+160+23           ;COORD (ADDR)
                                   ;OF LOGO ATTR
OTXTADR EQU #4000+#80+21+#700      ;OUTPUT TXT TO...
LOADADR EQU #5D3B       ;REMOVE LOADER TO...
        ;+---->>>> MUSIC
compile EQU #7600
play    EQU #7605
stop    EQU #7608
MODULE  EQU #BD00
        JP BEGIN
        ;   0123456789ABCDEFGHIJJIHGFEDCBA9876543210
PAGE1   DEFB "                PUMPED!!!",#0D                 ;1
        DB #0D                                             ;2
        DB " -+ B + R + A + I + N + W + A + V + E +-",#0D  ;3
        DB "                   OF",#0D                     ;4
        DB "        X  -  P  R  O  J  E  C  T",#0D         ;5
        DB #0D                                             ;6
        DEFB "BACKS 4 OLD GOOD",#0D                          ;7
        DEFB "               WITH THE GOLDEN GAME FROM",#0D  ;8
        DB #0D                                             ;9
        DEFB "       J O H N     P R A G N E L L",#0D        ;A
        DB #0D                                             ;B
        DEFB "O R B I X - T H E    T E R R O R B A L L",#0D  ;C
        DB "+--------------------------------------+",#0D  ;D
        DB "'ENTER'- NEXT PAGE, 'SPC'- TRAINER MENUE",#0D  ;E
PAGE2   DB "*=------------------------------------=*",#0D;1
        DEFB "               STUFF LIST:",#0D              ;2
        DB #0D
        DEFB "OLDSCHOOL CRACKTRO CODED......ALX/BW/XPJ",#0D;5
        DB "CRACKTRO GFX (LOGO & FONT)....ALX/BW/XPJ",#0D;6
        DB #0D                                           ;7
        DEFB "CRACKTRO MUZAK..............MEGUS/BW/XPJ",#0D;8
        DB #0D                                           ;9
        DEFB "GAME RESTORED.................ALX/BW/XPJ",#0D;A
        DEFB "TRAINED, DISKED, PACKED.......ALX/BW/XPJ",#0D;B
        DB #0D                                           ;C
        DEFB "RELIZE DATE.....................21.07.01",#0D;D
        DB "*=------------------------------------=*",#0D;E
        DEFB "SORRY,JUST ONE MULTICOLOR FIX (PENTAGON)",#0D
PAGE3   DB "                  NOW",#0D;1
        DB " -+ B + R + A + I + N + W + A + V + E +-",#0D;2
        DB "                  ARE:",#0D                  ;3
        DB #0D                                           ;4
        DB "MEGUS................MUZAKER + ORGANIZER",#0D;5
        DB "MAV........CODER + PROGRAMMER + SYSADMIN",#0D;6
        DB "ALX..CRACKER(NOT A COOKES;) + CODER + DJ",#0D;7
        DB "TIGRR..DESIGNER + GFX MAKER + PROGRAMMER",#0D;8
        DB "VIVID............MAIN CODER + PROGRAMMER",#0D;9
        DB "NAVIGATOR...........HARDWARE SERVICE ;-)",#0D;A
        DB "GRAPH EGO.................MAIN GFX MAKER",#0D;B
        DB "X-CYBER.................HARDWARE SERVICE",#0D;C
        DB "ABSTRACT...................CODER + GAMER",#0D;D
        DB "DAN THE HUMANOID......THE HUMANOID + ???",#0D;E
PAGE4   DEFB "               YAHOO! CATCH THE GREETZ!",#0D ;1
        DB #0D
        DB "X-PROJECT, MAD CAT, SERZHSOFT,  ANTARES",#0D ;3
        DEFB "EXTREME,  PLACEBO, RUSH, TRIUMPH,  BUSY",#0D ;4
        DEFB "4TH DIMENSION,  DIGITAL REALITY, HRUMER",#0D ;5
        DEFB "CONCERN CHAOS,  ACCEPT CORP, AXLR,  CTL",#0D ;6
        DEFB "LIGHT FUTURE GROUP,  RAZZLERS,  FREEDOM",#0D ;7
        DEFB "STUDIO STALL,  HOOY PROGRAMS,  RAW ARSE",#0D ;8
        DEFB "REAL MASTERS, K3L, 3SC,  DANZIL, SERGUN",#0D ;9
        DEFB "DELIRIUM TREMENS,   COPPER FEET,  MYHEM",#0D ;A
        DEFB "CYBERPUNKS UNITY,  PROGRESS,  ANDY FERR",#0D ;B
        DEFB #0D                                           ;C
        DEFB "AND TO ALL WHO KEEPS",#0D                    ;D
        DEFB "                     THE SCENE ALIVE!!!",#0D ;E

PAGE7   DB "WANNA CONTACT US?",#0D                       ;1
        DB "               OK! NOW RIGHT IT DOWN...",#0D ;2
        DEFB "MEGUS/BRAINWAVE........................",#0D ;3
        DEFB "E-MAIL: MEGUS_BW@MAIL.RU",#0D                ;4
        DEFB "FIDO: 2:5052/4.169",#0D                      ;5
        DEFB "ZX-NET: 500:8362/1",#0D                      ;6
        DEFB #0D                                           ;7
        DEFB "ALX/BRAINWAVE..........................",#0D ;8
        DEFB "E-MAIL: ALX_BW@MAIL.RU",#0D                  ;9
        DEFB #0D                                           ;A
        DEFB "TIGRR/BRAINWAVE........................",#0D ;B
        DEFB "E-MAIL: TIGRR_BW@MAIL.RU",#0D                ;C
        DEFB "FIDO: 2:5052/25.21",#0D                      ;D
        DEFB "ZX-NET: 500:8362/1.2",#0D                    ;E
PAGE8   DEFB "VIVID/BRAINWAVE........................",#0D ;1
        DEFB "E-MAIL: VIVID_BW@E-MAIL.RU",#0D              ;2
        DEFB "FIDO: 2:5052/30.8",#0D                       ;3
        DEFB "ZX-NET: 500:8362/1.3",#0D                    ;4
        DEFB #0D                                           ;5
        DEFB "ALL MEMBERS OF BRAINWAVE...............",#0D ;6
        DEFB "E-MAIL: BW_XPJ@MAIL.RU",#0D                  ;7
        DEFB #0D                                           ;8
        DEFB "PLEASE, DON'T SEND A SPAM!",#0D              ;9
        DEFB #0D                                           ;A
        DEFB "ALL OUR STUFF AVAIBLE..................",#0D ;B
        DEFB "HTTP://BRAINWAVE.DAX.RU",#0D                 ;C
        DEFB "TRY 2 FIND HIDDEN URI ;-)))",#0D             ;D
        DEFB "                                  C YA!",#0D ;E
PAGE_CH DEFB "     BRAINWAVE TRAINER MENUE (+01)",#0D
        DEFB "+-------------------------------------+",#0D
        DEFB #0D
        DEFB "1. UNLIMIT LIVES....................NOP",#0D
        DEFB #0D
        DEFB "+-------------------------------------+",#0D
        DEFB "    SORRY, THERE ARE ONLY ONE POINT",#0D
        DEFB "(BECAUSE OF MOTHERFUCKING STRUCTURE OF",#0D
        DEFB "   MACHING CODE IN THIS GAME... :-)",#0D
        DEFB #0D
        DEFB "WANNA HAVE A FUN?  O.K. SO, PRESS 'B+W'",#0D
        DEFB "      AFTER LOADING OF THIS GAME!",#0D
        DEFB "+-------------------------------------+",#0D
        DEFB "'ENTER' - TOGGLE, 'SPC' - END SELECTION",#0D
TRAIN1Y DEFB "YEP",#0D
TRAIN2N DB "NOP",#0D
PAGE_L  DW PAGE1
        DW PAGE2
        DW PAGE3
        DW PAGE4
        DW PAGE5
        DW PAGE6
        DW PAGE7
        DW PAGE8
        DW 0
PAGECHL DW PAGE_CH
BEGIN   LD A,#3F
        LD I,A
        IM 1
        XOR A
        LD (23693),A
        LD (23624),A
        CALL 3435
        INC A
        INC A
        ;+------- PRINT ALL FONT ;-)))))) -------------+
        LD HL,(23606)
        PUSH HL
        LD HL,FONT-#200
        LD (23606),HL
        CALL 5633
        EI
        LD A,32
        LD B,#60
LOOP    PUSH AF
        RST #10
        POP AF
        INC A
        DJNZ LOOP
        DI
        POP HL
        LD (23606),HL
        LD HL,TRAIN1Y
        LD DE,#4000
        LD C,8
        LDIR
        EXX
        PUSH HL
        PUSH IX
        PUSH IY
        LD (STEKRET+1),SP
        LD BC,#7FFD
        LD A,#10
        OUT (C),A
        ;+--- REMOVING MUSIC ----+
        LD HL,MUSIC
        LD DE,MODULE
        LD BC,DPAGES-MUSIC
        LDIR
        ;+--- REMOVIG LOADER ----+
        LD HL,LOADER
        LD DE,LOADADR
        LD BC,MUSIC-LOADER
        LDIR

        LD HL,DPAGES
        LD DE,#C200
        LD BC,ENDOBJ-DPAGES
        LDIR

        ;+---DECRUNCHING LOGO---+
        LD HL,D_LOGO
        CALL D_LZ
        LD HL,D_LOGO+7936
        CALL D_LZ
        LD B,8
        LD IX,LOGO
        LD HL,D_LOGO+128
        LD DE,#10
DLPIXML PUSH BC
        PUSH HL
        LD BC,61*8
DLPIXL  LD A,(IX)
        LD (HL),A
        ADD HL,DE
        INC IX
        DEC BC
        LD A,B
        OR C
        JR NZ,DLPIXL
        POP HL
        POP BC
        INC HL
        DJNZ DLPIXML
        LD HL,D_LOGO+8+128
        LD E,8
        LD B,61
DLATRML PUSH BC
        LD B,8
DLATRL2 PUSH BC
        PUSH IX
        PUSH HL
        LD B,8
DLATRL  LD A,(IX)
        LD (HL),A
        INC HL
        INC IX
        DJNZ DLATRL
        POP HL
        POP IX
        POP BC
        ADD HL,DE
        ADD HL,DE
        DJNZ DLATRL2
        ADD IX,DE
        POP BC
        DJNZ DLATRML
        ;+-- DECRUNCHING SINUS TABL 4 LOGO'S ACTION
        LD IX,SINUS
        LD IY,D_SINL
        LD DE,D_LOGO
        LD B,#80
D_SINLL LD L,(IX)
        INC IX
        LD H,(IX)
        INC IX
        ADD HL,HL
        ADD HL,HL
        ADD HL,HL
        ADD HL,HL
        ADD HL,DE
        LD (IY),L
        INC IY
        LD (IY),H
        INC IY
        DJNZ D_SINLL
        LD HL,(D_SINL+#FE)
        LD (D_SINL+#100),HL
        ;+-- DECRUNCHING SINUS FOR CHANGE PAGES --+
        LD IX,SINUS2
        LD IY,D_SINP
        LD DE,43
        LD B,#40
D_SINLP PUSH BC
        LD B,(IX)
        INC B
        INC IX
        LD HL,D_TOP-43
D_SINL1 ADD HL,DE
        DJNZ D_SINL1
        LD (IY),L
        INC IY
        LD (IY),H
        INC IY
        LD BC,43*14*8
        ADD HL,BC
        LD (IY),L
        INC IY
        LD (IY),H
        INC IY
        POP BC
        DJNZ D_SINLP
        ;+--- DECRUNCHING X COORD TABL 4 PRINT TXT PP ---+
        LD HL,D_PRNTX
        LD DE,#0F00
        LD BC,#A24
D_PRNTL LD (HL),C
        INC HL
        LD (HL),E
        INC HL
        LD (HL),C
        INC HL
        LD (HL),D
        INC HL
        INC C
        LD (HL),C
        INC HL
        LD (HL),E
        INC HL
        LD (HL),C
        INC HL
        LD (HL),D
        INC HL
        DEC C
        DEC C
        DEC C
        DEC C
        DEC C
        DJNZ D_PRNTL
        ;+--- DECRUNCHING Y COORD TABL 4 PRINT TXT
        LD IX,D_PRNTY
        LD HL,D_TOP+4
        LD DE,43*8
        LD B,28
D_PRNYL LD (IX),L
        INC IX
        LD (IX),H
        INC IX
        ADD HL,DE
        DJNZ D_PRNYL
        ;+--DECRUNCHING LOGO OUTPUT PP--+
        LD HL,#73ED         ;
        LD (D_DLOP),HL      ;LD (#9408),SP
        LD HL,#9408         ;
        LD (D_DLOP+2),HL    ;
        LD A,#31
        LD (D_DLOP+4),A
        LD HL,D_LOGO        ;!!!
        LD (D_DLOP+5),A     ;!!!
        LD IX,D_DLOP+7
        LD (IX),#3E
        INC IX
        LD (IX),0
        INC IX
        LD HL,LOGOATR
        LD DE,#20
        LD B,D
        EXX
        LD HL,LOGOPIX
        LD B,#6F
        PUSH HL
        CALL PUT4B
        LD (IX),#D3
        INC IX
        LD (IX),#FE
        INC IX
        POP HL
        CALL DOWN_SL
        CALL INSATR
DLOP_L  PUSH HL
        CALL PUT4B
        POP HL
        CALL DOWN_SL
        CALL INSATR
        DJNZ DLOP_L
        LD A,#31
        LD (#9407),A
        LD A,#C9
        LD (#940A),A
        ;+--- DECRUNCHING TEXT OUTPUT PP (T.O.P.) ---+
        LD HL,D_TOP
        LD BC,#AE0
        LD D,0
D_TOPML PUSH BC
        LD (HL),#31
        INC HL
        LD (HL),D
        INC HL
        LD (HL),D
        INC HL
D_TOPL  LD (HL),#21
        INC HL
        LD (HL),D
        INC HL
        LD (HL),D
        INC HL
        LD (HL),#E5
        INC HL
        DJNZ D_TOPL
        POP BC
        DEC C
        JR NZ,D_TOPML
        LD DE,TOPEXIT
        LD (HL),#C3
        INC HL
        LD (HL),E
        INC HL
        LD (HL),D
        ;+-- DECRUNCHING T.O.P.'S ADRESS INSTALLER (T.A.I.) --+
        LD DE,D_TAI
        LD HL,DATA1
        LD BC,7
        LDIR
        EX DE,HL
        LD DE,OTXTADR
        LD B,112
D_TAIML LD (HL),1
        INC HL
        EX DE,HL
        CALL DOWN_SL
        EX DE,HL
        LD (HL),E
        INC HL
        LD (HL),D
        INC HL
        LD (HL),#F9
        INC HL
        LD (HL),#C5
        INC HL
        LD (HL),#19
        INC HL
        DJNZ D_TAIML
        EX DE,HL
        LD HL,DATA2
        LD C,#4
        LDIR
        ;+--ACTIVATING INTERRUPT--+
        LD HL,#D800
        LD DE,#D801
        LD BC,#100
        LD A,H
        LD I,A
        LD (HL),#DF
        LDIR
        LD A,#C3
        LD (#DFDF),A
        LD HL,INT1
        LD (#DFE0),HL
        IM 2
        ;+---PREPARE SCREEN---+
        CALL PRNTNXT
        CALL TXTADRS
        LD HL,#4000+#80+#700
        CALL FF_LINE
        LD HL,#5000+#60
        CALL FF_LINE
        CALL compile
        EI
        HALT
        LD A,1+2+4+8+16+32
        CALL FLASH
        LD HL,0
        LD D,H
        LD E,L
        LD BC,2900
        LDIR
        LD HL,#5800
        LD DE,#5801
        LD C,#60
        LD (HL),BORDER*8+BORDER
        LDIR
        LD C,#40
        LD (HL),BORDER*8+7
        LDIR
        LD BC,#200-#40
        LD (HL),7
        LDIR
        LD C,#A0
        LD (HL),BORDER*8+7
        LDIR
        LD HL,INT2
        LD (#DFE0),HL
        ;--------------------
        LD HL,D_LOGO
        LD (D_DLOP+5),HL
        CALL PG_DOWN
        ;+-----------------------------------------------------+
        ;|                     MAIN PROCESS                    |
        ;+-----------------------------------------------------+
AGAIN   CALL CLRTXTD
        CALL PRNTNXT
        CALL WAITKEY
        HALT
        CALL REST_C
        HALT
        CALL PG_UP
        CALL CLRTXTU
        CALL PRNTNXT
        CALL WAITKEY
        HALT
        CALL REST_C
        HALT
        CALL PG_DOWN
        JR AGAIN

CHEAT   LD BC,(CORD_P+1)
        LD A,B
        XOR #1C
        LD B,A
        LD (CORD_P+1),BC
        PUSH BC
        PUSH BC
        PUSH BC
        LD A,B
        OR A
        PUSH AF
        CALL Z,CLRTXTU
        POP AF
        CALL NZ,CLRTXTD
        POP BC
        LD HL,PAGECHL
        LD (PAGE_P+1),HL
        LD (KUDA_P+1),BC
        CALL PRNTNXT
        HALT
        CALL REST_C
        HALT
        POP AF
        CP 0
        PUSH AF
        CALL Z,PG_DOWN
        POP AF
        CALL NZ,PG_UP
KUDA_P  LD HL,#2121
        INC H
        INC H
        LD L,72
        LD (KUDA_P2+1),HL
KEYS    HALT
        LD BC,#7FFE
        IN A,(C)
        RRA
        JP NC,EXIT
KUDA_P2 LD DE,#1111
        LD HL,TABL_CH
        LD BC,#BFFE
        IN A,(C)
        RRA
        JP NC,CH_TALL
        LD B,#F7
        INC D
        INC D
        INC D
        INC D
        IN A,(C)
        RRA
        CALL NC,CH_TGLS    ;1
        JP KEYS
        ;+--- TOGLE ALL ('ENTER') ---+
CH_TALL LD B,1
        INC D
        INC D
        INC D
        INC D
CH_TAL1 PUSH BC
        CALL NC,CH_TGLA
        HALT
        INC D
        INC D
        INC HL
        POP BC
        DJNZ CH_TAL1
        CALL KB_WAIT
        JP KEYS
CH_TGLS CALL CH_TGLA
        CALL KB_WAIT
        RET
CH_TGLA PUSH AF
        PUSH BC
        PUSH DE
        PUSH HL
        LD A,(HL)
        CPL
        LD (HL),A
        OR A
        CALL Z,AD4000
        CALL NZ,AD4004
        LD L,A
        LD H,#40
        PUSH HL
        LD B,D
        LD C,E
        LD D,3
        CALL CLRSYM
        POP HL
        CALL PRNTSTR
        POP HL
        POP DE
        POP BC
        POP AF
        RET
TABL_CH DEFB #FF
KB_WAIT HALT
        HALT
        HALT
        HALT
        HALT
        HALT
        HALT
        HALT
        HALT
        HALT
        RET
AD4000  LD A,0
        RET
AD4004  LD A,4
        RET
        ;-------------------- PRINT PAGES ROUTINES ----->>>>>
PRNTNXT
PAGE_P  LD HL,PAGE_L
        LD E,(HL)
        INC HL
        LD D,(HL)
        INC HL
        LD A,D
        OR E
        JR NZ,CORD_P
        LD HL,PAGE_L
        LD E,(HL)
        INC HL
        LD D,(HL)
        INC HL
CORD_P  LD BC,#0000
        PUSH DE
        PUSH HL
        PUSH BC
        EX DE,HL
        LD D,#E
        ;+--- PRINT TEXT PAGE ---+
        ;D - NUMBER OF LINES
PRNT_PG PUSH DE
        PUSH BC

        CALL PRNTSTR
NO_SPCE POP BC
        INC B
        INC B
        POP DE
        DEC D
        JR NZ,PRNT_PG
        POP BC
        POP HL
        POP DE
        LD A,B
        XOR #1C
        LD B,A
        LD (CORD_P+1),BC
        LD A,D
        OR E
        JR NZ,NO_ENDP
        LD HL,PAGE_L
NO_ENDP LD (PAGE_P+1),HL
        RET
        ;+---------------------------------+
        ;|          MAIN INT PP            |
        ;+---------------------------------+
INT2    DI
        PUSH AF
        PUSH BC
        PUSH DE
        PUSH HL
        ;+------- OUTPUT TXT ---------+
TXTPRNT LD (TOPEXIT+1),SP
TXTPRAD JP D_TOP
TOPEXIT LD SP,#3131
        LD HL,0
        LD D,H
        LD E,L
        LD BC,84
        LDIR
        OR B
        OR B
        OR B
        OR B
        OR B
        OR B
        OR B
        OR B
        ;+--- WHITE LINE ---+
        LD B,3
DJ_1U   DJNZ DJ_1U
        LD A,#3E
        LD A,#3E
        LD A,7
        OUT (#FE),A
        LD A,#3E
        LD A,#3E
        LD B,3
DJ_2U   DJNZ DJ_2U
        CALL D_DLOP
        LD HL,0
        LD DE,0
        LD BC,11
        LDIR
        ;+--- WHITE LINE ---+
        LD B,3
DJ_1D   DJNZ DJ_1D
        LD A,I
        LD A,7
        OUT (#FE),A
        LD B,15
DJ_2D   DJNZ DJ_2D
        LD A,R
        LD A,BORDER
        OUT (#FE),A
        PUSH IX
        PUSH IY
        EXX
        EX AF,AF'
        PUSH AF
        PUSH BC
        PUSH DE
        PUSH HL
        ;+------- LOGO ACTION ---------+
P_LA    LD A,#00
        INC A
        LD (P_LA+1),A
        BIT 7,A
        LD HL,(LA_HL+1)
        PUSH AF
        CALL NZ,LA_HLI
        POP AF
        CALL Z,LA_HLD
LA_HL   LD HL,D_SINL+#FE
        LD E,(HL)
        INC HL
        LD D,(HL)
        INC HL
        LD (D_DLOP+5),DE
        CALL play
        POP HL
        POP DE
        POP BC
        POP AF
        EXX
        EX AF,AF'
        POP IY
        POP IX
        POP HL
        POP DE
        POP BC
        POP AF
        EI
INT1    RET
        ;~~~~~~~~~~~~~~~~ END INT...
LA_HLI  INC HL
        INC HL
        JR NA_HOOY
LA_HLD  DEC HL
        DEC HL
NA_HOOY LD (LA_HL+1),HL
        RET
        ;+--------- WAIT KEY ----------+
WAITKEY LD BC,1000
WAIT_KL HALT
        PUSH BC
        LD BC,#7FFE
        IN A,(C)
        RRA
        JR NC,SPCPRES
        LD B,#BF
        IN A,(C)
        POP BC
        RRA
        RET NC
        DEC BC
        LD A,B
        OR C
        JR NZ,WAIT_KL
        RET
SPCPRES POP BC
        POP DE
        JP CHEAT
        ;+--- PAGE DOWN... ---++++++++++++++++++++++++++++++++++
PG_DOWN DI
        LD HL,(SAVE_JP+1)
        LD (HL),#31
LAST_BD LD BC,#3F
PG_DNL  LD H,#DC                ;IMPORTANT!!!
        LD L,B
        INC B
        INC B
        INC B
        INC B
        PUSH BC
        LD C,(HL)
        INC L
        LD B,(HL)
        INC L
        LD E,(HL)
        INC L
        LD D,(HL)
        LD H,B
        LD L,C
        CALL TXTADR1
        EI
        HALT
        POP BC
        DEC C
        JR NZ,PG_DNL
        LD A,B
        LD (LAST_BU+2),A
        RET
        ;+----------- PAGE UP... -----------------------------+
PG_UP   DI
        LD HL,(SAVE_JP+1)
        LD (HL),#31
LAST_BU LD BC,#F83F
PG_UPL  DI
        LD HL,(SAVE_JP+1)
        LD (HL),#31
        LD H,#DC              ;IMPROTANT!!!!
        LD L,B
        DEC B
        DEC B
        DEC B
        DEC B
        PUSH BC
        LD C,(HL)
        INC L
        LD B,(HL)
        INC L
        LD E,(HL)
        INC L
        LD D,(HL)
        LD H,B
        LD L,C
        DI
        CALL TXTADR1
        EI
        HALT
        POP BC
        DEC C
        JR NZ,PG_UPL
        LD A,B
        LD (LAST_BD+2),A
        RET
        ;+------ INSTALL TXT ADRS 4 TXT OUTPUT PP -------+
TXTADRS LD HL,D_TOP+4816
        LD DE,D_TOP+9632

        ;HL - TXTPRAD
        ;DE - NEWJP

TXTADR1 LD (TXTPRAD+1),HL
        INC HL
        INC HL
        INC HL
        LD (D_TAI+5),HL

        LD (SAVE_JP+1),DE
        LD A,#C3
        LD (DE),A
        INC DE
        LD HL,TOPEXIT
        LD A,L
        LD (DE),A
        INC DE
        LD A,H
        LD (DE),A
        LD DE,43

        JP D_TAI

        ;************** RESTORE CODE... ************************

REST_C  LD HL,D_TOP
SAVE_JP LD DE,#1111
        LD BC,#E02B
REST_L  PUSH BC
        LD A,H
        CP D
        JR NZ,NO_RJP
        LD A,L
        CP E
        JR NZ,NO_RJP
        JR REAL_JP
NO_RJP  LD (HL),#31
REAL_JP LD B,0
        ADD HL,BC
        POP BC
        DJNZ REST_L
        RET

        ;+--- PRINT TEXT LINE ---+]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
PRNTSTR LD A,(HL)
        CP #0D
        JP Z,ENDSTR1
        PUSH HL
        PUSH BC

        ;+--- PRINT CHARECTER ---+*---+++---+++---+++---+++--->>
        ;C - X COORD *2
        ;B - Y COORD *2
        ;A - CHARECTER

        EX AF,AF'

        LD H,#DA ;THIS IS VERY IMPORTANT!!!
        LD L,B
        LD E,(HL)
        INC L
        LD D,(HL)
        INC H
        LD L,C
        LD C,(HL)
        INC L
        LD A,(HL)
        OR A
        JR Z,PCHR_F0

        LD H,D
        LD L,E
        LD B,0
        ADD HL,BC
        LD C,43

        EX AF,AF'
        LD E,A
        LD D,#40

        LD A,(DE)       ;1
        INC D
        AND #F
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;2
        INC D
        AND #F
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;3
        INC D
        AND #F
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;4
        INC D
        AND #F
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;5
        INC D
        AND #F
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;6
        INC D
        AND #F
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;7
        INC D
        AND #F
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;8
        AND #F
        OR (HL)
        LD (HL),A

        JR ENDPCHR

PCHR_F0 LD H,D
        LD L,E
        LD B,0
        ADD HL,BC
        LD C,43

        EX AF,AF'

        LD E,A
        LD D,#40

        LD A,(DE)       ;1
        INC D
        AND #F0
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;2
        INC D
        AND #F0
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;3
        INC D
        AND #F0
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;4
        INC D
        AND #F0
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;5
        INC D
        AND #F0
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;6
        INC D
        AND #F0
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;7
        INC D
        AND #F0
        OR (HL)
        LD (HL),A
        ADD HL,BC

        LD A,(DE)       ;8
        AND #F0
        OR (HL)
        LD (HL),A

ENDPCHR POP BC
        INC C
        INC C
        POP HL
        INC HL
        JP PRNTSTR
        RET
ENDSTR1 INC HL
        RET


        ;"""""""""" CLEAR SYMBOL """"""""""""""""

CLRSYM  PUSH BC
CCHR_L  PUSH DE
        PUSH BC

        LD H,#DA ;ЕНТО ОЧЕНЬ ВАЖНО! (THIS IS VERY IMPORTANT!)
        LD L,B
        LD E,(HL)
        INC L
        LD D,(HL)
        INC H
        LD L,C
        LD C,(HL)
        INC L
        LD A,(HL)
        OR A
        JR Z,CCHR_F0

        LD H,D
        LD L,E
        LD B,0
        ADD HL,BC
        LD C,43


        LD A,#F0        ;1
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F0        ;2
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F0        ;3
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F0        ;4
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F0        ;5
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F0        ;6
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F0        ;7
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F0        ;8
        AND (HL)
        LD (HL),A

        JR ENDCLR1


CCHR_F0 LD H,D
        LD L,E
        LD B,0
        ADD HL,BC
        LD C,43


        LD A,#F         ;1
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F         ;2
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F         ;3
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F         ;4
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F         ;5
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F         ;6
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F         ;7
        AND (HL)
        LD (HL),A
        ADD HL,BC

        LD A,#F         ;8
        AND (HL)
        LD (HL),A
        ADD HL,BC

ENDCLR1 POP BC
        POP DE
        INC C
        INC C
        DEC D
        JP NZ,CCHR_L
        POP BC
        RET
        ;+>>>>>>>>>>>>>>> CLER TEXT <<<<<<<<<<<<<<<<<+

CLRTXTU LD HL,D_TOP
        JR CLRTXT
CLRTXTD LD HL,D_TOP+4816
CLRTXT  LD B,14*8
        XOR A
CLRTXTL INC HL
        INC HL
        INC HL
        INC HL
        LD (HL),A
        INC HL
        LD (HL),A
        INC HL
        INC HL
        INC HL
        LD (HL),A
        INC HL
        LD (HL),A
        INC HL
        INC HL
        INC HL
        LD (HL),A
        INC HL
        LD (HL),A
        INC HL
        INC HL
        INC HL
        LD (HL),A
        INC HL
        LD (HL),A
        INC HL
        INC HL
        INC HL
        LD (HL),A
        INC HL
        LD (HL),A
        INC HL
        INC HL
        INC HL
        LD (HL),A
        INC HL
        LD (HL),A
        INC HL
        INC HL
        INC HL
        LD (HL),A
        INC HL
        LD (HL),A
        INC HL
        INC HL
        INC HL
        LD (HL),A
        INC HL
        LD (HL),A
        INC HL
        INC HL
        INC HL
        LD (HL),A
        INC HL
        LD (HL),A
        INC HL
        INC HL
        INC HL
        LD (HL),A
        INC HL
        LD (HL),A
        INC HL
        INC HL
        DJNZ CLRTXTL
        RET






        ;+--- F_L_A_S_H ! ! ! (? ? ?) ---+

FLASH   PUSH AF
        LD A,7
        OUT (#FE),A
        POP AF
        LD HL,#5800
        LD DE,#5801
        LD BC,#2FF
        LD (HL),A
        LDIR
        RET

DATA1   LD (#BC56),SP
        LD HL,#2121
DATA2   LD SP,#3131
        RET

D_LZ    LD D,H
        LD E,L
        INC DE
        LD BC,128
        LD (HL),B
        LDIR
        RET

INSATR  EXX
        PUSH HL
        CALL PUT4B
        POP HL
        INC B
        LD A,B
        CP 8
        JR NZ,NO_ADD
        LD B,D
        ADD HL,DE
NO_ADD  EXX
        LD (IX),0       ;LD H,#26
        INC IX
        LD (IX),0
        INC IX
        LD (IX),0
        INC IX
        LD (IX),0
        INC IX
        RET


PUT4B   PUSH BC
        LD B,4
PUT4B_L LD (IX),#E1     ;POP HL
        INC IX
        LD (IX),#22     ;LD (...),HL
        INC IX
        LD (IX),L
        INC IX
        LD (IX),H
        INC IX
        INC HL
        INC HL
        DJNZ PUT4B_L
        POP BC
        RET

DOWN_SL INC H
        LD A,H
        AND 7
        RET NZ
        LD A,L
        ADD A,#20
        LD L,A
        RET C
        LD A,H
        SUB #8
        LD H,A
        RET

FF_LINE LD D,H
        LD E,L
        INC DE
        LD (HL),#FF
        LD BC,#1F
        LDIR
        RET






EXIT
        DI
        LD HL,INT1
        LD (#DFE0),HL
        EI
        HALT
        LD A,1+2+4+8+16+32
        CALL FLASH

        CALL stop

        EI
        HALT
        LD BC,#1BFE
        LD HL,#5B00
        LD DE,#5AFF
        OUT (C),L
        LD (HL),L
        LDDR
        DI
        LD A,#3B
        LD I,A
        IM 1

        ;------ CHEAT INSTALLER ------

        LD HL,TABL_CH
        LD A,(HL)
        OR A
        JR NZ,NOU1
        XOR A
        LD (TP1),A

NOU1
NOU9
STEKRET LD SP,#3131
        POP IY
        POP IX
        POP HL
        EXX
        LD HL,LOADADR
        PUSH HL
        LD HL,#6000
        LD DE,#6001
        LD BC,#9FFF
        LD (HL),L
        EI
        JP #33C3

        DS #FF,#33

        ORG #7600

PLAYER  INCBIN "PLAYER"

LOGO    INCBIN "LOGOcrk "

SINUS   INCBIN "SINUS_D"

SINUS2  INCBIN "SINUS_G"

FONT    INCBIN "FONTcrk2"

        ;+-------- L O A D E R ---------+


LOADER
        DISP #5D3B

        LD SP,24999
        LD HL,40000
        LD B,#0C
        CALL LOAD
        LD HL,#61A8
        LD B,#52
        CALL LOAD11
        XOR A

        JR NO_UN1
TP1     EQU $-1
        LD A,#FF

NO_UN1  LD (#FFFF),A
        EI
        RES 5,(IY+1)
WAITL   BIT 5,(IY+1)
        JR Z,WAITL

        JP #61A8
LOAD
        PUSH HL         ;!!!
LOAD11  LD DE,(#5CF4)
        LD C,5
LOAD1   PUSH HL
        PUSH DE
        PUSH BC
        CALL LL_3D13
        POP BC
        POP DE
        POP HL
        LD A,(23823)
        OR A
        JR NZ,LOAD1
        RET

LL_3D13 LD (REG_A+1),A
        PUSH HL
        LD HL,(23613)
        LD (ERR+1),HL
        LD HL,DRIA
        LD (#5CC3),HL
        LD HL,ERR
        EX (SP),HL
        LD (23613),SP
        EX AF,AF'
        LD A,#C3
        LD (#5CC2),A
        XOR A
        LD (23823),A
        LD (23824),A
        EXA
        LD (SP2),SP
REG_A   LD A,#C3
        JP #3D13
D_ERR   LD SP,#3131
SP2     EQU $-2
        LD (23823),A
ERR     LD HL,#2121
        LD (23613),A
        LD A,#C9
        LD (#5CC2),A
        RET
DRIA    EX (SP),HL
        PUSH AF
        LD A,H
        CP 13
        JR Z,RIA
        XOR A
        OR H
        JR Z,NO_ERR
NO_ERR  POP AF
        EX (SP),HL
        RET
RIA     POP HL
        POP HL
        POP HL
        XOR A
        LD (23560),A
        LD A,2
        OUT (#FE),A
        LD A,"R"
        LD HL,#3F7E
        EX (SP),HL
        JP #3D2F
        ENT
MUSIC   INCBIN "INTRO"
        DEFS 256,0
DPAGES
        DISP #C200
PAGE5   DEFB "LOOK FOR THE NEXT CRACK-RELEAZES:",#0D ;1
        DEFB #0D                                           ;2
        DEFB "#01 SURVIVOR.........R.G.R.-R.A.G.C.'87",#0D ;3
        DEFB "#02 BLOOD VALLEY.......................",#0D ;4
        DEFB "..........IMAGITEC/GREMLIN GRAPHICS '86",#0D ;5
        DEFB "#03 SPY HUNTER........U.S.GOLD/SEGA '??",#0D ;6
        DEFB "#04 BOUNDER........GREMLIN GRAPHICS '86",#0D ;7
        DEFB "#05 SABOTEUR COLLECTION................",#0D ;8
        DEFB ".................DURELL SOFTWARE '86-87",#0D ;9
        DEFB "#06 FOX FIGHTS BACK!...................",#0D ;A
        DEFB ".........IMAGE WORKS/DENTON DESIGNS '??",#0D ;B
        DEFB "#07 PETER PACK-RET.....................",#0D ;C
        DEFB "........SOFTWARE CREATIONS/FIREBIRD '88",#0D ;D
        DEFB "#08 STAINLESS STEEL........MIKROGEN '86",#0D ;E

PAGE6   DEFB "#09 CANYON WARRIOR.....MASTERTRONIC '88",#0D ;1
        DEFB "#0A SLIGHTLY MAGIC.....................",#0D ;2
        DEFB ".ASTONISHING ANIMATIONS/CODEMASTERS '90",#0D ;3
        DEFB "#0B LICENSE TO KILL..........DOMARK '89",#0D ;4
        DEFB "#0C MONTE CARLO CASINO..CODEMASTERS '??",#0D ;5
        DEFB "#0D AMAUROTE V1.0......MASTERTRONIC '87",#0D ;6
        DEFB "#0E ROGUE TROOPER+++..MACMILLAN LTD '86",#0D ;7
        DEFB "#0F ROAD WARS..........................",#0D ;8
        DEFB "....ARCADIA SYSTEMS/MELBOURNE HOUSE '86",#0D ;9
        DEFB "#10 SCOOBY DOO IN THE CASTLE MYSTERY...",#0D ;A
        DEFB ".....................ELITE SOFTWARE '86",#0D ;B
        DEFB "#11 ORBIX - THE TERRORBALL.............",#0D ;C
        DEFB "......................JOHN PRAGNELL '??",#0D ;D
        DEFB "            WAIT FOR OTHER COOL CRACKS!",#0D
        ENT
ENDOBJ
        ORG #6000

