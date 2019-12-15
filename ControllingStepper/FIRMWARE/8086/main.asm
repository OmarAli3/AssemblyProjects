    ;Stepper connected on PORTA[0..3]
    ;Keypad connected on PORTC[0..7]
    ;First 7-segment connected on PORTB[0..3]
    ;Second 7-segment connected on PORTB[4..7]
    ;Third 7-segment connected on PORTB[4..7]
    
COMMENT! 
coils sate and corresponding degrees if step angle is 90
1001b -> 0   DEG
0001b -> 45  DEG
0011b -> 90  DEG
0010b -> 135 DEG
0110b -> 180 DEG
0100b -> 225 DEG
1100b -> 270 DEG
1000b -> 315 DEG
!

.MODEL SMALL
.DATA	;data segment
    KEYPAD BYTE 10H,11H,12H,13H,    ;Keypad matrix
                 9 , 6 , 3 ,14H,
		 8 , 5 , 2 , 0 ,
		 7 , 4 , 1 ,15H
    	
    PORTA  = 00H    ;Address of Port A
    PORTB  = 02H    ;Address of Port B
    PORTC  = 04H    ;Address of Port C
    CONTW  = 06H    ;Address of Control Word
    DELAYT = 0FFFH
.STACK	 ;stack segment
   DB 128D DUP(0B)

.CODE   ;code segment
    .STARTUP
    MOV DX,PORTA
    MOV AL, 10000001B;   PORTA & PORTB as output in mode 0, PORTC[0..3] as input, PORTC[4..7] as output 
    OUT CONTW, AL  
MAIN PROC
  
  CALL GETKEY
  CMP AL,14H ;KEY FOR FULL STEP CLOCKWISE
  JZ L
  CMP AL,15H ;KEY FOR FULL STEP COUNTER CLOCKWISE
  JZ L2
  CMP AL,13H ;KEY FOR HALF STEP CLOCKWISE
  JZ L3
  CMP AL,12H ;KEY FOR HALF STEP COUNTER CLOCKWISE
  JZ L4
  JMP MAIN
  L: 
      MOV CX,000FH ; 002DH loop for 45 iterations and each function makes 8 degrees so the total is 360 degree 
      LFSCW:
      CALL FSCW
      LOOP LFSCW
  JMP MAIN
  L2:
      MOV CX,000FH
      LFSCCW:
      CALL FSCCW
      LOOP LFSCCW
  JMP MAIN
  L3: 
      MOV CX,000FH ; loop for 45 iterations and each function makes 8 degrees so the total is 360 degree 
      LHSCW:
      CALL HSCW
      LOOP LHSCW
  JMP MAIN
  L4:
      MOV CX,000FH
      LHSCCW:
      CALL HSCCW
      LOOP LHSCCW
  

JMP MAIN          ;Loop forever
MAIN ENDP
GETKEY PROC 	;pressed key stored in AL 
   MOV AL, 0FFH
   OUT PORTC, AL
   AGAIN:
   MOV SI,-4H
   MOV AH,10000000B
   waitForPress:
      ADD SI,4H
      NOT AH
      MOV AL, 0FFH
      AND AL,AH
      MOV BL,AL
      OUT PORTC,AL
      IN AL,PORTC
      CMP BL,AL
      JNZ DECODE
      CMP AH,11101111B
      JZ AGAIN 
      NOT AH
      SHR AH,1B
      ;CALL DELAY 
      JMP waitForPress
	 
      DECODE:
	 MOV BL,-1D
	 MOV CL,1H
	 LL:
	    INC BL
	    TEST AL,CL 
	    JZ NXT
	    SHL CL,1H
	    JMP LL
	 NXT:   
	    MOV BH,0H
	    ADD SI,BX
	    MOV AL,KEYPAD[SI]
	    OUT PORTB,AL
	    
	    
  RET
GETKEY ENDP
;-----------------------------
FSCW PROC ;Full step clock wise

  MOV AL, 00001001B
  OUT PORTA,AL

  CALL DELAY

  MOV AL, 00000011B
  OUT PORTA,AL 

  CALL DELAY
    
  MOV AL, 00000110B
  OUT PORTA,AL

  CALL DELAY
    
  MOV AL, 00001100B
  OUT PORTA,AL

  CALL DELAY
  RET
FSCW ENDP
;--------------------------  

HSCW PROC ;HAlF step clock wise

  MOV AL, 00001001B
  OUT PORTA,AL

  CALL DELAY

  MOV AL, 00000001B
  OUT PORTA,AL

  CALL DELAY

  MOV AL, 00000011B
  OUT PORTA,AL 

  CALL DELAY

  MOV AL, 00000010B
  OUT PORTA,AL

  CALL DELAY
    
  MOV AL, 00000110B
  OUT PORTA,AL

  CALL DELAY

  MOV AL, 00000100B
  OUT PORTA,AL

  CALL DELAY
   
  MOV AL, 00001100B
  OUT PORTA,AL

  CALL DELAY
  
  MOV AL, 00001000B
  OUT PORTA,AL

  CALL DELAY
  
  RET
HSCW ENDP
;--------------------------  
  
FSCCW PROC ;Full step counter clock wise

  MOV AL, 00001001B
  OUT PORTA,AL

  CALL DELAY

  MOV AL, 00001100B
  OUT PORTA,AL 

  CALL DELAY
    
  MOV AL, 00000110B
  OUT PORTA,AL

  CALL DELAY
    
  MOV AL, 00000011B
  OUT PORTA,AL

  CALL DELAY

  RET
  FSCCW ENDP
;--------------------------

HSCCW PROC ;HALF step counter clock wise

  MOV AL, 00001001B
  OUT PORTA,AL

  CALL DELAY

  MOV AL, 00001000B
  OUT PORTA,AL

  CALL DELAY

  MOV AL, 00001100B
  OUT PORTA,AL 

  CALL DELAY

  MOV AL, 00000100B
  OUT PORTA,AL

  CALL DELAY
    
  MOV AL, 00000110B
  OUT PORTA,AL

  CALL DELAY
 
  MOV AL, 00000010B
  OUT PORTA,AL

  CALL DELAY
   
  MOV AL, 00000011B
  OUT PORTA,AL

  CALL DELAY

  MOV AL, 00000001B
  OUT PORTA,AL

  CALL DELAY
  
  RET
  HSCCW ENDP
;----------------------------
  
DELAY PROC 
MOV BX,CX
MOV CX, DELAYT     ; Delay 
delay1:loop delay1   ;
MOV CX,BX
RET
DELAY ENDP


END MAIN
