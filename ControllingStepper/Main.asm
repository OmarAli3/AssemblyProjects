    ;Stepper connected on PORTA[0..3]
    ;Keypad connected on PORTC[0..7]
    ;First 7 - segment connected on PORTB[0..3]
    ;Second 7 - segment connected on PORTB[4..7]
    ;Third 7 - segment connected on PORTB[4..7]
    
    COMMENT!
    coils sate and corresponding degrees if step angle is 90
    1001b - > 0 DEG
    0001b - > 45 DEG
    0011b - > 90 DEG
    0010b - > 135 DEG
    0110b - > 180 DEG
    0100b - > 225 DEG
    1100b - > 270 DEG
    1000b - > 315 DEG
    !
    
    .MODEL SMALL
    .DATA                                                   ;data segment
       ;Ports Adresses
       PORTA  = 00H                                         ;Address of Port A
       PORTB  = 02H                                         ;Address of Port B
       PORTC  = 04H                                         ;Address of Port C
       CONTW  = 06H                                         ;Address of Control Word
       
       DELAYT = 0FFFFH					    ;Delay time
       
       KEYPAD DB 10H, 11H, 12H, 13H,                        ;Keypad matrix
		    9 , 6 , 3 , 14H, 
		    8 , 5 , 2 , 0 , 
		    7 , 4 , 1 , 15H
  
       ROTATION DB 00000001b,				    ;Complete rotation matrix
		   00000011b,
		   00000010b,
		   00000110b,
		   00000100b,
		   00001100b,
		   00001000b,
		   00001001b
	      
       SEG1 DB 0                                            ;Right  7Segment
       SEG2 DB 0     					    ;Middle 7Segment
       SEG3 DB 0                                            ;Left   7Segment
       INCNT DW -1				            ;Input counter
       ANGLE DB 0			          	    ;Stepper angle
       
    .STACK                                                  ;stack segment
      DB 128D DUP(0B)
    
    .CODE                                                   ;code segment

    .STARTUP
       MOV AL, 10000001B                                    ;PORTA & PORTB as output in mode 0, PORTC[0..3] as input, PORTC[4..7] as output
       OUT CONTW, AL					    ;Write configuration to control word
    
    
    ;-------------------------------------------------------------------------------------------------------
    MAIN PROC
	  CALL GETKEY					    ;Get the pressed key from the keypad		
	  CALL RUN					    
	  JMP MAIN                                          ;Loop forever
    MAIN ENDP
    ;-------------------------------------------------------------------------------------------------------
    
    
    GETKEY PROC                                             ; Note: Pressed key stored in AL
	  MOV AL, 0H					    ;Clear   AL
	  OUT PORTC, AL					    ;Clear the higher bits on port C
	  
      PRESSED:						    ;Check if the key still pressed (Wait for release)	
	  IN AL, PORTC				
	  CMP AL, 00001111B				    ; Compare port C with the pattern of the initial state (Keypad is not pressed)
	  JNZ PRESSED
	  
	  MOV AL, 0FFH
	  OUT PORTC, AL					    ;Set all matrix cols to high
	  
      AGAIN:
	  MOV SI, -4H					    ;Initializing rows index in SI to get values from the matrix
	  MOV AH, 10000000B				    ;Initializing AH to get input from port C
	  
      waitForPress:
	  ADD SI, 4H					    ;Increment rows counter
	  NOT AH					    ;Inverting all bits to send (low) to a certin column pin in keypad
	  MOV AL, 0FFH					    
	  AND AL, AH
	  MOV BL, AL
	  OUT PORTC, AL					    ;Output (low) on the specified column pin in keypad 
	  IN AL, PORTC					    ;Reading ports to get the pressed key
	  CMP BL, AL
	  JNZ DECODE
	  CMP AH, 11101111B				    ;Check if the reached the last col
	  JZ AGAIN					    ;Repeat the process
	  NOT AH
	  SHR AH, 1B					    ;Shift the bit of the column in the pattern
	  JMP waitForPress				
	  
      DECODE:						    ;Decoding the pressed key
	  MOV BL, -1D					    ;Column index				 	    
	  MOV CL, 1H
      LL:						    ;Loops on rows to get the pressed key
	  INC BL
	  TEST AL, CL
	  JZ NXT
	  SHL CL, 1H
	  JMP LL
      NXT:
	  MOV BH, 0H					    ;Clears the higher bits from BX to use it (Using the lower bits without the effect of higher bits)
	  ADD SI, BX					    ;Getting the index in the one dimensional matrix
	  
	  MOV AL, KEYPAD[SI]				    ;Storing the pressed key in AL
	  
	  CMP AL, 9					    ;Check if the pressed key is a number
	  JG FINISH					    ;Finish if not a number
	  
	  CMP INCNT, 2					    ;Check whether the screen is full
	  JGE FINISH					    ;Finish if full
	  
	  INC INCNT					    ;Increment (input counter) if screen isn't full
       
       Right:
	  CMP INCNT, 0					    ;Check whether to wirte on the right 7segment or not
	  JNZ Middle					    ;Don't write on the right 7segment
	  
	  ;Displaying on the right 7segment
	  MOV SEG1, AL					    
	  CALL SHOW					    
	  JMP FINISH
	  
      Middle:		
	  CMP INCNT, 1					    ;Check whether to wirte on the middle 7segment or not
	  JNZ Left				 	    ;Don't write on the middle 7segment
	  
	  ;Shifting and displaying on the middle and right 7segment
	  MOV BH, SEG1
	  MOV SEG2, BH
	  MOV SEG1, AL
	  CALL SHOW
	  JMP FINISH
	  
      Left:
	  ;Shifting and displaying on the middle and right 7segment
	  MOV BH, SEG2
	  MOV SEG3, BH
	  MOV BH, SEG1
	  MOV SEG2, BH
	  MOV SEG1 , AL
	  CALL SHOW
	  
      FINISH:
	  RET
    GETKEY ENDP
    
    RUN PROC
	 CMP AL, 10H                                             ;KEY FOR FULL STEP CLOCKWISE
	 JZ L
	 CMP AL, 11H                                             ;KEY FOR FULL STEP COUNTER CLOCKWISE
	 JZ L2
	 CMP AL, 12H                                             ;KEY FOR HALF STEP CLOCKWISE
	 JZ L3
	 CMP AL, 13H                                             ;KEY FOR HALF STEP COUNTER CLOCKWISE
	 JZ L4
	 CMP AL, 14H						 ;KEY FOR ERASE	
	 JZ L5
	 CMP AL, 15H						 ;KEY FOR CLEAR	
	 JZ L6

	 JMP FINISHRUN
      L:
	 CALL COMPUTE						 ;Compute the rotation angle
	 CMP ANGLE , 0						 ;Checks if there is no input
	 JE FINISHRUN
	 MOV CX, 0						 ;Clear CX for later use	
	 MOV CL, ANGLE                                           ;Make the angle as counter for the following loop (LFSCW)
	 
	 LFSCW:							 ;Simulate full step clockwise on the stepper motor
	    CALL FSCW
	    LOOP LFSCW
	    JMP FINISHRUN
	    
      L2:
	 CALL COMPUTE						 
	 CMP ANGLE , 0
	 JE FINISHRUN
	 MOV CX, 0
	 MOV CL, ANGLE                                           
	 LFSCCW:						 ;Simulate full step counter clockwise on the stepper motor
	    CALL FSCCW
	    LOOP LFSCCW
	    JMP FINISHRUN
	    
      L3:
	 CALL COMPUTE
	 CMP ANGLE , 0
	 JE FINISHRUN
	 MOV CX, 0
	 MOV CL, ANGLE                                           
	 LHSCW:							 ;Simulate half step clockwise on the stepper motor
	    CALL HSCW
	    LOOP LHSCW
	    JMP FINISHRUN
	    
      L4:
	 CALL COMPUTE
	 CMP ANGLE , 0
	 JE FINISHRUN
	 MOV CX, 0
	 MOV CL, ANGLE                                           
	 LHSCCW:						 ;Simulate half step counter clockwise on the stepper motor	
	    CALL HSCCW
	    LOOP LHSCCW
	    JMP FINISHRUN
	    
      L5:							 ;Erase button pressed
	 CALL ERASE
	 JMP FINISHRUN
	 
      L6:							 ;Clear button pressed
	 CALL CLEAR

      FINISHRUN:
	 RET
    RUN ENDP
    
    SHOW PROC
	 MOV AH, 0
	 PUSH AX						 ;Saving the AL value in the stack
	 
	 ;Output the lower bits of AL on the lower bits of port B
	 MOV AL, SEG1						 
	 OUT PORTB, AL
	 
	 ;Output the lower bits of AL on the lower bits of port B
	 MOV AL, SEG2							
	 MOV BH, 10H
	 MUL BH							 ;Shifting the value to be the higher bits
	 OR  AL, SEG1						 ;Keeping the value of the lower bits unchanged
	 OUT PORTB, AL
	 
	 ;Output the higher bits of AL on the higher bits of port A
	 MOV AL, SEG3
	 MUL BH
	 OUT PORTA, AL
	 
	 POP AX							 ;Restoring the AL value in the stack
	 RET
    SHOW ENDP
    
    COMPUTE PROC						 ;Compute the angle
	 
	 ;Angle = SEG3 * 100 + SEG2 * 10 + SEG1
	 MOV ANGLE, 0
	 MOV AL, SEG3
	 MOV BL, 100
	 MUL BL
	 ADD ANGLE, AL
	 MOV AL, SEG2
	 MOV BL, 10
	 MUL BL
	 ADD ANGLE , AL
	 MOV AL, SEG1
	 ADD ANGLE , AL

	 RET
    COMPUTE ENDP
    
    ERASE PROC							 ;Erase one digit
	 MOV AL, SEG2
	 MOV SEG1, AL
	 MOV AL, SEG3
	 MOV SEG2, AL
	 MOV SEG3, 0H
	 DEC INCNT
	 CALL SHOW
	 RET
    ERASE ENDP
    
    CLEAR PROC							 ;Clear the screen		
	 MOV SEG1, 0
	 MOV SEG2, 0
	 MOV SEG3, 0
	 MOV INCNT, -1
	 CALL SHOW
	 RET
    CLEAR ENDP

    FSCW PROC                                               	;Full step clock wise
	 MOV AL, SEG3
	 MOV BL, 10H
	 MUL BL
	 
	 PUSH CX
	 
	 MOV SI, -1
	 MOV CX, 4
	 ROTATE: 
	    AND AL, 0F0H
	    ADD SI, 2
	    OR AL, ROTATION[SI]
	    OUT PORTA,AL 
	    CALL DELAY
	 LOOP ROTATE
	 
	 POP CX
	 RET
    FSCW ENDP
    
    HSCW PROC                                                  ;HAlF step clock wise
	 MOV AL, SEG3
	 MOV BL, 10H
	 MUL BL

	 PUSH CX
	 
	 MOV SI, -1
	 MOV CX, 8
	 ROTATE: 
	    AND AL, 0F0H
	    INC SI
	    OR AL, ROTATION[SI]
	    OUT PORTA,AL 
	    CALL DELAY
	 LOOP ROTATE
	 
	 
	 POP CX
	 RET
    HSCW ENDP
    
    FSCCW PROC                                               ;Full step counter clock wise
	 MOV AL, SEG3
	 MOV BL, 10H
	 MUL BL
	 
	 PUSH CX
	 
	 MOV SI, 7
	 MOV CX, 3
	 ROTATE: 
	    AND AL, 0F0H
	    SUB SI, 2
	    OR AL, ROTATION[SI]
	    OUT PORTA,AL 
	    CALL DELAY
	 LOOP ROTATE
	    AND AL, 0F0H
	    OR AL, ROTATION[7]
	    OUT PORTA,AL 
	    CALL DELAY
	 
	 POP CX
	 RET
    FSCCW ENDP
    
    HSCCW PROC                                               ;HALF step counter clock wise
	 MOV AL, SEG3
	 MOV BL, 10H
	 MUL BL
	 
	 PUSH CX
	 
	 MOV SI, 7
	 MOV CX, 7
	 ROTATE: 
	    AND AL, 0F0H
	    DEC SI
	    OR AL, ROTATION[SI]
	    OUT PORTA,AL 
	    CALL DELAY
	 LOOP ROTATE
	 
	    AND AL, 0F0H
	    OR AL, ROTATION[7]
	    OUT PORTA,AL 
	    CALL DELAY
	    
	 POP CX
	 RET
    HSCCW ENDP
    
    DELAY PROC								
	 MOV BX, CX
	 MOV CX, DELAYT
      delay1:
	 loop delay1
	 MOV CX, BX
	 RET
    DELAY ENDP
    
    END