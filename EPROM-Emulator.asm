; Load a Intel hex file into IDT7134 Dual-port RAM.
; the Dual-port RAM connects through a ribbon cable 
; to the 2732 EPROM socket on the 4004 single
; board computer to act as as EPROM emulator.
; Version 1.0 - Initial release.
; Version 2.0 - Changed from Dallas Semiconductor DS89C420 to Atmel AT89S52 CPU.

#include "8052.51"
#include "macros.51"

;constants...
cr              .equ    0DH             ;character equates
lf              .equ    0AH
esc             .equ    1BH

LED             .equ    T0              ;LED connected to T0

bps28800	    .equ    0FFH
bps14400	    .equ    0FEH
bps9600	        .equ    0FDH
bps4800	        .equ    0FAH
bps2400	        .equ    0F4H
bps1200	        .equ    0E8H
bps300	        .equ    0A0H

reloadhi        .equ    04BH
reloadlo        .equ    0FCH            ;65536-46079=19457 less 5

t1mh            .equ    09AH
CKMOD           .equ    096H

;bit addressable RAM 20H-2FH
tx_ready        .equ    bit_20.0

;internal RAM used by monitor
loop_count      .equ    39H
tick_count      .equ    3BH             ;counters for timer 1 interrupt routine
seconds         .equ    3CH             ;RTC seconds counter
minutes         .equ    3DH             ;RTC minutes counter
hours           .equ    3EH             ;RTC hours counter
days            .equ    3FH             ;RTC days counter
stack_base      .equ    40H             ;beginning of stack

rcv_buffer      .equ    70H             ;8 bytes (70H - 77H)
rcv_in_ptr      .equ    78H             ;receive buffer input pointer
rcv_out_ptr     .equ    79H             ;receive buffer output pointer

                 .org    0
                ljmp    init

                .org    03              ;external interrupt 0
                ljmp	external0

                .org    0BH             ;timer 0
                ljmp	timer0

                .org    13H             ;external interrupt 1
                ljmp	external1

                .org    1BH             ;timer 1
                ljmp	timer1

                .org    23H             ;serial 0
                ljmp	serial0

                .org    2BH             ;timer 2
                ljmp	timer2


external0:      reti
external1:      reti
timer1:         reti
timer2:         reti
serial1:        reti


;start of monitor program

                .org    100H                    ;start of monitor code
                mov     A,#0                    ;short delay
                djnz    ACC,*                
init:           mov     IE,#0                   ;disable all interrupts
                mov     SP,#stack_base          ;set the stack pointer somewhere safe
                mov     PSW,#0                  ;set register bank zero

                mov     tick_count,#20          ;initialize RTC counters
                mov     seconds,#0
                mov     minutes,#0
                mov     hours,#0
                mov     days,#0

;initialize cpu's serial port
                mov     SCON,#01010010B         ;set serial port for mode 1, set ren and ti

;initialize the timer/counters.  
;timer 1 is to be configured as mode 2, 8 bit auto-reload to generate the serial port baud rate. 
;timer 0 is to be configured as mode 1, 16 bit counter/timer.  timer 0 interrupt is dedicated to the real time clock.
                mov     TMOD,#00100001B         ;set timer 1 to mode 2 (8 bit auto reload), set timer 0 to mode 1
                mov     TL0,#reloadlo           ;load timer 0 low byte
                mov     TH0,#reloadhi           ;load timer 0 high byte
                mov     TH1,#bps9600            ;load timer 1 value for 9600 bps
                mov     rcv_in_ptr,#rcv_buffer  ;initialize buffer pointers
                mov     rcv_out_ptr,#rcv_buffer

;enable interrupts                
                setb    TR0                     ;enable timer 0 for real time clock
                setb    TR1                     ;enable timer 1 for serial port baud rate
                setb    IP.4                    ;make serial interrupt higher priority
                setb    IP.1                    ;make timer 0 interrupt higher priority
                setb    IE.1                    ;enable timer 0 overflow interrupt
                setb    IE.4                    ;enable serial port interrupts
                setb    IE.7                    ;global interrupt enable
               
sign_on:        lcall   puts
                .text   "\r\n"
                .text   "IDT7134 EPROM Emulator Version 2.0.6\r\n"
                .db     0
                sjmp    showcmds

get_monitor_cmd:
                lcall   crlf
                orl     PSW,#00011000B          ;use register bank 3 for monitor
                lcall   puts
                .text   "\n\r>>"                ;prompt
                .db     0
               
mainloop:       inc     loop_count+01H          ;increment loop_count lo byte
                mov     A,loop_count+01H
                jnz     mloop1                  ;skip the next instruction if loop_count lo byte has not rolled over from 255 to 0
                inc     loop_count              ;increment loop_count hi byte
mloop1:         orl     A,loop_count
                jnz     mloop2                  ;skip the next instruction if hi and lo bytes are not both zero
                cpl     LED                     ;toggle the LED when the count rolls over from 65535 to 0 (about 3Hz)
mloop2:         lcall   serial_input            ;check for input from the serial port
                jnc     mainloop                ;no carry means no input is available
                lcall   toupper                 ;convert the input character to uppercase
                cje(A,#':',starthexdl)          ;start of Intel hex record(':') received at the prompt, jump to load routine
                cje(A,#'D',dump)                ;dump dual-port RAM contents
                cje(A,#'F',fill)                ;fill dual-port RAM with value
                cje(A,#'L',load)                ;load hex file into dual-port RAM
                cje(A,#'M',memory)              ;examine dual-port RAM contents
                cje(A,#'V',move)                ;move block of dual-port RAM
                cje(A,#'U',uptime)
                

showcmds:       lcall   puts
                .text   "\r\n"
                .text   " D - Dump IDT7134 Dual-port RAM\n\r"
                .text   " F - Fill IDT7134 Dual-port RAM\n\r"
                .text   " L - Load Intel hex file into IDT7134 Dual-port RAM\n\r"
                .text   " M - Examine/Modify IDT7134 Dual-port RAM\n\r"
                .text   " U - Show uptime\n\r"
                .text   " V - Move IDT7134 Dual-port RAM\n\r"
                .db     0
                ljmp    get_monitor_cmd          ;no match so back to get another input

;----------------------------------------------------------------------
; Examine/Modify Dual-port RAM
;----------------------------------------------------------------------
memory:         lcall   puts
                .text   "\n\rAddress: "
                .db     0
                lcall   read_four
                jnc     mem1
                ljmp    get_monitor_cmd

mem1:           lcall   crlf            ;go to a new line
                mov     A,DPH           ;show the address
                lcall   write_hex
                mov     A,DPL
                lcall   write_hex
                movx    A,@DPTR         ;show the current data
                push    ACC
                push    DPH
                push    DPL
                lcall   puts
                .text   "   --> "
                .db     0
                pop     DPL
                pop     DPH
                pop     ACC
                lcall   write_hex          ;show byte retrieved by patch
                push    DPH
                push    DPL
                lcall   puts
                .text   "   New value: "
                .db     0
                pop     DPL
                pop     DPH
                lcall   getdigit        ;get the next character
                cjne    A,#20H,mem4     ;is it a space?
                inc     DPTR            ;yes, increment pointer
                sjmp    mem1

mem4:           cje(A,#cr,get_monitor_cmd)
                lcall   read_two1       ;get the new value
                movx    @DPTR,A         ;move the new value to memory
                inc     DPTR            ;point to next location
                sjmp    mem1            ;and continue


;------------------------------------------------------------------------
; Download Intel HEX file into Dual-port RAM
; R5, holds the number of bytes per line, R6 holds the checksum for the line,
; R7 holds the checksum error count.
; NOTE: when using the Teraterm "Send File" function, make sure the "Binary" option is selected!
;------------------------------------------------------------------------
;A record (line of text) consists of six fields (parts) that appear in order from left to right:
;   1. Start code, one character, an ASCII colon ':'.
;   2. Byte count, two hex digits, indicating the number of bytes (hex digit pairs) in the data field. The maximum byte count is 255 (0xFF). 16 (0x10) and 32 (0x20) are commonly used byte counts.
;   3. Address, four hex digits, representing the 16-bit beginning memory address offset of the data. The physical address of the data is computed by adding this offset to a previously established base address, thus allowing memory addressing beyond the 64 kilobyte limit of 16-bit addresses. The base address, which defaults to zero, can be changed by various types of records. Base addresses and address offsets are always expressed as big endian values.
;   4. Record type, two hex digits (00=data, 01=end of file), defining the meaning of the data field.
;   5. Data, a sequence of n bytes of data, represented by 2n hex digits. Some records omit this field (n equals zero). The meaning and interpretation of data bytes depends on the application.
;   6. Checksum, two hex digits, a computed value (starting with the byte count) that can be used to verify the record has no errors.

;start of Intel hex record (':') received at the '>>' prompt
starthexdl:     mov     R7,#0                   ;clear error count
                sjmp    echofirstchar           ;join the hex file load routine

load:           lcall   puts                   ;prompt for start of xfer
                .text   "\n\n\rWaiting for hex download...\n\r"
                .db     0
                mov     R7,#0                   ;clear error count

getfirstchar:   lcall   getc                    ;get the first character
echofirstchar:  lcall   putc                    ;echo it
                cjne    A,#esc,checkstartchar
                ljmp    init                    ;restart if escape key

checkstartchar: cjne    A,#':',getfirstchar     ;go back and wait wait for start character
                lcall   read_two                ;get byte count
                jz      lastrecord              ;last record has byte count of zero
                mov     R5,A                    ;load byte count for line into counthi
                mov     R6,A                    ;load count into checksum
                lcall   read_two                ;get high byte of start address
                mov     DPH,A
                add     A,R6                    ;add high byte to checksum
                mov     R6,A
                lcall   read_two                ;get low byte of start address
                mov     DPL,A
                add     A,R6
                mov     R6,A                    ;add low byte to checksum
                lcall   read_two                ;get the record type
                add     A,R6
                mov     R6,A                    ;add record type to checksum
readloop:       lcall   read_two                ;get the data byte
                movx    @DPTR,A
                add     A,R6                    ;add to checksum
                mov     R6,A
                inc     DPTR                    ;move DPTR to next byte address
                djnz    R5,readloop             ;loop until finished
                mov     A,R6                    ;form two's complement of checksum
                cpl     A
                inc     A
                mov     R6,A                    ;put two's complement back in R6

                lcall   read_two                ;get checksum
                clr     C                       ;clear carry
                subb    A,R6                    ;subtract computed checksum from byte
                jz      csum_OK                 ;zero means they agree
                inc     R7                      ;increment error counter
csum_OK:        lcall   getc                    ;get line feed
                lcall   putc                    ;echo it
                lcall   getc                    ;get carriage return
                lcall   putc                    ;echo it                
                ajmp    getfirstchar            ;go back for next line

lastrecord:     lcall   read_two                ;get the last address high byte
                lcall   read_two                ;get the last address low byte
                lcall   read_two                ;get the last record type
                lcall   read_two                ;get the last checksum
                lcall   getc                    ;get the last carriage return
                lcall   putc                    ;echo it                
;               lcall   getc                    ;get the last line feed
;               lcall   putc                    ;echo it                

                mov     A,R7                    ;get error count
                swap    A                       ;swap nibbles
                anl     A,#00001111B            ;mask out bits
                lcall   crlf
                jz      around                  ;ignore leading zero
                lcall   hex2asc
                lcall   putc
around:         mov     A,R7                    ;send least significant digit
                lcall   hex2asc
                lcall   putc
                lcall   puts
                .text   " checksum errors during download.\n\r"
                .db     0
                ljmp    get_monitor_cmd     

;------------------------------------------------------------------------
; display dual-port RAM data (hex and ascii).  DPTR holds the address.
; R2 and R3 hold the number of bytes to dump.  R4 is a counter for bytes/line.
;------------------------------------------------------------------------
dump:           lcall   puts
                .text   "\n\rAddress: "
                .db     0
                lcall   read_four               ;read address from serial port
                jnc     dump1
                ljmp    get_monitor_cmd

dump1:          mov     A,DPL
                anl     A,#11110000B            ;start on 16-byte boundary
                mov     DPL,A
                push    DPH
                push    DPL
                lcall   puts
                .text   "   Count: "
                .db     0

                lcall   read_four               ;get the byte count
                jnc     dump2
                ljmp    get_monitor_cmd

dump2:          mov     R2,DPH
                mov     R3,DPL
                pop     DPL
                pop     DPH
                jc      dmpskp                  ;exit if escape key pressed

                lcall   crlf                    ;start on a new line
loutlp:         mov     R4,#16                  ;set up byte counter
                mov     A,DPH                   ;at start of line output address
                lcall   write_hex
                mov     A,DPL
                lcall   write_hex
                lcall   space                   ;add a space to make it tidy
                push    DPH                     ;save DPTR for later
                push    DPL
boutlp:         movx    A,@DPTR                 ;output hex one byte at a time
                lcall   write_hex
                lcall   space                   ;a space to make it neat
                inc     DPTR
                djnz    R4,boutlp               ;if not done, loop

                lcall   space                   ;a space to make it neat
                pop     DPL                     ;restore the DPTR
                pop     DPH
                mov     R6,#16                  ;set up byte counter
aoutlp:         movx    A,@DPTR                 ;output ASCII one byte at a time
                cjg(A,#127,aout3)               ;see if its >127
                cjl(A,#' ',aout3)               ;see if its <32
                sjmp    aout4                   ;it's a printable character, so print it
aout3:          mov     A,#'.'                  ;it's not a printable character, so print '.' instead
aout4:          lcall   putc                    ;print it
                inc     DPTR
                djnz    R6,aoutlp               ;if not done, loop

                lcall   crlf                    ;go to next line
                clr     C                       ;set up for subtract
                mov     A,R3                    ;subtract 16 from the count
                subb    A,#16
                mov     R3,A                    ;save lo byte
                mov     A,R2                    ;get the high byte
                subb    A,#0                    ;subtract carry if any
                mov     R2,A                    ;save the high byte
                orl     A,R3                    ;see if we've reached zero yet
                jz      dmpskp                  ;if so, we're done
                jnc     loutlp                  ;no carry, so not done yet
dmpskp:         ljmp    get_monitor_cmd


;-------------------------------------------------------------
; fill Dual-port RAM with a byte.  DPTR holds the target address.
; R2, R3 hold the number of locations to fill.
;-------------------------------------------------------------
fill:           lcall   puts
                .text   "\n\rAddress: "
                .db     0
                lcall   read_four               ;get address
                jc      fill_exit

                push    DPH
                push    DPL
                lcall   puts
                .text   "   Count: "
                .db     0

                lcall   read_four               ;get the byte count
                mov     R2,DPH
                mov     R3,DPL
                pop     DPL
                pop     DPH
                jc      fill_exit               ;exit if escape
                inc     R2                      ;fix djnz weirdness
                inc     R3                      ;fix djnz weirdness
                push    DPH
                push    DPL
                lcall   puts
                .text   "   Value: "
                .db     0
                pop     DPL
                pop     DPH
                lcall   read_two                ;byte to fill with
                jc      fill_exit
fill_loop:      movx    @DPTR,A                 ;store the byte
                inc     DPTR                    ;point to the next location
                djnz    R3,fill_loop            ;loop if not done
                djnz    R2,fill_loop            ;loop if not done
fill_exit:      ljmp    get_monitor_cmd


;-----------------------------------------------------------
; move a block of Dual-port RAM.  R2, R3 hold the number of bytes to move.
; R4, R5 hold the source address.  R6, R7 hold the destination address.
;------------------------------------------------------------
move:           lcall   puts
                .text   "\n\rSource: "
                .db     0
                lcall   read_four
                jc      move_exit
                mov     R4,DPH
                mov     R5,DPL

                lcall   puts
                .text   "   Destination: "
                .db     0
                lcall   read_four
                jc      move_exit
                mov     R6,DPH
                mov     R7,DPL

                lcall   puts
                .text   "   Count: "
                .db     0
                lcall   read_four
                jc      move_exit
                mov     R2,DPH
                mov     R3,DPL
                inc     R2
                inc     R3

move_loop:      mov     DPH,R4                  ;high byte of source address
                mov     DPL,R5                  ;low byte of source address
                movx    A,@DPTR                 ;fetch the data byte
                inc     DPTR                    ;point to the next location
                mov     R4,DPH                  ;save the high byte
                mov     R5,DPL                  ;save the low byte

                mov     DPH,R6                  ;high byte of destination address
                mov     DPL,R7                  ;low byte of destination address
                movx    @DPTR,A                 ;store the byte
                inc     DPTR                    ;point to the next location
                mov     R6,DPH                  ;save the high byte
                mov     R7,DPL                  ;save the low byte

                djnz    R3,move_loop            ;loop if not done
                djnz    R2,move_loop            ;loop if not done

move_exit:      ljmp    get_monitor_cmd


;------------------------------------------------------------------------
; reads two hex characters from the serial port and converts them into a
; byte returned in a.  returns with carry flag set if escape key is pressed.
;------------------------------------------------------------------------
read_two:       lcall   get_hex         ;get the first character
read_two1:      cje(A,#esc,read_two6)   ;jump if escape key
                cje(A,#cr,read_two6)    ;jump if return key
read_two2:      cjl(A,#'0',read_two)    ;try again if less than '0'

                lcall   asc2hex         ;convert to hex nibble
                push    B
                mov     B,A             ;save the first digit in b

read_two4:      lcall   get_hex         ;get next character
                cje(A,#esc,read_two5)   ;jump if escape key
                cje(A,#cr,read_two7     ;jump if enter key
                cjl(A,#'0',read_two4)   ;try again if less than '0'

                lcall   asc2hex         ;change to hex nibble
                push    ACC             ;save the second nibble on the stack
                mov     A,B             ;recall the first nibble from b
                swap    A               ;move the first nibble to the high order position
                mov     B,A             ;put the first nibble back in b
                pop     ACC             ;recall the second nibble from the stack
                orl     A,B             ;put the first nibble from b together with the second nibble in a
                pop     B               ;restore b
                clr     C               ;clear the carry flag
                ret

read_two5:      pop     B
read_two6:      setb    C
                ret

read_two7:      mov     A,B             ;recall the first nibble from b
                pop     B
                clr     C
                ret

;------------------------------------------------------------------------
; reads four hex characters from the serial port and converts them into
; two bytes returned in DPH and DPL.   returns with carry flag set if the
; escape key is pressed.
;------------------------------------------------------------------------
read_four:      mov     DPL,#0
                mov     DPH,#0
                lcall   get_hex                 ;get first digit
                jc      read_four4              ;exit if enter, escape, or space
                lcall   shift                   ;else shift digit into position
                mov     B,#3                    ;maximum of three more characters
read_four2:     lcall   get_hex                 ;get the next three digits
                cje(A,#cr,read_four3)           ;exit if enter key is pressed
                cje(A,#esc,read_four4)          ;exit if escape key is pressed
                cje(A,#' ',read_four2)          ;try again if space bar
                lcall   shift
                djnz    B,read_four2
read_four3:     clr     C
read_four4:     ret

shift:          lcall   asc2hex                 ;convert the ASCII character to a hex nibble
                push    ACC                     ;save the nibble in a
                mov     A,DPH                   ;get the value in DPH
                swap    A                       ;move low byte in DPH to hi position
                anl     A,#11110000B
                mov     DPH,A
                mov     A,DPL
                swap    A
                anl     A,#00001111B
                orl     A,DPH
                mov     DPH,A                   ;move hi byte in DPL to DPH

                mov     A,DPL
                swap    A
                anl     A,#11110000B
                mov     DPL,A

                pop     ACC
                anl     A,#00001111B
                orl     A,DPL
                mov     DPL,A
                ret

;-------------------------------------------------------------------------
; writes byte in a to the serial port as two ASCII hex characters.
;-------------------------------------------------------------------------
write_hex:      push    B
                mov     B,R6
                push    B
                mov     R6,A
                swap    A
                lcall   hex2asc
                lcall   putc
                mov     A,R6
                lcall   hex2asc
                lcall   putc
                pop     B
                mov     R6,B
                pop     B
                ret


;------------------------------------------------------------------------
; converts an ascii character in a to its hex equivalent.
; returns value in lower nibble, upper nibble zeros
;------------------------------------------------------------------------
asc2hex:        push    B
                cjl(A,#'a',no_action)
                subb    A,#20H                  ;convert to upper case
no_action:      clr     CY
                subb    A,#'0'
                mov     B,A
                subb    A,#10                   ;subtract 10 decimal
                jb      CY,a2lt10
                mov     A,B
                subb    A,#7
                mov     B,A
a2lt10:         mov     A,B
                pop     B
                ret


;------------------------------------------------------------------------
; converts the lower nibble in a to an ASCII character returned in a
;------------------------------------------------------------------------
hex2asc:        push    B
                anl     A,#00001111B
                clr     CY
                mov     B,A
                subb    A,#10
                mov     A,B
                jb      CY,h2lt10
                add     A,#7
h2lt10:         add     A,#'0'
                pop     B
                ret


;------------------------------------------------------------------------
; puts sends the text string that follows the call to the serial port.
; although is not ended with a ret, must be called like a subroutine.
; the text string must end with zero.  Uses DPTR and ACC
;------------------------------------------------------------------------
puts:           pop     DPH                     ;load DPTR with address of first char
                pop     DPL
xstr_1:         clr     A                       ;(zero offset)
                movc    A,@a+DPTR               ;fetch first char in string
                lcall   putc                    ;send it out
                inc     DPTR                    ;bump pointer
                clr     A
                movc    A,@a+DPTR               ;get next character
                cjne    A,#0,xstr_1             ;loop until zero read
                mov     A,#1
                jmp     @a+DPTR                 ;return to code after the zero


;----------------------------------------------------------------
; outputs a space to the serial port
;----------------------------------------------------------------
space:          push    ACC
                mov     A,#' '
                lcall   putc
                pop     ACC
                ret

;-----------------------------------------------------------------
; transmits a carriage return and line feed out the serial port
;-----------------------------------------------------------------
crlf:           push    ACC
                mov     A,#cr
                lcall   putc
                mov     A,#lf
                lcall   putc
                pop     ACC
                jnb     tx_ready,*            ;wait for ready
                ret


;------------------------------------------------------------------
; returns a character in ACC from the serial port
; does not echo the character back to the serial port
;------------------------------------------------------------------
getc:           lcall   serial_input
                jnc     getc            ;wait for a character
                ret

;------------------------------------------------------------------
; returns with a character in the Accumulator from the serial port. 
; echos the character back to the serial port if it is alphanumeric.
;------------------------------------------------------------------
getdigit:       lcall   serial_input
                jnc     getdigit        ;wait for a character
                anl     A,#01111111B    ;strip off parity bit
                cjl(A,#'0',digit6)      ;return without echo if less than '0'
                cjg(A,#'z',digit6)      ;return without echo if above 'z'
                cjl(A,#'a',digit5)
                subb    A,#20H          ;convert to upper case
digit5:         lcall   putc            ;echo all other characters
digit6:         ret


;------------------------------------------------------------------
; returns a hex character in the Accumulator from the serial port. echos the
; character back to the serial port. sets the carry flag if ENTER, ESCAPE, or space
;------------------------------------------------------------------
get_hex:        lcall   serial_input
                jnc     get_hex         ;wait for a character
                anl     A,#01111111B    ;mask out parity
                cje(A,#cr,get_hex3)     ;exit if enter key
                cje(A,#esc,get_hex3)    ;exit if escape key
                cje(A,#' ',get_hex3)    ;exit if space bar
                cjl(A,#'0',get_hex)     ;try again if less than '0'
                cjl(A,#'a',get_hex1)    ;jump if already upper case...
                subb    A,#20H          ;else convert to upper case
get_hex1:       cjg(A,#'F',get_hex)     ;try again if greater than 'F'
                cjl(A,#':',get_hex2)    ;continue if '0'-'9'
                cjl(A,#'A',get_hex)     ;try again if less than 'A'
get_hex2:       lcall   putc            ;echo the character
                clr     C               ;clear carry flag
                ret

get_hex3:       setb    C               ;set carry flag
                ret                     ;return with character in a

;------------------------------------------------------------------------
; puts the character in ACC into the transmit buffer,
; waits for transmitter ready if necessary
;------------------------------------------------------------------------
putc:           jnb     tx_ready,*             ;wait for transmitter ready flag
                clr     tx_ready               ;reset transmitter ready flag
                mov     SBUF,A
                ret

;------------------------------------------------------------------------
; sets the carry flag and returns with the character in ACC if there is a character
; available in the serial input buffer. clears the carry flag if buffer is empty.  
;------------------------------------------------------------------------
serial_input:   mov     A,rcv_out_ptr
                cjne    A,rcv_in_ptr,serial_input1 ;continue if something in buffer
                clr     C                       ;clear carry flag
                ret                             ;return empty handed

serial_input1:  push    B                       ;save b
                mov     B,R0
                push    B                       ;save R0
                mov     R0,rcv_out_ptr          ;put the pointer into R0
                mov     A,@R0                   ;get character from buffer
                inc     rcv_out_ptr             ;update output pointer
                anl     rcv_out_ptr,#07h        ;mask out bits to roll over
                orl     rcv_out_ptr,#rcv_buffer
                pop     B
                mov     R0,B                    ;restore R0
                pop     B                       ;restore b
                setb    C                       ;set carry flag
                ret
;------------------------------------------------------------------------                
; converts the ascii code in ACC to uppercase, if it is lowercase
;------------------------------------------------------------------------
toupper:        cjne	A,#'a',toupper2
toupper2:	    jc	    toupper4		         ;exit if character is already uppercase
                cjne	A,#'{',toupper3
toupper3:	    jnc	    toupper4		         ;end if ACC >= 123
                add	    A, #0e0h		         ;convert to uppercase
toupper4:	    ret                

;------------------------------------------------------------------------
; pauses for 1 millisecond times the number in a.
;------------------------------------------------------------------------
delay:          push    B
                mov     B,#201                  ;240 for 12 MHz, 201 for 11.0592 MHz
                djnz    B,*
                djnz    B,*
                pop     B
                djnz    ACC,delay
                ret

;------------------------------------------------------------------------
; pauses for 1 second
;------------------------------------------------------------------------
onesecond:      mov     R7,#10                  ;10 times
                mov     A,#100                  ;100 milliseconds
                acall   delay
                djnz    R7,$-4
                ret                
                
;------------------------------------------------------------------------
; displays the uptime.
;------------------------------------------------------------------------                
uptime:         lcall   crlf
                lcall   puts
                .text   "Uptime: "
                .db     0

showdays:       mov     A,days
                jz      showhrs
                lcall   printdecimal
                cjne    A,#1,showdays1
                lcall   puts
                .text   " day, "
                .db     0
                sjmp    showhrs
showdays1:      lcall   puts
                .text   " days, "
                .db     0

showhrs:        mov     A,hours
                jz      showmins
                lcall   printdecimal
                cjne    A,#1,showhrs1
                lcall   puts
                .text   " hour, "
                .db     0
                sjmp    showmins
showhrs1:       lcall   puts
                .text   " hours, "
                .db     0    
                
showmins:       mov     A,minutes
                jz      showsecs
                lcall   printdecimal
                cjne    A,#1,showmins1
                lcall   puts
                .text   " minute, "
                .db     0
                sjmp    showsecs
showmins1:      lcall   puts
                .text   " minutes, "
                .db     0   
                
showsecs:       mov     A,seconds
                lcall   printdecimal
                cjne    A,#1,showsecs1
                lcall   puts
                .text   " second"
                .db     0  
                ljmp    get_monitor_cmd        
showsecs1:      lcall   puts
                .text   " seconds"
                .db     0  
                ljmp    get_monitor_cmd
                
;------------------------------------------------------------------------
; prints the value in A as a decimal number
;------------------------------------------------------------------------                   
printdecimal:   push    ACC
                push    B
                push    0
                mov     B,#100
                div     AB                      ;leftmost digit in a
                add     A,#30h                  ;convert to ASCII
                mov     R0,A                    ;save the digit in R0
                cje(A,#'0',printdecimal1)       ;don't print leading zero
                lcall   putc                    ;print it
printdecimal1:  mov     A,B                     ;get remainder
                mov     B,#10
                div     AB                      ;middle digit in A, rightmost digit in B
                add     A,#30h                  ;convert to ASCII
                cjg(R0,#'0',printdecimal2)      ;skip the check for leading zero if the first digit was not zero
                cje(A,#'0',printdecimal3)       ;don't print leading zero
printdecimal2:  lcall   putc
printdecimal3:  mov     A,B                     ;get the remainder
                add     A,#30h                  ;convert to ASCII
                lcall   putc
                pop     0
                pop     B
                pop     ACC
                ret
            
                
;-------------------------------------------------------------------
;serial0 interrupt handler
;-------------------------------------------------------------------
serial0:	    push    PSW                     ;save flags and registers
                jnb     TI,s1                   ;jump if not a transmit interrupt
                clr     TI
                setb    tx_ready
s1:             jnb     RI,s2                   ;jump if not a receive interrupt
                clr     RI                      ;clear receive interrupt flag
                push    ACC                     ;save the ACCumulator
                mov     A,R0                    ;save register 0 in a
                mov     R0,rcv_in_ptr           ;use R0 as a pointer into the buffer
                mov     @R0,SBUF                ;get the character from the serial port
                mov     R0,A                    ;restore register 0 from a
                pop     ACC                     ;restore the ACCumulator
                inc     rcv_in_ptr              ;update the receive buffer input pointer
                anl     rcv_in_ptr,#00000111B
                orl     rcv_in_ptr,#rcv_buffer

s2:             pop     PSW
                reti

exit_serial0:   pop     PSW
                reti


;------------------------------------------------------------------------
; Timer 0 interrupt caused by Timer 0 overflow
;------------------------------------------------------------------------
;every T0 interrupt (every 50 milliseconds or 20 times per second)
timer0:     	clr     TR0
                mov     TH0,#reloadhi           ;reload timer 0 high byte
                mov     TL0,#reloadlo           ;reload timer low byte
                setb    TR0
                push    PSW
                djnz    tick_count,exit1

;every 20th T0 interrupt (every 1000 milliseconds or once per second)
                mov     tick_count,#20          ;restore counter
                push    ACC                     ;save accumulator (we'll need it)
                inc     seconds                 ;increment seconds counter
                mov     A,seconds
                cjne    A,#60,exit2             ;60 seconds?
                mov     seconds,#0
                inc     minutes                 ;increment minutes counter
                mov     A,minutes
                cjne    A,#60,exit2             ;60 minutes?
                mov     minutes,#0
                inc     hours                   ;increment hours counter
                mov     A,hours
                cjne    A,#24,exit2
                mov     hours,#0
                inc     days

exit2:          pop     ACC
exit1:          pop     PSW
                reti

                .end


