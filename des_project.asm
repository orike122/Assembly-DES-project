;Add this legendary project to Github Arctic Vault ;)
; multi-segment executable file template.

data segment
    ;the message after converted to hexa
     m db 8 dup(0) 
    ;encryption key that was choosed by the user
    ekey db 8 dup(0)
    ;pc-1 means - bit arrangment to create the permuted key - k+
    pc_1 db 57,49,41,33,25,17,09
         db 01,58,50,42,34,26,18
         db 10,02,59,51,43,35,27
         db 19,11,03,60,52,44,36
         db 63,55,47,39,31,23,15
         db 07,62,54,46,38,30,22
         db 14,06,61,53,45,37,29
         db 21,13,05,28,20,12,04
    
    pc_2 db 14,17,11,24,01,05
         db 03,28,15,06,21,10
         db 23,19,12,04,26,08
         db 16,07,27,20,13,02
         db 41,52,31,37,47,55
         db 30,40,51,45,33,48
         db 44,49,39,56,34,53
         db 46,42,50,36,29,32
         
    ip_tbl db 58,50,42,34,26,18,10,02
           db 60,52,44,36,28,20,12,04
           db 62,54,46,38,30,22,14,06
           db 64,56,48,40,32,24,16,08
           db 57,49,41,33,25,17,09,01
           db 59,51,43,35,27,19,11,03
           db 61,53,45,37,29,21,13,05
           db 63,55,47,39,31,23,15,07
     
    e_tbl db 32,01,02,03,04,05
          db 04,05,06,07,08,09
          db 08,09,10,11,12,13
          db 12,13,14,15,16,17
          db 16,17,18,19,20,21
          db 20,21,22,23,24,25
          db 24,25,26,27,28,29
          db 28,29,30,31,32,01
          
          
          
    s1 db 14,04,13,01,02,15,11,08,03,10,06,12,05,09,00,07
       db 00,15,07,04,14,02,13,01,10,06,12,11,09,05,03,08
       db 04,01,14,08,13,06,02,11,15,12,09,07,03,10,05,00
       db 15,12,08,02,04,09,01,07,05,11,03,14,10,00,06,13
       
       
    s2 db 15,01,08,14,06,11,03,04,09,07,02,13,12,00,05,10
       db 03,13,04,07,15,02,08,14,12,00,01,10,06,09,11,05
       db 00,14,07,11,10,04,13,01,05,08,12,06,09,03,02,15
       db 13,08,10,01,03,15,04,02,11,06,07,12,00,05,14,09
    
       
    s3 db 10,00,09,14,06,03,15,05,01,13,12,07,11,04,02,08
       db 13,07,00,09,03,04,06,10,02,08,05,14,12,11,15,01
       db 13,06,04,09,08,15,03,00,11,01,02,12,05,10,14,07
       db 01,10,13,00,06,09,08,07,04,15,14,03,11,05,02,12
       
    
    s4 db 07,13,14,03,00,06,09,10,01,02,08,05,11,12,04,15
       db 13,08,11,05,06,15,00,03,04,07,02,12,01,10,14,09
       db 10,06,09,00,12,11,07,13,15,01,03,14,05,02,08,04
       db 03,15,00,06,10,01,13,08,09,04,05,11,12,07,02,14
       
       
    s5 db 02,12,04,01,07,10,11,06,08,05,03,15,13,00,14,09
       db 14,11,02,12,04,07,13,01,05,00,15,10,03,09,08,06
       db 04,02,01,11,10,13,07,08,15,09,12,05,06,03,00,14
       db 11,08,12,07,01,14,02,13,06,15,00,09,10,04,05,03
       
       
    s6 db 12,01,10,15,09,02,06,08,00,13,03,04,14,07,05,11
       db 10,15,04,02,07,12,09,05,06,01,13,14,00,11,03,08
       db 09,14,15,05,02,08,12,03,07,00,04,10,01,13,11,06
       db 04,03,02,12,09,05,15,10,11,14,01,07,06,00,08,13
       
       
    s7 db 04,11,02,14,15,00,08,13,03,12,09,07,05,10,06,01
       db 13,00,11,07,04,09,01,10,14,03,05,12,02,15,08,06
       db 01,04,11,13,12,03,07,14,10,15,06,08,00,05,09,02
       db 06,11,13,08,01,04,10,07,09,05,00,15,14,02,03,12
       
       
    s8 db 13,02,08,04,06,15,11,01,10,09,03,14,05,00,12,07
       db 01,15,13,08,10,03,07,04,12,05,06,11,00,14,09,02
       db 07,11,04,01,09,12,14,02,00,06,10,13,15,03,05,08
       db 02,01,14,07,04,10,08,13,15,12,09,00,03,05,06,11
       
    p db 16,07,20,21
      db 29,12,28,17
      db 01,15,23,26
      db 05,18,31,10
      db 02,08,24,14
      db 32,27,03,09
      db 19,13,30,06
      db 22,11,04,25
      
      
   pc_minus db 40,08,48,16,56,24,64,32
            db 39,07,47,15,55,23,63,31
            db 38,06,46,14,54,22,62,30
            db 37,05,45,13,53,21,61,29
            db 36,04,44,12,52,20,60,28
            db 35,03,43,11,51,19,59,27
            db 34,02,42,10,50,18,58,26 
            db 33,01,41,09,49,17,57,25  
           
         
    k_plus db 7 dup(00)
    selected_byte db ?
    selected_bit db  ?
    temp_byte db 00000000b
    byte_builder_pointer db 0
    pointer dw 0000h
    k_plus_l dd 0h
    k_plus_r dd 0h
    shifts db 01,01,02,02,02,02,02,02,01,02,02,02,02,02,02,01
    c dd 0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h,0h
    d dd 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    cd db 112 dup(0)
    temp_dword dd 00000000h
    k db 96 dup(0)
    para db 00h ;multipurpose parameter
    ip db 8 dup(0)
    l_0 db 4 dup(0)
    r_0 db 4 dup(0)
    l db 64 dup(0)
    r db 64 dup(0)
    e db 6 dup(0)
    l_index db 0
    r_index db 0
    b db 8 dup(0)
    s_row db 0
    s_column db 0
    s db 4 dup(0)
    f db 4 dup(0)
    temp_offset1 dw 0000h
    temp_offset2 dw 0000h
    temp_arr db 4 dup(0)
    lp db 4 dup(0)
    rl db 8 dup(0) 
    rp db 4 dup(0)
    dec_msg db 8 dup(0)
    enc_msg db 8 dup(0)
    m_ascii db 19 dup(0)
    ekey_ascii db 19 dup(0)
    enc_msg_ascii 16 dup(0),"$"
    dec_msg_ascii db 16 dup(0),"$"
    error_msg db "You typed in some invaild input, check help for list of command.",10,"Prees any key to start over...","$"
    ask_msg db "Please enter the 16 charecters length hexa message:","$"
    ask_enc db "Please enter the 16 charecters length hexa encrypted message:","$"
    ask_key db "Please enter the 16 charecters length hexa encryption key:","$"
    ask_dec_key db "Please enter the 16 charecters length hexa decryption key:","$"
    show_enc db "This is the encrypted message:","$"
    show_dec db "This is the encrypted message:","$"
    ask_command db "Enter command:","$"
    help_msg db "List of commands:",10,"e -  encrypt message using DES algorithm.",10,"d - decrypt message using DES algorithm.",10,10,"*notice - all encrytion/decryption input has to be 16 charecters length",10," hexadecimal number",10,"without extra zero in the start,or h at the end","$"
    welcome_msg db "Hello, welcome to DES encryptor/decryptor.","$"
    command db 4 dup(0)
    wait_key db "Press any key to continue...","$"
    help db "help"
    chelp db "HELP"
    enc db "enc"
    cenc db "ENC"
    decr db "dec"
    cdecr db "DEC"
    exit db  "exit"
    cexit db "EXIT"
        
    
    
    
    
ends

stack segment
    dw   128  dup(0)
ends

code segment
    
;INPUT OUTPUT
;gets input
;get:dest,length
;return:string
macro get_input dest,len
     pusha
     mov dx,offset dest
     mov bx,dx
     mov cl,len
     inc cl
     mov [bx],cl
     mov ah,0ah
     int 21h
     popa
endm get_input
;converts string to hexa
;get:source,dest
;return: hexa in dest
macro str2hex source,dest
     pusha 
     LOCAL agn_bld_char,goback,error,check_hex,skp_hex,check_cap,skp_1st_char,end_str
     xor di,di
     xor si,si
     mov si,2
     xor cx,cx
     mov cx,8
     agn_bld_char:
     mov pointer,0
     xor ax,ax
     xor dx,dx
     xor bx,bx
     goback:
     mov al,source+si

     ;check
     cmp al,30h
     js error
     mov bl,39h
     cmp bl,al
     js check_hex
     sub al,30h
     jmp skp_hex
     
     check_hex:
     cmp al,41h
     js error
     mov bl,46h
     cmp bl,al
     js check_cap
     sub al,55
     jmp skp_hex
     
     check_cap:
     cmp al,61h
     js error
     mov bl,66h
     cmp bl,al
     js error
     sub al,87
     
     skp_hex:
     
     cmp pointer,0
     jnz skp_1st_char
     inc si
     inc pointer
     mov dh,al
     jmp goback
     skp_1st_char:
     mov bl,10h
     mov dl,al
     mov al,dh
     mul bl
     mov dh,al
     or dh,dl
     mov dest+di,dh
     inc si
     inc di
     loop agn_bld_char
     
     jmp end_str
     error:
     ;reset screen
     call cln_scr
     set_cur 0,0
     print_msg error_msg
     ; wait for any key....    
     mov ah, 1
     int 21h
     jmp ask_again

     end_str:
     popa
endm str2hex
;prints message
;get:msg
macro print_msg pmsg
      pusha
      mov ah,09h
      mov dx,offset pmsg
      int 21h


      popa
endm print_msg
;cleans screen
proc cln_scr
     pusha
      ;clean screen
      mov ah,6
      mov al,0
      mov bh,0_0fh
      xor cx,cx
      mov dh,200
      mov dl,200
      int 10h
      
     popa
     ret
endp cln_scr
;sets cursor position
;get:x,y
macro set_cur x,y
      ;set cursor
      pusha
      mov ah,2
      mov bh,0
      xor dx,dx
      int 10h
      popa
endm set_cur x,y
;gets message and key
proc get_msgnkey
     pusha
     ;reset screen
     call cln_scr
     set_cur 0,0
     print_msg ask_msg
     get_input m_ascii,16
     str2hex m_ascii,m
     ;reset screen
     call cln_scr
     set_cur 0,0
     print_msg ask_key
     get_input ekey_ascii,16
     str2hex ekey_ascii,ekey
     popa
     ret
endp get_msgnkey
;gets encrypted message and key.
proc get_encnkey
     pusha
     ;reset screen
     call cln_scr
     set_cur 0,0
     print_msg ask_enc
     get_input enc_msg_ascii,16
     str2hex enc_msg_ascii,enc_msg
     ;reset screen
     call cln_scr
     set_cur 0,0
     print_msg ask_dec_key
     get_input ekey_ascii,16
     str2hex ekey_ascii,ekey
     popa
     ret
endp get_encnkey
;converts hexa to string
;get:source,dest
;return: string in dest
macro hex2str source,dest
      pusha
      LOCAL again,cont,cont1,skp_hex,skp_hex1 
      xor si,si
      xor ax,ax
      xor cx,cx
      xor di,di
      mov cx,8
      again:
      mov al,source+si
      and al,11110000b
      shr al,4

      
      cmp al,0ah
      js skp_hex
      add al,55
      mov dest+di,al
      inc di
      jmp cont
      
      skp_hex:
      add al,30h
      mov dest+di,al
      inc di
      
      
      cont:
      mov al,source+si
      and al,00001111b
      
      cmp al,0ah
      js skp_hex1
      add al,55
      mov dest+di,al
      inc di
      jmp cont1
      
      skp_hex1:
      add al,30h
      mov dest+di,al
      inc di
      cont1:
      inc si
      loop again
      popa
endm hex2str

     

;END INPUT OUTPUT
;the macro gets double word var and number of left shift.
;it returns the var left shifted.
macro shift_left dword,shft_no
     pusha 
     xor ax,ax
     xor bx,bx
     xor si,si
     xor di,di
     xor cx,cx
     xor dx,dx
     
     LOCAL shftt,contt,soff

     mov di,2
     mov ax,dword+0
     mov dx,dword+2
     mov cl,shft_no
     shftt:
     rcl ax,1
     pushf
     and ax,1111111111111110b
     rcl dx,1
     pushf
     and dx,1111111111111110b
     popf
     jnc contt
     or ax,0000000000010000b
     contt:
     popf
     jnc soff
     or dx,0000000000000001b
     soff:
     loop shftt
     mov dword+0,ax
     mov dword+2,dx
     popa
endm shift_left

;the macro permutate var using permutation table
;get:p table,byte index,adding parameter(if not used keep on zero)
;return: temp_byte containing the permutated byte. 
macro permute_key pc_table,byte_no,key,addpara
     
     pusha
     LOCAL again,skip_add,skip_check,skip_1,skip_2,skip_3,skip_4,skip_5,skip_6,skip_7,bit_1,bit_2,bit_3,bit_4,bit_5,bit_6,bit_7,end_bit_0,end_bit_1,end_bit_2,end_bit_3,end_bit_4,end_bit_5,end_bit_6,end_bit_7,end_builder,endd
     xor ax,ax
     xor bx,bx
     xor cx,cx
     xor dx,dx
     mov pointer,0000h
     mov temp_byte,00000000b
     mov byte_builder_pointer,0h
     mov ax,8
     mov dx,byte_no
     mul dx
     add ax,offset pc_table
     mov pointer,ax
     xor ax,ax
     xor dx,dx


     mov cx,8
     again:
     xor bx,bx
     xor ax,ax
     mov bx,pointer
     mov al,[bx]
     xor bx,bx
     mov dx,8
     div dl
     cmp ah,00h
     jnz skip_check
     dec al
     mov ah,08
     skip_check:
     mov selected_byte,al;**** ;maybe al and ah need to be switched, you need to check it out
     mov selected_bit,ah
     mov bx,offset key
     xor ax,ax
     mov al,addpara
     add bx,ax ;in case you want to add by a certain parameter you can use addpara otherwise keep it zero.beware addpara has to be byte size
     xor ax,ax
     mov al,selected_byte
     add bx,ax ;bx gets the offset of the selected byte
     mov dl,[bx] ;the selected byte copy to dl
     
     ;finds the selected bit using "and" and "shl"
     ;you need to add "cmp" before the "jnz endd".you need to add shr's(after delete this comment)
     cmp selected_bit,1
     jnz skip_1
        and dl,10000000b
     jmp endd
     skip_1:
     cmp selected_bit,2
     jnz skip_2
        and dl,01000000b
        shl dl,1
     jmp endd
     skip_2:
     cmp selected_bit,3
     jnz skip_3
        and dl,00100000b
        shl dl,2
     jmp endd
     skip_3:
     cmp selected_bit,4
     jnz skip_4
        and dl,00010000b
        shl dl,3
     jmp endd
     skip_4:
     cmp selected_bit,5
     jnz skip_5
        and dl,00001000b
        shl dl,4
     jmp endd
     skip_5:
     cmp selected_bit,6
     jnz skip_6
        and dl,00000100b
        shl dl,5
     jmp endd
     skip_6:
     cmp selected_bit,7
     jnz skip_7
        and dl,00000010b
        shl dl,6
     jmp endd
     skip_7:
     cmp selected_bit,8
     jnz endd
        and dl,00000001b
        shl dl,7
     endd:
        nop
        
        
     cmp byte_builder_pointer,0
     
     jnz bit_1 
        
        cmp dl,10000000b
        jnz end_bit_0
        xor temp_byte,10000000b
        end_bit_0:   
        jmp end_builder
        
     bit_1:
        
        cmp byte_builder_pointer,1
        
        jnz bit_2
        
        cmp dl,10000000b
        jnz end_bit_1
        xor temp_byte,01000000b
        end_bit_1:
        jmp end_builder
        
     bit_2:
    
       cmp byte_builder_pointer,2
       
       jnz bit_3
       
        cmp dl,10000000b
        jnz end_bit_2
        xor temp_byte,00100000b
        end_bit_2:
        jmp end_builder
        
     bit_3:
    
       cmp byte_builder_pointer,3
       
       jnz bit_4
       
        cmp dl,10000000b
        jnz end_bit_3
        xor temp_byte,00010000b
        end_bit_3:
        jmp end_builder
     
     bit_4:
     
       cmp byte_builder_pointer,4
       
       jnz bit_5
       
        cmp dl,10000000b
        jnz end_bit_4
        xor temp_byte,00001000b
        end_bit_4:
        jmp end_builder
        
     bit_5:
     
       cmp byte_builder_pointer,5
       
       jnz bit_6
       
        cmp dl,10000000b
        jnz end_bit_5
        xor temp_byte,00000100b
        end_bit_5:
        jmp end_builder
        
     bit_6:
     
       cmp byte_builder_pointer,6
       
       jnz bit_7
       
        cmp dl,10000000b
        jnz end_bit_6
        xor temp_byte,00000010b
        end_bit_6:
        jmp end_builder
        
     bit_7:
     
       cmp byte_builder_pointer,7
       
       jnz end_builder
       
        cmp dl,10000000b
        jnz end_bit_7
        xor temp_byte,00000001b
        end_bit_7:
        end_builder:
            nop
        
        
        inc pointer
        inc byte_builder_pointer   
        loop again
        
            popa
           
endm permute_key 
;arranges k_plus and do left shifts
;get: ekey
;return:c,d  
proc arrange_k_plus
     
     pusha
     xor ax,ax
     xor si,si
     mov cx,7
     agn_prmt_key:
     permute_key pc_1,si,ekey,0
     mov al,temp_byte
     mov k_plus+si,al
     inc si
     loop agn_prmt_key 
     
     xor bx,bx
     mov bl,k_plus+1
     mov k_plus_l+2,bx
     mov bl,k_plus
     mov k_plus_l+3,bx
     
     mov bl,k_plus+3
     mov bh,k_plus+2
     and bl,11110000b
     mov k_plus_l,bx
     
     xor bx,bx
     mov bh,k_plus+3
     and bh,00001111b
     mov bl,k_plus+4
     mov k_plus_r+2,bx
     mov bh,k_plus+5
     mov bl,k_plus+6
     mov k_plus_r,bx
     
     xor ax,ax
     xor dx,dx
     xor di,di
     xor bx,bx
     xor si,si
     mov cx,16
     mov di,2
     mov ax,k_plus_l+0
     mov dx,k_plus_l+2
     shuv:
     push cx
     xor cx,cx
     mov cl,shifts+si
     shft:
     rcl ax,1
     pushf
     and ax,1111111111111110b
     rcl dx,1
     pushf
     and dx,1111111111111110b
     popf
     jnc cont
     or ax,0000000000010000b
     cont:
     popf
     jnc sof
     or dx,0000000000000001b
     sof:
     loop shft
     mov c+bx,ax
     mov c+di,dx
     inc si
     add bx,4
     add di,4
     pop cx
     loop shuv
     
     xor ax,ax
     xor bx,bx
     xor si,si
     xor di,di
     xor cx,cx
     xor dx,dx


     mov di,2
     mov ax,k_plus_r+0
     mov dx,k_plus_r+2
     mov cl,4
     shftt:
     rcl ax,1
     pushf
     and ax,1111111111111110b
     rcl dx,1
     pushf
     and dx,1111111111111110b
     popf
     jnc contt
     or ax,0000000000010000b
     contt:
     popf
     jnc soff
     or dx,0000000000000001b
     soff:
     loop shftt
     mov k_plus_r+0,ax
     mov k_plus_r+2,dx
     
     xor ax,ax
     xor bx,bx
     xor si,si
     xor di,di
     xor cx,cx
     xor dx,dx
     
     mov cx,16
     mov di,2
     mov ax,k_plus_r+0
     mov dx,k_plus_r+2
     shuvvv:
     push cx
     xor cx,cx
     mov cl,shifts+si
     shfttt:
     rcl ax,1
     pushf
     and ax,1111111111111110b
     rcl dx,1
     pushf
     and dx,1111111111111110b
     popf
     jnc conttt
     or ax,0000000000010000b
     conttt:
     popf
     jnc sofff
     or dx,0000000000000001b
     sofff:
     loop shfttt
     mov d+bx,ax
     mov d+di,dx
     inc si
     add bx,4
     add di,4
     pop cx
     loop shuvvv
     
            
     popa
     ret
endp arrange_k_plus
;join lefts part of k with right parts of k
;get:c,d
;return:cd
proc join_16_cd
     pusha
     xor di,di
     xor si,si
     xor cx,cx
     mov di,2
     mov cx,16
     
     repeat:
     xor bx,bx
     xor ax,ax
     xor dx,dx
     
     mov temp_byte,0h
     mov ax,c+di
     mov cd+si,ah
     inc si
     mov cd+si,al
     inc si
     sub di,2
     mov ax,c+di
     mov cd+si,ah
     inc si
     add di,2

     mov temp_byte,al
     mov dx,d+di
     and dh,11110000b
     shr dh,4
     or temp_byte,dh
     mov bl,temp_byte
     mov cd+si,bl
     mov bx,d+di
     and bh,00001111b
     mov d+di,bx
     sub di,2
     inc si
     
     ;shift left for c
     mov bx,d+di
     mov temp_dword,bx
     mov bx,d+di+2
     mov temp_dword+2,bx
     shift_left temp_dword,4
     ;trans temp_dword to cd
     mov dx,temp_dword+2
     mov cd+si,dh
     inc si
     mov cd+si,dl
     inc si
     mov dx,temp_dword+0
     mov cd+si,dh
     inc si
     add di,6
     loop repeat 
     popa
     ret
endp join_16_cd
;the proc generates k using permutation with pc2
;get:cd
;return:k
proc generate_k
     
     pusha
     xor cx,cx
     xor si,si
     xor di,di
     xor bx,bx
     xor ax,ax
     mov para,0h
     mov cx,16
     repeat3:
     push cx
     mov cx,6
     repeat2:
     permute_key pc_2,di,cd,para
     mov al,temp_byte
     mov k+si,al
     inc si
     inc di
     loop repeat2
     xor di,di
     add para,7
     pop cx
     loop repeat3
     popa
     ret
endp generate_k
;arranges ip using permutate m with ip_tbl, and arranges l_0 and r_0
;get: m
;return: l_0,r_0
proc arrange_ip
     pusha
     xor cx,cx
     xor si,si
     xor ax,ax
     mov cx,8
     agn:
     permute_key ip_tbl,si,m,0
     mov al,temp_byte
     mov ip+si,al
     inc si
     loop agn
     xor cx,cx
     xor si,si
     xor ax,ax
     xor di,di
     mov cx,4
     agn2:
     mov al,ip+si
     mov l_0+di,al
     inc di
     inc si
     loop agn2
     mov cx,4
     xor di,di
     agn3:
     mov al,ip+si
     mov r_0+di,al
     inc di
     inc si
     loop agn3
     
     popa
     ret
endp arrange_ip
;the proc creating next l using feistel
;get: r_0/r
;return:l,lp
proc create_l
     pusha
     xor si,si;l index pointer(0 is 1, 1 is 2 and so on..)
     xor di,di;l byte pointer 
     xor ax,ax
     xor cx,cx
     ;create l_1
      
     ;1
     cmp l_index,0
     jnz skp_i0
     mov cx,4
     cpy:
     mov al,r_0+di
     mov l+di,al
     inc di
     loop cpy
     jmp end_i
     
     skp_i0:
     ;create l_2-16
     xor cx,cx
     xor si,si
     xor ax,ax
     mov cx,4
     ;copy old l to lp
     cpy_lp:
     mov al,l+si
     mov lp+si,al
     inc si
     loop cpy_lp
     
     ;copy old r to l
     xor si,si
     mov cx,4
     cpy_r2l:
     mov al,r+si
     mov l+si,al
     inc si
     loop cpy_r2l
     end_i:
     inc l_index
     
     popa
     ret
endp create_l
;the proc creating next r using feistel
;get: l
;return:r,rp
proc dec_r
     pusha
     ;create rp(previous r)
     mov si,offset r
     mov di,offset rp
     mov cx,4
     rep
     movsb
     ;copy old l to r
     mov si,offset l
     mov di,offset r
     mov cx,4
     rep
     movsb
     dec r_index
     popa
     ret
endp dec_r
;creates r using feistel
;get:l/l_0,k
;return:r
proc create_r 
      pusha
      
      xor si,si
      xor di,di
      xor di,di
      xor ax,ax
      xor cx,cx
      xor bx,bx
      mov para,0h
      ;function f
      ;generate e,
      cmp r_index,0
      jnz cont1
      mov cx,6
      agn4:
      permute_key e_tbl,si,r_0,0
      mov bl,temp_byte
      mov e+si,bl
      inc si
      loop agn4
      jmp endd_e
      
      cont1:
      mov cx,6
      agn5:
      permute_key e_tbl,si,r,0
      mov bl,temp_byte
      mov e+si,bl
      inc si
      loop agn5
      endd_e:
      
      xor si,si
      xor di,di
      mov al,r_index
      mov bl,6
      mul bl
      mov para,al

      xor_multibytes e,k,6,0,para 
       
      call generate_8_b
      call access_s_boxes
      call arrange_f
      
      ;1
      cmp r_index,0
      jnz skp_bld0
      
      xor_multibytes f,l_0,4,0,0 
      jmp skp_rest
      
      ;2-16
      skp_bld0: 
      xor_multibytes f,lp,4,0,0 
      
      skp_rest:
      
      xor si,si
      xor cx,cx
      xor ax,ax
      mov cx,4
      
      cpy2:
      mov al,f+si
      mov r+si,al
      inc si
      loop cpy2
      
      inc r_index
      popa
      ret
endp create_r
;creates r using feistel
;get:r,k
;return:l
proc dec_l
     pusha
     xor ax,ax
     xor si,si
     xor di,di
     xor bx,bx
     xor cx,cx
     
     ;f function decryption
     ;generate b
     mov cx,6
     dec_b:
     permute_key e_tbl,si,l,0
     mov bl,temp_byte
     mov e+si,bl
     inc si
     loop dec_b
     
      xor si,si
      xor di,di
      mov al,l_index
      mov bl,6
      mul bl
      mov para,al

      xor_multibytes e,k,6,0,para 
       
      call generate_8_b
      call access_s_boxes
      call arrange_f
      
      xor_multibytes f,rp,4,0,0
      
      xor si,si
      xor cx,cx
      xor ax,ax
      mov cx,4
      
      cpy_f2l:
      mov al,f+si
      mov l+si,al
      inc si
      loop cpy_f2l
      
      dec l_index
      popa
      ret
endp dec_l 
     
      
     
;the macro do xor for to multibytes arrays
;it gets 2 vars, and return the xored data to the 1st var
;also it gets no of bytes in each array
;add para to add certain parameter otherwise keep it zero
macro xor_multibytes var1,var2,bytes_no,addpara1,addpara2
      pusha
      LOCAL again
      xor si,si
      xor ax,ax
      xor dx,dx
      xor cx,cx
      xor di,di
      mov al,addpara1
      mov di,ax
      mov al,addpara2
      mov si,ax
      xor ax,ax
      mov cx,bytes_no
      again:
      mov al,var1+di
      mov dl,var2+si
      xor al,dl
      mov var1+di,al
      inc si
      inc di
      loop again 
      popa
endm xor_multibytes
;the proc generates 8 groups of 6 bits b
;get:e
;return:b
proc generate_8_b
     pusha
     xor si,si
     xor cx,cx
     mov temp_byte,0h
     ;zero b
     mov cx,8
     agn6:
     mov b+si,0h
     inc si
     loop agn6
     
     ;taking first 6 bits from e and put them
     ;into first cell of b
     xor si,si
     xor di,di
     xor ax,ax
     xor bx,bx
     xor cx,cx
     ;building 4 chunks of 6 bits
     ;doing this twice
     mov cx,2
     agn7:
 
     mov al,e+si
     and al,11111100b
     mov b+di,al
     inc di
     

     mov al,e+si
     and al,00000011b
     shl al,6
     inc si
     mov bl,e+si
     and bl,11110000b
     shr bl,2
     or bl,al
     mov b+di,bl
     inc di
     
     mov bl,e+si
     and bl,00001111b
     shl bl,4
     inc si
     mov al,e+si
     and al,11000000b
     shr al,4
     or bl,al
     mov b+di,bl
     inc di
     
     mov bl,e+si
     and bl,00111111b
     shl bl,2
     inc si
     mov b+di,bl
     inc di
     loop agn7
     popa
     ret
endp generate_8_b
;the proc replaces b with s-box data
;get:b
;return:s
proc access_s_boxes
     pusha
     xor ax,ax
     xor si,si
     xor cx,cx
     mov cx,8
     
     agn_box:
     ;find s box row
     mov ah,b+si
     and ah,00000100b
     shr ah,2
     mov al,ah
     mov ah,b+si
     and ah,10000000b
     shr ah,6
     or al,ah
     mov s_row,al
     
     ;find s box column
     xor ax,ax
     mov al,b+si
     and al,01111000b
     shr al,3
     mov s_column,al
     
     ;access to s box
     xor di,di
     xor ax,ax
     xor bx,bx
     mov al,s_row
     mov bl,16
     mul bl
     add al,s_column
     mov di,ax
     
     
     xor bx,bx
     ;s1
     cmp si,0
     jnz box2
     mov bl,s1+di
     jmp end_box
     
     ;s2
     box2:
     cmp si,1
     jnz box3
     mov bl,s2+di
     jmp end_box
     
     ;s3
     box3:
     cmp si,2
     jnz box4
     mov bl,s3+di
     jmp end_box
     
     ;s4
     box4:
     cmp si,3
     jnz box5
     mov bl,s4+di
     jmp end_box
     
     ;s5
     box5:
     cmp si,4
     jnz box6
     mov bl,s5+di
     jmp end_box
     
     ;s6
     box6:
     cmp si,5
     jnz box7
     mov bl,s6+di
     jmp end_box
     
     ;s7
     box7:
     cmp si,6
     jnz box8
     mov bl,s7+di
     jmp end_box
     
     ;s8
     box8:
     cmp si,7
     jnz end_box
     mov bl,s8+di
     
     end_box:
     
     mov s+si,bl
     
     inc si
     loop agn_box
     
     xor ax,ax
     xor si,si
     xor di,di
     xor cx,cx
     mov cx,4
     agn_arr:
     mov al,s+si
     shl al,4
     inc si
     mov ah,s+si
     or al,ah
     inc si
     mov s+di,al
     inc di
     loop agn_arr
     
     popa
     ret
endp access_s_boxes
;the proc arranges and permutate f
;get:s
;return:f
proc arrange_f
     pusha
     xor ax,ax
     xor si,si
     xor cx,cx
     
     mov cx,4
     xor si,si
     agnf:
     permute_key p,si,s,0
     mov al,temp_byte
     mov f+si,al
     inc si
     loop agnf
     
     
     popa
     ret
endp arrange_f
;the proc joins encrypted rl
;get:r,l
;return:rl
proc join_rl
     pusha
     xor ax,ax
     xor si,si
     xor cx,cx
     
     mov cx,4
     cpy_r:
     mov al,r+si
     mov rl+si,al
     inc si
     loop cpy_r
     
     xor ax,ax
     xor di,di
     mov cx,4
     cpy_l:
     mov al,l+di
     mov rl+si,al
     inc si
     inc di
     loop cpy_l
     
     xor di,di
     xor cx,cx
     xor ax,ax
     mov cx,8
     agn_pr:
     permute_key pc_minus,di,rl,0
     mov al,temp_byte
     mov enc_msg+di,al 
     inc di
     loop agn_pr
     popa
     ret
endp join_rl
;the proc joins decrypted lr
;get:l,r
;return:lr
proc  join_dec_lr
      pusha
      xor di,di
      xor cx,cx
      xor ax,ax
      xor si,si
      mov cx,4
      copy_l:
      mov al,l+si
      mov m+di,al
      inc si
      inc di
      loop copy_l
      
      
      xor cx,cx
      xor ax,ax
      xor si,si
      mov cx,4
      copy_r:
      mov al,r+si
      mov m+di,al
      inc si
      inc di
      loop copy_r
      popa
      ret
endp  join_dec_lr
;the proc divide the encrypted msg
;get:
;return: r,l
proc divide_enc_msg
     pusha
     xor cx,cx
     xor si,si
     xor di,di
     xor ax,ax
     mov cx,4
     copy_right:
     mov al,m+di
     mov r+si,al
     inc si
     inc di
     loop copy_right
     xor si,si
     mov cx,4
     copy_left:
     mov al,m+di
     mov l+si,al
     inc si
     inc di
     loop copy_left
     popa
     ret
endp divide_enc_msg     
  
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax
    ;WELCOME:
    ;reset screen
    call cln_scr
    set_cur 0,0
    print_msg welcome_msg
    ;new line
    mov dl,10
    mov ah,2
    int 21h
    ;print help
    print_msg help_msg
    ;new line
    mov dl,10
    mov ah,2
    int 21h
    ask_again:
    ;zeromsg
    xor si,si
    mov cx,8
    xoragn:
    mov m+si,0
    mov ekey+si,0
    inc si
    loop xoragn
    ;zeromsg ascii
    xor si,si
    mov cx,16
    xoragn2:
    mov m_ascii+si,0
    mov enc_msg_ascii+si,0
    mov dec_msg_ascii+si,0
    mov enc_msg+si,0
    mov dec_msg,0
    inc si
    loop xoragn2
    mov si,2
    mov cx,16
    xorkeyascii:
    mov ekey_ascii+si,0
    inc si
    loop xorkeyascii
    
    
    print_msg ask_command
    ;new line
    mov dl,10
    mov ah,2
    int 21h
    ;get input
    mov ah,1
    int 21h
    ;reset screen
    call cln_scr
    set_cur 0,0
    ;check input
    cmp al,'e'
    jz encrypt
    cmp al,'E'
    jz encrypt
    cmp al,"d"
    jz decrypt
    cmp al,"D"
    jz decrypt
    jmp error2
    
    
    
    encrypt:
    ;ENCRYPTION

    ;INPUT
    call get_msgnkey
    
    ;CALCULATIONS
    call arrange_k_plus
    call join_16_cd
    call generate_k
    call arrange_ip
    ;zero indexes
    mov r_index,0
    mov l_index,0
    xor cx,cx
    mov cx,16
    crt_l_r:
    call create_l
    call create_r    
    loop crt_l_r
    
    call join_rl
     
    ;OUTPUT
    hex2str enc_msg,enc_msg_ascii
    ;reset screen
    call cln_scr
    set_cur 0,0
    print_msg show_enc
    print_msg enc_msg_ascii
    
    ;new line
    mov dl,10
    mov ah,2
    int 21h
    jmp ask_again 
    
    
    decrypt:
    ;DECRYPTION
    ;INPUT
    call get_encnkey 
    
    ;CALCULATIONS
    
    mov cx,8
    xor si,si
    prmt:
    permute_key ip_tbl,si,enc_msg,0,0 
    mov al,temp_byte
    mov m+si,al
    inc si
    loop prmt
    call divide_enc_msg
    call arrange_k_plus
    call join_16_cd
    call generate_k
    
    xor cx,cx
    mov cx,16
    mov r_index,0fh
    mov l_index,0fh
    dec_l_r:
    call dec_r
    call dec_l
    loop dec_l_r
    
    call join_dec_lr
    mov cx,8
    xor si,si
    prmt2:
    permute_key pc_minus,si,m,0,0
    mov al,temp_byte
    mov dec_msg+si,al
    inc si
    loop prmt2
    
    ;OUTPUT
    hex2str dec_msg,dec_msg_ascii
    ;reset screen
    call cln_scr
    set_cur 0,0
    print_msg show_dec
    print_msg dec_msg_ascii
    
    ;new line
    mov dl,10
    mov ah,2
    int 21h
    jmp ask_again
 
    error2:
    ;reset screen
    call cln_scr
    set_cur 0,0
    print_msg error_msg
    ; wait for any key....    
    mov ah, 1
    int 21h
    ;new line
    mov dl,10
    mov ah,2
    int 21h
    jmp ask_again
    
    
   
     
    
    

    mov ax, 4c00h ; exit to operating system.
    int 21h 
    
    

end start ; set entry point and stop the assembler.
