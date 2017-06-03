; multi-segment executable file template.

data segment
    ;the message after converted to hexa
     m db 01h,23h,45h,67h,89h,0abh,0cdh,0efh 
    ;encryption key that was choosed by the user
    ekey db 13h,34h,57h,79h,9Bh,0BCh,0DFh,0F1h
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
           
         
    k_plus db 7 dup(00)
    selected_byte db ?
    selected_bit db  ?
    temp_byte db 00000000b
    byte_builder_pointer db 0
    pointer db offset pc_1
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
        
    
    
    
    
ends

stack segment
    dw   128  dup(0)
ends

code segment

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
          
    
    
    
    
    
    
    
    
    
    
    
    
;the proc rearrange the ekey according to the permution table pc-1, and then puts it in k_plus
;use selected_byte to know the specific byte in the ekey, use selected bit to know the specific bit it the byte                                          
;may use temp_byte as helper
macro permute_key pc_table,byte_no,key,addpara
     
     pusha
     LOCAL again,skip_add,skip_check,skip_1,skip_2,skip_3,skip_4,skip_5,skip_6,skip_7,bit_1,bit_2,bit_3,bit_4,bit_5,bit_6,bit_7,end_bit_0,end_bit_1,end_bit_2,end_bit_3,end_bit_4,end_bit_5,end_bit_6,end_bit_7,end_builder,endd
     xor ax,ax
     xor bx,bx
     xor cx,cx
     xor dx,dx
     mov pointer,0h
     mov temp_byte,00000000b
     mov byte_builder_pointer,0h
     mov ax,8
     mov dx,byte_no
     mul dx
     add ax,offset pc_table
     mov pointer,al
     xor ax,ax
     xor dx,dx


     mov cx,8
     again:
     xor bx,bx
     xor ax,ax
     mov bl,pointer
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
     add bl,addpara ;in case you want to add by a certain parameter you can use addpara otherwise keep it zero.beware addpara has to be byte size
     add bl,selected_byte ;bx gets the offset of the selected byte
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

proc arrange_k_plus
     
     pusha
     xor ax,ax
     ;1st byte
     permute_key pc_1,0,ekey,0
     mov al,temp_byte
     mov k_plus,al 
     ;2nd byte
     permute_key pc_1,1,ekey,0
     mov al,temp_byte
     mov k_plus+1,al
     ;3rd byte
     permute_key pc_1,2,ekey,0
     mov al,temp_byte
     mov k_plus+2,al       
     ;4th byte
     permute_key pc_1,3,ekey,0
     mov al,temp_byte
     mov k_plus+3,al
     ;5th byte
     permute_key pc_1,4,ekey,0
     mov al,temp_byte
     mov k_plus+4,al     
     ;6th byte
     permute_key pc_1,5,ekey,0
     mov al,temp_byte
     mov k_plus+5,al                    
     ;7th byte
     permute_key pc_1,6,ekey,0
     mov al,temp_byte
     mov k_plus+6,al                    
     
     
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

macro print_bin var,bytes_no
      local again,again2,skip,endd
      pusha
      xor cx,cx
      xor ax,ax
      xor si,si 
      mov cx,bytes_no
      again:
      mov al,var+si
      push cx
      mov cx,8
      again2:
      rcl al,1
      jnc skip
      push ax
      mov ah,02h
      mov dl,31h
      int 21h
      pop ax
      jmp endd
      skip:
      push ax
      mov ah,02h       
      mov dl,30h
      int 21h
      pop ax
      endd:
      loop again2
      inc si
      push ax
      mov ah,02h
      mov dl,20h
      int 21h
      pop ax
      pop cx
      loop again
      popa
endm print_bin 

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

proc create_l
     pusha
     xor si,si;l index pointer(0 is 1, 1 is 2 and so on..)
     xor di,di;l byte pointer 
     xor ax,ax
     xor cx,cx
     ;create l_1
     mov cx,4
     cpy:
     mov al,r_0+di
     mov l+di,al
     inc di
     loop cpy
     
     ;create l_2-16
     popa
     ret
endp create_l

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
      mov al,r_index
      mov bl,4
      mul bl 
      mov para,al
      sub para,4
      mov cx,6
      agn5:
      permute_key e_tbl,si,r,para
      mov bl,temp_byte
      mov e+si,bl
      inc si
      loop agn5
      endd_e:
      
      mov al,r_index
      mov bl,6
      mul bl
      mov para,al
      sub para,6
      xor_multibytes e,k,6
      call generate_8_b
      
      
      inc r_index
      popa
      ret
endp create_r
;the macro do xor for to multibytes arrays
;it gets 2 vars, and return the xored data to the 1st var
;also it gets no of bytes in each array
;add para to add certain parameter otherwise keep it zero
macro xor_multibytes var1,var2,bytes_no
      pusha
      xor si,si
      xor ax,ax
      xor dx,dx
      xor cx,cx
      xor di,di
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
     
      
     
      
      
      
     
      
      
     
        
      
     
     
     
     
         
    
start:
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax
    
    call arrange_k_plus
    call join_16_cd
    call generate_k
    call arrange_ip
    call create_r
    print_bin b,8
    mov ax, 4c00h ; exit to operating system.
    int 21h 
    
    

end start ; set entry point and stop the assembler.
