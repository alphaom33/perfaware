bits 16

mov bx, 1000; 4, bx <== 1000
mov bp, 2000; 4, bp <== 2000
mov si, 3000; 4, si <== 3000
mov di, 4000; 4, di <== 4000
mov cx, bx; 2, cx <== 1000
mov dx, 12; 4, dx <== 12
mov dx, [1000]; 14, dx <== 0
mov cx, [bx + 0]; 13, cx <== 0
mov cx, [bp]; 17, cx <== 0
mov [si + 0], cx; 14, [3000] <== 0
mov [di + 0], cx; 14, [4000] <== 0
mov cx, [bx + 1000]; 17, cx <== 0
mov cx, [bp + 1000]; 17, cx <== 0
mov [si + 1000], cx; 18, [4000] <== 0
mov [di + 1000], cx; 18, [5000] <== 0
add cx, dx; 3, cx <== 0 + 0, flags <== [PZ]
add [di + 1000], cx; 25, [5000] <== 0 + 0
add dx, 50; 4, dx <== 0 + 50, flags <== []

; cycles: 196

; ax: 0
; cx: 0
; dx: 50
; bx: 1000
; sp: 0
; bp: 2000
; si: 3000
; di: 4000
; es: 8192
; cs: 0
; ss: 4096
; ds: 0

; flags: []
; ip: 55

; memory: 
