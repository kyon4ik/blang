.text
.globl _char
.globl _lchar

_char:
ldrb w0, [x0, x1]
ret

_lchar:
strb w2, [x0, x1]
ret
