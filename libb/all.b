printn(n,b) {
    extrn putchar, printn;
    auto a;

    if (a = n / b) /* assignment, not test for equality */
        printn(a, b); /* recursive */
    putchar(n % b + '0');
}

printf(fmt, x1, x2, x3, x4, x5, x6, x7, x8, x9) {
    extrn printn, char, putchar;
    auto adx, x, c, i, j;

    i = 0; /* fmt index */
    adx = &x1; /* argument pointer */
loop:
    while((c = char(fmt,i++)) != '%') {
        if(c == '*e')
            return;
        putchar(c);
    }
    x = *adx;
    adx =+ 8;
    switch c = char(fmt,i++) {
        case 'd': /* decimal */
        case 'o': /* octal */
            if(x < 0) {
                x = -x;
        	    putchar('-');
            }
            printn(x, c == 'o' ? 8 : 10);
            goto loop;

        case 'c': /* char */
            putchar(x);
            goto loop;

        case 's': /* string */
            while((c = char(x, j++)) != '*e')
                putchar(c);
            goto loop;
    }
    putchar('%');
    i--;
    adx =- 8;
    goto loop;
}
