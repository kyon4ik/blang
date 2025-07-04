nop() {
    return;
}

add(a, b) {
    return (a + b);
}

main() {
    extrn printf, nop, add;
    nop();
    printf("%d*n", add(34, 35));
}
