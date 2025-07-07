here[];

main() {
    extrn here, smth;
here:
    goto smth;
}

here() {
    goto here;
}
