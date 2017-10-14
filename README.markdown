# xxb

xxb is a fancy/silly xxd replacement I wrote in like 2015 or something, for fun and to learn about what extremely low-level Haskell was like. Features include:

* Ability to produce colored output
* Different methods for displaying bytes as one or two characters on the right-side column of the hexdump
* Speed, using (possibly gratuitous) bytewise pointer magic. When colored output is disabled, it runs faster than xxd on my computer!
* Although xxb outputs the byte positions just like xxd, it ignores the byte positions when reversing a file, making it much easier to insert, delete, and rearrange bytes in a hexdump. When I've used xxb, I've never wished for it to respect the old byte positions.
