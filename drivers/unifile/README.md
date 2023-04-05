# Apple /// driver for Unifile

Unifile/Duofile drivers have surfaced; this is the beginning of their disassembly and documentation.

The code for the driver (`df1.driver_code_0x2000`) was extracted from a working disk using Rob Justice's a3driverutil
project: https://github.com/robjustice/a3driverutil .
The disassembly can be reproduced with the `da65` tool, part of the `cc65` toolchain: https://github.com/cc65/cc65 .
The invocation looks like this:

```
da65 df1.driver_code_0x2000 -i unifile.info
```

There are several problems with the disassembly, most notably it isn't capable of making sense of the table of
addresses that are "minus-one" values of locations; it's used by the dispatcher to push program counter values
to the stack, then running an `RTS` instruction to branch to it.  `da65` isn't having it, so right now
these values are hacked to be `something-1` so we can at least get a view into what's going on.  That will need
to be (manually?) fixed up later.

Also, the constants for SOS return codes as well as zero page addresses can be ambiguous, so a bunch of those aren't
being assigned yet.
