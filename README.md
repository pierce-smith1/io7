# io7
An in-depth look into the compression format used to store Windows 95/98's boot graphics (which I am calling "IO7").

## Background 

In Windows 95 and 98, you can override the default boot splash by placing
your own bitmap in the root of the C: drive with the name `LOGO.SYS`. When
booting, Windows will display your bitmap instead of the default one.

But where is the original bitmap actually stored? As Wikipedia will tell us, 
it's kept in `IO.SYS`, a crucial DOS system file. But it's obscured with some 
kind of bespoke properietary compression, so we cannot extract or replace it.

Until now. Not being able to find a detailed description of this compression format
anywhere on the internet, I spent days hex-editing, educated guessing, and bug
hunting to fully understand how it worked, so that the world can finally unlock
what Microsoft has been trying to hide from us for almost 30 years.

...or, you know, you could just keep using the `LOGO.SYS` trick...

## Contents

* `io7.ts`: A full description of the IO7 format, and a TypeScript implementation of a decompressor and compressor
* `logo.io7`: Sample file for use with the script: the raw compressed bitmap data extracted directly from Windows 95's `IO.SYS` (offset `0x1B210` to offset `0x2B17A`)
* `logo98.io7`: Sample file for use with the script: the raw compressed bitmap data extracted directly from Windows 98's `IO.SYS` (offset `0x1E030` to offset `0x2E285`)
* `ftlogo.bmp`: Sample file for use with the script: An alternative boot splash in a `LOGO.SYS` compatible format that can be compressed and patched into `IO.SYS` on 95/98

## Running

For the benefit of those not familiar with TypeScript:

The easiest way to run a TypeScript file directly is to use `npm` to install `ts-node` globally, then run with `npx`:

```bash
apt install npm # (or equivalent for your platform)
npm install -g typescript ts-node
npx ts-node io7.ts
```

If that doesn't work, Google "how to run a typescript file" and be very confused for the next 30-60 minutes.

## Acknowledgements

I would have had no chance at understanding the IO7 format if it weren't for [this open source implementation](https://github.com/chenall/grub4dos/blob/3c1d05f39e49ec1d7543caa825df00068b96620b/stage2/builtins.c#L441-L621)
of a decompressor for a very similar format used in Windows ME. Thank you, bean, whoever and wherever you are.
