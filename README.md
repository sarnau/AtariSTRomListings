# Atari ST XBIOS/BIOS ROM Listings

This repository contains a collection of Atari ST specific information I've compiled and generated from different sources.

## Atari ST Book Information
- Register Listing, which are specific to the Atari ST Book
- ST Book ROMDISK driver source code
The upper half of the ST Book ROM contains a ROM disk, which is mapped as drive 'P' into GEMDOS.
- POWER.PRG source code
The POWER.PRG for the ST Book manages the power states as a resident application.

## Shared Information
- TOS 2.06, TOS 3.06 and ST Book BIOS ROM Listing (not GEMDOS, VDI, AES, DESKTOP, which were written in C)
There are plenty of ROM listings for the Atari ST, but AFAIK nobody every did a comparison between the different TOS versions. I tried to copy as much comments as possible from the original source code, which is available, and added machine specific information to it.

The source has 3 EQUs at the beginning, which allow to compile it into 3 different TOS versions. It allows to see the difference between the machines.

I hope you enjoy it,
Markus Fritze
