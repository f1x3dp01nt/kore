This is a new version of Kore with new syntax and semantics,
implemented in Standard ML.

There are two versions implemented here, sharing common code:
* A traditional textual programming language (1Kore)
* A graphical programming language (2Kore)

## 1Kore

To compile 1kore, run `make 1kore`.

## 2Kore

It depends on mlton because we're using Mltons's FFI.  Later we may
switch to MLNLFFI which is more portable, just I haven't found any
documentation for it in my mlton distribution.  It depends on libdrm
(and Direct Rendering Manager support in the kernel) for framebuffer
access, which most modern Linux systems probably have. I heard OpenBSD
has it as well.  To compile 2Kore, run `make 2Kore`.  To run 2Kore, go
to a non-X VT (pressing ctrl+alt+fN where n is not 6,7, or 8 seems to
work for most systems) and run it. It may need root.

A low performance `X11/Wayland/Windows/Mac` compatible (so on top
something like GTK but only to use it for 2D rendering) version may be
added in the future.  The classic "Framebuffer" API of Linux will not
be used because it doesn't give a vblank signal (the API specifies it,
but no drivers implement it, except maybe 1 or 2 out of the 150. and
one of those is just the DRM-backed implementation)
