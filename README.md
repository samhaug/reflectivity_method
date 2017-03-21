# reflectivity_method

ERZSOL3

This routine is designed for calculating the response of a stratified set of uniform solid layers to excitation by a point moment tensor source.
The calculations uisng slowness-frequency domain synthesis include all near field terms.

The model includes attenuation - constant Q (without causal corrections) with up to 500
layers in the standard settings.
Integrations are carried out over slowness (2500 slowness points currently allowed)
for each distance and then converted to time using an FFT.

The seismograms are written in a very simple format (ZST) that can be plotted using the accompanying zst routine or readily converted to other formats.

The algorithm is described in 
"Seismic Waves Propagation in Stratified Media" by B.L.N. Kennett
first published by Cambridge University press, 1993
Obtainable as PDF download from Http://rses.anu.edu.au/~brian

Source files:
 erzsol3.f  - self contained rotuiens for the response of the medium
 qbessel.f  - bessel functions for horizontal phase trerms
 qfcoolr.f  - FFT routine

 zst.f      - plotting routine for the simple zst format to produce record sections
 ps_rpost.f - postscript library
 bpltlib.f  - medium level plotting routines

Compilation:

 f77 -o bin/erzsol3  erzsol3.f qbessel.f qfcoolr.f

 f77 -o bin/zst-s    zst.f ps_rpost.f bpltlib.f

Manuals:
 erzsol3.html    - HTML file with full explanation of input parameters and zst format

 zst-manual.txt  - text file describing zst-s program

Input files:
 ew1.cmd     - input for erzsol3
 ew1.mod     - specification of 1-D earth model
 ew.wav      - source wavelet
 ew.dst      - ranges and azimuths

 zst.ew1.cmd - corresponding input for zst-s

To run:
 erzsol3 < ew1.cmd > ew1.out

 this produces an output file ew1.tx.z in zst format (as specified in  ew1.cmd)

 zst-s < zst.ew1.cmd

 produces Postscript output on zst.ps


