"ERZSOL3-ew1  "                         Title
"ew1.tx.z"                              File for T-X seismogram output
"ew1.mod"                               Velocity model file
"HS"                                    Surface Condition (HS,H1,WF,WS)
  1500                                  Number of slownesses (<2500)
  0.0001                                Minimum slowness
  0.7001                                Maximum slowness
10                                      Slowness taper plo (n samples)
10                                      Slowness taper phi (n samples)
"WA"                                    Wavelet input or Ricker (WA/RI)
"ew.wav"                                Wavelet file
"YE"                                    Exponential damping? (YE/NO)
  4096                                  Number of time points
  0.005                                  Time step
   0.125  0.25                           Frequency taper (low)
   8.0    16.0                           Frequency taper (high)
   2.0                                  Dominant frequency      [RI]
   1.00      0.50       0.00            Moment tensor Components
   0.50      0.00       0.00
   0.00      0.00      -1.00
   3.0                                  Depth of source
"ew.dst"                                Range and azimuth file
  0.0                                   Reduction slowness
  0.000                                 Start time (reduced)
"NO"                                    Debug/frequency-wavenumber (YE/NO)
"NO"                                    Debug/waveform (YE/NO)
