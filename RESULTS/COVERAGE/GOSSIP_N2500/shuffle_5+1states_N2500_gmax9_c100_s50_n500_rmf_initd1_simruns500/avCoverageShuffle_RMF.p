set key right bottom
set term postscript eps color 26
set output 'mfCoverageShuffle_RMF.eps'
set xlabel 'time'
set ylabel 'number of nodes'
#'coverage (% of nodes)'
set pointsize 1.5
#plot [0:3000][] 'javashuffle900-event.txt' u ($1):($4):($5) every 10 t 'protocol #simulation' pt 5 lw 5 with yerror, 'rmfCoverage_c100_s50_n500_fsharp.txt' u ($1):($2) #every 10 t 'refined mf' pt 13 lw 5, 'cmfCoverage_c100_s50_n500_fsharp.txt' u ($1):($2) #every 10 t 'classic mf' pt 13 lw 5 #with yerror
plot [0:3000][0:3000] 'javashuffle2500-event.txt' u ($1):($4):($5) every 10 t 'protocol simulation' pt 5 lw 5 with yerror, 'rmfCoverage_c100_s50_n500_fsharp.txt' u ($1):($2) every 30 t 'refined mf' w l dt (10,2,10,2) lc 2 lw 7, 'cmfCoverage_c100_s50_n500_fsharp.txt' u ($1):($2) every 10 t 'classic mf' w l dt 1 lc 3 lw 7, 'smfCoverage_c100_s50_n500_fsharp.txt' u ($1):($2) every 10 t 'model simulation' w l dt 1 lc 4 lw 7 #with yerror w l dt (10,2,10,2) lc 2 lw 7

