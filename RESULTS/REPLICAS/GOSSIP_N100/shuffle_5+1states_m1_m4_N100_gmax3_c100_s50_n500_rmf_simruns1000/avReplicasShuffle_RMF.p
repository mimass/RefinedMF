set key right bottom
set term postscript eps color 26
set output 'avReplicasRMF_n500.eps'
set xlabel 'time'
set ylabel 'number of replicas'
#'replicas (% of nodes)'
set pointsize 1.5
#plot [0:3000][] 'javashuffle2500-event.txt' u ($1):($2):($3) every 10 t 'protocol #simulation' pt 5 lw 5 with yerror, 'rmfReplicas_c100_s50_n500_fsharp.txt' u ($1):($2) #every 10 t 'refined mf' pt 13 lw 5, 'cmfReplicas_c100_s50_n500_fsharp.txt' u ($1):($2) #every 10 t 'classic mf' pt 13 lw 5 #with yerror
plot [0:1000][0:30] 'javashuffle100-event.txt' u ($1):($2):($3) every 10 t 'protocol simulation' pt 5 lw 5 with yerror, 'rmfReplicas_c100_s50_n500_fsharp.txt' u ($1):($2) every 30 t 'refined mf' w l dt (10,2,10,2) lc 2 lw 7, 'cmfReplicas_c100_s50_n500_fsharp.txt' u ($1):($2) every 10 t 'classic mf' w l dt 1 lc 3 lw 7, 'smfReplicas_c100_s50_n500_fsharp_N100.txt' u ($1):($2) every 10 t 'model simulation' w l dt 1 lc 4 lw 7 #with yerror w l dt (10,2,10,2) lc 2 lw 7

