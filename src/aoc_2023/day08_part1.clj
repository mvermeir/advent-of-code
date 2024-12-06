(ns aoc-2023.day08-part1)

(def input (list
  77757
  75684
  75324
  73077
  72966
  72658
  72208
  72050
  71968
  71861
  71400
  70753
  70421
  69637
  69486
  69477
  69423
  68943
  68852
  68785
  68723
  68695
  68333
  67907
  67885
  67866
  67823
  67616
  67259
  67105
  67015
  66712
  66601
  66168
  66165
  66092
  66081
  65976
  65756
  65730
  65494
  64751
  64538
  64493
  64370
  64033
  63917
  63883
  63844
  63673
  63633
  63611
  63552
  63550
  63520
  63461
  63288
  63032
  62835
  62767
  62480
  62405
  62263
  62238
  61998
  61894
  61822
  61759
  61264
  61152
  60946
  60899
  60793
  60743
  60273
  60204
  60182
  60073
  59929
  59763
  59677
  59543
  59175
  59130
  59058
  58935
  58925
  58777
  58725
  58547
  58343
  58191
  57685
  57424
  57309
  57068
  57029
  57016
  56833
  56813
  56710
  56658
  56655
  56532
  56527
  56454
  56388
  56254
  56097
  56054
  56004
  55906
  55876
  55815
  55596
  55520
  55520
  55468
  55307
  55302
  55057
  55047
  54801
  54695
  54598
  54584
  54549
  54507
  54500
  54479
  54438
  54336
  54252
  54194
  54175
  54140
  54055
  54027
  53982
  53870
  53673
  53651
  53647
  53627
  53591
  53559
  53459
  53365
  53202
  53178
  53160
  52925
  52915
  52909
  52869
  52631
  52519
  52431
  52225
  52210
  51874
  51821
  51719
  51683
  51591
  51541
  51526
  51420
  51344
  51341
  51283
  51212
  51196
  51194
  50984
  50952
  50802
  50763
  50758
  50718
  50703
  50660
  50591
  50394
  50275
  50265
  50155
  50134
  50094
  50056
  49937
  49886
  49855
  49795
  49684
  49556
  49515
  49485
  49385
  49330
  49308
  49275
  49179
  49072
  48980
  48946
  48885
  48859
  48768
  48710
  48676
  48641
  48497
  48493
  48452
  48342
  48239
  48228
  48201
  48175
  48037
  48029
  48028
  47968
  47802
  47734
  47724
  47666
  47508
  47471
  47351
  47242
  47204
  47147
  47142
  46955
  46909
  46746
  46723
  46693
  46510
  46484
  46403
  46189
  46083
  45575
  45255
  45187
  44789
  44558
  44413
  44308
  43889
  43778
  43661
  42839
  42784
  42428
  41715
  41667
  41560
  41225
  41222
  41209
  41034
  41018
  40943
  40783
  40658
  40444
  40278
  40207
  40192
  39991
  39923
  39815
  38963
  38480
  38347
  38333
  38316
  38085
  38067
  37997
  37932
  37901
  37568
  37368
  37357
  37334
  37307
  37256
  37160
  37138
  37107
  36940
  36639
  36409
  36408
  36223
36156
35869
35807
35771
35606
35268
35268
35181
34684
34642
34595
34571
34497
34421
34388
34367
34249
34245
34218
34193
33952
33920
33862
33781
33756
33682
33497
33374
33352
33312
33301
33292
33230
33141
33032
32967
32941
32885
32740
32728
32434
32414
32257
32077
32072
32040
31945
31907
31724
31494
31312
31238
31166
30833
30747
30309
30058
29625
29303
28614
27230
26257
25738
24296
17813
))


(comment (->> (reduce (fn [[tot xnt] x]
                   [(+ tot x) (inc xnt)])
                 [0 0]
                 input)
              ((fn [[tot cnt]] (/ tot cnt)))))
