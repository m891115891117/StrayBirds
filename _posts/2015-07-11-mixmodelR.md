---
layout: post
title: mixed model
category: study
comments: true
---
   
    
    
    data<-read.table("School.txt",header=TRUE)
    attach(data)
    
    # You need to install the nlme package.
    library(nlme)
    
    # We will be using two functions from this package
    ?lme
    ?xyplot
    
    # This data set has 5 columns given test scores for 180 school children.  The children each 
    # took two different tests and we are interested in whether a child who achieved a high score
    # in the first test (score1) will achieve a high score in the second test (score2).  The data
    # includes three different schools (school) and three different classes (class) within each school.
    # Lastly the gender of each student is known (Sex).
    
    
    # This first thing to do is plot the data
    plot(score2,score1)
    
    # To find out if there is relationship between the two scores, we can do a simple linear regression.
    # Check the data meets the assumptions of a simple linear regression and then run
    
    mod1<-lm(score1~score2,data=data)
    summary(mod1)
    
    # However, there may be other factors that influence the score of test 1.  For instance, male students may
    # do better than female students on average (or vice versa).  We can include more variables in the model
    # and do a multiple linear regression
    
    mod1a<-lm(score1~score2+Sex,data=data)
    summary(mod1a)
    
    # You should find Sex not to have a significant effect on score 1 (after accounting for score2) and therefore
    # you should remove it from the model.  On the other hand, if sex was significant, then it should be 
    # included in the final model.
    
    
    # One of the assumptions of the linear model is that the samples are independent. However, we know that 
    # the samples were taken from three randomly selected schools.  There may be a difference between the three
    # schools.  For example, children from one school may do well in both test whereas children from another
    # school may do well in test one but badly in test two.  This will have an unwanted effect on the linear 
    # model results!   
    
    
    # To see if there is a difference between the three school, we will plot the scores for each school separately.
    
    xyplot(score1 ~ score2 | school, data=data,
     panel=function(x, y){
     panel.xyplot(x, y)
     panel.lmline(x, y, lty=2)
    }
    )
    
    # This gives three scatterplots, one for each school. The dotted line is a best fit line for each sample.
    # Is there a difference? To account for this, we can do a mixed effect model, and include school as a random
    # effect.  This just means that you are accounting for the fact you have randomly sampled three schools from
    # a wider population of schools.
    
    mod2<-lme(score1 ~ score2, data=data, random=~1|school)
    summary(mod2)
    
    # Lastly, we also know that within each school, three different classes were randomly sampled.  Again, this
    # may have an unwanted affect on the result and may need to be accounted for.  We will first compare the scores
    # for each class within each school separately:
    
    # School 1
    xyplot(score1 ~ score2 | class, data=data[data[,"school"]==1,],
     panel=function(x, y){
     panel.xyplot(x, y)
     panel.lmline(x, y, lty=2)
    }
    )
    
    # School 2
    xyplot(score1 ~ score2 | class, data=data[data[,"school"]==2,],
     panel=function(x, y){
     panel.xyplot(x, y)
     panel.lmline(x, y, lty=2)
    }
    )
    
    # School 3
    xyplot(score1 ~ score2 | class, data=data[data[,"school"]==3,],
     panel=function(x, y){
     panel.xyplot(x, y)
     panel.lmline(x, y, lty=2)
    }
    )
    
    # We can also account for this additional structure using a nested random effect.  Thist just means we are
    # accounting for the fact that we have randomly sampled three classes from three randomly selected schools.
    
    mod3<-lme(score1 ~ score2, data=data, random=~1|school/class)
    summary(mod3)
    
    
    # Now compare the results from the three separate models!  Acoounting for the underlying structure in the
    # data (i.e. schools and class) has an effect on the final conclusion.
    
    plot(score2, score1)
    abline(mod1$coefficients,lwd=2,col="red") #Red line is from the simple linear regression
    abline(mod2$coefficients[[1]],lwd=2,col="blue") #Blue line is from including school as random
    abline(mod3$coefficients[[1]],lwd=2,col="green") #Green line is from including the nested effect.
    
    # Within association studies, individuals belonging to the same family or sampling for a selection of
    # structured populations can influence results in a similar way to schools affecting the results of a
    # test.

        school class score1 score2 Sex
        1 1 1 59 90 Male
        2 1 1 31 100 Female
        3 1 1 31 96 Female
        4 1 1 35 92 Female
        5 1 1 29 88 Female
        6 1 1 41 93 Male
        7 1 1 34 99 Male
        8 1 1 44 100 Female
        9 1 1 29 93 Female
        10 1 1 58 100 Female
        11 1 1 40 86 Male
        12 1 1 36 100 Female
        13 1 1 58 84 Male
        14 1 1 38 89 Male
        15 1 1 36 88 Female
        16 1 1 48 100 Male
        17 1 1 42 98 Male
        18 1 1 40 86 Female
        19 1 1 44 100 Female
        20 1 1 36 72 Male
        21 1 2 53 85 Male
        22 1 2 67 78 Male
        23 1 2 56 70 Male
        24 1 2 51 70 Male
        25 1 2 53 72 Female
        26 1 2 62 100 Female
        27 1 2 65 87 Male
        28 1 2 59 86 Male
        29 1 2 70 87 Male
        30 1 2 66 72 Female
        31 1 2 44 85 Male
        32 1 2 41 78 Female
        33 1 2 63 74 Female
        34 1 2 68 70 Female
        35 1 2 49 89 Female
        36 1 2 71 77 Male
        37 1 2 64 81 Male
        38 1 2 59 71 Female
        39 1 2 66 93 Female
        40 1 2 68 73 Female
        41 1 3 57 52 Male
        42 1 3 55 53 Female
        43 1 3 53 46 Male
        44 1 3 48 60 Male
        45 1 3 47 52 Male
        46 1 3 45 58 Male
        47 1 3 55 55 Male
        48 1 3 54 56 Female
        49 1 3 56 47 Male
        50 1 3 54 68 Male
        51 1 3 50 58 Female
        52 1 3 58 47 Female
        53 1 3 58 63 Male
        54 1 3 50 53 Male
        55 1 3 58 57 Male
        56 1 3 62 60 Female
        57 1 3 54 51 Female
        58 1 3 63 57 Male
        59 1 3 53 53 Male
        60 1 3 55 51 Male
        61 2 1 65 43 Male
        62 2 1 55 32 Female
        63 2 1 71 27 Female
        64 2 1 63 40 Male
        65 2 1 45 31 Female
        66 2 1 82 28 Male
        67 2 1 66 28 Female
        68 2 1 57 20 Female
        69 2 1 53 35 Female
        70 2 1 49 33 Female
        71 2 1 71 35 Male
        72 2 1 62 24 Female
        73 2 1 43 33 Male
        74 2 1 44 23 Male
        75 2 1 54 45 Male
        76 2 1 57 14 Male
        77 2 1 74 17 Female
        78 2 1 63 25 Male
        79 2 1 50 39 Male
        80 2 1 49 19 Female
        81 2 2 100 30 Male
        82 2 2 95 4 Male
        83 2 2 100 28 Male
        84 2 2 91 36 Male
        85 2 2 84 32 Male
        86 2 2 100 39 Male
        87 2 2 94 28 Female
        88 2 2 69 42 Male
        89 2 2 96 25 Female
        90 2 2 83 47 Female
        91 2 2 89 24 Female
        92 2 2 88 25 Female
        93 2 2 82 35 Male
        94 2 2 86 18 Female
        95 2 2 87 30 Male
        96 2 2 97 14 Female
        97 2 2 84 31 Female
        98 2 2 80 36 Male
        99 2 2 100 19 Male
        100 2 2 85 24 Male
        101 2 3 64 36 Male
        102 2 3 81 38 Male
        103 2 3 62 38 Female
        104 2 3 60 42 Male
        105 2 3 61 32 Male
        106 2 3 71 39 Female
        107 2 3 69 31 Male
        108 2 3 67 45 Female
        109 2 3 64 50 Male
        110 2 3 72 44 Male
        111 2 3 69 41 Female
        112 2 3 70 41 Male
        113 2 3 72 36 Male
        114 2 3 71 32 Female
        115 2 3 68 40 Male
        116 2 3 67 34 Female
        117 2 3 66 37 Female
        118 2 3 69 45 Female
        119 2 3 71 36 Male
        120 2 3 68 40 Male
        121 3 1 32 56 Female
        122 3 1 29 51 Male
        123 3 1 46 58 Male
        124 3 1 45 39 Female
        125 3 1 47 70 Female
        126 3 1 54 38 Male
        127 3 1 38 51 Male
        128 3 1 35 47 Male
        129 3 1 36 35 Male
        130 3 1 35 40 Male
        131 3 1 26 40 Female
        132 3 1 49 62 Female
        133 3 1 59 43 Male
        134 3 1 25 46 Male
        135 3 1 51 45 Male
        136 3 1 40 40 Male
        137 3 1 32 39 Female
        138 3 1 47 40 Female
        139 3 1 35 48 Female
        140 3 1 32 43 Female
        141 3 2 69 81 Male
        142 3 2 65 97 Female
        143 3 2 70 88 Female
        144 3 2 70 71 Female
        145 3 2 68 73 Male
        146 3 2 74 100 Female
        147 3 2 73 82 Male
        148 3 2 70 72 Female
        149 3 2 74 92 Female
        150 3 2 67 74 Female
        151 3 2 63 82 Male
        152 3 2 63 90 Female
        153 3 2 67 54 Female
        154 3 2 63 74 Female
        155 3 2 81 74 Female
        156 3 2 64 86 Male
        157 3 2 71 83 Male
        158 3 2 77 92 Female
        159 3 2 86 78 Male
        160 3 2 73 72 Male
        161 3 3 56 57 Female
        162 3 3 51 58 Female
        163 3 3 53 60 Male
        164 3 3 52 64 Female
        165 3 3 63 60 Female
        166 3 3 60 61 Male
        167 3 3 54 60 Male
        168 3 3 55 53 Male
        169 3 3 59 67 Male
        170 3 3 54 54 Male
        171 3 3 57 55 Female
        172 3 3 58 60 Male
        173 3 3 51 67 Male
        174 3 3 64 67 Male
        175 3 3 59 63 Female
        176 3 3 48 55 Male
        177 3 3 57 56 Female
        178 3 3 58 57 Male
        179 3 3 53 57 Male
        180 3 3 57 66 Male
