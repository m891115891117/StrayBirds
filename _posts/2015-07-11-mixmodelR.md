    
    
    
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

