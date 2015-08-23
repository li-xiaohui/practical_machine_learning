Intro
-----

In this project, I will be using data from accelerometers on the belt,
forearm, arm, and dumbell of 6 participants and quantify how well each
participant did each activity. Participants were asked to perform
barbell lifts correctly and incorrectly in 5 different ways. More
information is available from the website here:
<http://groupware.les.inf.puc-rio.br/har>
(<http://groupware.les.inf.puc-rio.br/har>) (see the section on the
Weight Lifting Exercise Dataset).

Load libraries and set seed to ensure reproducibility of results.

Data preprocessing
------------------

In this section, \#DIV/0! were removed and replaced with NA values and
empty strings were replaced by NA values. Columns with every row being
NA are removed from training and testing data. To make sure learned
model can be applied to testing data, only complete cases are retained
in both training and testing data sets. Finally, the common features in
both training and testing data sets are used for training.

    pml_training <- read.csv(file="pml-training.csv", header=TRUE, na.strings=c("NA","#DIV/0!",""))
    dim(pml_training)

    ## [1] 19622   160

    #summary(pml_training)
     
    # function to remove columns with every row being NA
    removeNAcols   <- function(x) { x[, colSums( is.na(x) ) < nrow(x) ] }
     
    tr.na.rm <- removeNAcols(pml_training)
     
    # only take the complete cases
    tr.complete <- tr.na.rm[complete.cases(tr.na.rm), ]
     
     
    pml_testing <- read.csv(file="pml-testing.csv", header=TRUE, na.strings=c("NA","#DIV/0!",""))
     
    # only keep names that appear in training
    names <- names(tr.na.rm)
    names <- names[1: length(names)-1]
    testing <- pml_testing[, names]
     
    # remove columns with every row being NA
    testing <- removeNAcols(testing)
     
     
    # only keep columns that appear in testing
    tr.complete <- tr.complete[ , c(names(testing), "classe")]
     
    #split training into training and testing
    inTrain <- createDataPartition(y=tr.complete$classe, p=0.6, list=FALSE)
    tr.training <- tr.complete[inTrain, ]; tr.testing <- tr.complete[-inTrain, ]

Fit a decision tree model
-------------------------

We fit a decision tree model.

    treeFit<- rpart(classe ~ . , data=tr.training, method="class")
     
    #plot(treeFit)
    fancyRpartPlot(treeFit)

![](figs/decision%20tree%20model-1.png)

    tr.testing$tree_pred <- predict(treeFit, tr.testing, type="class")
     
    confusionMatrix(tr.testing$tree_pred, tr.testing$classe)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  A  B  C  D  E
    ##          A 21  0  0  0  0
    ##          B  0 18  0  0  0
    ##          C  0  1 14  0  0
    ##          D  0  0  0 14  1
    ##          E  0  0  0  0 16
    ## 
    ## Overall Statistics
    ##                                         
    ##                Accuracy : 0.976         
    ##                  95% CI : (0.918, 0.997)
    ##     No Information Rate : 0.247         
    ##     P-Value [Acc > NIR] : <2e-16        
    ##                                         
    ##                   Kappa : 0.97          
    ##  Mcnemar's Test P-Value : NA            
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity             1.000    0.947    1.000    1.000    0.941
    ## Specificity             1.000    1.000    0.986    0.986    1.000
    ## Pos Pred Value          1.000    1.000    0.933    0.933    1.000
    ## Neg Pred Value          1.000    0.985    1.000    1.000    0.986
    ## Prevalence              0.247    0.224    0.165    0.165    0.200
    ## Detection Rate          0.247    0.212    0.165    0.165    0.188
    ## Detection Prevalence    0.247    0.212    0.176    0.176    0.188
    ## Balanced Accuracy       1.000    0.974    0.993    0.993    0.971

The results is good with accuracy being 1.

Fit a random forest model
-------------------------

With random forest, we do repeated data cross validation by dividnig
data into 5 groups and repeat 3 times.

    cvCtrl <- trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 3)
     
    rfFit<- train(classe ~ . , data=tr.training, method="rf", trControl=cvCtrl, importance=TRUE)
     
    tr.testing$dt_pred <- predict(rfFit, tr.testing)
    confusionMatrix(tr.testing$dt_pred, tr.testing$classe)

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  A  B  C  D  E
    ##          A 21  0  0  0  0
    ##          B  0 18  0  0  0
    ##          C  0  1 14  0  0
    ##          D  0  0  0 14  1
    ##          E  0  0  0  0 16
    ## 
    ## Overall Statistics
    ##                                         
    ##                Accuracy : 0.976         
    ##                  95% CI : (0.918, 0.997)
    ##     No Information Rate : 0.247         
    ##     P-Value [Acc > NIR] : <2e-16        
    ##                                         
    ##                   Kappa : 0.97          
    ##  Mcnemar's Test P-Value : NA            
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity             1.000    0.947    1.000    1.000    0.941
    ## Specificity             1.000    1.000    0.986    0.986    1.000
    ## Pos Pred Value          1.000    1.000    0.933    0.933    1.000
    ## Neg Pred Value          1.000    0.985    1.000    1.000    0.986
    ## Prevalence              0.247    0.224    0.165    0.165    0.200
    ## Detection Rate          0.247    0.212    0.165    0.165    0.188
    ## Detection Prevalence    0.247    0.212    0.176    0.176    0.188
    ## Balanced Accuracy       1.000    0.974    0.993    0.993    0.971

We also want to investigate the variable importances by plotting the
importance levels of variables.

    varImpPlot(rfFit$finalModel)

![](figs/importance%20plot-1.png)

Generating submission files
---------------------------

Function to generate files with predictions to submit for assignment

    pred <- predict(rfFit, testing, type = "class")
     
    pml_write_files = function(x){
      n = length(x)
      for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
      }
    }
     
    pml_write_files(pred)
