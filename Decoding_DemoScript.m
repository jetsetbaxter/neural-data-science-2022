%% Decoding demo script
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% This demo uses LDA with 5-fold cross-validation to classify data in 
% neurons.txt (features) and conditions.txt (labels)
% - plots the average 
% - creates a 3D scatter plot in feature space
% - classifies the data
% - reports accuracies
% - makes a confusion matrix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                        % ELR 2021 
%%
%load the data
neurons = importdata('Neurons.txt');
conditions = importdata('Conds.txt');

%%
% First, plot the mean response of the neurons to each
% condition

numneurons = length(neurons(1,:));

figure 
mn = [];
for k=1:numneurons
    for j=1:max(conditions)
        mn(j) = nanmean(neurons(find(conditions==j),k));
    end
    color = [.5 .5 .5];
    color(k) = .8;
    plot([1:max(conditions)],mn,'color',color,'LineWidth',2)
    hold on
end
xlim([0 4])
xlabel('Condition')
ylabel('Mean Responses')
legend('Feature 1','Feature 2','Feature 3')
set(gca,'FontSize',14)

% We can also visulaize the raw data in "feature space", i.e. in a plot
% where the axes correspond to each feature 
figure
for k=1:3
    color = [.5 .5 .5];
    color(k) = .8;
    scatter3(neurons(find(conditions==k),1),neurons(find(conditions==k),2),neurons(find(conditions==k),3),15,color)
    hold on
    %let's also plot the mean of the clusters as filled circles
    color = [.1 .1 .1];
    color(k) = .8;
    scatter3(mean(neurons(find(conditions==k),1)),mean(neurons(find(conditions==k),2)),mean(neurons(find(conditions==k),3)),60,color,'filled')
end
xlabel('Feature 1')
ylabel('Features 2')
zlabel('Feature 3')
legend('condition 1','c1 mean', 'condition 2','c2 mean', 'condition 3','c3 mean')
set(gca,'FontSize',14)

%%
% Now let's use a classifier to decode categories
% there are functions that will build data folds for you. Here I'll do
% it the long way

numtrials = length(conditions);
folds = 5;
trialorder = randperm(numtrials); %I'm randomizing trial order within folds - 
%this is a good practice, in case there's anything that changes over  
%consecutive trials, but it's not required

%this section finds the trial indices (inds) that will be in each fold of data
foldstart = 0;
inds = [];
for k=1:folds
    foldstart = foldstart+1;
    inds(:,k) = trialorder(foldstart:foldstart+numtrials/folds-1);
    foldstart = foldstart+numtrials/folds-1;
end
clear foldstart

%Now for each fold, identify the training and test data, and run a
%classifier and find the predicted categories
PredClass = [];
ActualClass = [];
for k= 1:folds
    test = neurons(inds(:,k),:); %this is the test data
    testcond = conditions(inds(:,k)); %these are the condition identities of the test data
    train = neurons;
    train(inds(:,k),:) = []; %remove the test data from the training set
    traincond = conditions;
    traincond(inds(:,k)) = []; %also remove the test data conditions from the training conditions
    lda = fitcdiscr(train,traincond); %this trains an LDA classifier and saves it as a variable
    ldaClass = predict(lda,test); %this uses the trained classifier (lda) to predict new data
    PredClass = [PredClass;ldaClass]; %these are the predicted categories
    ActualClass = [ActualClass;testcond]; %these are the actual categories
end

%Now find the overall accuraccy. Remember that chance accuracy is
%1/#possible categories
Accur = length(find(PredClass==ActualClass))/length(ActualClass);
chance = 1/length(unique(conditions));
disp(['Overall Accuracy = ' num2str(Accur*100) '% Chance = ' num2str(chance*100) '%']) 

% And the accuracy within each category
CatAccur = [];
for k = 1:max(conditions)
    CatAccur(k) = length(find(PredClass==ActualClass & ActualClass==k))/length(find(ActualClass==k));
end
disp(['Category Accuracies = ' num2str(CatAccur.*100)])

%%
% Now let's look at the confusion matrix

Confmat = NaN(max(conditions),max(conditions));

for actual = 1:max(conditions)
    for predicted = 1:max(conditions)
        Confmat(actual,predicted) = length(find(PredClass == predicted & ActualClass==actual))./length(find(ActualClass==actual));
    end
end

figure
imagesc(Confmat)
xlabel('Predicted Category')
ylabel('Actual Category')
title('Confusion Matrix')
set(gca,'FontSize',14)