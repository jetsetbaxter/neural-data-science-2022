%% Demo Script - Data mining
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%      - (1) Builds data with different underlying patterns
%      - (2) Runs PCA and K-means 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% Let's make some features that follow an underlying patterns
% We'll initialize the pattern as responses observed to 8 differnt
% conditions (of in 8 different observations)
pattern1 = [5 5 10 10 5 5 10 10];
var = 1; 

% Next we'll make a population of 10 features that follow this pattern with
% some noise
pop1 = []; 
% let's make 10 features per subpopulation
for k=1:10
    for j=1:8 %there are 8 observations 
        noise = normrnd(0,var); %this will add random noise, drawn from a gaussian centered at 0 with standard deviation = var, to each observation
        pop1(k,j) = pattern1(j)+noise; 
    end
end

%Let's plot the features - note that you can see the patterns in the first
%two subpopulations
figure
plot((1:8),pop1','LineWidth',2)
xlabel('Observations')
title('Data from 10 features')

%alternatively we can make a heatplot
figure
subplot(1,4,1)
imagesc(pop1)
ylabel('Features')
xlabel('Observations')
title('Data from 10 features')

% Now let's use PCA to see how it recovers patterns
[coeff,pcs,~,~,explained]=pca(pop1'); % note that the function that computes PCA has to take input matrices in the correct orientation (which may vay by platform)

% Let's look at scores first. These are the PCs - there are 7PCs, and 8 observations, so scores should be a 8x7 matrix
subplot(1,4,2)
%first plot the first PC - note that it recapitulates the pattern we made!
plot((1:8),pcs(:,1),'color',[0 0 0],'LineWidth',2)
title('PCs')
xlabel('Observations')
hold on
% now plot the rest of the PCs - note that there's not much variance there
for k=2:7
    plot((1:8),pcs(:,k))
end
xlim([0 10])
legend('PC1','PC2','PC3','PC4','PC5','PC6','PC7')   

% It looks like PC1 accounts for most of the variance - let's look closer
subplot(1,4,3)
% the output 'explained' is the variance explained by each PC
plot((0:7),[0;cumsum(explained)],'color',[0 0 0],'LineWidth',2)
ylabel('Cumulative variance explained (%)')
xlabel('PCs')
ylim([0 110])

% Finally, let's look at the PC loadings, or coefficients
subplot(1,4,4)
imagesc([],[],coeff,[-.7 .7])
title('PC loadings')
xlabel('PCs')
ylabel('Features')
% this shows that all observations contribute fairly equally to PC1, the
% most important in this data set, whereas lower PCs weigh particuarly on
% one or two observations - meaning they're capturing more idiosyncratic variance

%%
% Let's do it again with a more complex data set
% Additional patterns
pattern2 = [5 10 5 10 5 10 5 10];
pattern3 = [5 5 5 5 10 10 10 10];

% and create two more populations that follow different patterns
pop2 = []; pop3 = [];
for k = 1:10
    for j = 1:8
        noise = normrnd(0,var); % noise should be independent for this simulation
        pop2(k,j) = pattern2(j)+noise;
        noise = normrnd(0,var);
        pop3(k,j) = pattern3(j) + noise;
    end
end
pop = [pop1;pop2;pop3]; % Our full feature matrix is all of these subpopulations together
        
% first let's look at the data        
figure
subplot(1,4,1)
imagesc(pop)
l = line([.5 8.5],[10.5 10.5],'color',[0 0 0],'LineWidth',2);
l = line([.5 8.5],[20.5 20.5],'color',[0 0 0],'LineWidth',2);
ylabel('Features')
xlabel('Observations')
title('Data')

%Next we'll do PCA again and plot the results
[coeff,pcs,~,~,explained]=pca(pop'); 

subplot(1,4,2)
plot((1:8),pcs(:,1),'color',[0 0 0],'LineWidth',2)
title('PCs')
xlabel('Observations')
hold on
xlim([0 10])
for k=2:7
    plot((1:8),pcs(:,k)) % note that there are now 2 PCs with a lot of variance!
end
legend('PC1','PC2','PC3','PC4','PC5','PC6','PC7')   

subplot(1,4,3)
plot((0:7),[0;cumsum(explained)],'color',[0 0 0],'LineWidth',2)
ylabel('Cumulative variance explained (%)')
xlabel('PCs')
ylim([0 110])

subplot(1,4,4)
imagesc([],[],coeff,[-.7 .7])
title('PC loadings')
xlabel('PCs')
ylabel('Features')

%%
% let's project the data into PC space and try to reconstruct some signals

%first project your data onto PC1. This is done by taking the loading (or
%weight coefficient) for PC1 for each feature, and multiplying it by the
%original data. 
%(technical note - in Matlab, PCA rescales data so you may see the scaling doesn't
%match but the patterns do. there are ways to correct this but we'll ignore
%it for now)

pc1proj = [];
for k=1:length(pop(:,1))
    coeff1 = coeff(k,1); %find the PC1 loading for each feature
    x = pop(k,:); %and the original data for that feature
    pc1proj(k,:) = x.*coeff1;
end

%let's look at it
figure
subplot(1,3,1)
imagesc(pc1proj)
title('PC1 projected data')

%now let's do the same for pc2 and pc3
pc2proj = [];
pc3proj = [];
for k=1:length(pop(:,1))
    coeff2 = coeff(k,2); 
    x = pop(k,:); 
    pc2proj(k,:) = x.*coeff2;
    coeff3 = coeff(k,3); 
    x = pop(k,:); 
    pc3proj(k,:) = x.*coeff3;
end

subplot(1,3,2)
imagesc(pc2proj)
title('PC2 projected data')
subplot(1,3,3)
imagesc(pc3proj)
title('PC3 projected data')
% these plots will vary a little based on the randomly generated noise when we made the data and how PCA parses that, but
% there should be some parts of each pattern discernable in each pc, but
% the whole pattern isn't that clear
%here's another way to look at one feature:
i = 25; %select any feature (e.g. channel) to look at
figure
subplot(1,2,1)
plot(pop(i,:),'color',[0 0 0],'LineWidth',2) %plot the feature across all observations
set(gca,'YTickLabel',[])
subplot(1,2,2)
plot(pc1proj(i,:),'LineWidth',1.5) %now plot the same feature projected onto each PC
hold on
plot(pc2proj(i,:),'LineWidth',1.5)
plot(pc3proj(i,:),'LineWidth',1.5)
set(gca,'YTickLabel',[])


%%
% Now let's try clustering instead of PCA
% we'll use the same data in 'pop'

[idx,centers] = kmeans(pop,3); %first run kmeans with 3 clusters
figure
%plot the original data for comparison
subplot(3,4,[1 2 5 6 9 10])
imagesc(pop)
l = line([.5 8.5],[10.5 10.5],'color',[0 0 0],'LineWidth',2);
l = line([.5 8.5],[20.5 20.5],'color',[0 0 0],'LineWidth',2);
ylabel('Features')
xlabel('Observations')
title('Data')
% plot the cluster identity
subplot(3,4,[3 7 11])
imagesc(idx)
set(gca,'YTickLabel',([]))
set(gca,'XTickLabel',([]))
title('Cluster Index')

% Now look at the cluster centers
subplot(3,4,4)
plot(centers(1,:),'color',[0 0 0],'LineWidth',2)
title('Center 1')
subplot(3,4,8)
plot(centers(2,:),'color',[0 0 0],'LineWidth',2)
title('Center 2')
subplot(3,4,12)
plot(centers(3,:),'color',[0 0 0],'LineWidth',2)
title('Center 3')

%%
%what happens if we choose the wrong number of clusters?
% run the code below to see variability in the outcomes
[idx,centers] = kmeans(pop,2); 
figure
%plot the original data for comparison
subplot(2,4,[1 2 5 6])
imagesc(pop)
l = line([.5 8.5],[10.5 10.5],'color',[0 0 0],'LineWidth',2);
l = line([.5 8.5],[20.5 20.5],'color',[0 0 0],'LineWidth',2);
ylabel('Features')
xlabel('Observations')
title('Data')
% plot the cluster identity
subplot(2,4,[3 7])
imagesc(idx)
set(gca,'YTickLabel',([]))
set(gca,'XTickLabel',([]))
title('Cluster Index')

% Now look at the cluster centers
subplot(2,4,4)
plot(centers(1,:),'color',[0 0 0],'LineWidth',2)
title('Center 1')
subplot(2,4,8)
plot(centers(2,:),'color',[0 0 0],'LineWidth',2)
title('Center 2')

%%
%what happens if we choose the wrong number of clusters? - part II
% run the code below to see variability in the outcomes
[idx,centers] = kmeans(pop,4); 
figure
%plot the original data for comparison
subplot(4,4,[1 2 5 6 9 10 13 14])
imagesc(pop)
l = line([.5 8.5],[10.5 10.5],'color',[0 0 0],'LineWidth',2);
l = line([.5 8.5],[20.5 20.5],'color',[0 0 0],'LineWidth',2);
ylabel('Features')
xlabel('Observations')
title('Data')
% plot the cluster identity
subplot(4,4,[3 7 11 15])
imagesc(idx)
set(gca,'YTickLabel',([]))
set(gca,'XTickLabel',([]))
title('Cluster Index')

% Now look at the cluster centers
subplot(4,4,4)
plot(centers(1,:),'color',[0 0 0],'LineWidth',2)
title('Center 1')
subplot(4,4,8)
plot(centers(2,:),'color',[0 0 0],'LineWidth',2)
title('Center 2')
subplot(4,4,12)
plot(centers(3,:),'color',[0 0 0],'LineWidth',2)
title('Center 3')
subplot(4,4,16)
plot(centers(4,:),'color',[0 0 0],'LineWidth',2)
title('Center 4')

%%
% Use the elbow method to find the optimal number of clusters

numiter = 1000; %to find the optimal solution for each value of k, we'll need to rerun the algorithm with random seeds (random seeds are default in matlab)
numk = length(pop(:,1)); %we'll test each value of k until we have 1 per feature (e.g. neuron)
distances = [];
for k = 1:numk 
    d = [];
    for iter = 1:numiter
        [~,~,sumd] = kmeans(pop,k); %sumd is the sum of distances to each cluster center
        d(iter) = sum(sumd); %the total distance is the sum of sumd
    end
    distances(k) = min(d); %the minimum distance is the optimal solution for that k
end

% now plot the results
figure
plot((1:length(distances)),distances,'color',[0 0 0],'LineWidth',2)
xlabel('k')
ylabel('Sum of distances')
