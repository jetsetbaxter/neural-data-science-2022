
%% Permutation test demo for trial-wise data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this script 
% - builds a "neuron" responding to 4 different (ordinal) conditions
% - performs a standard linear regression analysis to test encoding
% - performs a permutation test to test encoding
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
% First let's create a fake neuron 
% this will be a vector of average firing rates on each trial, where firing
% depends on the trial's condition (1 to 4)

numtrials = 500; %500 trials
varrange = [18 32]; % variance that will be added to firing rates
trials = NaN(numtrials,1); %preallocate a vector for trial-by-trial responses

firingrates = [];
firingrates(1,:) = [2:.1:5];
firingrates(2,:) = [5:.1:8];
firingrates(3,:) = [8:.1:11];
firingrates(4,:) = [11:.1:14]; %possible mean firing rates for each of 4 conditions
% (note: this script was written so you can build populations with similar
% responses but different means. You can also hard-code these means)

condition = [];
for k=1:numtrials
    condition(k,1) = randi(4); %randomly assign each trial to a condition
end

%define the mean response of the neuron to each condition
for k = 1:4
meanresponse(k,1) = randi(length(firingrates(1,:))); %index the firing mean firing rate
meanresponse(k,1) = firingrates(k,meanresponse(k)); % select the value
end
disp(['mean firing rates = ' num2str(meanresponse')]) %print the mean responses

%now create the neuron's response by adding noise from varrange to the mean
%response to the condition on each trial
var = randi(varrange);
for k = 1:numtrials
noise = normrnd(0,var);
trials(k,1) = meanresponse(condition(k)) +noise;
end

%Let's plot the neuron's response to make sure it looks right
figure
mfr = []; stfr = [];
for k=1:max(condition)
    mfr(k) = nanmean(trials(find(condition==k)));
    stfr(k) = nanstd(trials(find(condition==k)))./sqrt(length(find(condition==k))); %SEM
end
errorbar(mfr,stfr,'color',[0 0 0],'LineWidth',2)
xlim([0 5])
xlabel('Condition Number')
ylabel('Average Firing rate (Hz)')

%great! we made a neuron whose average response increases across
%conditions, with some reasonable amount of variance

%%
% Next, let's do some stats on this neuron's response 
% Since condition is an ordinal variable, we can use a linear regression

% there are a number of different functions in matlab that will do linear
% regression - they should all give the same results if you use them
% correctly. Here I'll use the old school glmfit
[~,~,stats] = glmfit(condition,trials);
p = stats.p(2); %note that the first p-value in the stats structure is for the model's intercept. The rest are for the model predictors
beta = stats.beta(2); %same for beta

disp(['Regression p = ' num2str(p) ' beta = ' num2str(beta)]) %print the stats

% this shows a positive beta value, meaning that firing rate increases
% across condition number. This beta value (the slope) is
% significantly non-zero (the p-value)

% note that you can get the same p-value with a simple Pearson correlation
[r,p]=corr(condition,trials);

disp(['Pearson Corr p = ' num2str(p) ' r = ' num2str(r)]) %print the stats

    
%%
% Next, let's compare this significance test to a permutation test
% we'll do 10,000 shuffles to create our null distribution

numshuf = 10000;
Rs = NaN(numshuf,1); %we'll use the r-values from Pearson correlations to test whether the trend across conditions is significant.
% note we could use the beta values if we wanted to test whether the slope
% is non-zero

for shuf = 1:numshuf
    i = randperm(length(condition)); %generate randomly shuffled indices
    shufcond = condition(i); % now shuffle the conditions
    Rs(shuf) = corr(shufcond,trials); %measure the r value of the correlation from the shuffled trials
end

% plot the shuffled distribution
figure
histogram(Rs)
xlabel('R value')
ylabel('number of observations')
hold on
% this is now your null distribution because you generated it by taking the
% original data and destroying the relationship you aim to test (firing
% rate vs condition number)

% now see where your real correlation (r) falls in this distribution
ymax = ylim;
ymax = ymax(2);
line([r r],[0 ymax],'color',[0 0 0],'LineWidth',1);
% it's far in the tail of the distribution, meaning you only rarely (if
% ever) get an r value that extreme by chance

% can get a p-value empirically as the proportion of shuffled data above
% your r value, though the precision of this is going to depend on how many
% shuffles you have
p_permute = length(find(abs(Rs)>=abs(r)))/numshuf;
% this is computing a two-sided p-value, so it's using absolute values to
% find Rs at least as extreme as r (regardless of sign)

disp(['Pearson p = ' num2str(p) ' Permutation test p = ' num2str(p_permute)])

% run this script a few times to generate new neurons - change the noise
% and/or mean firing rates and see how well the permutation tests do


    
