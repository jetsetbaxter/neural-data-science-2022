%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% demo script:
%   -loads text files LFP.txt and events.txt (both 1kHz sampling rate)
%   -aligns data to task events and plots the results
%   -smooths and plots the average response
%
% * make sure you're in the right directory or have added the directory with
%   the data to your path
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% load the data
fileID = fopen('LFP.txt');
data = fscanf(fileID,'%f');
fileID = fopen('events.txt');
events = fscanf(fileID,'%f');
clear fileID
%%
% visualize the whole time series with events
figure
plot(data,'color',[.5,.5,.5])
for k=1:length(events)
    line([events(k) events(k)],[min(data) max(data)],'color',[1 0 0],'LineWidth',.5);
end
%%
% separate the time series into "trials": 3000ms around each event
trials = NaN(length(events),3000);
times = (-999:2000);
for k=1:length(events)
    trials(k,:) = data(events(k)-999:events(k)+2000);
end
% visualize all trials in a heatplot
figure
imagesc(times,[],trials)
line([0 0],[0 549],'color',[1 0 0],'LineWidth',.5);
xlabel('Time from event (ms)')
ylabel('Trials')

%visualize the average response across trials
figure
plot(times,mean(trials,1),'color',[.5,.5,.5])
line([0 0],[min(mean(trials,1)) max(mean(trials,1))],'color',[1 0 0],'LineWidth',.5);
xlabel('Time from event (ms)')
ylabel('Average event-related response')
set(gca,'FontSize',12)

%%
% smooth the original data and plot again
datasm = smoothdata(data,'movmean',100);
trials = NaN(length(events),3000);
times = (-999:2000);
for k=1:length(events)
    trials(k,:) = datasm(events(k)-999:events(k)+2000);
end

figure
plot(times,mean(trials,1),'color',[.5,.5,.5])
line([0 0],[min(mean(trials,1)) max(mean(trials,1))],'color',[1 0 0],'LineWidth',.5);
xlabel('Time from event (ms)')
ylabel('Average event-related response')
set(gca,'FontSize',12)