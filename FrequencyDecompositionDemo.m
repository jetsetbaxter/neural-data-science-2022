%% Demo Script - Frequency Decomposition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%      - (1) Builds a signal of superimposed sine waves and computes a PSD
%      - (2) Loads LFP2.txt and events2.txt
%      -     Plots an ERP
%      -     Bandpasses in theta, alpha, beta and plots the filtered
%            signal, amplitude, and phase
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Let's build an artificial signal
% 
% specify some parameters
Fs = 1000;            % Sampling frequency (1kHz)                          
L = 5000;             % Length of signal (5sec)
t = (0:L-1)*(1/Fs);   % Time vector (in sec)

% make some sine waves
% each will be amp*sin(2*pi*freq*t) where amp = amplitude, freq =
% sine wave frequency and t = time vector
sinwaves(1,:) = sin(2*pi*2*t);
sinwaves(2,:) = .8*sin(2*pi*8*t);
sinwaves(3,:) = .6*sin(2*pi*20*t);
sinwaves(4,:) = .4*sin(2*pi*50*t);
sinwaves(5,:) = .2*sin(2*pi*120*t);
noise = 2*randn(size(t));

%Plot these to see them separately
figure
for k=1:5
    subplot(2,3,k)
    plot(t,sinwaves(k,:),'color',[0 0 0])
    xlim([0 1.5])
    ylim([-1 1])
end
subplot(2,3,6)
plot(t,noise,'color',[0 0 0])
xlim([0 1.5])

% Now add them all together - this is our fake signal
sig = sum(sinwaves,1) + noise;
figure
plot(t,sig,'color',[0 0 0])
title('Fake signal')


%% Bandpassing
% 
% We can recover parts of this summed signal with a bandpass filter
% here I will build my own simple FIR filter for bandpassing

Fs = 1000;
order = round(Fs); %determines the filter order
Nyquist=floor(Fs/2);
band = [1 3]; %let's recover frequencies between 1 and 3 Hz
MyFilt=fir1(order,band./Nyquist); %this is the filter
filt_data = filtfilt(MyFilt,1,sig); 

% plot the filtered data 
figure
plot(t,filt_data,'color',[0 0 0],'LineWidth',2)
title(['Bandpassed signal ' num2str(band(1)) ' - ' num2str(band(2)) ' Hz'])
xlabel('Time (ms)')
hold on
% and overlay the original 
plot(t,sinwaves(1,:),'color',[.8 0 0],'LineWidth',1.5,'LineStyle','--')
legend('recovered signal','original signal')

% let's find a passband that has two sinewaves in it
Fs = 1000;
order = round(Fs); %determines the filter order
Nyquist=floor(Fs/2);
band = [1 9]; %this will capture the 2hz and 8hz waves
MyFilt=fir1(order,band./Nyquist); %this is the filter
filt_data = filtfilt(MyFilt,1,sig); 

% plot the filtered data 
figure
plot(t,filt_data,'color',[0 0 0],'LineWidth',2)
title(['Bandpassed signal ' num2str(band(1)) ' - ' num2str(band(2)) ' Hz'])
xlabel('Time (ms)')
hold on
% and overlay the original 
plot(t,sinwaves(1,:),'color',[.8 0 0],'LineWidth',1)
plot(t,sinwaves(2,:),'color',[0 .8 .8],'LineWidth',1)
legend('recovered signal','2Hz signal','8Hz signal')


%% Power spectral densities

% Now let's compute a PSD
Y = fft(sig); % FFT gives a complex valued signal
PSD = abs(Y/L); % abs takes the real part of the signal
PSD = PSD(1:L/2+1); % FFT output is mirrored halfs - these two lines correct it
PSD(2:end-1) = 2*PSD(2:end-1);
f = Fs*(0:(L/2))/L;
figure
plot(f,PSD,'color',[0 0 0],'LineWidth',2)
xlim([0 150])
xlabel('Frequency (Hz)')
ylabel('Power')

%% Use these analyses with a real neural signal

% load the data
fileID = fopen('LFP2.txt');
data = fscanf(fileID,'%f');
fileID = fopen('events2.txt');
events = fscanf(fileID,'%f');
clear fileID

% First let's look at the LFP aligned to the events
trials = NaN(length(events),5000);
for k=1:length(events)
    trials(k,:) = data(events(k)-1999:events(k)+3000);
end
figure
plot([-1999:3000],mean(trials,1),'color',[0 0 0],'LineWidth',2)
xlabel('Time from event (ms)')
title ('Event Related Potential (ERP)') 

% Now, we could generate a PSD for the whole timeseries, but it's huge and would
% take a long time to compute. Instead let's plot the average PSD over
% trials
Fs = 1000;            % Sampling frequency (1kHz)                          
L = 5000;             % Length of signal (5sec)
t = (0:L-1)*(1/Fs);   % Time vector (in sec)

Y = fft(trials,[],2); 
PSD = abs(Y./L); 
PSD = PSD(:,1:L/2+1); 
PSD(:,2:end-1) = 2*PSD(:,2:end-1);
f = Fs*(0:(L/2))/L;

figure 
plot(f,mean(PSD,1),'color',[0 0 0],'LineWidth',2)
xlim([0 200])
xlabel('Frequency (Hz)')
ylabel('Power (AU)')
title('LFP2 PSD')
hold on

%there is a lot of line noise. Let's notch filter and try again
%Note: notch filtering should be done as a preprocessing step

notched_data = simple_notch_filter(data,[60 120 180],1000);

trials = NaN(length(events),5000);
for k=1:length(events)
    trials(k,:) = notched_data(events(k)-1999:events(k)+3000);
end

Y = fft(trials,[],2); 
PSD = abs(Y./L); 
PSD = PSD(:,1:L/2+1); 
PSD(:,2:end-1) = 2*PSD(:,2:end-1);
f = Fs*(0:(L/2))/L;
plot(f,mean(PSD,1),'color',[.8 .2 .2],'LineWidth',1.5,'LineStyle','--')
legend('Raw','Notch filtered')

% Now let's look at some band-passed signals
% Theta: 4-8 Hz
% Alpha: 8-12 Hz
% Beta: 13-30 Hz

bands=[4 8;8 12; 13 30];
% the bandpass filter will be rebuilt for each frequency, 
% so we will put it in a for loop

order = round(Fs); %determines the filter order
Nyquist=floor(Fs/2);

filt_data = [];
for k = 1:length(bands(:,1))
    MyFilt=fir1(order,bands(k,:)./Nyquist);
    filt_data(k,:) = filtfilt(MyFilt,1,notched_data); 
end

% plot a short length of the filtered data 
figure
for k=1:3
    subplot(3,3,k)
    plot(filt_data(k,1:3000),'color',[0 0 0])
    title([num2str(bands(k,1)) ' - ' num2str(bands(k,2)) ' Hz'])
end

% Now let's get the analytic amplitudes and phases and plot these
for k = 1:3
    hilb_data(k,:) = hilbert(filt_data(k,:)); %these are complex values
    amps(k,:) = abs(hilb_data(k,:)); %these are the amplitudes
    phases(k,:) = angle(hilb_data(k,:)); %these are the phases
    
    subplot(3,3,k+3)
    plot(filt_data(k,1:3000),'color',[0 0 0]) %replot the pass band
    hold on
    plot(amps(k,1:3000),'color',[.8 0 0],'LineWidth',1.5) %plot the amplitude
    title('amplitudes')
    subplot(3,3,k+6)
    plot(phases(k,1:3000),'color',[0 0 .8]) %plot the phases
    title('phases')
end
    